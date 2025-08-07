{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
-- | This module uses template Haskell. Following is a simplified explanation of usage for those unfamiliar with calling Template Haskell functions.
--
-- The function 'embedFile' in this modules embeds a file into the executable
-- that you can use it at runtime. A file is represented as a 'ByteString'.
-- However, as you can see below, the type signature indicates a value of type
-- @Code m ByteString@ will be returned. In order to convert this into a 
-- 'ByteString', you must use Typed Template Haskell syntax, e.g.:
--
-- > $$(embedFile "myfile.txt")
--
-- This expression will have type 'ByteString'. Be certain to enable the
-- TemplateHaskell language extension, usually by adding the following to the
-- top of your module:
--
-- > {-# LANGUAGE TemplateHaskell #-}
--
-- Note that this module is the typed variant of @Data.FileEmbed@.
-- As a result, you'll have to use 'bindCode' to use 'Data.FileEmbed.RelativePath'
-- with the functions in this module, and as such it is re-exported for your
-- convenience.
module Data.FileEmbed.Typed
    ( -- * Embed at compile time
      embedFile
    , embedFileRelative
    , embedFileIfExists
    , embedOneFileOf
    , embedDir
    , embedDirListing
      -- * Embed as a IsString
    , embedStringFile
    , embedOneStringFileOf
      -- * Re-exports
    , bindCode
      -- * Internals
    , bsToExp
    , strToExp
    ) where

import Language.Haskell.TH.Syntax
    ( Quasi(..), Quote, Code, unsafeCodeCoerce, bindCode, Exp (LitE), bindCode_,
    )
import Language.Haskell.TH ( mkBytes, bytesPrimL )
import qualified Data.ByteString.Internal as B
import System.Directory (doesDirectoryExist, doesFileExist,
                         getDirectoryContents)
import Control.Exception (tryJust)
import Control.Monad (filterM, guard)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Control.Arrow ((&&&))
import Data.ByteString.Unsafe (unsafePackAddressLen)
import System.IO.Error (isDoesNotExistError)
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath ((</>))
import Data.String (fromString, IsString)
import Prelude as P
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Functor (($>))
import GHC.Exts (Addr#)
import Data.Bitraversable (bitraverse)
import Data.FileEmbed.RelativePath (makeRelativeToProject)

-- | Embed a single file in your source code.
--
-- > import qualified Data.ByteString
-- >
-- > myFile :: Data.ByteString.ByteString
-- > myFile = $$(embedFile "dirName/fileName")
--
-- @since 0.1.0.0
embedFile :: (Quote m, Quasi m) => FilePath -> Code m B.ByteString
embedFile fp =
    (qAddDependentFile fp >> qRunIO (B.readFile fp)) `bindCode` bsToExp

-- | Embed a single file in your source code.
--   Unlike 'embedFile', path is given relative to project root.
--
-- @since 0.1.0.0
embedFileRelative :: (Quote m, Quasi m) => FilePath -> Code m B.ByteString
embedFileRelative fp = makeRelativeToProject fp `bindCode` embedFile

-- | Maybe embed a single file in your source code depending on whether or not file exists.
--
-- Warning: When a build is compiled with the file missing, a recompile when the
-- file exists might not trigger an embed of the file. You might try to fix this
-- by doing a clean build or using GHC's -fforce-recomp flag.
--
-- > import qualified Data.ByteString
-- >
-- > maybeMyFile :: Maybe Data.ByteString.ByteString
-- > maybeMyFile = $$(embedFileIfExists "dirName/fileName")
--
-- @since 0.1.0.0
embedFileIfExists :: (Quote m, Quasi m) => FilePath -> Code m (Maybe B.ByteString)
embedFileIfExists fp = do
  maybeFile `bindCode` \case
    Nothing -> [|| Nothing ||]
    Just bs -> [|| Just $$(bsToExp bs) ||]
  where
    maybeFile :: Quasi m => m (Maybe B.ByteString)
    maybeFile = do
      qRunIO (tryJust (guard . isDoesNotExistError) (B.readFile fp)) >>= \case
        Left _ -> pure Nothing
        Right bs -> qAddDependentFile fp $> Just bs

-- | Embed a single existing file in your source code
-- out of list a list of paths supplied.
--
-- Warning: When a build is compiled with initial file(s) in the list missing, a
-- recompile when one of those files exists might not trigger an embed of the
-- first such file. You might try to fix this by doing a clean build or using
-- GHC's -fforce-recomp flag.
--
-- > import qualified Data.ByteString
-- >
-- > myFile :: Data.ByteString.ByteString
-- > myFile = $$(embedOneFileOf [ "dirName/fileName", "src/dirName/fileName" ])
--
-- @since 0.1.0.0
embedOneFileOf :: (Quote m, Quasi m) => [FilePath] -> Code m B.ByteString
embedOneFileOf ps =
  readExistingFile B.readFile ps `bindCode` bsToExp

-- | Utility function to read a file and add it as a dependent file using the
-- given file reader.
readExistingFile :: Quasi m => (FilePath -> IO s) -> [FilePath] -> m s
readExistingFile readFile' xs = do
  ys <- qRunIO $ filterM doesFileExist xs
  case ys of
    (p:_) -> qRunIO (readFile' p) >>= (qAddDependentFile p $>)
    _ -> error "Cannot find file to embed as resource"

-- | Embed a directory recursively in your source code.
--
-- Warning: When a build is compiled with a file missing from the directory, a
-- recompile when that file is added might not trigger an embed of the file. You
-- might try to fix this by doing a clean build or using GHC's -fforce-recomp
-- flag.
--
-- > import qualified Data.ByteString
-- >
-- > myDir :: [(FilePath, Data.ByteString.ByteString)]
-- > myDir = $$(embedDir "dirName")
--
-- @since 0.1.0.0
embedDir :: (Quasi m, Quote m) => FilePath -> Code m [(FilePath, B.ByteString)]
embedDir fp = do
  qRunIO (getDir fp) `bindCode` (convertList . fmap (pairToExp fp))
  where
  pairToExp :: forall m . (Quote m, Quasi m) => FilePath -> (FilePath, B.ByteString) -> Code m (FilePath, B.ByteString)
  pairToExp root (path, bs) = do
    qAddDependentFile @m (root ++ '/' : path) `bindCode_` let bs' = bsToExp bs in [||(path, $$bs')||]

-- | Embed a directory listing recursively in your source code.
--
-- > myFiles :: [FilePath]
-- > myFiles = $(embedDirListing "dirName")
--
-- @since 0.1.0.0
embedDirListing :: (Quote m, Quasi m) => FilePath -> Code m [FilePath]
embedDirListing fp = do
  qRunIO (getDir fp) `bindCode` (convertList . fmap (strToExp . fst))

-- | Utility to turn a list of Codes into a Code of a list.
convertList :: (Quote m) => [Code m a] -> Code m [a]
convertList =
  foldr (\a z -> [|| $$a : $$z ||]) [|| [] ||]

{-# WARNING in "x-file-embed-internals" bsToExp "This function is meant for internal `file-embed` use" #-}
-- | Embed a bytestring into a static string.
bsToExp :: Quote m => B.ByteString -> Code m B.ByteString
bsToExp bs =
  [|| unsafePerformIO (unsafePackAddressLen (fromIntegral $ B8.length bs) $$addr) ||]
  where
  -- can't lift Bytes into an Addr# directly
  addr :: Quote m => Code m Addr#
  addr = unsafeCodeCoerce $ pure $ LitE $ bytesPrimL (
    let B.PS ptr off sz = bs
    in  mkBytes ptr (fromIntegral off) (fromIntegral sz))

-- | Embed a single file in your source code.
--
-- > import Data.String
-- >
-- > myFile :: IsString a => a
-- > myFile = $$(embedStringFile "dirName/fileName")
--
-- @since 0.1.0.0
embedStringFile :: (IsString s, Quote m, Quasi m) => FilePath -> Code m s
embedStringFile fp = (qAddDependentFile fp >> qRunIO (P.readFile fp)) `bindCode` strToExp

-- | Embed a single existing string file in your source code
-- out of list a list of paths supplied.
--
-- Warning: When a build is compiled with initial file(s) in the list missing, a
-- recompile when one of those files exists might not trigger an embed of the
-- first such file. You might try to fix this by doing a clean build or using
-- GHC's -fforce-recomp flag.
--
-- @since 0.1.0.0
embedOneStringFileOf :: (IsString s, Quote m, Quasi m) => [FilePath] -> Code m s
embedOneStringFileOf ps =
  readExistingFile P.readFile ps `bindCode` strToExp

{-# WARNING in "x-file-embed-internals" strToExp "This function is meant for internal `file-embed` use" #-}
-- | Lifts a stringy value into TH.
strToExp :: (IsString s, Quote m) => String -> Code m s
strToExp s = [|| fromString s ||]
