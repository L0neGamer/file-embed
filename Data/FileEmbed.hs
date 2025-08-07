{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
-- | This module uses template Haskell. Following is a simplified explanation of usage for those unfamiliar with calling Template Haskell functions.
--
-- The function 'embedFile' in this modules embeds a file into the executable
-- that you can use it at runtime. A file is represented as a 'ByteString'.
-- However, as you can see below, the type signature indicates a value of type
-- @Q Exp@ will be returned. In order to convert this into a 'ByteString', you
-- must use Template Haskell syntax, e.g.:
--
-- > $(embedFile "myfile.txt")
--
-- This expression will have type 'ByteString'. Be certain to enable the
-- TemplateHaskell language extension, usually by adding the following to the
-- top of your module:
--
-- > {-# LANGUAGE TemplateHaskell #-}
--
-- We also have @Data.FileEmbed.Typed@ for typed versions of @Q Exp@ functions
-- in this module.
module Data.FileEmbed
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
      -- * Inject into an executable
      -- $inject
    , dummySpace
    , dummySpaceWith
    , inject
    , injectFile
    , injectWith
    , injectFileWith
      -- * Relative path manipulation
    , makeRelativeToProject
    , makeRelativeToLocationPredicate
    , getDir
    -- * Internal
    , stringToBs
    , bsToExp
    , strToExp
    ) where

import qualified Data.FileEmbed.Typed as Typed
import Language.Haskell.TH (Exp (AppE, VarE), Q, unTypeCode, Code)
import Data.FileEmbed.Injection
    ( dummySpace,
      dummySpaceWith,
      inject,
      injectFile,
      injectWith,
      injectFileWith )
import Data.FileEmbed.RelativePath
    (makeRelativeToProject, makeRelativeToLocationPredicate, getDir)
import Data.String (fromString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

-- | Embed a single file in your source code.
--
-- > import qualified Data.ByteString
-- >
-- > myFile :: Data.ByteString.ByteString
-- > myFile = $(embedFile "dirName/fileName")
embedFile :: FilePath -> Q Exp
embedFile = unTypeCode . Typed.embedFile

-- | Embed a single file in your source code.
--   Unlike 'embedFile', path is given relative to project root.
-- @since 0.0.16.0
embedFileRelative :: FilePath -> Q Exp
embedFileRelative = unTypeCode . Typed.embedFileRelative

-- | Maybe embed a single file in your source code depending on whether or not file exists.
--
-- Warning: When a build is compiled with the file missing, a recompile when the
-- file exists might not trigger an embed of the file. You might try to fix this
-- by doing a clean build or using GHC's -fforce-recomp flag.
--
-- > import qualified Data.ByteString
-- >
-- > maybeMyFile :: Maybe Data.ByteString.ByteString
-- > maybeMyFile = $(embedFileIfExists "dirName/fileName")
--
-- @since 0.0.14.0
embedFileIfExists :: FilePath -> Q Exp
embedFileIfExists = unTypeCode . Typed.embedFileIfExists

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
-- > myFile = $(embedOneFileOf [ "dirName/fileName", "src/dirName/fileName" ])
embedOneFileOf :: [FilePath] -> Q Exp
embedOneFileOf = unTypeCode . Typed.embedOneFileOf

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
-- > myDir = $(embedDir "dirName")
embedDir :: FilePath -> Q Exp
embedDir = unTypeCode . Typed.embedDir

-- | Embed a directory listing recursively in your source code.
--
-- > myFiles :: [FilePath]
-- > myFiles = $(embedDirListing "dirName")
--
-- @since 0.0.11
embedDirListing :: FilePath -> Q Exp
embedDirListing = unTypeCode . Typed.embedDirListing

-- | Embed a single file in your source code.
--
-- > import Data.String
-- >
-- > myFile :: IsString a => a
-- > myFile = $(embedStringFile "dirName/fileName")
--
-- Since 0.0.9
embedStringFile :: FilePath -> Q Exp
embedStringFile = stringyAsIsString . Typed.embedStringFile

-- | Embed a single existing string file in your source code
-- out of list a list of paths supplied.
--
-- Warning: When a build is compiled with initial file(s) in the list missing, a
-- recompile when one of those files exists might not trigger an embed of the
-- first such file. You might try to fix this by doing a clean build or using
-- GHC's -fforce-recomp flag.
--
-- Since 0.0.9
embedOneStringFileOf :: [FilePath] -> Q Exp
embedOneStringFileOf = 
  stringyAsIsString . Typed.embedOneStringFileOf

-- | Constrain the type of the embeded stringy values, and then `fromString` them
-- again.
stringyAsIsString :: Code Q String -> Q Exp
stringyAsIsString s = (VarE 'fromString `AppE`) <$> unTypeCode s

{-# WARNING in "x-file-embed-internals" bsToExp "This function is meant for internal `file-embed` use" #-}
-- | Embed a bytestring into a static string.
bsToExp :: B.ByteString -> Q Exp
bsToExp = unTypeCode . Typed.bsToExp

{-# WARNING in "x-file-embed-internals" strToExp "This function is meant for internal `file-embed` use" #-}
-- | Lifts a stringy value into TH.
strToExp :: String -> Q Exp
strToExp = stringyAsIsString . Typed.strToExp

{-# DEPRECATED stringToBs "Use Data.ByteString.Char8.pack instead" #-}
stringToBs :: String -> B.ByteString
stringToBs = B8.pack
