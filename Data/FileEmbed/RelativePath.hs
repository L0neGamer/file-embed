{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.FileEmbed.RelativePath
  ( makeRelativeToProject,
    makeRelativeToLocationPredicate,
    getDir,
  )
where

import Control.Arrow ((&&&))
import Control.Monad (filterM)
import Data.Bitraversable (bitraverse)
import qualified Data.ByteString as B
import Data.List (sortBy)
import Data.Ord (comparing)
import Language.Haskell.TH.Syntax
  ( Quasi (..),
    loc_filename,
    qLocation,
  )
import System.Directory
  ( canonicalizePath,
    doesDirectoryExist,
    doesFileExist,
    getDirectoryContents,
  )
import System.FilePath (takeDirectory, takeExtension, (</>))
import Prelude as P

-- | Take a relative file path and attach it to the root of the current
-- project.
--
-- The idea here is that, when building with Stack, the build will always be
-- executed with a current working directory of the root of the project (where
-- your .cabal file is located). However, if you load up multiple projects with
-- @stack ghci@, the working directory may be something else entirely.
--
-- This function looks at the source location of the Haskell file calling it,
-- finds the first parent directory with a .cabal file, and uses that as the
-- root directory for fixing the relative path.
--
-- @$(makeRelativeToProject "data/foo.txt" >>= embedFile)@
--
-- @$$(makeRelativeToProject "data/foo.txt" \`bindCode\` Typed.embedFile)@
--
-- @since 0.0.10
makeRelativeToProject :: (Quasi m) => FilePath -> m FilePath
makeRelativeToProject = makeRelativeToLocationPredicate $ (==) ".cabal" . takeExtension

-- | Take a predicate to infer the project root and a relative file path, the given file path is then attached to the inferred project root
--
-- This function looks at the source location of the Haskell file calling it,
-- finds the first parent directory with a file matching the given predicate, and uses that as the
-- root directory for fixing the relative path.
--
-- @$(makeRelativeToLocationPredicate ((==) ".cabal" . takeExtension) "data/foo.txt" >>= embedFile)@
--
-- @since 0.0.15.0
makeRelativeToLocationPredicate :: (Quasi m) => (FilePath -> Bool) -> FilePath -> m FilePath
makeRelativeToLocationPredicate isTargetFile rel = do
  loc <- qLocation
  qRunIO $ do
    srcFP <- canonicalizePath $ loc_filename loc
    mdir <- findProjectDir srcFP
    case mdir of
      Nothing -> error $ "Could not find .cabal file for path: " ++ srcFP
      Just dir -> return $ dir </> rel
  where
    findProjectDir x = do
      let dir = takeDirectory x
      if dir == x
        then return Nothing
        else do
          contents <- getDirectoryContents dir
          if any isTargetFile contents
            then return (Just dir)
            else findProjectDir dir

-- not the same as the other two functions but gets files relative to a root

-- | Given a root folder, recursively get all files found in all subdirectories.
-- Sorts by filepath.
--
-- Skips "hidden" files; specifically, those with a `.` at the front.
getDir :: FilePath -> IO [(FilePath, B.ByteString)]
getDir root = fileList ""
  where
    fileList :: FilePath -> IO [(FilePath, B.ByteString)]
    fileList top = do
      allContents <- filter notHidden <$> getDirectoryContents (root </> top)
      let -- relative paths from root and absolute paths from root for each file
          relAndAbsPaths :: [(FilePath, FilePath)] = map ((top </>) &&& ((root </> top) </>)) allContents
      files <-
        filterM (doesFileExist . snd) relAndAbsPaths
          >>= mapM (bitraverse pure B.readFile)
      dirs <-
        filterM (doesDirectoryExist . snd) relAndAbsPaths
          >>= mapM (fileList . fst)
      return $ sortBy (comparing fst) $ concat $ files : dirs
      where
        notHidden :: FilePath -> Bool
        notHidden ('.' : _) = False
        notHidden _ = True
