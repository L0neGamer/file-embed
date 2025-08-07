{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Data.FileEmbed.Injection
  ( -- * Inject into an executable
    -- $inject
    dummySpace,
    dummySpaceTyped,
    dummySpaceWith,
    dummySpaceWithTyped,
    inject,
    injectFile,
    injectWith,
    injectFileWith,
  )
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Unsafe (unsafePackAddressLen)
import GHC.Exts (Addr#)
import Language.Haskell.TH.Syntax
  ( Code,
    Exp (LitE),
    Lit (..),
    Q,
    Quote,
    unTypeCode,
    unsafeCodeCoerce,
  )
import System.IO.Unsafe (unsafePerformIO)
import Prelude as P

magic :: B.ByteString -> B.ByteString
magic x = B8.concat ["fe", x]

sizeLen :: Int
sizeLen = 20

-- | Snap off the front of the bytestring which contains our magic data, then
-- take a part of the rest of the bytestring.
getInner :: B.ByteString -> B.ByteString
getInner b =
  let (sizeBS, rest) = B.splitAt sizeLen b
   in case reads $ B8.unpack sizeBS of
        (i, _) : _ -> B.take i rest
        [] -> error "Data.FileEmbed (getInner): Your dummy space has been corrupted."

-- | Show a number and leftpad with 0's.
padSize :: Int -> String
padSize i =
  let s = show i
   in replicate (sizeLen - length s) '0' ++ s

-- | Allocate the given number of bytes in the generate executable. That space
-- can be filled up with the 'inject' and 'injectFile' functions.
--
-- See 'dummySpace' for an untyped variant.
--
-- @since 0.1.0.0
dummySpaceTyped :: (Quote m) => Int -> Code m B.ByteString
dummySpaceTyped = dummySpaceWithTyped "MS"

-- | Allocate the given number of bytes in the generate executable. That space
-- can be filled up with the 'inject' and 'injectFile' functions.
--
-- See 'dummySpaceTyped' for a typed variant.
--
-- @since 0.0.4
dummySpace :: Int -> Q Exp
dummySpace = dummySpaceWith "MS"

-- | Like 'dummySpace', but takes a postfix for the magic string.  In
-- order for this to work, the same postfix must be used by 'inject' /
-- 'injectFile'.  This allows an executable to have multiple
-- 'ByteString's injected into it, without encountering collisions.
--
-- See 'dummySpaceWith' for an untyped variant.
--
-- @since 0.1.0.0
dummySpaceWithTyped :: (Quote m) => B.ByteString -> Int -> Code m B.ByteString
dummySpaceWithTyped postfix space = do
  let size = padSize space
      magic' = magic postfix
      start = B8.unpack magic' ++ size
      magicLen = B8.length magic'
      len = magicLen + sizeLen + space
      -- can't lift [Word8] into an Addr# directly
      chars :: (Quote m) => Code m Addr#
      chars =
        unsafeCodeCoerce $
          pure $
            LitE $
              StringPrimL $
                map (toEnum . fromEnum) $
                  start ++ replicate space '0'
  [||getInner (B.drop magicLen (unsafePerformIO (unsafePackAddressLen len $$chars)))||]

-- | Like 'dummySpace', but takes a postfix for the magic string.  In
-- order for this to work, the same postfix must be used by 'inject' /
-- 'injectFile'.  This allows an executable to have multiple
-- 'B.ByteString's injected into it, without encountering collisions.
--
-- See 'dummySpaceWithTyped' for a typed variant.
--
-- @since 0.0.8
dummySpaceWith :: B.ByteString -> Int -> Q Exp
dummySpaceWith postfix space = unTypeCode $ dummySpaceWithTyped postfix space

-- | Inject some raw data inside a 'B.ByteString' containing empty, dummy space
-- (allocated with 'dummySpace'). Typically, the original 'B.ByteString' is an
-- executable read from the filesystem.
--
-- @since 0.0.4
inject ::
  -- | bs to inject
  B.ByteString ->
  -- | original BS containing dummy
  B.ByteString ->
  -- | new BS, or Nothing if there is insufficient dummy space
  Maybe B.ByteString
inject = injectWith "MS"

-- | Like 'inject', but takes a postfix for the magic string.
--
-- @since 0.0.8
injectWith ::
  -- | postfix of magic string
  B.ByteString ->
  -- | bs to inject
  B.ByteString ->
  -- | original BS containing dummy
  B.ByteString ->
  -- | new BS, or Nothing if there is insufficient dummy space
  Maybe B.ByteString
injectWith postfix toInj orig =
  if toInjL > size
    then Nothing
    else Just $ B.concat [before, magic', B8.pack $ padSize toInjL, toInj, B8.pack $ replicate (size - toInjL) '0', after]
  where
    magic' = magic postfix
    toInjL = B.length toInj
    (before, rest) = B.breakSubstring magic' orig
    (sizeBS, rest') = B.splitAt sizeLen $ B.drop (B8.length magic') rest
    size = case reads $ B8.unpack sizeBS of
      (i, _) : _ -> i
      [] -> error $ "Data.FileEmbed (inject): Your dummy space has been corrupted. Size is: " ++ show sizeBS
    after = B.drop size rest'

-- | Same as 'inject', but instead of performing the injecting in memory, read
-- the contents from the filesystem and write back to a different file on the
-- filesystem.
--
-- @since 0.0.4
injectFile ::
  -- | bs to inject
  B.ByteString ->
  -- | template file
  FilePath ->
  -- | output file
  FilePath ->
  IO ()
injectFile = injectFileWith "MS"

-- | Like 'injectFile', but takes a postfix for the magic string.
--
-- @since 0.0.8
injectFileWith ::
  -- | postfix of magic string
  B.ByteString ->
  -- | bs to inject
  B.ByteString ->
  -- | template file
  FilePath ->
  -- | output file
  FilePath ->
  IO ()
injectFileWith postfix inj srcFP dstFP = do
  src <- B.readFile srcFP
  case injectWith postfix inj src of
    Nothing -> error "Insufficient dummy space"
    Just dst -> B.writeFile dstFP dst

-- $inject
--
-- The inject system allows arbitrary content to be embedded inside a Haskell
-- executable, post compilation. Typically, file-embed allows you to read some
-- contents from the file system at compile time and embed them inside your
-- executable. Consider a case, instead, where you would want to embed these
-- contents after compilation. Two real-world examples are:
--
-- * You would like to embed a hash of the executable itself, for sanity checking in a network protocol. (Obviously the hash will change after you embed the hash.)
--
-- * You want to create a self-contained web server that has a set of content, but will need to update the content on machines that do not have access to GHC.
--
-- The typical workflow use:
--
-- * Use 'dummySpace' or 'dummySpaceWith' to create some empty space in your executable
--
-- * Use 'injectFile' or 'injectFileWith' from a separate utility to modify that executable to have the updated content.
--
-- The reason for the @With@-variant of the functions is for cases where you wish
-- to inject multiple different kinds of content, and therefore need control over
-- the magic key. If you know for certain that there will only be one dummy space
-- available, you can use the non-@With@ variants.