{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------
-- |
-- Module:      Main
-- Copyright:   (c) 2015 Martin Grabmueller
-- License:     BSD3
--
-- Maintainer:  martin@grabmueller.de
-- Stability:   provisional
-- Portability: portable
--
-- Waddle is a library of WAD file utilities.
--
-- This is a convenience module which re-exports the modules which are
-- essential for using Waddle.
----------------------------------------------------------------------------
module Main(main) where

import Game.Waddle.Types
import Game.Waddle.Load

import Text.Printf
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> wadInfo fname
    _ -> putStrLn $ unlines ["Usage: wad-info FILENAME",
                             "FILENAME    WAD file"]

wadInfo :: FilePath -> IO ()
wadInfo fname = do
  wad@Wad{..} <- load fname

  forM_ wadDirectory (\ WadEntry{..} -> do
                         printf "%s offset: %d, length:%d\n" (BS8.unpack wadEntryName)
                           wadEntryOffset wadEntrySize)

  printf "levels: %d\n" (Map.size wadLevels)
  forM_ (Map.elems wadLevels) $ \ Level{..} -> do
    printf "Level %s:\n" (BS8.unpack levelName)
    printf "  vertices: %d\n" (length levelVertices)
    printf "  sideDefs: %d\n" (length levelSideDefs)
    printf "  lineDefs: %d\n" (length levelLineDefs)
    printf "  sectors: %d\n" (length levelSectors)
    printf "  reject: %d\n" (maybe 0 (BS.length . rejectBytes) levelReject)
    printf "  blockmap: %d\n"
      (case levelBlockmap of
          Just Blockmap{..} -> (blockmapColumns * blockmapRows)
          Nothing -> 0)
    printf "  things: %d\n" (length levelThings)
  printf "flats: %d\n" (Map.size wadFlats)
  printf "sprites: %d\n" (Map.size wadSprites)
  printf "textures: %d\n" (Map.size wadTextures)
  printf "patches: %d\n" (Map.size wadPatches)
  printf "pnames: %d\n" (Map.size wadPNames)
