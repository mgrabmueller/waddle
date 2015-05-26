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
import Game.Waddle.ExportJS

import Data.List
import Text.Printf
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Map(Map)
import qualified Data.Map as Map
import Data.CaseInsensitive(CI)
import System.Environment
import Codec.Picture
import System.Directory

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname, dir] -> convertToPng fname dir
    _ -> putStrLn $ unlines ["Usage: export-to-png FILENAME DIR",
                             "FILENAME    WAD file",
                             "DIR         target directory for exported PNGs"]

convertToPng :: FilePath -> FilePath -> IO ()
convertToPng fname dirName = do
  wad@Wad{..} <- load fname
  printf "flats: %d\n" (Map.size wadFlats)
  exportFlats dirName wadPalettes wadFlats
  printf "sprites: %d\n" (Map.size wadSprites)
  exportSprites dirName wadPalettes wadSprites
  exportSpriteAnimations dirName wadPalettes wadSprites
  printf "patches: %d\n" (Map.size wadPatches)
  exportPatches dirName wadPalettes wadPatches

exportFlats :: FilePath -> Maybe Palettes -> Map (CI LumpName) Flat -> IO ()
exportFlats dirName (Just (Palettes pals)) flats = do
  forM_ (Map.elems flats) $ \ flat@Flat{..} -> do
    createDirectoryIfMissing True (dirName ++ "/flats")
    writePng (dirName ++ "/flats/" ++ BS8.unpack flatName ++ ".png") $
      generateImage (genPixels flat) 64 64
 where
   genPixels Flat{..} x y =
     let idx = y * 64 + x
         i = BS.index flatData idx
         (r,g,b) = head pals !! fromIntegral i
     in PixelRGBA8 r g b 255

exportSprites :: FilePath -> Maybe Palettes -> Map (CI LumpName) Sprite -> IO ()
exportSprites dirName pals sprites = do
  forM_ (Map.elems sprites) $ \ sprite@Sprite{..} -> do
    exportPicture dirName "sprites" spriteName pals spritePicture

exportSpriteAnimations :: FilePath -> Maybe Palettes -> Map (CI LumpName) Sprite -> IO ()
exportSpriteAnimations dirName pals sprites = do
  forM_ (groupSprites $ Map.elems sprites) $ \ sprites@(Sprite{..}:_) -> do
    case exportSpriteAnimation dirName "animations" (BS.take 4 spriteName) pals sprites of
      Left err -> putStrLn $ "ERROR: " ++ err
      Right act -> do
        createDirectoryIfMissing True (dirName ++ "/animations")
        act
 where
   groupSprites sprites = groupBy (\ a b ->
                                    BS.take 4 (spriteName a) ==
                                    BS.take 4 (spriteName b)) sprites

exportPatches :: FilePath -> Maybe Palettes -> Map (CI LumpName) Patch -> IO ()
exportPatches dirName pals patches = do
  forM_ (Map.elems patches) $ \ patch@Patch{..} -> do
    exportPicture dirName "patches" patchName pals patchPicture


exportPicture :: FilePath -> FilePath -> LumpName -> Maybe Palettes -> Picture -> IO ()
exportPicture dirName subDir pictureName (Just (Palettes pals)) picture@Picture{..} = do
  createDirectoryIfMissing True (dirName ++ "/" ++ subDir)
  writePng (dirName ++ "/" ++ subDir ++ "/" ++ BS8.unpack pictureName ++ ".png") $
      generateImage (genPixels picture) pictureWidth pictureHeight
 where
   findPix [] _ = Nothing
   findPix (Post{..}:ps) y | y >= fromIntegral postTop &&
                             y < fromIntegral postTop + BS.length postPixels =
     Just $ BS.index postPixels (y - fromIntegral postTop)
   findPix (_:ps) y = findPix ps y
   genPixels Picture{..} x y =
     let col = picturePosts !! x
         i = findPix col y
         (r,g,b,a) = case i of
           Nothing -> (0,0,0,0)
           Just i' -> let (r',g',b') = head pals !! fromIntegral i'
                      in (r',g',b',255)
     in PixelRGBA8 r g b a

exportSpriteAnimation :: FilePath -> FilePath -> LumpName -> Maybe Palettes -> [Sprite] -> Either String (IO ())
exportSpriteAnimation dirName subDir pictureName (Just (Palettes pals)) sprites =
  let (width, height) = foldr (\ (w, h) (wx, hx) ->
                                (max w wx, max h hx))
                         (0,0)
                         (map pixSize sprites)
      (lofs, tofs) = foldr (\ (w, h) (wx, hx) ->
                             (max w wx, max h hx))
                         (0,0)
                         (map pixOfs sprites)
  in
   writeGifImages (dirName ++ "/" ++ subDir ++ "/" ++ BS8.unpack pictureName ++ ".gif")
    LoopingForever $
      map (encodeFrame width height) sprites
 where
   pixSize Sprite{..} =
     let picture@Picture{..} = spritePicture
     in (pictureWidth, pictureHeight)

   pixOfs Sprite{..} =
     let picture@Picture{..} = spritePicture
     in (pictureLeftOffset, pictureTopOffset)

   encodeFrame width height Sprite{..} =
     let picture@Picture{..} = spritePicture
         palette = generateImage genPal 256 1
         image = generateImage (genPixels picture) width height
     in (palette, 100, image)
   genPal x y = let (r,g,b) = head pals !! x
                in PixelRGB8 r g b

   findPix [] _ = Nothing
   findPix (Post{..}:ps) y | y >= fromIntegral postTop &&
                             y < fromIntegral postTop + BS.length postPixels =
     Just $ BS.index postPixels (y - fromIntegral postTop)
   findPix (_:ps) y = findPix ps y
   genPixels Picture{..} x y | x < pictureWidth &&
                               y < pictureHeight =
                                 let col = picturePosts !! x
                                     i = findPix col y
                                 in case i of
                                   Nothing -> 0
                                   Just i' -> i'
   genPixels _ _ _ = 0
