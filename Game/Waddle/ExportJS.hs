{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------
-- |
-- Module:      Game.Waddle.ExportJS
-- Copyright:   (c) 2015 Martin Grabmueller
-- License:     BSD3
--
-- Maintainer:  martin@grabmueller.de
-- Stability:   provisional
-- Portability: portable
--
-- The function 'exportJS' exports a WAD file into several JavaScript
-- files, where each file defines an object:
--
-- * One file for each level, called @level-E1M1.js@, @level-MAP03.js@
-- etc. Each file defines an object for the level, called
-- e.g. @level_E1M1@.  Example:
--
-- > var level_E1M1 = {
-- >  things: [ ... ],
-- > ...
-- >  vertices: [
-- >     {x:1088,y:-3680},
-- > ...
-- >  ],
-- > ...
-- > };
--
-- * One file @levels.js@, which includes all levels and defines an
-- object called @levels@. Example:
--
-- > var levels = {"MAP01": level_MAP01, ..., "MAP32": level_MAP32};
--
-- * One file for textures, called @textures.js@, defining an object
-- @textures@. Example:
--
-- > var textures = {
-- >   "AASHITTY": {name:"AASHITTY",width:64,height:64,patches:[
-- >   {xoffset:0,yoffset:0,pname:0,stepdir:1,colormap:0}
-- >  ]},
-- > ...
-- > };
--
-- * One file for flats (floors and ceilings), called @flats.js@,
-- defining an object @flats@. Example:
--
-- > var flats = {
-- >   "BLOOD1":{name:"BLOOD1",data:[46,46,45,...]},
-- >   ...
-- > };
--
-- * One file for sprites, called @sprites.js@, defining an object
-- @sprites@. No example, I think you get the idea!
--
-- * One file for patches, called @patches.js@, defining an object
-- @patches@.
--
-- * One file for pnames, called @pnames.js@, defining a list @pnames@.
--
-- * One file for palettes, called @palettes.js@, defining a list of
-- lists @palettes@.
--
-- * One file for colormaps, called @colormap.js@, defining a list
-- @colormap@.
--
-- To see how this data can be used, have a look at the HTML5 view
-- included in the distribution in directory "visualize".
----------------------------------------------------------------------------
module Game.Waddle.ExportJS
       (exportJS) where

import Game.Waddle.Types

import System.IO
import Data.List
import Text.Printf
import Data.Bits
import Data.Word
import Data.CaseInsensitive(CI)
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

-- | Exports a WAD structure into several JavaScript files:
--
-- * One file for each level, called like @level_E1M1.js@ or @level_MAP13.js@
-- * @levels.js@
-- * @textures.js@
-- * @flats.js@
-- * @sprites.js@
-- * @patches.js@
-- * @pnames.js@
-- * @palettes.js
-- * @colormap.js@
--
exportJS :: Wad -> FilePath -> IO ()
exportJS Wad{..} dir = do

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

  forM_ (Map.elems wadLevels) $ \ level@Level{..} -> do
    withFile (printf "%s/level-%s.js" dir (BS8.unpack levelName)) WriteMode $ \ h ->
      exportLevel h level
  withFile (printf "%s/levels.js" dir) WriteMode $ \ h ->
      exportLevels h wadLevels
  withFile (printf "%s/sprites.js" dir) WriteMode $ \ h ->
    exportSprites h wadSprites
  withFile (printf "%s/patches.js" dir) WriteMode $ \ h ->
    exportPatches h wadPatches
  withFile (printf "%s/textures.js" dir) WriteMode $ \ h ->
    exportTextures h wadTextures
  withFile (printf "%s/pnames.js" dir) WriteMode $ \ h ->
    exportPNames h wadPNames
  withFile (printf "%s/flats.js" dir) WriteMode $ \ h ->
    exportFlats h wadFlats
  withFile (printf "%s/colormap.js" dir) WriteMode $ \ h ->
    exportColormap h wadColormap
  withFile (printf "%s/palettes.js" dir) WriteMode $ \ h ->
    exportPalettes h wadPalettes
  return ()

exportThing :: Handle -> (String, Thing) -> IO ()
exportThing h (comma, Thing{..}) = do
  hPrintf h "    %s{x:%d,y:%d,angle:%d,type:\"%s\",flags:%d}\n" comma
    thingX thingY thingAngle (show thingType) thingFlags

exportVertex :: Handle -> (String, Vertex) -> IO ()
exportVertex h (comma, Vertex{..}) = do
  hPrintf h "    %s{x:%d,y:%d}\n" comma
    vertexX vertexY

exportLineDef :: Handle -> (String, LineDef) -> IO ()
exportLineDef h (comma, LineDef{..}) = do
  hPrintf h "    %s{start:%d,end:%d,flags:%d,effect:%d,tag:%d,right:%d,left:%s}\n" comma
    lineDefStartVertex lineDefEndVertex lineDefFlags lineDefEffect lineDefTag
    lineDefRightSideDef (maybe "null" show lineDefLeftSideDef)

exportSideDef :: Handle -> (String, SideDef) -> IO ()
exportSideDef h (comma, SideDef{..}) = do
  hPrintf h "    %s{xofs:%d,yofs:%d,upperTexture:%s,lowerTexture:%s,middleTexture:%s,sector:%d}\n" comma
    sideDefXOffset sideDefYOffset (show sideDefUpperTextureName)
    (show sideDefLowerTextureName) (show sideDefMiddleTextureName)
    sideDefSector


exportNode :: Handle -> (String, Node) -> IO ()
exportNode h (comma, Node{..}) = do
  hPrintf h "    %s{x:%d,y:%d,dx:%d,dy:%d,rbbuy:%d,rbbly:%d,rbblx:%d,rbbux:%d,lbbuy:%d,lbbly:%d,lbblx:%d,lbbux:%d,rightNodeOrSSector:%d,leftNodeOrSSector:%d}\n" comma
    nodeX nodeY nodeDX nodeDY
    nodeRightBBUY nodeRightBBLY nodeRightBBLX nodeRightBBUX
    nodeLeftBBUY nodeLeftBBLY nodeLeftBBLX nodeLeftBBUX
    ((either fromIntegral ((.|. 0x8000) . fromIntegral) nodeRightNodeOrSSector) :: Word16)
    ((either fromIntegral ((.|. 0x8000) . fromIntegral) nodeLeftNodeOrSSector) :: Word16)

exportSector :: Handle -> (String, Sector) -> IO ()
exportSector h (comma, Sector{..}) = do
  hPrintf h "    %s{floorHeight:%d,ceilingHeight:%d,floorFlat:%s,ceilingFlat:%s,lightLevel:%d,special:%d,tag:%d}\n" comma
    sectorFloorHeight sectorCeilingHeight (show sectorFloorFlat) (show sectorCeilingFlat)
    sectorLightLevel sectorSpecial sectorTag

exportSeg :: Handle -> (String, Seg) -> IO ()
exportSeg h (comma, Seg{..}) = do
  hPrintf h "    %s{start:%d,end:%d,angle:%d,lineDef:%d,direction:%d,offset:%d}\n" comma
    segStartVertex segEndVertex segAngle segLineDef segDirection segOffset

exportSSector :: Handle -> (String, SSector) -> IO ()
exportSSector h (comma, SSector{..}) = do
  hPrintf h "    %s{segCount:%d,segStart:%d}\n" comma
    ssectorSegCount ssectorSegStart

exportBlockmap :: Handle -> Maybe Blockmap -> IO ()
exportBlockmap h Nothing = hPrintf h "null"
exportBlockmap h (Just Blockmap{..}) = do
  hPrintf h "    {originX:%d,originY:%d,columns:%d,rows:%d,\n"
    blockmapOriginX blockmapOriginY blockmapColumns blockmapRows
  hPrintf h "    lists:%s}" (show blockmapBlocklists)

exportLevel :: Handle -> Level -> IO ()
exportLevel h Level{..} = do
  hPrintf h "var level_%s = {\n" (BS8.unpack levelName)
  hPrintf h "  things: [\n"
  mapM_ (exportThing h) (zip (" ":repeat ",") levelThings)
  hPrintf h "  ],\n"
  hPrintf h "  vertices: [\n"
  mapM_ (exportVertex h) (zip (" ":repeat ",") levelVertices)
  hPrintf h "  ],\n"
  hPrintf h "  linedefs: [\n"
  mapM_ (exportLineDef h) (zip (" ":repeat ",") levelLineDefs)
  hPrintf h "  ],\n"
  hPrintf h "  sidedefs: [\n"
  mapM_ (exportSideDef h) (zip (" ":repeat ",") levelSideDefs)
  hPrintf h "  ],\n"
  hPrintf h "  segs: [\n"
  mapM_ (exportSeg h) (zip (" ":repeat ",") levelSegs)
  hPrintf h "  ],\n"
  hPrintf h "  ssectors: [\n"
  mapM_ (exportSSector h) (zip (" ":repeat ",") levelSSectors)
  hPrintf h "  ],\n"
  hPrintf h "  sectors: [\n"
  mapM_ (exportSector h) (zip (" ":repeat ",") levelSectors)
  hPrintf h "  ],\n"
  hPrintf h "  nodes: [\n"
  mapM_ (exportNode h) (zip (" ":repeat ",") levelNodes)
  hPrintf h "  ],\n"
  hPrintf h "  reject: %s,\n" (maybe "[]"  (show . BS.unpack . rejectBytes) levelReject)
  hPrintf h "  blockmap: \n"
  exportBlockmap h levelBlockmap
  hPrintf h "\n"
  hPrintf h "};\n"

exportLevels :: Handle -> Map (CI LumpName) Level -> IO ()
exportLevels h mp = do
  hPrintf h "var levels = {%s};\n"
    (intercalate (","::String) $ (map (\ (_, Level{..}) ->
           printf "%s: level_%s" (show levelName) (BS8.unpack levelName))
     (zip ((" "::String):repeat",") $ Map.elems mp)))

exportPicture :: Handle -> Picture -> IO ()
exportPicture h Picture{..} = do
  hPrintf h "  width:%d,height:%d,leftOffset:%d,topOffset:%d,columns:[\n" pictureWidth pictureHeight pictureLeftOffset pictureTopOffset
  forM_ (zip (" ":repeat ",") picturePosts) $ \ (comma1, plist) -> do
    hPrintf h "    %s[" (comma1 :: String)
    forM_ (zip (" ":repeat ",") plist) $ \ (comma, Post{..}) -> do
      hPrintf h "%s{top:%d,pixels:%s}" (comma::String) postTop (show (BS.unpack postPixels))
    hPrintf h "]\n"
  hPrintf h "  ]"

exportSprite :: Handle -> (String, (CI LumpName, Sprite)) -> IO ()
exportSprite h (comma1, (_, Sprite{..})) = do
  hPrintf h "  %s%s: {name: %s,\n" comma1 (show spriteName) (show spriteName)
  exportPicture h spritePicture
  hPrintf h "\n  }\n"

exportSprites :: Handle -> Map (CI LumpName) Sprite -> IO ()
exportSprites h mp = do
  hPrintf h "var sprites = {\n"
  forM_ (zip (" ":repeat ",") $ Map.toList mp) (exportSprite h)
  hPrintf h "  };\n"


exportPatch :: Handle -> (String, (CI LumpName, Patch)) -> IO ()
exportPatch h (comma1, (_, Patch{..})) = do
  hPrintf h "  %s%s: {name: %s,\n" comma1 (show patchName) (show patchName)
  exportPicture h patchPicture
  hPrintf h "\n  }\n"

exportPatches :: Handle -> Map (CI LumpName) Patch -> IO ()
exportPatches h mp = do
  hPrintf h "var patches = {\n"
  forM_ (zip (" ":repeat ",") $ Map.toList mp) (exportPatch h)
  hPrintf h "};\n"


exportTexture :: Handle -> (String, (CI LumpName, Texture)) -> IO ()
exportTexture h (comma1, (_, Texture{..})) = do
  hPrintf h "  %s%s: {name:%s,width:%d,height:%d,patches:[\n" comma1 (show textureName) (show textureName) textureWidth textureHeight
  forM_ (zip (" ":repeat ",") texturePatchDescriptors) $ \ (comma, PatchDescriptor{..}) ->
    hPrintf h "  %s{xoffset:%d,yoffset:%d,pname:%d,stepdir:%d,colormap:%d}\n" (comma :: String)
      patchDescriptorXOffset patchDescriptorYOffset patchDescriptorPNameIndex patchDescriptorStepDir patchDescriptorColorMap
  hPrintf h "  ]}\n"

exportTextures :: Handle -> Map (CI LumpName) Texture -> IO ()
exportTextures h mp = do
  hPrintf h "var textures = {\n"
  forM_ (zip (" ":repeat ",") $ Map.toList mp) (exportTexture h)
  hPrintf h "};\n"

exportPNames :: Handle -> Map Int LumpName -> IO ()
exportPNames h mp = do
  hPrintf h "var pnames = [\n"
  forM_ (zip (" " : repeat ",") $ Map.elems mp) $ \ (comma, ln) -> do
    hPrintf h "%s%s" (comma :: String) (show ln)
  hPrintf h "  ];\n"

exportFlats :: Handle -> Map (CI LumpName) Flat -> IO ()
exportFlats h mp = do
  hPrintf h "var flats = {\n"
  forM_ (zip (" ":repeat ",") (Map.elems mp)) $ \ (comma, Flat{..}) -> do
    hPrintf h "  %s%s:{name:%s,data:%s}\n" (comma :: String)
      (show flatName) (show flatName) (show (BS.unpack flatData))
  hPrintf h "  }\n"

exportColormap :: Handle -> Maybe Colormap -> IO ()
exportColormap _ Nothing = return ()
exportColormap h (Just (Colormap bs)) = do
  hPrintf h "var colormap = \n"
  hPrintf h "  %s\n" (show (map BS.unpack bs))
  hPrintf h "  ;\n"

exportPalettes :: Handle -> Maybe Palettes -> IO ()
exportPalettes _ Nothing = return ()
exportPalettes h (Just (Palettes pals)) = do
  hPrintf h "var palettes = [\n"
  forM_ (zip (" ":repeat ",") pals) $ \ (comma, pal) -> do
    hPrintf h "  %s[" (comma :: String)
    forM_ (zip (" ":repeat ",") pal) $ \ (comma', (r,g,b)) -> do
      hPrintf h "%s[%d,%d,%d]" (comma' :: String) r g b
    hPrintf h "]\n"
  hPrintf h "  ];\n"
