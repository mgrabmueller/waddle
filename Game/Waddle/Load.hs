{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------
-- |
-- Module:      Game.Waddle.Load
-- Copyright:   (c) 2015 Martin Grabmueller
-- License:     BSD3
--
-- Maintainer:  martin@grabmueller.de
-- Stability:   provisional
-- Portability: portable
--
-- WAD file loader. Loads the file into memory and parses the common
-- lumps into Haskell values.
--
-- I recommend the Unofficial Doom Specification by Matthew S Fell,
-- available at <http://aiforge.net/test/wadview/dmspec16.txt> and the
-- Doom Wiki at <http://doomwiki.org> for details.
----------------------------------------------------------------------------
module Game.Waddle.Load
       (load) where

import Game.Waddle.Types

import Control.Exception
import Text.Printf
import Data.Bits
import Data.Int
import Data.Word
import Data.CaseInsensitive(CI, mk)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Binary.Get
import Control.Monad
import Control.Applicative
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL

getInt32le :: Get Int32
getInt32le = fromIntegral <$> getWord32le

getInt16le :: Get Int16
getInt16le = fromIntegral <$> getWord16le

getWadHeader :: Get WadHeader
getWadHeader =
  WadHeader <$>
  getByteString 4 <*>
  getInt32le <*>
  getInt32le

getWadEntry :: Get WadEntry
getWadEntry =
  WadEntry <$>
  getInt32le <*>
  getInt32le <*>
  (trimNUL <$> getByteString 8)

getWadEntryList :: Int -> Int -> Get [WadEntry]
getWadEntryList offset cnt = skip offset >>  sequence (replicate cnt getWadEntry)

trimNUL :: ByteString -> ByteString
trimNUL s =
  case BS.findIndex (== 0) s of
    Nothing -> s
    Just i  -> BS.take i s

-- | Load a WAD file into a 'Wad' value.  The complete file is read
-- into memory eagerly, assuming that all the content will be needed
-- anyway by the application.
--
-- May throw 'IOException' or 'WadException'.
--
load :: FilePath -> IO Wad
load fp = do
  wadContent <- BS.readFile fp
  header@WadHeader{..} <- runGet' "WAD header" getWadHeader wadContent
  wadEntries <- runGet' "WAD directory"
                (getWadEntryList
                 (fromIntegral wadHeaderDirectoryOffset)
                 (fromIntegral wadHeaderLumpCount))
                  wadContent
  let lumpLookup = mkLookup wadEntries wadContent
      wadLumps   = mkWadLumps wadEntries wadContent
  p <- parseWad wadEntries wadLumps lumpLookup
  return $ Wad {wadHeader     = header,
                wadDirectory  = wadEntries,
                wadLumps      = wadLumps,
                wadLumpLookup = lumpLookup,
                wadFlats      = psFlats p,
                wadSprites    = psSprites p,
                wadPatches    = psPatches p,
                wadTextures   = psTextures p,
                wadPNames     = psPNames p,
                wadLevels     = psLevels p,
                wadColormap   = psColormap p,
                wadPalettes   = psPalettes p}
 where
   mkWadLumps entries content =
     map (\ WadEntry{..} ->
               (BS.take (fromIntegral wadEntrySize) (BS.drop (fromIntegral wadEntryOffset) content))) entries
   mkLookup entries content =
     foldr (\ WadEntry{..} tab ->
             Map.insert
               (mk wadEntryName)
               (BS.take (fromIntegral wadEntrySize) (BS.drop (fromIntegral wadEntryOffset) content))
               tab)
       Map.empty entries


data PState
  = NoState
  | InLevel LumpName
  | InSprites
  | InFlats
  | InPatches
 deriving (Show)

data ExtPicture = ExtPicture {
  extPictureWidth      :: Int16,
  extPictureHeight     :: Int16,
  extPictureLeftOffset :: Int16,
  extPictureTopOffset  :: Int16,
  extPictureColStarts  :: [Int32]
  }
 deriving (Show)


getExtPicture :: Get ExtPicture
getExtPicture = do
  w <- getInt16le
  h <- getInt16le
  l <- getInt16le
  t <- getInt16le
  colStarts <- replicateM (fromIntegral w) getInt32le
  return $ ExtPicture w h l t colStarts

data ExtPost
  = ExtPostEnd
  | ExtPost {
    extPostTop    :: Word8,
    extPostCount  :: Word8,
    extPostPixels :: ByteString,
    extPostNext   :: ExtPost
    }
  deriving (Show)

getExtPost :: Get ExtPost
getExtPost = do
  top <- getWord8
  case top of
    255 ->
      return ExtPostEnd
    _ -> do
      cnt <- getWord8
      _skip1 <- getWord8
      px <- getByteString (fromIntegral cnt)
      _skip2 <- getWord8
      e <- getExtPost
      return $ ExtPost top cnt px e

convertPosts :: ExtPost -> [Post]
convertPosts ExtPostEnd = []
convertPosts ExtPost{..} =
  Post {postTop = extPostTop,
        postPixels = extPostPixels} : convertPosts extPostNext

convertPicture :: ExtPicture -> [ExtPost] -> Picture
convertPicture ExtPicture{..} extPosts =
  Picture {pictureWidth = fromIntegral extPictureWidth,
           pictureHeight = fromIntegral extPictureHeight,
           pictureLeftOffset = fromIntegral extPictureLeftOffset,
           pictureTopOffset = fromIntegral extPictureTopOffset,
           picturePosts = map convertPosts extPosts}

getThingList :: Int -> Get [Thing]
getThingList cnt =
  sequence (replicate cnt $
            Thing <$>
            getInt16le <*>
            getInt16le <*>
            getInt16le <*>
            (thingTypeFromNumber <$> getInt16le) <*>
            getInt16le)

getVertexList :: Int -> Get [Vertex]
getVertexList cnt =
  sequence (replicate cnt $
            Vertex <$>
            getInt16le <*>
            getInt16le)

getLineDefList :: Int -> Get [LineDef]
getLineDefList cnt =
  sequence (replicate cnt $
            LineDef <$>
            getInt16le <*>
            getInt16le <*>
            getInt16le <*>
            getInt16le <*>
            getInt16le <*>
            getInt16le <*>
            (toMB <$> getInt16le))
 where
   toMB n | n < 0 = Nothing
   toMB n = Just n

getSideDefList :: Int -> Get [SideDef]
getSideDefList cnt =
  sequence (replicate cnt $
            SideDef <$>
            getInt16le <*>
            getInt16le <*>
            (trimNUL <$> getByteString 8) <*>
            (trimNUL <$> getByteString 8) <*>
            (trimNUL <$> getByteString 8) <*>
            getInt16le)

getSeg :: Get Seg
getSeg =
  Seg <$>
  getInt16le <*>
  getInt16le <*>
  getInt16le <*>
  getInt16le <*>
  getInt16le <*>
  getInt16le

getSegList :: Int -> Get [Seg]
getSegList cnt =
  sequence (replicate cnt getSeg)

getSSector :: Get SSector
getSSector =
  SSector <$>
  getInt16le <*>
  getInt16le

getSSectorList :: Int -> Get [SSector]
getSSectorList cnt =
  sequence (replicate cnt getSSector)

getSectorList :: Int -> Get [Sector]
getSectorList cnt =
  sequence (replicate cnt $
            Sector <$>
            getInt16le <*>
            getInt16le <*>
            (trimNUL <$> getByteString 8) <*>
            (trimNUL <$> getByteString 8) <*>
            getInt16le <*>
            getInt16le <*>
            getInt16le)

getNode :: Get Node
getNode =
  Node <$>
  getInt16le <*>
  getInt16le <*>
  getInt16le <*>
  getInt16le <*>
  getInt16le <*>
  getInt16le <*>
  getInt16le <*>
  getInt16le <*>
  getInt16le <*>
  getInt16le <*>
  getInt16le <*>
  getInt16le <*>
  (lr <$> getWord16le) <*>
  (lr <$> getWord16le)
 where
   lr x = if x .&. 0x8000 == 0
          then Left (fromIntegral x)
          else Right (fromIntegral (x .&. 0x7fff))

getNodeList :: Int -> Get [Node]
getNodeList cnt =
  sequence (replicate cnt getNode)

getBlocklists :: Int -> Get [Blocklist]
getBlocklists cnt | cnt == 0 = return []
getBlocklists cnt = do
  0 <- getInt16le
  vals <- getVals
  rest <- getBlocklists (cnt - 1)
  return $ vals : rest
 where
   getVals :: Get Blocklist
   getVals = do
     i <- getInt16le
     case i of
       -1 -> return []
       _ -> do
         r <- getVals
         return $ i : r

getBlocklist :: Get Blocklist
getBlocklist = do
  0 <- getInt16le
  go
 where
   go = do
     i <- getInt16le
     case i of
       -1 -> return []
       _ -> do
         r <- go
         return $ i : r

data ExtBlockmap = ExtBlockmap {
    extBlockmapOriginX :: Int16,
    extBlockmapOriginY :: Int16,
    extBlockmapColumns :: Int16,
    extBlockmapRows :: Int16,
    extBlockmapOffsets :: [Word16]
  }

getBlockmap :: Get ExtBlockmap
getBlockmap = do
  ox <- getInt16le
  oy <- getInt16le
  cols <- getInt16le
  rows <- getInt16le
  offsets <- sequence (replicate (fromIntegral (cols * rows)) getWord16le)
  return ExtBlockmap {
    extBlockmapOriginX = ox,
    extBlockmapOriginY = oy,
    extBlockmapColumns = cols,
    extBlockmapRows = rows,
    extBlockmapOffsets = offsets
    }

getPatchDescriptor :: Get PatchDescriptor
getPatchDescriptor =
  PatchDescriptor <$>
  (fromIntegral <$> getWord16le) <*>
  (fromIntegral <$> getWord16le) <*>
  (fromIntegral <$> getWord16le) <*>
  (fromIntegral <$> getWord16le) <*>
  (fromIntegral <$> getWord16le)

getTexture :: Get Texture
getTexture = do
  n <- trimNUL <$> getByteString 8
  0 <- getWord16le
  0 <- getWord16le
  w <- fromIntegral <$> getWord16le
  h <- fromIntegral <$> getWord16le
  0 <- getWord16le
  0 <- getWord16le
  pdCnt <- fromIntegral <$> getWord16le
  pDescs <- sequence (replicate pdCnt getPatchDescriptor)
  return Texture {
    textureName = n,
    textureWidth = w,
    textureHeight = h,
    texturePatchDescriptors = pDescs
    }

getTextures :: Get (Map (CI LumpName) Texture)
getTextures = do
  cnt <- fromIntegral <$> getWord32le
  _ <- sequence (replicate cnt getWord32le)
  textures <- sequence (replicate cnt getTexture)
  return $ Map.fromList $ map (\ tex@Texture{..} -> (mk textureName, tex)) textures

getPNames :: Get (Map Int LumpName)
getPNames = do
  cnt <- fromIntegral <$> getWord32le
  names <- sequence (replicate cnt (getByteString 8))
  return $ Map.fromList (zip [0..] (map trimNUL names))

data ParseState = ParseState {
  psState    :: PState,
  psLumpLookup :: Map (CI LumpName) ByteString,
  psMaps     :: Map (CI LumpName) Level,
  psSprites  :: Map (CI LumpName) Sprite,
  psFlats    :: Map (CI LumpName) Flat,
  psPatches  :: Map (CI LumpName) Patch,
  psPNames   :: Map Int LumpName,
  psTextures :: Map (CI LumpName) Texture,
  psLevels   :: Map (CI LumpName) Level,

  psThings   :: [Thing],
  psVertices :: [Vertex],
  psLineDefs :: [LineDef],
  psSideDefs :: [SideDef],
  psSegs     :: [Seg],
  psSSectors :: [SSector],
  psSectors  :: [Sector],
  psNodes    :: [Node],
  psReject   :: Maybe Reject,
  psBlockmap :: Maybe Blockmap,
  psPalettes :: Maybe Palettes,
  psColormap :: Maybe Colormap
  }

initParseState :: Map (CI LumpName) ByteString -> ParseState
initParseState lu = ParseState {
  psState      = NoState,
  psLumpLookup = lu,
  psMaps       = Map.empty,
  psSprites    = Map.empty,
  psFlats      = Map.empty,
  psPatches    = Map.empty,
  psPNames     = Map.empty,
  psTextures   = Map.empty,
  psLevels     = Map.empty,
  psThings     = [],
  psVertices   = [],
  psLineDefs   = [],
  psSideDefs   = [],
  psSegs       = [],
  psSSectors   = [],
  psSectors    = [],
  psNodes      = [],
  psReject     = Nothing,
  psBlockmap   = Nothing,
  psPalettes   = Nothing,
  psColormap   = Nothing
  }

-- | Run a 'Get a' on a strict bytestring and return it's result. On
-- decoding error, throw a 'WadExceptionDecodeError' exception.
--
runGet' :: String -> Get a -> ByteString -> IO a
runGet' ctxt get bs =
  case runGetOrFail get (BSL.fromChunks [bs]) of
    Left (_, _, err) -> throwIO $ WadExceptionDecodeError ctxt err
    Right (_, _, r) -> return r

-- | Parser for WAD file contents. The resulting parse state contains
-- all data from the WAD, decoded and organized.
--
parseWad :: [WadEntry] -> [ByteString] -> Map (CI LumpName) ByteString -> IO ParseState
parseWad entries lumps lumpMap = foldM parseStep (initParseState lumpMap) (zip lumps entries)
 where
   parseStep ps@ParseState{psState = NoState}
     lumpWe@(_, WadEntry{..}) = do
     case wadEntryName of
       "F_START"  -> return ps{psState = InFlats}
       "S_START"  -> return ps{psState = InSprites}
       "P_START"  -> return ps{psState = InPatches}
       "PLAYPAL"  -> parsePalettes ps lumpWe
       "COLORMAP" -> parseColormap ps lumpWe
       "ENDOOM"   -> return ps
       "DEMO1"    -> return ps
       "DEMO2"    -> return ps
       "DEMO3"    -> return ps
       "TEXTURE1" -> parseTextures ps lumpWe
       "TEXTURE2" -> parseTextures ps lumpWe
       "PNAMES"   -> parsePNames ps lumpWe
       "GENMIDI"  -> return ps
       "HELP"     -> return ps
       "HELP1"    -> return ps
       "VICTORY2" -> return ps
       "PFUB1"    -> return ps
       "PFUB2"    -> return ps
       "END0"     -> return ps
       "END1"     -> return ps
       "END2"     -> return ps
       "END3"     -> return ps
       "END4"     -> return ps
       "END5"     -> return ps
       "END6"     -> return ps
       "ENDPIC"   -> return ps
       "TITLEPIC" -> return ps
       "CREDIT"   -> return ps
       "BOSSBACK" -> return ps
       _ | "AMMNUM" `BS8.isPrefixOf` wadEntryName  -> return ps
       "STBAR" -> return ps
       "INTERPIC" -> return ps
       "_DEUTEX_" -> return ps
       _ | "STGNUM" `BS8.isPrefixOf` wadEntryName  -> return ps
       _ | "BRDR"   `BS8.isPrefixOf` wadEntryName  -> return ps
       _ | "WI"     `BS8.isPrefixOf` wadEntryName  -> return ps
       _ | "ST"     `BS8.isPrefixOf` wadEntryName  -> return ps
       _ | "M_"     `BS8.isPrefixOf` wadEntryName  -> return ps
       _ | "D"      `BS8.isPrefixOf` wadEntryName  -> return ps
       _ | "CWILV"  `BS8.isPrefixOf` wadEntryName  -> return ps
       _ | wadEntryName `elem` knownMapNames -> return ps{psState = InLevel wadEntryName}
       _ -> do
         printf "unrecognized lump: %s at %d\n" (BS8.unpack wadEntryName)
           wadEntryOffset :: IO ()
         return ps

   parseStep ps@ParseState{psState = InLevel curLevel} lumpWe@(lump, WadEntry{..}) = do
     case wadEntryName of
       "THINGS" -> do
         things <- runGet' "THINGS lump"
                   (getThingList (fromIntegral $ wadEntrySize `div` 10)) lump
         return ps{psThings = things}
       "LINEDEFS" -> do
         lineDefs <- runGet' "LINEDEFS lump"
                     (getLineDefList (fromIntegral $ wadEntrySize `div` 14)) lump
         return ps{psLineDefs = lineDefs}
       "SIDEDEFS" -> do
         sideDefs <- runGet' "SIDEDEF lump"
                     (getSideDefList (fromIntegral $ wadEntrySize `div` 30)) lump
         return ps{psSideDefs = sideDefs}
       "VERTEXES" -> do
         vertices <- runGet' "VERTEXES lump"
                     (getVertexList (fromIntegral $ wadEntrySize `div` 4)) lump
         return ps{psVertices = vertices}
       "SEGS" -> do
         segs <- runGet' "SEGS lump"
                     (getSegList (fromIntegral $ wadEntrySize `div` 12)) lump
         return ps{psSegs = segs}
       "SSECTORS" -> do
         ssectors <- runGet' "SSECTORS lump"
                     (getSSectorList (fromIntegral $ wadEntrySize `div` 4)) lump
         return ps{psSSectors = ssectors}
       "NODES" ->  do
         nodes <- runGet' "NODES lump"
                    (getNodeList (fromIntegral $ wadEntrySize `div` 28)) lump
         return ps{psNodes = nodes}
       "SECTORS" -> do
         sectors <- runGet' "SECTORS lump"
                    (getSectorList (fromIntegral $ wadEntrySize `div` 26)) lump
         return ps{psSectors = sectors}
       "REJECT" ->
         return ps{psReject = Just $ Reject lump}
       "BLOCKMAP" -> do
         ExtBlockmap{..} <- runGet' "BLOCKMAP lump"
                            getBlockmap lump
         blocklists <- mapM (\ offset ->
                              runGet' "blocklist" getBlocklist
                                (BS.drop ((fromIntegral offset) * 2) lump))
                       extBlockmapOffsets
         return ps{psBlockmap =
                      Just $ Blockmap {
                        blockmapOriginX = extBlockmapOriginX,
                        blockmapOriginY = extBlockmapOriginY,
                        blockmapColumns = extBlockmapColumns,
                        blockmapRows = extBlockmapRows,
                        blockmapBlocklists = blocklists
                        }}
       _ -> parseStep ps{psState    = NoState,
                         psThings   = [],
                         psLineDefs = [],
                         psSideDefs = [],
                         psVertices = [],
                         psSegs     = [],
                         psSSectors = [],
                         psNodes    = [],
                         psSectors  = [],
                         psReject   = Nothing,
                         psBlockmap = Nothing,
                         psLevels   = Map.insert (mk curLevel)
                                      Level {
                                        levelName     = curLevel,
                                        levelThings   = psThings ps,
                                        levelLineDefs = psLineDefs ps,
                                        levelSideDefs = psSideDefs ps,
                                        levelVertices = psVertices ps,
                                        levelSegs     = psSegs ps,
                                        levelSSectors = psSSectors ps,
                                        levelNodes    = psNodes ps,
                                        levelSectors  = psSectors ps,
                                        levelReject   = psReject ps,
                                        levelBlockmap = psBlockmap ps
                                        } (psLevels ps)
                        }
            lumpWe  -- Repeat step on current dir entry, with new state.

   parseStep ps@ParseState{psState = InSprites} lumpWe@(_, WadEntry{..}) = do
     case wadEntryName of
       "S_END" -> return ps{psState = NoState}
       _       -> parseSprite ps lumpWe

   parseStep ps@ParseState{psState = InFlats} lumpWe@(_, WadEntry{..}) = do
     case wadEntryName of
       "F_END"    -> return ps{psState = NoState}
       "F1_START" -> return ps
       "F1_END"   -> return ps
       "F2_START" -> return ps
       "F2_END"   -> return ps
       "F3_START" -> return ps
       "F3_END"   -> return ps
       _          -> parseFlat ps lumpWe

   parseStep ps@ParseState{psState = InPatches} lumpWe@(_, WadEntry{..}) = do
     case wadEntryName of
       "P_END" -> return ps{psState = NoState}
       "P1_START" -> return ps
       "P1_END"   -> return ps
       "P2_START" -> return ps
       "P2_END"   -> return ps
       "P3_START" -> return ps
       "P3_END"   -> return ps
       _       -> parsePatch ps lumpWe

   parseColormap ps (lump, WadEntry{..}) = do
     cm <- runGet' "COLORMAP lump"
           (sequence (replicate 34 $ getByteString 256)) lump
     return ps{psColormap = Just $ Colormap cm}

   parsePalettes ps (lump, WadEntry{..}) = do
     pals <- runGet' "PLAYPAL lump"
             (sequence
              (replicate 14 $
               (sequence
                (replicate 256
                 ((,,) <$> getWord8 <*> getWord8 <*> getWord8)))))
          lump
     return ps{psPalettes = Just $ Palettes pals}

   parsePNames ps (lump, WadEntry{..}) = do
     pnames <- runGet' "PNAMES lump" getPNames lump
     forM_ (Map.toList pnames) $ \ (_, n) ->
       case Map.lookup (mk n) (psLumpLookup ps) of
         Just _ -> return ()
         Nothing -> throwIO $ WadExceptionFormatError "PNAMES lump"
                    ("reference to non-existant patch lump: " ++ BS8.unpack n)
     return ps{psPNames = pnames}

   parseTextures ps (lump, WadEntry{..}) = do
     ts <- runGet' (BS8.unpack wadEntryName ++ " lump") getTextures lump
     return ps{psTextures = Map.union ts (psTextures ps)}

   parseSprite ps e@(_, WadEntry{..}) = do
     pic <- parsePicture ps e
     let sprite = Sprite{spriteName = wadEntryName,
                         spritePicture = pic}
     return ps{psSprites = Map.insert (mk wadEntryName) sprite (psSprites ps)}

   parsePatch ps e@(_, WadEntry{..}) = do
     pic <- parsePicture ps e
     let patch = Patch{patchName = wadEntryName,
                       patchPicture = pic}
     return ps{psPatches = Map.insert (mk wadEntryName) patch (psPatches ps)}

   parseFlat ps (lump, WadEntry{..}) = do
     unless (BS.length lump == 4096) $
       throwIO $ WadExceptionDecodeError "flat" $ BS8.unpack wadEntryName ++
         ": flat has wrong size (expected=4096, actual=" ++ show (BS.length lump) ++ ")"
     let flat = Flat {flatName =  wadEntryName, flatData = lump}
     return ps{psFlats = Map.insert (mk wadEntryName) flat (psFlats ps)}

   parsePicture _ (lump, WadEntry{..}) = do
     pic@ExtPicture{..} <- runGet' ("picture " ++ BS8.unpack wadEntryName)
                           getExtPicture lump
     posts <- mapM (parseColumn lump) extPictureColStarts
     return $ convertPicture pic posts
    where
      parseColumn s colStart =
        runGet' "picture post" getExtPost (BS.drop (fromIntegral colStart) s)


-- | This list contains all known map names from DOOM/DOOM II.
--
knownMapNames :: [ByteString]
knownMapNames = [BS8.pack (printf "E%dM%d" ep mp) | ep <- [1..4::Int], mp <- [1..9::Int]] ++
                [BS8.pack (printf "MAP%02d" i) | i <- [1..32::Int]]
