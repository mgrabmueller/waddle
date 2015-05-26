{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------------
-- |
-- Module:      Game.Waddle.Types
-- Copyright:   (c) 2015 Martin Grabmueller
-- License:     BSD3
--
-- Maintainer:  martin@grabmueller.de
-- Stability:   provisional
-- Portability: portable
--
-- All public data types used by the Waddle library.  The 'Wad' type
-- is a good entry point into understanding the types.
--
-- I recommend the Unofficial Doom Specification by Matthew S Fell,
-- available at <http://aiforge.net/test/wadview/dmspec16.txt> and the
-- Doom Wiki at <http://doomwiki.org> for details.
----------------------------------------------------------------------------
module Game.Waddle.Types
       ( -- * Exceptions
         WadException(..),
         -- * WAD file structures
         Wad(..),
         LumpName,
         WadHeader(..),
         WadEntry(..),
         -- * Sprites and textures
         Sprite(..),
         Picture(..),
         Post(..),
         Texture(..),
         Patch(..),
         PatchDescriptor(..),
         Flat(..),
         -- * Palettes and colormaps
         Colormap(..),
         Palettes(..),
         -- * Level geometry and details
         Level(..),
         Thing(..),
         ThingType(..),
         thingTypeFromNumber,
         Vertex(..),
         SideDef(..),
         LineDef(..),
         Sector(..),
         Node(..),
         SSector(..),
         Seg(..),
         Reject(..),
         Blocklist,
         Blockmap(..)
  ) where

import Control.Exception
import Data.Typeable

import Data.Int
import Data.Word
import Data.ByteString(ByteString)
import Data.CaseInsensitive(CI)
import Data.Map(Map)

-- | Exception thrown when reading and decoding WAD files.
--
data WadException
  = WadExceptionFormatError String String
    -- ^ General WAD file format error.  The first string is the
    -- context where the error occured, the second contains details on
    -- the error.
  | WadExceptionDecodeError String String
    -- ^ Decoding of the general WAD format or individual lumps
    -- failed.  The first string is the context where the error
    -- occured, the second contains details on the error.
 deriving (Eq, Show, Typeable)

instance Exception WadException

-- | WAD structure, including the file contents and decoded common
-- lumps.
--
data Wad = Wad {
  wadHeader     :: WadHeader,
  -- ^ WAD header.
  wadDirectory  :: [WadEntry],
  -- ^ All WAD directory entries, in the same order as in the file.
  wadLumps :: [ByteString],
  -- ^ All WAD lumps, each entry matching the corresponding entry in wadDirectory.
  wadLumpLookup :: Map (CI LumpName) ByteString,
  -- ^ Mapping from lump names to lump content.
  wadFlats :: Map (CI LumpName) Flat,
  -- ^ Mapping from lump names to flats (floors and ceilings).
  wadSprites :: Map (CI LumpName) Sprite,
  -- ^ Mapping from lump names to sprites (monsters and things).
  wadPatches :: Map (CI LumpName) Patch,
  -- ^ Mapping from lump names to patches (parts of wall textures).
  wadTextures :: Map (CI LumpName) Texture,
  -- ^ Mapping from lump names to wall textures.
  wadLevels :: Map (CI LumpName) Level,
  -- ^ Mapping from lump names to levels.
  wadPNames :: Map Int LumpName,
  -- ^ Mapping from patch indices to patch names.
  wadColormap :: Maybe Colormap,
  -- ^ WAD colormap for mapping palette entries according to light
  -- levels.
  wadPalettes :: Maybe Palettes
  -- ^ Palettes for mapping color indices to RGB tuples.
  }

-- | WAD file header.
--
data WadHeader = WadHeader {
  wadHeaderIdentifier :: ByteString,
  -- ^ Normally \"IWAD\" or \"PWAD\", always of length 4.
  wadHeaderLumpCount :: Int32,
  -- ^ Number of lumps in the file.
  wadHeaderDirectoryOffset :: Int32
  -- ^ Byte offset (relative to beginning of the file) of the WAD
  -- directory.
  }

-- | Entry in WAd directory.
--
data WadEntry = WadEntry {
  wadEntryOffset :: Int32,
  -- ^ Offset of the lump data in the file.
  wadEntrySize :: Int32,
  -- ^ Size (in bytes) of the lump data.
  wadEntryName :: ByteString
  -- ^ Name of the lump. Note that trailing NULs are stripped when the
  -- name is read in.
  }

-- | Lump name. This is at mot 8 bytes long, and internally, all
-- trailing NULs are stripped.
--
type LumpName = ByteString

-- | One level.
--
data Level = Level {
  levelName :: LumpName,
  -- ^ Level name, E?M? style for DOOM 1 maps, MAP?? for DOOM 2 maps.
  levelThings :: [Thing],
  -- ^ List of things that are to be placed in the level on start.
  levelVertices :: [Vertex],
  -- ^ List of vertices referenced by linedefs, segs, etc.
  levelLineDefs :: [LineDef],
  -- ^ List of linedefs.
  levelSideDefs :: [SideDef],
  -- ^ List of sidedefs.
  levelSegs :: [Seg],
  -- ^ List of segs (parts of linedefs referenced in BSP tree.
  levelSSectors :: [SSector],
  -- ^ List of ssectors (sub-sectors), created from sectors during BSP
  -- building.  A ssector is made up of segs.
  levelSectors :: [Sector],
  -- ^ List of sectors of the level.
  levelNodes :: [Node],
  -- ^ BSP tree nodes.
  levelReject :: Maybe Reject,
  -- ^ Reject bitmap. Used for determining whether one sector can be
  -- seen from another.
  levelBlockmap :: Maybe Blockmap
  -- ^ Blockmap. For each block of the map, lists the linedefs
  -- intersecting that block.  Used for actor-wall collision
  -- detection.
  }

-- | Picture.  Sprites and wall patches are stored in this format.
--
data Picture = Picture {
  pictureWidth      :: Int,
  -- ^ Width of the picture.
  pictureHeight     :: Int,
  -- ^ Height of the picture.
  pictureLeftOffset :: Int,
  -- ^ Offset of the left side to the origin of the picture.
  pictureTopOffset  :: Int,
  -- ^ Offset of the top to the origin of the picture.
  picturePosts      :: [[Post]]
  -- ^ Each element in this list is a column, where each column is a
  -- list of posts.
  }

-- | A 'Post' is a part of a column.  There can (and often will) be
-- gaps in columns for transparent parts in sprites and walls.
--
data Post
  = Post {
    postTop    :: Word8,
    -- ^ Where to start drawing this part of the column.
    postPixels :: ByteString
    -- ^ Pixels of this post.  The length of this field defines how
    -- many pixels to draw.
    }
  deriving (Show)

-- | Sprites are used for players, monsters, and things in general.
--
data Sprite = Sprite {
  spriteName    :: LumpName,
  -- ^ Lump name for this sprite.
  spritePicture :: Picture
  -- ^ Picture for the sprite. It is drawn relative to the thing's
  -- position.
  }

-- | Flats are images for texturing floors and ceiling.
--
data Flat = Flat {
  flatName :: LumpName,
  -- ^ Name of this flat.
  flatData :: ByteString
  -- ^ Always 64 x 64 =  4096 bytes.
  }

-- | A wall patch. Wall textures are made up of one or more patches,
-- which are positioned as defined by the patch descriptors in the
-- 'Texture' value.
--
data Patch = Patch {
  patchName :: LumpName,
  -- ^ Name of this patch.
  patchPicture :: Picture
  -- ^ Picture for the patch. The offsets in the picture are ignored,
  -- because positioning is defined in the patch descriptor
  -- referencing this patch.
  }

-- | Things are parts of levels.  When a level is loaded, the things
-- define where to place players, monsters, items and so on.
--
data Thing = Thing {
  thingX     :: Int16,
  -- ^ X position of the thing.
  thingY     :: Int16,
  -- ^ Y position of the thing.
  thingAngle :: Int16,
  -- ^ Angle the thing is looking at when created. This only affects
  -- things that have a direction, such as players and monsters.
  thingType  :: ThingType,
  -- ^ Kind of thing.
  thingFlags :: Int16
  -- ^ Flags of the thing. Not decoded yet.
  }

-- | A vertex defines the X\/Y coordinates of linedefs, segs etc.
--
-- They are referenced by their position in the VERTEXES lump of the
-- level they are used in.
data Vertex = Vertex {
  vertexX :: Int16,
  -- ^ X coordinate.
  vertexY :: Int16
  -- ^ Y coordinate.
  }

-- | Linedefs make up the geometry of a level and additionally define
-- most of the interactivity.
--
data LineDef = LineDef {
  lineDefStartVertex  :: Int16,
  -- ^ The linedef starts at the vertex with this index,
  lineDefEndVertex    :: Int16,
  -- ^ ... and ends at the vertex with this index.
  lineDefFlags        :: Int16,
  -- ^ Linedef flags. Not decoded yet.
  lineDefEffect       :: Int16,
  -- ^ Linedef effect. Not decoded yet.
  lineDefTag          :: Int16,
  -- ^ Linedef tag. Triggers on this linedef affect sectors witht the
  -- same tag.
  lineDefRightSideDef :: Int16,
  -- ^ Right sidedef of this linedef.  Defines textures and sector
  -- this linedef is connected to.
  lineDefLeftSideDef  :: Maybe Int16
  -- ^ For two-sided linedefs, this is the left side.
  }

-- | A sidedef defines the textures to use on walls and to what sector
-- the wall is connected.
--
-- Linedefs can have one or two sidedefs.
data SideDef = SideDef {
  sideDefXOffset           :: Int16,
  -- ^ X offset of the sidedef.
  sideDefYOffset           :: Int16,
  -- ^ Y offset of the sidedef.
  sideDefUpperTextureName  :: ByteString,
  -- ^ Name of upper texture.
  sideDefLowerTextureName  :: ByteString,
  -- ^ Name of lower texture.
  sideDefMiddleTextureName :: ByteString,
  -- ^ Name of middle texture.
  sideDefSector            :: Int16
  -- ^ Index of sector this sidedef faces.
  }

-- | Segs are split up linedefs that are produced by the BSP
-- construction process. Whenever a BSP node splits a linedef, two
-- segs are created representing both sides of the split.
--
data Seg = Seg {
  segStartVertex :: Int16,
  -- ^ Index of start vertex.
  segEndVertex :: Int16,
  -- ^ Index of end vertex.
  segAngle :: Int16,
  -- ^ Angle of the seg.
  segLineDef :: Int16,
  -- ^ Index of linedef this seg is part of.
  segDirection :: Int16,
  -- ^ 0 if seg is in same diretion as linedef, 1 otherwise.
  segOffset :: Int16
  -- ^ Offset of the seg relative to the linedef.
  }

-- | A SSector (sub-sector?) is also produced by the BSP construction
-- process. All sectors are split into ssectors (convex polygons).
--
data SSector = SSector {
  ssectorSegCount :: Int16,
  -- ^ Number of segs that make up this ssector.
  ssectorSegStart :: Int16
  -- ^ Index of first seg of this sector.
  }

-- | Sectors are defined by enclosing linedefs, and the properties
-- below. In WADs, each region of a map with different ceiling or
-- floor heights or textures, or different specials and tags need to
-- be their own sectors.
--
data Sector = Sector {
  sectorFloorHeight   :: Int16,
  -- ^ Height of floor.
  sectorCeilingHeight :: Int16,
  -- ^ Height of ceiling.
  sectorFloorFlat     :: LumpName,
  -- ^ Name of flat for floor texturing.
  sectorCeilingFlat   :: LumpName,
  -- ^ Name of flat for ceiling texturing.
  sectorLightLevel    :: Int16,
  -- ^ Light level of sector. Used as index in COLORMAP for darkening
  -- colors.
  sectorSpecial       :: Int16,
  -- ^ Sector special.  Not decoded yet.
  sectorTag           :: Int16
  -- ^ Sector tag.  Not decoded yet.
}

-- | Node of the BSP tree.  This is created by the BSP construction
-- process and is used to speed up rendering.
--
data Node = Node {
  nodeX :: Int16,
  -- ^ X position of start of node line.
  nodeY :: Int16,
  -- ^ Y position of start of node line.
  nodeDX :: Int16,
  -- ^ Delta X for end of node line.
  nodeDY :: Int16,
  -- ^ Delta Y for end of node line.
  nodeRightBBUY :: Int16,
  -- ^ Upper Y coordinate of right bounding box.
  nodeRightBBLY :: Int16,
  -- ^ Lower Y coordinate of right bounding box.
  nodeRightBBLX :: Int16,
  -- ^ Lower X coordinate of right bounding box.
  nodeRightBBUX :: Int16,
  -- ^ Upper X coordinate of right bounding box.
  nodeLeftBBUY :: Int16,
  -- ^ Upper Y coordinate of left bounding box.
  nodeLeftBBLY :: Int16,
  -- ^ Lower Y coordinate of left bounding box.
  nodeLeftBBLX :: Int16,
  -- ^ Lower X coordinate of left bounding box.
  nodeLeftBBUX :: Int16,
  -- ^ Upper X coordinate of left bounding box.
  nodeRightNodeOrSSector :: Either Int16 Int16,
  -- ^ When Left, index of right recursive node, when Right index of
  -- right ssector.
  nodeLeftNodeOrSSector :: Either Int16 Int16
  -- ^ When Left, index of left recursive node, when Right index of
  -- left ssector.
  }

-- | Reject array. This is a bit map and not decoded yet.
--
data Reject = Reject {
  rejectBytes :: ByteString
  }

-- | Blocklist is a list of linedef indices.
--
type Blocklist = [Int16]

-- | Blockmap, determines which blocks intersect with linedefs.
--
data Blockmap = Blockmap {
  blockmapOriginX :: Int16,
  -- ^ X origin in level coordinates.
  blockmapOriginY :: Int16,
  -- ^ Y origin in level coordinates.
  blockmapColumns :: Int16,
  -- ^ Number of columns.
  blockmapRows :: Int16,
  -- ^ Number of rows.
  blockmapBlocklists :: [Blocklist]
  -- ^ Blocklists for all blocks, left-to-right, bottom-to-top in
  -- level coordinates.
  }

-- | 14 palettes, each a list of 256 RGB tuples.
--
data Palettes = Palettes [[(Word8, Word8, Word8)]]

-- | Colormap contains 34 maps, 256 bytes each.
--
data Colormap = Colormap [ByteString]

-- | Patch descriptor.
--
data PatchDescriptor = PatchDescriptor {
  patchDescriptorXOffset :: Int16,
  -- ^ X offset in wall coordinate system for this patch.
  patchDescriptorYOffset :: Int16,
  -- ^ Y offset in wall coordinate system for this patch.
  patchDescriptorPNameIndex :: Int16,
  -- ^ Index in PNAMES of the picture to use.
  patchDescriptorStepDir :: Int16,
  -- ^ Documented in UDS, but usage unknown.
  patchDescriptorColorMap :: Int16
  -- ^ Documented in UDS, but usage unknown.
  }

-- | Wall texture.
--
data Texture = Texture {
  textureName :: LumpName,
  -- ^ Name of the texture.
  textureWidth :: Int16,
  -- ^ Texture width.
  textureHeight :: Int16,
  -- ^ Texture height.
  texturePatchDescriptors :: [PatchDescriptor]
  -- ^ List of patches, in the order they appear in the WAD.
  }

-- | All supported thing types.  Unrecogized types are encoded as
-- 'ThingTypeOther'.
--
data ThingType
  = ZeroThing -- Appears in PLUTONIA.WAD
  | Player1StartPos
  | Player2StartPos
  | Player3StartPos
  | Player4StartPos
  | DeathMatchStartPos
  | FormerHuman
  | WolfensteinOfficer
  | FormerHumanSergeant
  | FormerHumanCommando
  | Imp
  | Demon
  | Spectre
  | LostSoul
  | Cacodemon
  | HellKnight
  | BaronOfHell
  | Arachnotron
  | PainElemental
  | Revenant
  | Mancubus
  | ArchVile
  | Spiderdemon
  | Cyberdemon
  | BossBrain

  | TeleportLanding
  | BossShooter
  | SpawnSpot

  | Chainsaw
  | Shotgun
  | SuperShotgun
  | Chaingun
  | RocketLauncher
  | Plasmagun
  | BFG9000

  | AmmoClip
  | ShotgunShells
  | Rocket
  | CellCharge
  | BoxOfAmmo
  | BoxOfShells
  | BoxOfRockets
  | CellChargePack
  | Backpack

  | StimPack
  | Medikit
  | HealthPotion
  | SpiritArmor
  | SecurityArmor
  | CombatArmor
  | MegaSphere
  | SoulSphere
  | Invulnerability
  | BerserkPack
  | Invisibility
  | RadiationSuit
  | ComputerMap
  | LightAmplificationGoggles

  | BlueKeyCard
  | RedKeyCard
  | YellowKeyCard
  | BlueSkullKey
  | RedSkullKey
  | YellowSkullKey

  | Barrel
  | BurningBarrel
  | Candle
  | Candelabra
  | TallTechnocolumn
  | TallGreenPillar
  | TallRedPillar
  | ShortGreenPillar
  | ShortGreenPillarWithHeart
  | ShortGreenPillarWithBeatingHeart
  | ShortRedPillar
  | ShortRedPillarWithSkull
  | Stalagmite
  | BurntGrayTree
  | LargeBrownTree
  | TallBlueFirestick
  | TallGreenFirestick
  | TallRedFirestick
  | ShortBlueFirestick
  | ShortGreenFirestick
  | ShortRedFirestick
  | FloorLamp
  | TallTechnoLamp
  | ShortTechnoLamp
  | EvilEyeSymbol
  | FlamingSkullRock
  | ImpaledHuman
  | TwitchingImpaledHuman
  | SkullOnPole
  | FiveSkullShishKebap
  | PileOfSkullsAndCandles
  | HangingVictim
  | HangingVictimTwitching
  | HangingPairOfLegs
  | HangingVictim1Leg
  | HangingLeg
  | HangingVictimNoGuts
  | HangingVictimNoGutsBrain
  | HangingTorsoLookingDown
  | HangingTorsoOpenSkull
  | HangingTorsoLookingUp
  | HangingTorsoNoBrain
  | HangingBilly

  | DeadPlayer
  | DeadFormerHuman
  | DeadFormerSergeant
  | DeadImp
  | DeadDemon
  | DeadCacodemon
  | DeadLostSoulInvisible
  | BloodyMessExplodedPlayer
  | BloodyMessAsAbove
  | PoolOfBlood
  | PoolOfGuts
  | SmallPoolOfGuts
  | PoolOfBrains
  | HangingVictimTwitching2
  | HangingVictimArmsSpread
  | HangingVictim1Legged
  | HangingPairOfLegs2
  | HangingLeg2
  | ThingTypeOther Int
    deriving (Show)


-- | Convert an integer thing type as found in the WAD file to
-- Haskell.
--
thingTypeFromNumber :: Integral a => a -> ThingType
thingTypeFromNumber n = case n of
-- Mostly taken from: UDS in the version at
-- http://web.archive.org/web/20100906191901/http://the-stable.lancs.ac.uk/~esasb1/doom/uds/things.html
--
  0 -> ZeroThing -- Appears in PLUTONIA.WAD
  1 -> Player1StartPos
  2 -> Player2StartPos
  3 -> Player3StartPos
  4 -> Player4StartPos
  11 -> DeathMatchStartPos

  3004 -> FormerHuman
  84 -> WolfensteinOfficer
  9 -> FormerHumanSergeant
  65 -> FormerHumanCommando
  3001 -> Imp
  3002 -> Demon
  58 -> Spectre
  3006 -> LostSoul
  3005 -> Cacodemon
  69 -> HellKnight
  3003 -> BaronOfHell
  68 -> Arachnotron
  71 -> PainElemental
  66 -> Revenant
  67 -> Mancubus
  64 -> ArchVile
  7 -> Spiderdemon
  16 -> Cyberdemon
  88 -> BossBrain

  14 -> TeleportLanding
  89 -> BossShooter
  87 -> SpawnSpot

  2005 -> Chainsaw
  2001 -> Shotgun
  82 -> SuperShotgun
  2002 -> Chaingun
  2003 -> RocketLauncher
  2004 -> Plasmagun
  2006 -> BFG9000

  2007 -> AmmoClip
  2008 -> ShotgunShells
  2010 -> Rocket
  2047 -> CellCharge
  2048 -> BoxOfAmmo
  2049 -> BoxOfShells
  2046 -> BoxOfRockets
  17 -> CellChargePack
  8 -> Backpack

  2011 -> StimPack
  2012 -> Medikit
  2014 -> HealthPotion
  2015 -> SpiritArmor
  2018 -> SecurityArmor
  2019 -> CombatArmor
  83 -> MegaSphere
  2013 -> SoulSphere
  2022 -> Invulnerability
  2023 -> BerserkPack
  2024 -> Invisibility
  2025 -> RadiationSuit
  2026 -> ComputerMap
  2045 -> LightAmplificationGoggles

  5 -> BlueKeyCard
  13 -> RedKeyCard
  6 -> YellowKeyCard
  40 -> BlueSkullKey
  38 -> RedSkullKey
  39 -> YellowSkullKey

  2035 -> Barrel
  70 -> BurningBarrel
  34 -> Candle
  35 -> Candelabra
  48 -> TallTechnocolumn
  30 -> TallGreenPillar
  32 -> TallRedPillar
  31 -> ShortGreenPillar
  24 -> ShortGreenPillarWithHeart
  36 -> ShortGreenPillarWithBeatingHeart -- According to http://doom.wikia.com/wiki/Thing_types
  33 -> ShortRedPillar
  37 -> ShortRedPillarWithSkull
  47 -> Stalagmite
  43 -> BurntGrayTree
  54 -> LargeBrownTree
  44 -> TallBlueFirestick
  45 -> TallGreenFirestick
  46 -> TallRedFirestick
  55 -> ShortBlueFirestick
  56 -> ShortGreenFirestick
  57 -> ShortRedFirestick
  2028 -> FloorLamp
  85 -> TallTechnoLamp
  86 -> ShortTechnoLamp
  41 -> EvilEyeSymbol
  42 -> FlamingSkullRock
  25 -> ImpaledHuman
  26 -> TwitchingImpaledHuman
  27 -> SkullOnPole
  28 -> FiveSkullShishKebap
  29 -> PileOfSkullsAndCandles
  50 -> HangingVictim
  49 -> HangingVictimTwitching
  52 -> HangingPairOfLegs
  51 -> HangingVictim1Leg
  53 -> HangingLeg
  73 -> HangingVictimNoGuts
  74 -> HangingVictimNoGutsBrain
  75 -> HangingTorsoLookingDown
  76 -> HangingTorsoOpenSkull
  77 -> HangingTorsoLookingUp
  78 -> HangingTorsoNoBrain
  72 -> HangingBilly

  15 -> DeadPlayer
  18 -> DeadFormerHuman
  19 -> DeadFormerSergeant
  20 -> DeadImp
  21 -> DeadDemon
  22 -> DeadCacodemon
  23 -> DeadLostSoulInvisible
  10 -> BloodyMessExplodedPlayer
  12 -> BloodyMessAsAbove
--  24 -> PoolOfBlood  -- Duplicate with ShortGreenPillarWithHeart above
  79 -> PoolOfGuts
  80 -> SmallPoolOfGuts
  81 -> PoolOfBrains
  63 -> HangingVictimTwitching
  59 -> HangingVictimArmsSpread
  61 -> HangingVictim1Legged
  60 -> HangingPairOfLegs
  62 -> HangingLeg
  _ -> ThingTypeOther (fromIntegral n)
