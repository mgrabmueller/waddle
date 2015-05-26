{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------
-- |
-- Module:      Game.Waddle
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
module Game.Waddle
       (module Game.Waddle.Types,
        module Game.Waddle.Load,
        module Game.Waddle.ExportJS,
        mergeWads
       ) where

import Game.Waddle.Types
import Game.Waddle.Load
import Game.Waddle.ExportJS

import qualified Data.Map as Map

-- FIXME: This is not correct yet. We have to figure out how to
-- properly merge WAD files.
--
mergeWads :: [Wad] -> Wad
mergeWads wads = foldl1 mrg wads
 where
   mrg wad1 wad2 = Wad {
     wadHeader = wadHeader wad2,
     wadDirectory = wadDirectory wad1 ++ wadDirectory wad2,
     wadLumps = wadLumps wad1 ++ wadLumps wad2,
      -- Later WADs override earlier ones.
     wadLumpLookup = Map.union (wadLumpLookup wad2) (wadLumpLookup wad1),
     wadFlats = Map.union (wadFlats wad2) (wadFlats wad1),
     wadSprites = Map.union (wadSprites wad2) (wadSprites wad1),
     wadPatches = wadPatches wad2,
     wadTextures = Map.union (wadTextures wad2) (wadTextures wad1),
     wadLevels = Map.union (wadLevels wad2) (wadLevels wad1),
     wadPNames = wadPNames wad2,
     wadColormap = wadColormap wad2,
     wadPalettes = wadPalettes wad2
     }
