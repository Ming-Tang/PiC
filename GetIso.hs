module GetIso where

import qualified Data.Map.Strict as M
import Types

isos :: [(String, String, Iso)]
isos = [ ("i", "i", I1)
       , ("zeroe", "zeroi", ZeroE)
       , ("swapS", "swapS", SwapS)
       , ("assocLS", "assocRS", AssocLS)
       , ("unite", "uniti", UnitE)
       , ("swapP", "swapP", SwapP)
       , ("assocLP", "assocRP", AssocLP)
       , ("distrib0", "factor0", Distrib0)
       , ("distrib", "factor", Distrib) ]

pisos :: [(String, String, PIso)]
pisos = [ ("i", "i", PI1)
        , ("zeroe", "zeroi", PZeroE)
        , ("swapS", "swapS", PSwapS)
        , ("assocLS", "assocRS", PAssocLS)
        , ("unite2", "uniti2", PUnitE2)
        --, ("unite", "uniti", PUnitE2)
        , ("swapP", "swapP", PSwapP)
        , ("assocLP", "assocRP", PAssocLP)
        , ("distrib0", "factor0", PDistrib0)
        , ("distrib1", "factor1", PDistrib1)
        --, ("distrib", "factor", PDistrib1)
        ]

isoMap :: Map Iso (String, String)
pisoMap :: Map PIso (String, String)
isoMap = mi isos
pisoMap = mi pisos

mi :: Ord i => [(a, b, i)] -> Map i (a, b)
mi = M.fromList . map (\(a, b, c) -> (c, (a, b)))

class GetIso i where
  getIso :: i -> String
  getSymIso :: i -> String
  isoNames :: [(String, String, i)]

instance GetIso Iso where
  getIso = fst . (isoMap M.!)
  getSymIso = snd . (isoMap M.!)
  isoNames = isos

instance GetIso PIso where
  getIso = fst . (pisoMap M.!)
  getSymIso = snd . (pisoMap M.!)
  isoNames = pisos

