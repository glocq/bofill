{-|
Module      : Obj
Description : Types and functions for producing .obj files
Copyright   : (c) Grégoire Locqueville, 2021
License     : All rights Reserved

The specification is implemented only (very) partially as of now,
I'm adding features as I need them.
-}

{-# LANGUAGE OverloadedStrings #-}

module Obj where

import qualified Data.Text as T

--------------------------------------------------------------------------------

-- | Regular geometric vertex
data GVert = GVert { gx :: Double,
                     gy :: Double,
                     gz :: Double,
                     gw :: Maybe Double }

instance Show GVert where
    show v = "v " ++ show (gx v)
             ++ " " ++ show (gy v)
             ++ " " ++ show (gz v)
             ++ "\n"


-- | Point in parameter space
data PVert = PVert { pu :: Double,
                     pv :: Maybe Double,
                     pw :: Maybe Double }

instance Show PVert where
    show vp = "vp " ++ show (pu vp)
              ++ maybe "" (\x -> " " ++ show x) (pv vp)
              ++ maybe "" (\x -> " " ++ show x) (pw vp)
              ++ "\ns"


-- | Normal vector
data Normal = Normal { ni :: Double,
                       nj :: Double,
                       nk :: Double }

instance Show Normal where
    show vn = "vn " ++ show (ni vn)
              ++ " " ++ show (nj vn)
              ++ " " ++ show (nk vn)
              ++ "\n"


-- | Texture vertex
data TVert = TVert { tu :: Double,
                     tv :: Maybe Double,
                     tw :: Maybe Double }

instance Show TVert where
    show vt = "vt " ++ show (tu vt)
              ++ maybe "" (\x -> " " ++ show x) (tv vt)
              ++ maybe "" (\x -> " " ++ show x) (tw vt)
              ++ "\ns"


--------------------------------------------------------------------------------

-- | All vertex data
-- Lists are to be seen as going from right to left!
data Vertices = Vertices { vg :: [GVert ], nvg :: Int,
                           vp :: [PVert ], nvp :: Int,
                           vn :: [Normal], nvn :: Int,
                           vt :: [TVert ], nvt :: Int }

vToText :: Vertices -> T.Text
vToText verts =
    (foldr dneppa "" . fmap (T.pack . show) $ vg verts) `T.append` "\n" `T.append`
    (foldr dneppa "" . fmap (T.pack . show) $ vp verts) `T.append` "\n" `T.append`
    (foldr dneppa "" . fmap (T.pack . show) $ vn verts) `T.append` "\n" `T.append`
    (foldr dneppa "" . fmap (T.pack . show) $ vt verts)
    -- We add vertices in reverse order
    where dneppa a b = T.append b a


instance Semigroup Vertices where
    -- |Left argument is assumed small
    v1 <> v2 = Vertices { vg  = vg  v1 ++ vg  v2,
                          nvg = nvg v1 +  nvg v2,
                          vp  = vp  v1 ++ vp  v2,
                          nvp = nvp v1 +  nvp v2,
                          vn  = vn  v1 ++ vn  v2,
                          nvn = nvn v1 +  nvn v2,
                          vt  = vt  v1 ++ vt  v2,
                          nvt = nvt v1 +  nvt v2 }

instance Monoid Vertices where
    mempty = Vertices { vg = [], nvg = 0, vp = [], nvp = 0,
                        vn = [], nvn = 0, vt = [], nvt = 0 }


--------------------------------------------------------------------------------

-- | ... well, a face
-- We assume given an element of type Vertices.
-- The Ints refer to vertices in that element (indexed starting from 1)
-- (with the lists in the fields of Vertices going from right to left)
data Face = Face   [ Int           ] |
            FaceT  [(Int, Int     )] |
            FaceN  [(Int,      Int)] |
            FaceTN [(Int, Int, Int)]

instance Show Face where
    show f = case f of
        (Face   idxs) -> "f" ++ aux   idxs ++ "\n"
        (FaceT  idxs) -> "f" ++ auxT  idxs ++ "\n"
        (FaceN  idxs) -> "f" ++ auxN  idxs ++ "\n"
        (FaceTN idxs) -> "f" ++ auxTN idxs ++ "\n"
        where
            -- Better do it all manually, it gets confusing trying to figure out
            -- how to fit the folds and the maps together
            aux   []             = "" -- this shouldn't happen, but whatever
            aux   (i:is)         = " " ++ show i                                     ++ aux   is
            auxT  []             = ""
            auxT  ((i,iT   ):is) = " " ++ show i ++ "/"  ++ show iT                  ++ auxT  is
            auxN  []             = ""
            auxN  ((i,   iN):is) = " " ++ show i ++ "//"                  ++ show iN ++ auxN  is
            auxTN []             = ""
            auxTN ((i,iT,iN):is) = " " ++ show i ++ "/" ++ show iT ++ "/" ++ show iN ++ auxTN is


--------------------------------------------------------------------------------

data Obj = Obj { vertices :: Vertices, faces :: [Face] }

objToText :: Obj -> T.Text
objToText obj = vToText (vertices obj) `T.append` "\n" `T.append` facesToText (faces obj) `T.append` "\n"
    where facesToText []     = ""
          facesToText (f:fs) = T.pack (show f) `T.append` facesToText fs


instance Semigroup Obj where
    -- |Left argument is assumed small
    o1 <> o2 = Obj { vertices = vertices o1 <> vertices o2,
                     faces = fmap (shiftVerts $ vertices o2) (faces o1)
                             ++ faces o2 }
               where shiftVerts verts (Face   idxs) = Face   $
                         fmap (+ nvg verts                              )               idxs
                     shiftVerts verts (FaceT  idxs) = FaceT  $
                         fmap (\(v,vt   ) -> (v+nvg verts, vt+nvt verts))               idxs
                     shiftVerts verts (FaceN  idxs) = FaceN  $
                         fmap (\(v,vn   ) -> (v+nvg verts, vn+nvn verts))               idxs
                     shiftVerts verts (FaceTN idxs) = FaceTN $
                         fmap (\(v,vt,vn) -> (v+nvg verts, vt+nvt verts, vn+nvn verts)) idxs

instance Monoid Obj where
    mempty = Obj { vertices = mempty, faces = [] }
