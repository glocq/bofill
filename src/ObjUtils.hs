{-|
Module      : ObjUtils
Description : Various utilities for dealing with Obj data
Copyright   : (c) Grégoire Locqueville, 2021
License     : All rights Reserved

Various geometric transformations and more
-}

module ObjUtils where

import Obj



translate :: (Double, Double, Double) -> GVert -> GVert
translate (x,y,z) vert = vert { gx = gx vert + x,
                                gy = gy vert + y,
                                gz = gz vert + z }

-- |Flip the order of a face's vertices
reverseFace :: Face -> Face
reverseFace (Face   l) = Face   (reverse l)
reverseFace (FaceT  l) = FaceT  (reverse l)
reverseFace (FaceN  l) = FaceN  (reverse l)
reverseFace (FaceTN l) = FaceTN (reverse l)

-- |Make an Obj value containing a polygon determined by the input vertices
polygon :: [GVert] -> Obj
polygon verts = Obj { vertices = auxV, faces = [Face [1..length verts]] }
    where auxV = mempty { vg = reverse verts, nvg = length verts }


-- |Extrude a surface. Shifts a polygon twice and performs an extrusion
-- between the two resulting polygons
extrude :: [GVert] -- ^ Vertices of the base polygon. Counterclockwise as seen from above
        -> (Double, Double, Double) -- ^ Translation vector for the /bottom/ face
        -> (Double, Double, Double) -- ^ Translation vector for the /top/ face
        -> Obj
extrude verts shiftBottom shiftTop =
    let n = length verts
        bottomVerts = fmap (translate shiftBottom) $ verts
        topVerts    = fmap (translate shiftTop   )   verts
        allVerts = bottomVerts ++ topVerts
        bottomFace = Face [n, (n-1) .. 1]
        topFace    = Face [(n+1) .. (2*n)]
        -- A bit of dark magic going on here, but you can check that we get
        -- a quadrilateral for each original pair of contiguous vertices
        lateralFaces = [Face [ i    `mod` n + 1,
                              (i+1) `mod` n + 1,
                              (i+1) `mod` n + 1 + n,
                               i    `mod` n + 1 + n] | i <- [0 .. n-1] ]
    in mempty { vertices = mempty { vg = allVerts, nvg = 2*n },
                faces = bottomFace : topFace : lateralFaces }
