{-|
Module      : Main
Description : Simple functionality test
-}

module Main where

import qualified Data.Text.IO as T
import Obj
import ObjUtils


v1, v2, v3, v4, v5, v6 :: GVert
v1 = GVert 0 0 0 Nothing
v2 = GVert 1 0 0 Nothing
v3 = GVert 1 1 0 Nothing
v4 = GVert 2 1 0 Nothing
v5 = GVert 1 2 0 Nothing
v6 = GVert 0 1 0 Nothing

vol = extrude [v1, v2, v3, v4, v5, v6] (-1, 0, -2) (0, 1, 5)

main :: IO ()
main = T.writeFile "test.obj" $ objToText vol
