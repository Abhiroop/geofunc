{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module SquareLimit where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Prelude hiding (cycle)

{- 1982 Henderson combinators

beside :: (Int, Int, Picture, Picture) -> Picture
beside (m, n, p, q) = p to the left of q; width ratio (m : n)

above :: (Int, Int, Picture, Picture) -> Picture
above (m, n, p, q) = p on top of q; height ratio (m : n)

rot :: Picture -> Picture
rot p = 90 degrees anticlockwise rotation

-}


squareLimit :: Diagram B
squareLimit = cycle corner

corner = nonet corner2 side2 side2
               (rot side2) uTile (rot tTile)
               (rot side2) (rot tTile) (rot qTile)

nonet p1 p2 p3 p4 p5 p6 p7 p8 p9
  = onTopOf 1 2  (nextTo 1 2 p1 (nextTo 1 1 p2 p3))
   (onTopOf 1 1  (nextTo 1 2 p4 (nextTo 1 1 p5 p6))
                 (nextTo 1 2 p7 (nextTo 1 1 p8 p9)))

nextTo m n d1 d2 = scaledD1 ||| scaledD2
  where
    scaledD1 = d1 # scaleX (m / total)
    scaledD2 = d2 # scaleX (n / total)
    total = m + n

onTopOf m n d1 d2 = scaledD1 === scaledD2
  where
    scaledD1 = d1 # scaleY (m / total)
    scaledD2 = d2 # scaleY (n / total)
    total = m + n


side1   = quartet blankTile blankTile (rot tTile) tTile
side2   = quartet side1 side1 (rot tTile) tTile
corner1 = quartet blankTile blankTile blankTile uTile
corner2 = quartet corner1 side1 (rot side1) uTile

pseudocorner = quartet corner2 side2 (rot side2) (rot tTile)
pseudolimit  = cycle pseudocorner

example = cycle (rot tTile)

uTile = cycle $ rot qTile
tTile = quartet pTile qTile rTile sTile

cycle p1 = quartet p1 (rot $ rot $ rot p1) (rot p1) (rot $ rot p1)

cycle' p = quartet p (rot p) (rot (rot p)) (rot $ rot $ rot p)

quartet p q r s = scale (1/2) $ centerXY ((p ||| q) === (r ||| s))

rot p = rotate (90 @@ deg) p

pTile = makeTile markingsP
qTile = makeTile markingsQ
rTile = makeTile markingsR
sTile = makeTile markingsS

blankTile :: Diagram B
blankTile = lw none $ square 16

makeTile :: [[P2 Double]] -> Diagram B
makeTile =
  lw thin . centerXY . mconcat . map fromVertices


-- measurements from https://frank-buss.de/lisp/functional.html

markingsP = [
  [ (4^&4), (6^&0) ],
  [ (0^&3), (3^&4), (0^&8), (0^&3) ],
  [ (4^&5), (7^&6), (4^&10), (4^&5) ],
  [ (11^&0), (10^&4), (8^&8), (4^&13), (0^&16) ],
  [ (11^&0), (14^&2), (16^&2) ],
  [ (10^&4), (13^&5), (16^&4) ],
  [ (9^&6), (12^&7), (16^&6) ],
  [ (8^&8), (12^&9), (16^&8) ],
  [ (8^&12), (16^&10) ],
  [ (0^&16), (6^&15), (8^&16), (12^&12), (16^&12) ],
  [ (10^&16), (12^&14), (16^&13) ],
  [ (12^&16), (13^&15), (16^&14) ],
  [ (14^&16), (16^&15) ]
  ]

markingsQ = [
  [ (2^&0), (4^&5), (4^&7) ],
  [ (4^&0), (6^&5), (6^&7) ],
  [ (6^&0), (8^&5), (8^&8) ],
  [ (8^&0), (10^&6), (10^&9) ],
  [ (10^&0), (14^&11) ],
  [ (12^&0), (13^&4), (16^&8), (15^&10), (16^&16), (12^&10), (6^&7), (4^&7), (0^&8) ],
  [ (13^&0), (16^&6) ],
  [ (14^&0), (16^&4) ],
  [ (15^&0), (16^&2) ],
  [ (0^&10), (7^&11) ],
  [ (9^&12), (10^&10), (12^&12), (9^&12) ],
  [ (8^&15), (9^&13), (11^&15), (8^&15) ],
  [ (0^&12), (3^&13), (7^&15), (8^&16) ],
  [ (2^&16), (3^&13) ],
  [ (4^&16), (5^&14) ],
  [ (6^&16), (7^&15) ]
  ]

markingsR = [
  [ (0^&12), (1^&14) ],
  [ (0^&8), (2^&12) ],
  [ (0^&4), (5^&10) ],
  [ (0^&0), (8^&8) ],
  [ (1^&1), (4^&0) ],
  [ (2^&2), (8^&0) ],
  [ (3^&3), (8^&2), (12^&0) ],
  [ (5^&5), (12^&3), (16^&0) ],
  [ (0^&16), (2^&12), (8^&8), (14^&6), (16^&4) ],
  [ (6^&16), (11^&10), (16^&6) ],
  [ (11^&16), (12^&12), (16^&8) ],
  [ (12^&12), (16^&16) ],
  [ (13^&13), (16^&10) ],
  [ (14^&14), (16^&12) ],
  [ (15^&15), (16^&14) ]
  ]

markingsS = [
  [ (0^&0), (4^&2), (8^&2), (16^&0) ],
  [ (0^&4), (2^&1) ],
  [ (0^&6), (7^&4) ],
  [ (0^&8), (8^&6) ],
  [ (0^&10), (7^&8) ],
  [ (0^&12), (7^&10) ],
  [ (0^&14), (7^&13) ],
  [ (8^&16), (7^&13), (7^&8), (8^&6), (10^&4), (16^&0) ],
  [ (10^&16), (11^&10) ],
  [ (10^&6), (12^&4), (12^&7), (10^&6) ],
  [ (13^&7), (15^&5), (15^&8), (13^&7) ],
  [ (12^&16), (13^&13), (15^&9), (16^&8) ],
  [ (13^&13), (16^&14) ],
  [ (14^&11), (16^&12) ],
  [ (15^&9), (16^&10) ]
  ]
mainMethod = mainWith squareLimit


-- Experiments

rotTest :: Diagram B
rotTest = hcat [pTile, strutX 1, rot pTile]

almostFish :: Diagram B
almostFish = pTile === rTile
