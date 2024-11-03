{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Start where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

myCircle :: Diagram B
myCircle = circle 1 # fc blue -- fill colour
                    # lw veryThick -- line width
                    # lc purple -- line colour
                    # dashingG [0.2,0.05] 0 --

example :: Diagram B
example = circle 1 # fc red # lw none
      ||| circle 1 # fc green # lw none

example2 :: Diagram B
example2 =
  (square 1 # fc aqua `atop` circle 1) # showOrigin

example3 :: Diagram B
example3 = (s1 # showOrigin) ||| (s2 # showOrigin)
  where
    s1 = circle 1 ||| square 2
    s2 = circle 1 === square 2

example4 :: Diagram B
example4 = vcat (replicate 3 circles)
  where
    circles = hcat (map circle [1..6])

circleSqV1 = beside (r2 (1,1)) (circle 1) (square 2)

circleSqV2 = beside (r2 (1,-2)) (circle 1) (square 2)

example5 :: Diagram B
example5 = hcat [circleSqV1, strutX 1, circleSqV2]

example6 :: Diagram B
example6 = ell ||| vrule 3 ||| ell
  where ell = circle 1 # scaleX 0.5 # rotateBy (1/6)

example7 :: Diagram B
example7 = ell # snugR <> ell # snugL
  where ell = circle 1 # scaleX 0.5 # rotateBy (1/6)

circleRect  = circle 1 # scale 0.5
          ||| square 1 # scaleX 0.3

circleRect2 = circle 1 # scale 0.5
          ||| square 1 # scaleX 0.3
                       # rotateBy (1/6)
                       # scaleX 0.5

example8 :: Diagram B
example8 = hcat [circleRect, strutX 1, circleRect2]

example9 :: Diagram B
example9 = hrule (2 * sum sizes) === circles # centerX
  where
    circles = hcat . map alignT . zipWith scale sizes
                $ repeat (circle 1)
    sizes   = [2,5,4,7,1,3]

example10 :: Diagram B
example10 = regPoly 6 1



example11 :: Diagram B
example11 =
  atPoints (trailVertices $ regPoly 6 1)
  (map node [1..6])


node :: Int -> Diagram B
node n = text (show n) # fontSizeL 0.2 # fc white
      <> circle 0.2 # fc green # named n

arrowOpts = with & gaps       .~ small
                 & headLength .~ local 0.15

tournament :: Int -> Diagram B
tournament n =
  atPoints (trailVertices $ regPoly n 1)
  (map node [1..n])
  # applyAll [connectOutside' arrowOpts j k
             | j <- [1 .. n-1], k <- [j+1 .. n]]

example12 :: Diagram B
example12 = tournament 6

mainMethod = mainWith example12

