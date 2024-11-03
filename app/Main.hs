module Main where

import Start
import System.Environment

main :: IO ()
main =
  withArgs ["-o", "circle.svg", "-w", "400"] mainMethod
