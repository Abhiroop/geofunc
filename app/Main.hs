module Main where

import SquareLimit
import System.Environment

main :: IO ()
main =
  withArgs ["-o", "sqlimit.svg", "-w", "800"] mainMethod
