{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
#-}

module Main where
import System.Environment   
import System.Directory  
import System.IO  
import Control.Monad
import Text.LaTeX.Base
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Parser

main = do
  args <- getArgs
  let arg = args!!0
  -- txt <- readFileTex arg
  latex <- parseLaTeXFile arg
  writeFile "latex.txt" (show latex)
