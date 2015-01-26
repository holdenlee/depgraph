module Main where
import Diagrams
import Utilities
import ParseUtilities

main =
  do
    let d3 = emptyD -: tr2 (0,0) "X_R" "A_R" "\\Spec R" "\\ph_R" "" "" -: c "s" "-" (-3) -: tr2 (3,-1) "X_K" "A_K" "\\Spec K" "\\ph" "" "" -: ls (a "") [(i+3,i)|i<-[1..3]] 
    putStrLn (show d3)
    appendFile "text.tex" (lay d3)
    appendFile "text.tex" (show d3)
