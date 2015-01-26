module Main where
import Diagrams
import Utilities
import ParseUtilities

main =
  do
--    let s1 = emptyD -: sq (0,0) "A" "B" "C" "D" "f_1" "f_2" "f_3" "f_4"
--    appendFile "text.tex" (lay s1)
--    appendFile "text.tex" (show s1)
    
--    let d2 = emptyD -: tr1 (0,0) "A" "B" "C" "f_1" "f_2" "f_3"
--    appendFile "text.tex" (lay d2)
    --putStrLn (show d2)

    let d3 = emptyD -: grid (0,0) (2,5) ["K_A", "\\tau^*\\cal P", "\\Hom(A, (\\av)^{\\vee})", "\\Pic_e(\\av\\times A)", "\\Hom(A, A^{\\vee})", "\\Pic_e(A\\times A)", "\\la^{\\vee}\\circ K_A", "(\\la \\times 1)^* \\tau^* \\cal P_A", "\\la^{\\vee}", "(1\\times \\la)^*\\cal P_A"]             
              -: ls (at " " "\\in") [(1,3),(2,4),(7,5),(8,6)]
              -: ls (at "i" "") [(3,4),(5,6)]
              -: ls (at "|" "") [(1,2),(7,8),(9,10)]
              -: ls (at "=" "") [(7,9),(8,10)]
              -: c "n1" "\\sim" (-1)
              -: a "\\la^{\\vee}\\circ \\bullet" (3,5)
              -: a "(\\la \\times 1)^*" (4,6)
              -: ls (a "") [(1,7), (2,8)]
              -: ls2 (c "c") ["-3","3"] [-2,-1]

    putStrLn (show d3)
    appendFile "text.tex" (lay d3)
    appendFile "text.tex" (show d3)

    --let d4 = emptyD -: grid (0,0) (4,3) ["\id_\av","\Hom(\av,\av)","\Pic_{e,A/k}(\av)","\cal P_A","f^{\vee}","\Hom(B^{\vee},\av)","\Pic_{e,A/k}","\text{same}","\id_{B^{\vee}}","\Hom(B^{\vee},B^{\vee})","\Pic_{e,B/k}(B^{\vee})","\cal P_B"] -: ls (at "|" "") [(1,5),(4,8),(9,5),(12,8)] -: ls (at "i" "") [(2,3),(6,7),(10,11)] -: ls (at " " "\in") [(1,2),(9,10)] -: ls (at " " "\ni") [(3,4),(11,12)] -: a "()\circ f^{\vee}" (2,6) -: a "(1\times f^{\vee})^*" (3,7) -: a "f^{\vee}\circ()" (10,6) -: a "( f^{\vee}\times 1)^*" (11,7) -: ls (c "p" "_") [-1,-2] 
        
--    let d5 = emptyD -: ses (0,0) "0" "ker\al_0(N)" "A[\ell^N]" "A[\ell^N]" "\coker \al_0(N)" "0" "" "\al_0(N)" "" -: ses (0,0) "0" "ker\al_0(N+1)" "A[\ell^{N+1}]" "A[\ell^{N+1}]" "\coker \al_0(N+1)" "0" "" "\al_0(N+1)" "" -: ls (a "") [(8,2),(9,3),(10,4),(11,5)] -: ls (n "\vdots") [(1,2),(2,2),(3,2)] -: ls (c "n1" "[\ell]") [11,12,13] -: c "n2" "\cong" 14
