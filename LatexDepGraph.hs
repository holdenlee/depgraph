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

import Data.Maybe
import Data.Monoid
import Data.Graph.Inductive
import qualified Data.Map.Strict as M
import qualified Data.MultiMap as MM

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Error

import Text.LaTeX.Base
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Syntax

import Debug.Trace

import Utilities
import ParseUtilities
import DAGViz

debug = flip trace

lexer :: P.TokenParser ()
lexer  = P.makeTokenParser (emptyDef)

whiteSpace= P.whiteSpace lexer
lexeme    = P.lexeme lexer
symbol    = P.symbol lexer
natural   = P.natural lexer
parens    = P.parens lexer
semi      = P.semi lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer

texSeqs :: [LaTeX] -> LaTeX
texSeqs = foldl mappend TeXEmpty

--takes care of bug where quotes appear
stripQuotes :: String -> String
stripQuotes str = reverse $ tail $ reverse $ tail str

showLatex :: LaTeX -> String
showLatex = stripQuotes . show . render

argToLatex :: TeXArg -> LaTeX
argToLatex ta = 
    case ta of 
      FixArg latex -> latex
      OptArg latex -> latex	
      MOptArg latexs -> texSeqs latexs	
      SymArg latex -> latex	
      MSymArg latexs -> texSeqs latexs

argsToLatex :: [TeXArg] -> LaTeX
argsToLatex tas = texSeqs $ map argToLatex tas

{-| 
  Given a section of tex, find the string inside \label{...}.
  The map should have the entry ("Labels", [<list of label keywords>]).
  Typically, the label keyword is just "label"
  It will use the first occurrence of label.
-}
findLabel :: [String] -> LaTeX -> String
findLabel labs latex =
    case latex of
      TeXRaw txt -> ""
      TeXComm str args -> 
          if str `elem` labs 
             then showLatex $ argsToLatex args
             else findLabel labs (argsToLatex args)
      TeXCommS str -> ""	
      TeXEnv str args latex2 -> ""	
      TeXMath mt latex2 -> ""
      TeXLineBreak mmeas bool -> ""	
      TeXBraces latex2 -> findLabel labs latex2
      TeXComment comment -> ""
      TeXSeq latex1 latex2 -> 
          let 
              ans1 = findLabel labs latex1
              ans2 = findLabel labs latex2
          in
            if ans1 == "" then ans2 else ans1
      TeXEmpty -> ""

{-| 
  Given a section of tex, find all strings inside \ref{...}.
  The map should have the entry ("Refs", [<list of label keywords>]).
  Typically, the refs keyword is just "ref".
-}
findRefs :: [String] -> LaTeX -> [String]
findRefs refs latex =
    case latex of
      TeXRaw txt -> []
      TeXComm str args -> 
          if str `elem` refs
             then [showLatex $ argsToLatex args]
             else findRefs refs (argsToLatex args)
      TeXCommS str -> []	
      TeXEnv str args latex2 -> findRefs refs latex2	
      TeXMath mt latex2 -> findRefs refs latex2
      TeXLineBreak mmeas bool -> []	
      TeXBraces latex2 -> findRefs refs latex2
      TeXComment comment -> []
      TeXSeq latex1 latex2 -> (findRefs refs latex1) ++ (findRefs refs latex2)
      TeXEmpty -> []

{-|
  Given a multimap of Theorems, Proofs, Refs, Labels names, a section of latex, and ProgramInfo, parses the latex into the ProgramInfo.
-}
latexToPI' :: MM.MultiMap String String -> LaTeX -> ProgramInfo -> ProgramInfo
latexToPI' mm latex pi = 
    case latex of
      TeXRaw txt -> pi 
                    --skip over raw TeX
      TeXComm str args -> latexToPI' mm (argsToLatex args) pi
      TeXCommS str -> pi	
      TeXEnv str args latex2 -> ifelselist
                                [(str `elem` (MM.lookup "Theorems" mm), let {
           lab = findLabel (MM.lookup "Labels" mm) latex2;
           name = if length args > 0 
                    then 
                        case args!!0 of 
                          OptArg n -> showLatex n
                    else
                        ""
                                                                    }
                                                                    in pi{current = lab} |> insertSF "type" str |> insertField lab |> doIf (name /= "") (insertSF "name" name)),
                                 (str `elem` (MM.lookup "Proofs" mm), pi{current = if length args > 0 
                                                                                then
                                                                                    findLabel (MM.lookup "Refs" mm) (argsToLatex args)
                                                                                else
                                                                                    current pi}|> foldIterate insertDep (findRefs (MM.lookup "Refs" mm) latex2))]
               (latexToPI' mm latex2 pi)
      TeXMath mt latex2 -> pi
      TeXLineBreak mmeas bool -> pi	
      TeXBraces latex2 -> latexToPI' mm latex2 pi	
      TeXComment comment -> pi
      TeXSeq latex1 latex2 -> latexToPI' mm latex2 (latexToPI' mm latex1 pi)	
      TeXEmpty -> pi

latexToPI :: MM.MultiMap String String -> LaTeX -> ProgramInfo
latexToPI mm latex = latexToPI' mm latex emptyPI

{-
latexToPI :: LaTeX -> ProgramInfo -> ProgramInfo
latexToPI latex pi = 
    case latex of
      TeXRaw txt -> pi
      TeXComm str args -> pi
      TeXCommS str -> pi	
      TeXEnv str args latex2 -> pi	
      TeXMath mt latex2 -> pi
      TeXLineBreak mmeas bool -> pi	
      TeXBraces latex2 -> pi	
      TeXComment comment -> pi
      TeXSeq latex1 latex2 -> pi	
      TeXEmpty -> pi
-}

parseLaTeX2 :: String -> LaTeX
parseLaTeX2 str = 
    case parseLaTeX $ fromString str of
      Left _ -> TeXEmpty
      Right t -> t

chainPI2:: [String] -> (LaTeX -> ProgramInfo -> ProgramInfo) -> IO ProgramInfo 
chainPI2 inputFs parser = 
    do
      handles <- sequence (fmap (\x -> openFile x ReadMode) inputFs)
      contents <- sequence (map ((fmap parseLaTeX2) . hGetContents) handles)
      return $ foldl (\pi (fileName, latex) -> parser latex (pi{currentFile = fileName})) emptyPI (zip inputFs contents)

latexAuxParser::ProgramParser
latexAuxParser pi =
  do {eof; return pi}
    <|> try (do { 
      string "\\newlabel{";
      lab <- many1 (noneOf "{}");
      many1 (oneOf "{}");
      num <- many1 (noneOf "{}");
      latexAuxParser (insertSF2 lab "num" num pi)})
    <|> do{ many1 (noneOf "\n"); do{eof;return pi} <|> do{string "\n";latexAuxParser pi}}
--figure out how to do pattern matching!

showThm::String -> ProgramInfo -> String
showThm propName pi = (removeJustWithDefault (lookupSF propName "type" pi) "") ++ " " ++ (removeJustWithDefault ( lookupSF propName "num" pi) "") ++ ": " ++ removeJustWithDefault (lookupSF propName "name" pi) propName

getDepGraph:: ProgramInfo-> Gr String ()
getDepGraph pi = 
  let 
    mp = deps pi
    ks = fields pi --MM.keys mp
    kNums = M.fromList (zip ks [1..])
    --first lookup the key in the map to find dependencies
    --then find the num associated to each key.
    --adjs::Int -> [((), Int)]
    adjs = (\k-> 
      let 
        num = lookup2 k kNums
        ds = MM.lookup k mp
        nums = filterJust (fmap (\kd -> (M.lookup kd kNums)) ds)
      in
        fmap (\n -> ((), n)) nums) 
    --ctxts = fmap (\k -> (adjs k `debug` (show $ adjs k), lookup2 k kNums, k, [])) ks
  in 
    mkGraph (zip [1..] ks) (concat $ map (\k -> map (\(x,y) -> (y,lookup2 k kNums,x)) (adjs k)) ks)
    --This has a bug where it won't make edges that reference nodes earlier in the list.
    --buildGr ctxts `debug` (show ctxts)
 
--should separate out the IO...
--(Gr String ())
--need aux files     
latexToDepGraph:: String -> String -> IO ()
latexToDepGraph inputF outputF = 
 do
  handle <- openFile inputF ReadMode
  contents <- hGetContents handle
  let fields = readFields contents
  let inputFs = MM.lookup "Files" fields
  --removeJustWithDefult (M.lookup "Files" fields) []
  let auxF = (MM.lookup "Aux" fields) !! 0
  auxHandle <- openFile auxF ReadMode
  auxContents <- hGetContents auxHandle
  pi <- chainPI2 inputFs (latexToPI' fields)
  case (parse (latexAuxParser pi) "error" auxContents) of 
    Left error -> putStrLn (show error)
    Right pi2 ->
      do
        --putStrLn $ show pi2
        let graph = getDepGraph pi2
        let dot = defaultDotC2 (\_ l -> showThm l pi2) (\_ l -> lookupSF l "file" pi) graph
        writeFile outputF (dot)
--should have safety?

main:: IO ()
main = do
  args <- getArgs
  let inputF = args !! 0
  let outputF = args !! 1
  latexToDepGraph inputF outputF

test2:: IO ()
test2 = do
  putStrLn $ findLabel ["ref"] (argsToLatex  [OptArg (TeXSeq (TeXRaw $ fromString "Proof of Theorem~") (TeXComm "ref" [FixArg (TeXRaw $ fromString "thm:1")]))])

{-
test::IO ()
test = 
  do
    let fields = readFields (getFile "test1.txt")
    putStrLn (show fields)
    let pi = emptyPI{currentFile = "test1.tex"}
    putStrLn (show pi)
    let pi1 = justRight (parse (latexEnvParser ["thm"] ["label"] pi) "error" (getFile "test1.tex"))
    putStrLn (show pi1)
    let [envs, labs, proofs, refs] = map (tryWithDefault (flip M.lookup fields) []) ["Theorems", "Labels", "Proofs", "Refs"] 
    putStrLn (show [envs, labs, proofs, refs])
    let pi2 = (parse (latexDepParser fields pi) "error" (getFile "test1.tex"))
    putStrLn (show pi2)
-}
