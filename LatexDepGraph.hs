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
import Data.Graph.Inductive
import qualified Data.List.Ordered
import Data.Tree
import qualified Data.List
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.Parsec
import Data.Char
import System.IO.Unsafe
import qualified Data.MultiMap as MM
import Data.Maybe
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

import Utilities
import ParseUtilities
import DAGViz

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

endWith:: String -> Parser ()
endWith str = 
  try (do {
    symbol str;
    return ()
  }) <|> do {
    identifier;
    endWith str
  } <|> do {
    anyToken;
    endWith str
  }

endWithAndLookOut:: String -> Parser a -> a -> Parser a
endWithAndLookOut end par res = 
  try (do {
    symbol end;
    return res
  }) <|> try (do {
    res2 <- par;
    endWithAndLookOut end par res2
  }) <|> do {
    identifier;
    endWithAndLookOut end par res
  } <|> do {
    anyToken;
    endWithAndLookOut end par res
  }

oneOfSyms:: [String] -> Parser String
oneOfSyms strs = 
  foldl1 (<|>) (map (try.symbol) strs) 
  --do
    --expr <- identifier
    --if env `elem` envs then fail "" else --else do nothing
    
latexEnvParserInt:: String -> [String] -> Parser String
latexEnvParserInt env labs = endWithAndLookOut ("\\end{" ++ env ++ "}") par ""
  where 
    par = do {
      string "\\";
      oneOfSyms labs;
      string "{";
      expr <- many1 (noneOf "}");
      symbol "}";
      return expr
    }
{-
  try (do {
    symbol ("\\end{" ++ env ++ "}");
    return str;
  }) <|> (try(do {
    string "\\";
    oneOfSyms labs;
    string "{";
    expr <- many1 (noneOf "}");
    symbol "}";
    latexEnvParserInt env labs expr
  })) <|> do {
    identifier;
    latexEnvParserInt env labs str
  } <|> do {
    anyToken;
    latexEnvParserInt env labs str
  }
  -}

latexEnvParser:: [String] -> [String] -> ProgramParser
latexEnvParser envs labs pi = 
  do
    --parse \begin{thm}
    symbol ("\\begin{")
    env <- oneOfSyms envs
    symbol "}"
    --parse [label]
    name <- option Nothing (try(do {
		symbol "[";
		expr <- many1 (noneOf "]");
		symbol "]";
		return (Just expr)}));
    many (oneOf " \n");
    --parse \label{...} currently it has to be at the beginning
    {-string "\\";
    oneOfSyms labs;
    string "{";
    expr <- many1 (noneOf "}");
    symbol "}";
    let ref = expr-}    
    ref <- latexEnvParserInt env labs;
    --problem with the following: label could be anywhere.
    {-ref <- option "unlabeled" (try(do {
                string "\\";
                oneOfSyms labs;
                string "{";
                expr <- many1 (noneOf "}");
                symbol "}";
                return expr}))-}
    --parse \end{thm}
    --endWith ("\\end{" ++ env ++ "}")
    return (pi{current = ref}
           -: insertSF "type" env
           -: insertField ref
           -: doIf (isJust name) (insertSF "name" (removeJust name)))

latexProofParser:: [String] -> [String] -> ProgramParser
latexProofParser proofs refs pi =
  do 
    symbol ("\\begin{") --parse \begin{proof}
    env <- oneOfSyms proofs
    symbol "}"
    proofOf <- ((try (latexProofBrackets refs (current pi))) <|> (return (current pi)))
    let pi2 = pi{current = proofOf}
    latexProofParserInt env refs pi2

latexProofBrackets:: [String] -> String -> Parser String
latexProofBrackets refs cur = 
 do
  symbol "["
  endWithAndLookOut "]" par cur 
 where par = do {
      string "\\";
      oneOfSyms refs; --use another ref?
      string "{";
      expr <- many1 (noneOf "}");
      symbol "}";
      return expr}

--use endBy?
latexProofParserInt:: String -> [String] -> ProgramParser
latexProofParserInt env refs pi = 
  --parse \end{proof} 
  do {try (symbol ("\\end{" ++ env ++ "}")); return pi}
    <|> 
    --parse \ref{...}
    try (do {string "\\"; 
        oneOfSyms refs; 
        string "{";
        expr <- many1 (noneOf "}");
        symbol "}";
        latexProofParserInt env refs (insertDep expr pi)})
    <|> do {
      identifier;
      latexProofParserInt env refs pi
    } <|> do {
      anyToken;
      latexProofParserInt env refs pi
    }    
    

--followed by?
--parser for header: tries to parse a Dependency Unit header. Return PI with new state and info if successful, fail otherwise.
--parser for body: tries to parse the inside of a Dep Unit body. Adds dependency fields.
depParser:: ProgramParser -> ProgramParser -> ProgramParser
depParser headerP bodyP pi = 
  --do{pi2 <- headerP pi; return pi2}
    do {eof; return pi}
      <|> (try (do{pi2 <- headerP pi; depParser headerP bodyP pi2}))
      <|> (try (do{pi2 <- bodyP pi; depParser headerP bodyP pi2}))
      <|> do{ identifier;  (depParser headerP bodyP pi)}
      <|> do{ anyToken; (depParser headerP bodyP pi)}

-- # Theorems, Labels, Refs, Proofs, Files
latexDepParser::Map.Map String [String] -> ProgramParser
latexDepParser mp = 
  let 
    [envs, labs, proofs, refs] = map (tryWithDefault (flip Map.lookup mp) []) ["Theorems", "Labels", "Proofs", "Refs"] 
  in
    depParser (latexEnvParser envs labs) (latexProofParser proofs refs)
--envs labs proof refs

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
    kNums = Map.fromList (zip ks [1..])
    --first lookup the key in the map to find dependencies
    --then find the num associated to each key.
    
    adjs = (\k-> 
      let 
        num = lookup2 k kNums
        ds = MM.lookup k mp
        nums = filterJust (fmap (\kd -> (Map.lookup kd kNums)) ds)
      in
        fmap (\n -> ((), n)) nums) 
    ctxts = fmap (\k -> (adjs k, lookup2 k kNums, k, [])) ks
  in 
    buildGr ctxts
 
--should separate out the IO...
--(Gr String ())
--need aux files     
latexToDepGraph:: String -> String -> IO ()
latexToDepGraph inputF outputF = 
 do
  handle <- openFile inputF ReadMode
  contents <- hGetContents handle
  let fields = readFields contents
  let inputFs = lookup2 "Files" fields
  --removeJustWithDefult (Map.lookup "Files" fields) []
  let auxF = (lookup2 "Aux" fields) !! 0
  auxHandle <- openFile auxF ReadMode
  auxContents <- hGetContents auxHandle
  pi <- chainPI inputFs (latexDepParser fields)
  case (parse (latexAuxParser pi) "error" auxContents) of 
    Left error -> putStrLn (show error)
    Right pi2 ->
      do
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

test::IO ()
test = 
  do
    let fields = readFields (getFile "test1.txt")
    putStrLn (show fields)
    let pi = emptyPI{currentFile = "test1.tex"}
    putStrLn (show pi)
    let pi1 = justRight (parse (latexEnvParser ["thm"] ["label"] pi) "error" (getFile "test1.tex"))
    putStrLn (show pi1)
    let [envs, labs, proofs, refs] = map (tryWithDefault (flip Map.lookup fields) []) ["Theorems", "Labels", "Proofs", "Refs"] 
    putStrLn (show [envs, labs, proofs, refs])
    let pi2 = (parse (latexDepParser fields pi) "error" (getFile "test1.tex"))
    putStrLn (show pi2)
