{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
#-}

module IsabelleParser where
import System.Environment   
import System.Directory  
import System.IO  
import Control.Monad
import Data.Graph.Inductive
import qualified Data.List.Ordered
import Data.Tree
import qualified Data.List
import qualified Data.Map.Strict as Map
import Search
import Text.ParserCombinators.Parsec
import Data.Char
import System.IO.Unsafe
import qualified Data.MultiMap as MM
import Data.Maybe
--import Text.Parsec.Prim

--import theorem-prover.Runner as R

import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
--import qualified Text.Parsec.Prim as Pr

-- http://stackoverflow.com/questions/5034685/what-is-the-haskell-syntax-to-import-modules-in-subdirectories
-- All you need to do is pass the -i flag to the compiler with a colon-delimited list of directories. The compiler will then check those directories for the source files of the imported modules.
import Utilities
import DAGViz

isaKeywords :: [String]
isaKeywords = ["lemma", "locale", "definition", "abbreviation", "theorem", "lemmas"]

lexer :: P.TokenParser ()
lexer  = P.makeTokenParser (emptyDef{P.commentStart = "(*", P.commentEnd = "*)", P.reservedNames = isaKeywords})

whiteSpace= P.whiteSpace lexer
lexeme    = P.lexeme lexer
symbol    = P.symbol lexer
natural   = P.natural lexer
parens    = P.parens lexer
semi      = P.semi lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer

propInfo :: Parser (String, Maybe String, Maybe String)
propInfo = do {
	locale <- option Nothing (do {
		symbol "(";
		symbol "in";
		expr <- identifier;
		symbol ")";
		return (Just expr)});
	propName <- identifier;
	rule <- option Nothing (do {
		symbol "[";
		expr <- identifier; --squares
		symbol "]";
		return (Just expr)});
	return (propName, locale, rule)} <?> "prop"

instance (Show a, Show b, Ord a) => Show (MM.MultiMap a b) where
	show mm = unlines (fmap (\k -> show k ++ ": " ++ show (MM.lookup k mm)) (MM.keys mm))

data ProgramInfo = ProgramInfo {deps:: MM.MultiMap String String
                 , subfields:: Map.Map String (Map.Map String String)
                 , current:: String
		 , fields:: [String]
                 , currentFile:: String} deriving Show
--current name of file

emptyPI:: ProgramInfo
emptyPI = ProgramInfo MM.empty Map.empty "" [] ""
--instance R.Pointed ProgramInfo where
--  point = ProgramInfo Map.empty Map.empty ""
                 
insert2:: (Ord a, Eq b) => a -> b -> MM.MultiMap a b -> MM.MultiMap a b
insert2 x y mm = if y `elem` (MM.lookup x mm) then mm else (MM.insert x y mm)

insertDep:: String -> ProgramInfo -> ProgramInfo
insertDep y pi = pi{deps=insert2 (current pi) y (deps pi)}

insertSF:: String -> String -> ProgramInfo -> ProgramInfo
insertSF name y pi = 
          let 
              currentMap = Map.lookup (current pi) (subfields pi)
          in
              case currentMap of
                   Nothing -> pi{subfields=Map.insert (current pi) (Map.singleton name y) (subfields pi)}
                   Just mp -> pi{subfields=Map.insert (current pi) (Map.insert name y mp) (subfields pi)}

lookupSF:: String -> String -> ProgramInfo -> Maybe String
lookupSF propName field pi = 
         case (Map.lookup propName (subfields pi)) of 
              Nothing -> Nothing
              Just sfs-> Map.lookup field sfs

insertField:: String -> ProgramInfo -> ProgramInfo
insertField y pi = pi{fields = y:(fields pi)}

isabelleParser:: ProgramInfo -> Parser ProgramInfo	
isabelleParser pi = do {
	do {
		eof;
		return pi
	} <|>
	try( do {
--annoying
		propType <- ((reserved "lemma") <|> (reserved "lemmas") <|> (reserved "theorem"));
-- <|> (reserved "definition")
-- <|> (reserved "abbreviation")
-- <|> (reserved "locale")
--foldl1 <|> (fmap reserved isaKeywords);
		(propName,maybeLocale,maybeRule) <- propInfo;
--                if propName == "to"
--                 then Pr.parserFail "to found!"
--		 else 
                 isabelleParser (pi{current = propName}
			-: insertField propName
                        -: insertSF "file" (currentFile pi) 
			-: doIf (isJust maybeLocale) (insertSF "locale" (removeJust maybeLocale))
                        -: doIf (isJust maybeRule) (insertSF "rule" (removeJust maybeRule)))
	}) <|>
	try (do {
		x <- identifier;
		if x `elem` (fields pi)
			then isabelleParser (insertDep x pi)
			else isabelleParser pi
	}) <|>
	do {
		anyToken;
		isabelleParser pi
	} <|>
	do {
		fail "!!"
		--many (noneOf " \n");
		--spaces;
		--isabelleParser pi
	}}
		
               
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
        nums = fmap (\kd -> (lookup2 kd kNums)) ds
      in
        fmap (\n -> ((), n)) nums) 
    ctxts = fmap (\k -> (adjs k, lookup2 k kNums, k, [])) ks
  in 
    buildGr ctxts

--unsafe
getFile::String -> String
getFile name = unsafePerformIO (readFile name)
      
parseToGraph:: String -> Gr String ()
parseToGraph input = 
  let
    pi = justRight (parse (isabelleParser emptyPI) "error" input)
  in
    getDepGraph (pi)
    

{-
defaultDotC2 :: (Show el, Ord cl) => (Node -> nl  -> String) -> (Node -> Maybe cl) -> Gr nl el -> String
-}
parseToDotGraph:: String -> String
parseToDotGraph input = 
	let 
		pi = justRight (parse (isabelleParser emptyPI) "error" input)
		graph = getDepGraph (pi)
	in
		defaultDotC2 (\_ l -> l ++ (
                             case (lookupSF l "rule" pi) of
                                  Nothing -> ""
                                  Just str-> " [" ++ str ++ "]")) (\_ l -> lookupSF l "file" pi) graph
--		  case MM.lookup (removeJust (lab graph n)) (deps pi) of
--		    [] -> Nothing
--		    l  -> Just (l !! 0)
--		  ) graph	

piToDotGraph:: ProgramInfo -> String
piToDotGraph pi = 
	let 
		graph = getDepGraph (pi)
	in
		defaultDotC2 (\_ l -> l ++ (
                             case (lookupSF l "rule" pi) of
                                  Nothing -> ""
                                  Just str-> " [" ++ str ++ "]")) (\_ l -> lookupSF l "file" pi) graph

ioFile:: String -> String -> (String -> String) -> IO ()
ioFile inputF outputF f =
 do  
  handle <- openFile inputF ReadMode
  contents <- hGetContents handle
  appendFile outputF (f contents)

ioFiles:: [String] -> String -> (String -> String) -> IO ()
ioFiles inputFs outputF f =
  do  
--illegal
    handles <- sequence (fmap (\x -> openFile x ReadMode) inputFs)
    contents <- sequence (fmap hGetContents handles)
    let content = unlines contents
    appendFile outputF (f content)

main:: IO ()
main = do 
     let inputFs = ["FunctionLemmas.thy", "RingModuleFacts.thy", "MonoidSums.thy", "LinearCombinations.thy", "SumSpaces.thy", "VectorSpace2.thy"] 
     let outputF = "output.dot"
     handles <- sequence (fmap (\x -> openFile x ReadMode) inputFs)
     contents <- sequence (fmap hGetContents handles)
     let pi = foldIterate2 (\fileName text pi -> justRight (parse (isabelleParser pi{currentFile = fileName}) "error" text)) inputFs contents emptyPI
     writeFile outputF (piToDotGraph pi)     
--	ioFiles ["FunctionLemmas.thy", "RingModuleFacts.thy", "MonoidSums.thy", "LinearCombinations.thy", "SumSpaces.thy", "VectorSpace2.thy"] "output.dot" parseToDotGraph
--	handle <- openFile "VectorSpace2.thy" ReadMode
--  	contents <- hGetContents handle
--	let x = parse (isabelleParser emptyPI) "error" contents
--	putStrLn (show x)

  --acc ++ 
  --  "set" ++ n ++ " :: " ++ t ++ " -> " ++
  --  dataName ++ " -> " ++ dataName ++ "\n" 
  --  ++ "
    
--"set?n :: ?t -> ?t -> 
    --n ++ " :: " ++ " -> " ++ 
   
  
