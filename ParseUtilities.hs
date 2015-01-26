{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
#-}

module ParseUtilities ( ProgramInfo (current, deps, fields, currentFile), emptyPI, insertDep, insertSF, insertSF2, lookupSF, insertField, ProgramParser, ioFile, ioFiles, fieldsParser, readFields, chain, chainPI, getFile) where
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

instance (Show a, Show b) => Show (MM.MultiMap a b) where
  show m = show (MM.toMap m)

data ProgramInfo = ProgramInfo {deps:: MM.MultiMap String String
                 , subfields:: Map.Map String (Map.Map String String)
                 , current:: String
		 , fields:: [String]
                 , currentFile:: String
                 --, names:: Map.Map String String
                 } deriving Show
                 --display names
                 --, labels:: Map.Map String String} deriving Show

emptyPI:: ProgramInfo
emptyPI = ProgramInfo MM.empty Map.empty "" [] ""

insert2:: (Ord a, Eq b) => a -> b -> MM.MultiMap a b -> MM.MultiMap a b
insert2 x y mm = if y `elem` (MM.lookup x mm) then mm else (MM.insert x y mm)

--insertName:: String -> ProgramInfo -> ProgramInfo
--insertName str pi = pi{labels = Map.insert (current pi) str}

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
                   
insertSF2:: String -> String -> String -> ProgramInfo -> ProgramInfo
insertSF2 lab name y pi = 
          let 
              currentMap = Map.lookup lab (subfields pi)
          in
              case currentMap of
                   Nothing -> pi{subfields=Map.insert lab (Map.singleton name y) (subfields pi)}
                   Just mp -> pi{subfields=Map.insert lab (Map.insert name y mp) (subfields pi)}

lookupSF:: String -> String -> ProgramInfo -> Maybe String
lookupSF propName field pi = 
         case (Map.lookup propName (subfields pi)) of 
              Nothing -> Nothing
              Just sfs-> Map.lookup field sfs

insertField:: String -> ProgramInfo -> ProgramInfo
insertField y pi = pi{fields = y:(fields pi)}

type ProgramParser = ProgramInfo -> Parser ProgramInfo

ioFile:: String -> String -> (String -> String) -> IO ()
ioFile inputF outputF f =
 do  
  handle <- openFile inputF ReadMode
  contents <- hGetContents handle
  appendFile outputF (f contents)

ioFiles:: [String] -> String -> (String -> String) -> IO ()
ioFiles inputFs outputF f =
  do  
    handles <- sequence (fmap (\x -> openFile x ReadMode) inputFs)
    contents <- sequence (fmap hGetContents handles)
    let content = unlines contents
    appendFile outputF (f content)

fieldsParser:: ProgramParser
fieldsParser pi = 
    do {eof; return pi}
      <|> (try (do {
        symbol "#";
        expr <- identifier;
        fieldsParser (pi{current = expr})
        --parses eol automatically?
      })) 
      <|> (do {
        expr <- many1 (noneOf "\n");
        fieldsParser (insertDep expr pi) -- does this preserve the order?
      })
      <|> do{ anyToken; fieldsParser pi}

-- -> (String -> Parser ())
readFields:: String  -> Map.Map String [String]
readFields contents = MM.toMap $ deps $ justRight (parse (fieldsParser emptyPI) "error" contents)
  

chain:: [String] -> a -> (a -> String -> a) -> (a -> Parser a) -> IO a
chain inputFs init action parser = 
  do 
    handles <- sequence (fmap (\x -> openFile x ReadMode) inputFs)
    contents <- sequence (fmap hGetContents handles)
    let pi = foldIterate2 (\fileName text pi -> justRight (parse (parser (action pi fileName)) "error" text)) inputFs contents init
    return pi

chainPI:: [String] -> ProgramParser -> IO ProgramInfo 
chainPI inputFs parser = chain inputFs emptyPI (\pi fileName -> pi{currentFile = fileName}) parser

--unsafe
getFile::String -> String
getFile name = unsafePerformIO (readFile name)


--chainPI:: [String] -> (ProgramInfo -> Parser ProgramInfo) -> IO ProgramInfo
