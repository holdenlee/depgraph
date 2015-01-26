{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
#-}

module Diagrams ( ArrowBody, abTex, ArrowHead, ahTex, ArrowTail, atTex, ArrowStyle, ArrowPos, posTex, getPos, Coord, NodeInfo, defStyle, getStyle, ArrowInfo, defArr, Diagram, emptyD, newNodeL, newArrL, lookupN, lookupA, addArrL, addArr, addNodeL, addNode, styleToTex, posToTex, ifl, caseEqL, arrowTextToTex, arrowToTex, ls, ls2, nl, atl, at, grid, lay, allA, allN, sq, tr1, tr2, a, c) where
import System.Environment   
import System.Directory  
import System.IO  
import Control.Monad
--import Data.Graph.Inductive
--import qualified Data.List.Ordered
import Data.Tree
import qualified Data.List as L
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

data ArrowBody = Norm | Dash | Dot | Eql | Wig | None deriving (Eq, Show)

--abTex:: Map ArrowBody String
abTex :: ArrowBody -> String
abTex x = caseEqL x [(Norm, "-"), (Dash, "--"), (Dot, "."), (Wig, "~"), (Eql, "="), (None, "")]
-- Map.fromList

data ArrowHead = NormA | TwoA | NoA deriving (Eq, Show)

ahTex:: ArrowHead -> String 
ahTex x = caseEqL x  [(NormA, ">"), (TwoA, ">>"), (NoA, "")]

data ArrowTail = Bar | Hook | NoT deriving (Eq, Show)

atTex:: ArrowTail -> String 
atTex x = caseEqL x  [(Bar, "|"), (Hook, "^("), (NoT, "")]

type ArrowStyle = (ArrowTail, ArrowBody, ArrowHead)

data ArrowPos = Up | Down | Middle deriving (Eq, Show)
posTex:: ArrowPos -> String
posTex x = caseEqL x  [(Up, "^"), (Down, "_"), (Middle, "-")]

getPos::String -> ArrowPos
getPos s = caseEqL s [("^", Up), ("_", Down), ("-", Middle)]

type Coord = (Int, Int, Int)

data NodeInfo = NodeInfo {nodeL::Int, disp::String, nodeC::Coord} deriving Show

--data ArrowAttr = ArrowAttr {style::ArrowStyle, textPos::ArrowPos, curve::Into, textCent::Bool, textNudge::Int}

defStyle::ArrowStyle
defStyle = (NoT, Norm, NormA)  

--defAttr::ArrowAttr
--defAttr = ArrowAttr defStyle Up 0 True 0
getStyle::String -> ArrowStyle
getStyle str =
  case str of
   "" -> (NoT, Norm, NormA)
   ch:rs ->
     let
       (x,y,z) = getStyle rs
     in 
       case ch of
         's' -> ((x, y, TwoA))
         '=' -> (NoT, Eql, NormA)
         'i' -> ((Hook, y, z))
         '.' -> ((x, Dot, z))
         '-' -> ((x, Dash, z))
         'b' -> ((Hook, y, TwoA))
         '|' -> ((Bar, y, z))
         ' ' -> (NoT, None, NoA)
         '~' -> ((x, Wig, z))
   

data ArrowInfo = ArrowInfo {arrL:: Int, dom:: Int, codom:: Int, name1::String, name2:: String, style::ArrowStyle, textPos::ArrowPos, curve::Int, textCent::Bool, textNudge::Int} deriving Show
                            --attr::ArrowAttr} --, domC::Coord, codomC::Coord

defArr::ArrowInfo
defArr = ArrowInfo 0 0 0 "" "" defStyle Up 0 True 0  

--data Rect = Rect {x0::Int, x1::Int, y0::Int, y1::Int}

--data Layout = Layout { bounds::Rect, table::Map Int (Map Int (NodeInfo, [ArrowInfo]))}

data Diagram = Diagram {nodes:: Map.Map Int NodeInfo, arrs:: Map.Map Int ArrowInfo, zdir::(Int, Int)} deriving Show

emptyD::Diagram
emptyD = Diagram (Map.empty) (Map.empty) (0,0)

newNodeL:: Diagram -> Int
newNodeL d =
  let
    ks = Map.keys (nodes d)
  in
    if null ks then 1 else maximum ks + 1

newArrL:: Diagram -> Int
newArrL d =
  let
    ks = Map.keys (arrs d)
  in
    if null ks then 1 else maximum ks + 1

convIndexN:: Int -> Diagram -> Int
convIndexN i d =
    if (i<0) then ((newNodeL d) + i) else i

convIndexA:: Int -> Diagram -> Int
convIndexA i d =
    if (i<0) then ((newArrL d) + i) else i

lookupN:: Int -> Diagram -> NodeInfo
lookupN i d =
  let
    ns = nodes d
  in
    lookup2 (convIndexN i d) ns

lookupA:: Int -> Diagram -> ArrowInfo
lookupA i d =
  let
    arrows = arrs d
  in
    (lookup2 (convIndexA i d) arrows)

addArrL:: Int -> ArrowInfo -> Diagram -> Diagram
addArrL i ai d = d{arrs = Map.insert i ai (arrs d)}

addArr:: ArrowInfo -> Diagram -> Diagram
addArr ai d = addArrL (newArrL d) ai d

addNodeL:: Int -> NodeInfo -> Diagram -> Diagram
addNodeL i ni d = d{nodes = Map.insert i ni (nodes d)}

addNode:: NodeInfo -> Diagram -> Diagram
addNode ni d = addNodeL (newNodeL d) ni d

styleToTex:: ArrowStyle -> String
styleToTex (tail, body, head) = (atTex tail) ++ (abTex body) ++ (ahTex head)

rep2:: a -> a-> Int -> [a]
rep2 a b t = if (t<=0) then (replicate (-t) a)
                     else (replicate t b)

posToTex2:: Int -> Int -> String
posToTex2 x y = 
  let
    xTex = rep2 'l' 'r' x
    yTex = rep2 'u' 'd' y
  in xTex++yTex

posToTex:: (Int,Int) -> Coord -> String
posToTex zd (x,y,z) = 
  let 
    --(x,y,z) = domC ai
    --(x2,y2,z2) = codomC ai
    (zx, zy) = zd
  in 
    posToTex2 (x+zx*z) (y+zy*z)

ifl::[(Bool, b)] -> b
ifl li = case li of
  (bool,x):xs -> if bool then x else ifl xs 
--must be nonempty

caseEqL:: (Eq a)=> a -> [(a,b)]->b
caseEqL x li = case li of
  (xval,y):xs -> if (x==xval) then y else caseEqL x xs

arrowTextToTex::String -> String -> ArrowPos -> Bool-> Int->String
arrowTextToTex n1 n2 tp tc tn =
  let
    (s1,s2) = case tp of
      Up -> ("^","_")
      Down -> ("_","^")
      Middle -> ("-", "_")
    tnText = (if tc then "-" else "") ++ (rep2 '<' '>' tn)
  in
      s1 ++ tnText ++ "{" ++ n1 ++ "}" ++ s2 ++ tnText ++ "{" ++ n2 ++ "}"
  
--{style::ArrowStyle, textPos::Pos, curve::Into, textCent::Bool, textNudge::Int}
--z tilt, 
arrowToTex:: Coord -> ArrowInfo -> Diagram -> String
arrowToTex coor ai diag = 
  let
    (x,y,z) = coor
    zdi = zdir diag
    d = dom ai
    cod = codom ai
    n1 = name1 ai 
    n2 = name2 ai
    --dc = domC ai 
    --cdc= codomC ai
    --lookup the codomain in the diagram
    (x2,y2,z2) = nodeC (lookupN cod diag)
      --nodeC (lookup2 cod (nodes diag))
    (xd,yd,zd) = (x2-x,y2-y,z2-z)
    sty = style ai
    tp  = textPos ai
    curv= curve ai
    tc  = textCent ai
    tn  = textNudge ai
  in
    "\\ar@{" ++ styleToTex sty ++ "}"
      ++ (ifl [(curv<0, "@/_"++(show curv)++"pc/"),
           (curv>0, "@/^"++(show curv)++"pc/"),
           (curv==0, "")])
      ++ "[" ++ (posToTex zdi (xd,yd,zd)) ++ "]"
      ++ (arrowTextToTex n1 n2 tp tc tn)

ls:: (a->b->b)->[a]->b->b
ls f li st = foldl (flip f) st li

ls2:: (a->c->b->b)->[a]->[c]->b->b
ls2 f li1 li2 st = foldl (\z (x,y) -> f x y z) st (zip li1 li2)

nl::Int -> String -> (Int, Int) -> Diagram -> Diagram
nl i nam (x,y) d = addNode (NodeInfo i nam (x,y,0)) d

n:: String -> (Int, Int) -> Diagram -> Diagram
n nam (x,y) d = nl (newNodeL d) nam (x,y) d

atl::Int -> String -> String -> String -> (Int, Int) -> Diagram -> Diagram
atl i str n1 n2 (st,end) d =
  if str=="m" then
    addArr (defArr{arrL=i,dom=convIndexN st d,codom=convIndexN end d, name1=n1, name2=n2, style = getStyle str, textPos = Middle}) d
  else 
    addArr (defArr{arrL=i,dom=convIndexN st d,codom=convIndexN end d, name1=n1, name2=n2, style = getStyle str}) d

at2::String -> String -> String -> (Int, Int) -> Diagram -> Diagram
at2 str n1 n2 (st,end) d = atl (newArrL d) str n1 n2 (st,end) d

at::String -> String -> (Int, Int) -> Diagram -> Diagram
at str n1 (st,end) d = atl (newArrL d) str n1 "" (st,end) d

a::String -> (Int, Int) -> Diagram -> Diagram
a = at ""

a2::String -> String -> (Int, Int) -> Diagram -> Diagram
a2 = at2 ""

set3d::(Int,Int)->Diagram->Diagram
set3d (x,y) d = d{zdir=(x,y)}

draw3d::Diagram -> Diagram
draw3d = set3d (1,-1)

grid:: (Int,Int) -> (Int,Int) -> [String] -> Diagram -> Diagram
grid (x,y) (w,h) names d = (ls2 n names [(x+i,y+j)  | j<-[0..(h-1)], i<-[0..(w-1)]]) d

--change arrow
c::String -> String -> Int -> Diagram -> Diagram
c chg arg i d =
  let
    oldArr = (lookupA i d)
    i2 = if (i<0) then ((newArrL d) + i) else i
    newArr =
      case chg of
       "s" -> oldArr{style = getStyle arg}
       "n1"-> oldArr{name1 = arg}
       "n2"-> oldArr{name2 = arg}
       "p" -> oldArr{textPos = getPos arg}
       "c" -> oldArr{curve = read arg}
       "tc"-> oldArr{textCent = not (textCent oldArr)}
       "tn"-> oldArr{textNudge= read arg}
       "r" -> oldArr{dom = (codom oldArr), codom = (dom oldArr)}
  in
    d{arrs = Map.insert i2 newArr (arrs d)}
--change arrow style
-- ca::Int -> String -> Diagram -> Diagram
-- ca i str d =
--   let
--     newArr = (lookup2 i (arrs d)){style = getStyle str}
--   in
--     d{arrs = insert i newArr (arrs d)}

--change last arrow style
-- cal:: String -> Diagram -> Diagram
-- cal str d = ca (newArrL d - 1) str d

--change arrow position 
-- cp::Int -> String -> Diagram -> Diagram
-- cp i str d =
--   let
--     newArr = (lookup2 i (arrs d)){textPos = getPos str}
--   in
--     d{arrs = insert i newArr (arrs d)}

--change last arrow position
--cpl:: String -> Diagram -> Diagram
--cpl str d = cp (newArrL d - 1) str d

lay::Diagram -> String
lay d =
  let
    --first compile the nodes
    nodeMap = Map.empty -: ls (\node mp -> (
                             let
                               (x,y,z) = nodeC node
                               (zx,zy) = zdir d
                             in
                               Map.insert (x+zx*z,y+zy*y) node mp)) (Map.elems (nodes d))
    ks = Map.keys nodeMap
    xks = map (\(x,y)->x) ks
    yks = map (\(x,y)->y) ks
    xmin = minimum xks
    xmax = maximum xks
    xd = xmax - xmin
    ymin = minimum yks
    ymax = maximum yks
    yd = ymax - ymin
    --next compile the arrows
    --dom arr is an index
    arrMap = MM.empty -: ls (\arr mp -> (
                               MM.insert (nodeL (lookupN (dom arr) d)) arr mp)) (Map.elems (arrs d))
    showCell = (\i j -> case (Map.lookup (i,j) nodeMap) of
                 Nothing -> ""
                 Just node -> disp node ++ (
                   let
                     arrowList = MM.lookup (nodeL node) arrMap
                   in
                     join (map (\ai -> arrowToTex (i,j,0) ai d) arrowList))
                 )
    showRow = (\j -> L.intercalate " & " (map (\i -> showCell i j) [xmin..xmax]))
  in
    "\\xymatrix{" ++ (L.intercalate "\\\\\n" (map showRow [ymin..ymax])) ++ "}"

allA:: Diagram -> [Int]
allA d = Map.keys (arrs d)

allN:: Diagram -> [Int]
allN d = Map.keys (nodes d)
--using this you can easily reverse all arrows, etc.

sq::(Int, Int) -> String -> String -> String -> String -> String -> String -> String -> String -> Diagram -> Diagram
sq (x,y) n1 n2 n3 n4 a1 a2 a3 a4 d = d -: grid (x,y) (2,2) [n1,n2,n3,n4]
                                     -: a a1 (-4,-3)
                                     -: a a2 (-4,-2)
                                     -: a a3 (-3,-1)
                                     -: a a4 (-2,-1)
                                     -: ls (c "p" "_") [-3,-1]

tr1:: (Int, Int) -> String -> String -> String -> String -> String -> String -> Diagram -> Diagram
tr1 (x,y) n1 n2 n3 a1 a2 a3 d = d -: n n1 (x,y) -: n n2 (x+1,y) -: n n3 (x+1, y+1)
                                -: a a1 (-3,-2)
                                -: a a2 (-3,-1)
                                -: a a3 (-2,-1)

tr2:: (Int, Int) -> String -> String -> String -> String -> String -> String -> Diagram -> Diagram
tr2 (x,y) n1 n2 n3 a1 a2 a3 d = d -: n n1 (x,y) -: n n2 (x+2,y) -: n n3 (x+1, y+1)
                                -: a a1 (-3,-2)
                                -: a a2 (-3,-1)
                                -: a a3 (-2,-1)

ses:: (Int, Int) -> String -> String -> String -> String -> String-> String -> String ->  Diagram -> Diagram
ses (x,y) n1 n2 n3 n4 n5 a1 a2 d = d -: grid (x,y) (5,1) [n1,n2,n3,n4,n5] -: a "" (-5,-4) -: a a1 (-4,-3) -: a a2 (-3,-2) -: a "" (-2,-1)
