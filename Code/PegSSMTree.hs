
module PegSSMTree where

import Data.List
import PegSSM
import Text.PrettyPrint.HughesPJ as HPJ

data RoseTree a = Node a [RoseTree a] deriving (Show, Eq)
data NColor = Green | Red deriving (Show, Eq)

type PTree = RoseTree (E,  NColor, Int, Int)


-- S -> aSb/eps
gex2 = [("S",Alt (Seq (Lit 'a') (Seq (Var "S") (Lit 'b'))) (Eps)  )]

start :: Zipp -> Int
start (i,[]   ,_,_) = i
start (_,(i:_)  ,_,_) = i

stop :: Zipp -> Int
stop (s,_   ,_,_) = s

selExit :: [State] -> [State]
selExit xs = [y |y@(g,Up,_,_,_,_) <- xs]

color :: R -> NColor
color Top = Green
color Bot = Red

mkTree :: [State] -> [PTree] -> [PTree]
mkTree [] xs = xs
mkTree ((_,Up,e@(Lit _),ctx,zp,r):xs) ys
   = mkTree xs ( (Node (e, color r, start zp, stop zp) []):ys )
mkTree ((_,Up,e@(Seq _ _),ctx,zp,r):xs) (a:b:ys)
   = mkTree xs ((Node (e, color r, start zp, stop zp) [b,a]) : ys)
mkTree ((_,Up,e@(Alt _ _),ctx,zp,r):xs) (a:b:ys)
   = mkTree xs ((Node (e, color r, start zp, stop zp) [b,a]) : ys)
mkTree ((_,Up,e@(Kle _),ctx,zp,r):xs) (a:ys)
   = mkTree xs ((Node (e, color r, start zp, stop zp) [a]) : ys)
mkTree ((_,Up,e@(Not _),ctx,zp,r):xs) (a:ys)
   = mkTree xs ((Node (e, color r, start zp, stop zp) [a]) : ys)
mkTree ((_,Up,Eps,ctx,zp,r):xs) ys
   = mkTree xs ((Node (Eps, color r, start zp, stop zp) []) : ys)
mkTree ((_,Up,(Var s),ctx,zp,r):xs) (a:ys)
   = mkTree xs ((Node ((Var s), color r, start zp, stop zp) [a]) : ys)
mkTree ((_,Dw,_,_,_,_):xs) ys = mkTree xs ys
mkTree (x:xs) ys = error ((show x) ++ "\n------------------\n YS = \n" ++ (show ys))


quotedField :: String -> String -> Doc
quotedField f v = (doubleQuotes (text f)) <+> (text ":") <+> (doubleQuotes (text v)) <+> comma

field :: String -> String -> Doc
field f v = (doubleQuotes (text f)) <+> (text ":") <+> (text v) <+> comma

fieldd :: String -> Doc -> Doc
fieldd f v = (doubleQuotes (text f)) <+> (text ":") <+> v

inters :: [a] -> (a -> a) -> [a]
inters [] _ = []
inters [x] _ = [x]
inters (x:xs) f = (f x):(inters xs f)

vlist :: [Doc] -> Doc
vlist [] = lbrace <+> rbrace
vlist xs = vcat [lbrack,
                 vcat (inters xs ( HPJ.<> comma) ),
                 rbrack]

ppcolor :: NColor -> String
ppcolor Green = "green"
ppcolor Red = "red"

qnode :: Int -> String ->  (E, NColor, Int, Int) -> [PTree] -> Doc
qnode n p (e, c, i, f) xs
   = vcat [ quotedField "name" (pprint e),
            quotedField "parent" p,
            quotedField "type" (ppcolor c),
            field "from" (show i),
            field "to" (show f),
            (if null xs
                  then empty
                  else fieldd "_children" (vlist (map (pptree (n+2) (pprint e)) xs)))]

pptree :: Int -> String -> PTree -> Doc
pptree n p (Node d xs)
        = (vcat [
                  lbrace,
                  nest n (qnode n p d xs),
                  rbrace
                ])

st2tree :: [State] -> Doc
st2tree xs =  lbrack <+>
              pptree 0 "null" (head (mkTree xs [])) <+>
              rbrack

makeTree :: G -> E -> String -> String
makeTree g e s = render (st2tree (run g e s))
