
module PegSSMTree where

import Data.List
import PegSSM
import Text.PrettyPrint.HughesPJ as HPJ

data RoseTree a = Node a [RoseTree a] deriving (Show, Eq)
data NColor = Green | Red deriving (Show, Eq)

data NM = O | I | II | K Int deriving (Show, Eq)

type PTree = RoseTree (E,  NColor, Int, Int)
type DotTree = RoseTree (Int, E,  NColor, Int, Int)
type IState = (Int,Int,E,R,NM)
type CState = (D,E,[E], Int, R)

simp :: State -> CState 
simp (_,d,e,ctx,(i,_,_,_),r) = (d,e,ctx,i,r)

foldrRT :: (a -> b -> b) -> b -> RoseTree a -> b
foldrRT f v (Node x xs) =  f x (foldr (\a b -> foldrRT f b a) v xs)


stSplit :: [a] -> ([a],[a])
stSplit xs = (ys,reverse zs)
   where (ys,zs) = splitAt (div (length xs) 2) xs


inc :: NM -> NM
inc O = I
inc I = II
inc II = K 3
inc (K n) = (K $ n + 1)


cState2iState = cState2iState1 0 O [] []

cState2iState1 :: Int -> NM -> [IState] -> [CState] -> [CState] -> [IState]
cState2iState1 _ _ acc _ [] = acc

cState2iState1 n q acc ((Dw, e@(Kle _), ctx1, i1,_):stk) (y@(Up, e1@(Kle _), ctx, i,Top):ys)
   | (ctx == ctx1)   =  cState2iState1 n (nn Top ctx) ((i1,i,e,Top, K n):acc) stk ys
   | otherwise =  error ("Unmached context for expressions " ++ (show e1) ++ " and " ++ (show e))

cState2iState1 n q acc ((Dw, e1, ctx1, i1,_):stk) (y@(Up, e, ctx, i,Top):ys)
   | (e1 == e) && (ctx == ctx1)   =  cState2iState1 n (nn Top ctx) ((i1,i,e,Top, q):acc) stk ys
   | otherwise =  error ("Unmached context for expressions " ++ (show e1) ++ " and " ++ (show e))
cState2iState1 n q acc ((Dw, e1, ctx1, i1,_):stk) (y@(Up, e, ctx, i, Bot):ys)
   | (e1 == e) && (ctx == ctx1)   =  cState2iState1 n (nn Bot ctx) ((i1,i,e,Bot, q):acc) stk ys
   | otherwise =  error ("Unmached context for expressions " ++ (show e1) ++ " and " ++ (show e))

cState2iState1 n q acc stk (y@(Dw, e@(Kle _), ctx, i,Top):ys) = cState2iState1 0 O acc (y:stk) ys
cState2iState1 n q acc stk (y@(Dw, e, (Kle _):ctx, i,Top):ys) =  cState2iState1 (n+1) O acc (y:stk) ys

cState2iState1 n q acc stk (y@(Dw, e, ctx, i,r):ys) = cState2iState1 n O acc (y:stk) ys
cState2iState1 n q acc stk (y:ys) = error ( " y : " ++ (show y) ++ "\n stk :\n" ++ (show stk))


mkT :: [IState] -> [PTree] -> [PTree]
mkT [] stk = stk
mkT ((i,f,e@(Any),r,_):xs) stk            = mkT xs ((Node (e, (color r), i, f) [] ):stk)
mkT ((i,f,e@(Lit _),r,_):xs) stk          = mkT xs ((Node (e, (color r), i, f) [] ):stk)
mkT ((i,f,e@(Eps),r,_):xs) stk            = mkT xs ((Node (e, (color r), i, f) [] ):stk)
mkT ((i,f,e@(Var _),r,_):xs) (n:stk)      = mkT xs ((Node (e, (color r), i, f) [n] ):stk)
mkT ((i,f,e@(Seq _ _),r,I):xs) (n:stk)    = mkT xs ((Node (e, (color r), i, f) [n]):stk)
mkT ((i,f,e@(Seq _ _),r,II):xs) (m:n:stk) = mkT xs ((Node (e, (color r), i, f) [n,m]):stk)
mkT ((i,f,e@(Alt _ _),r,I):xs) (n:stk)    = mkT xs ((Node (e, (color r), i, f) [n]):stk)
mkT ((i,f,e@(Alt _ _),r,II):xs) (m:n:stk) = mkT xs ((Node (e, (color r), i, f) [n,m]):stk)

mkT ((i,f,e@(Kle _ ),r,II):xs) (n:m:stk)  = mkT xs ((Node (e, (color r), i, f) [m,n]):stk)
mkT ((i,f,e@(Kle _ ),r,I):xs) (n:stk)     = mkT xs ((Node (e, (color r), i, f) [n]):stk)
mkT ((i,f,e@(Kle _ ),r,K n):xs) stk  = mkT xs ((Node (e, (color r), i, f) (reverse $ take n stk)):(drop n stk))

mkT ((i,f,e@(Not _ ),r,_):xs) (n:stk)     = mkT xs ((Node (e, (color r), i, f) [n]):stk)
mkT (x:xs) (n:stk)     = error ("\n --------------- \n x : \n" ++ (show x) ++
                                "\n ---------------\n n : \n " ++
                                (show n) ++
                                "\n ---------------\n ys : \n " ++
                                (show stk))

inv :: D -> D
inv Up = Dw
inv Dw = Up

testSplit :: ([CState],[CState]) -> [((D,E,[E]),(D,E,[E]),Bool)]  
testSplit (xs,zs) = zipWith (\(d,e,ctx,i,r) (d1,e1,ctx1,i1,r1) -> ((d,e,ctx),(d1,e1,ctx1),(and [d == inv d1,e==e1,ctx==ctx1]) )) xs zs
   
-- S -> aSb/eps
gex2 = [("S",Alt (Seq (Lit 'a') (Seq (Var "S") (Lit 'b'))) (Eps)  )]

gex3 = [("S", Seq (Kle (Lit 'a'))  (Lit 'b'))  ]

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

nn :: R -> [E] -> NM
nn _ [] = O
nn Top ((Alt _ NOP):_) = II
nn Top ((Alt NOP _):_) = I
nn Top ((Seq NOP _):_) = O
nn _ ((Seq _ NOP):_) = II
nn _ _ = I


numberNodes :: PTree -> DotTree
numberNodes t = snd (numberNodes1 0 t)


numberNodes1 :: Int -> PTree -> (Int,DotTree)
numberNodes1 n (Node (e,c,i,f) [])     = (n+1, Node (n,e,c,i,f) [])
numberNodes1 n (Node (e,c,i,f) xs) = (m+1, Node (m,e,c,i,f) zs)
   where
   (m,zs) = numChilds n xs

numChilds :: Int -> [PTree] -> (Int,[DotTree])
numChilds k [] = (k,[])
numChilds k (x:xs) = (k'',t:ts)
  where
    (k',t) = numberNodes1 k x
    (k'',ts) = numChilds k' xs


nodes :: DotTree -> [(Int,E,NColor,Int,Int)]
nodes = foldrRT (\x l -> x:l) []


edges :: DotTree -> [(Int,Int,NColor)]
edges (Node _ []) = []
edges (Node (i,_,c ,_,_) xs) = [(i,i',c') | (Node (i',_ ,c',_ , _ ) _)<- xs ]  ++
                               (concatMap edges xs)


treeDoc :: DotTree -> Doc
treeDoc t = text "digraph G {" $+$
            nest 5 (vcat [ns,es]) $+$
            text "}"

   where
      es = edgeListDoc (edges t)
      ns = vcat $ map nodeDoc (nodes t)


nodeDoc :: (Int,E,NColor,Int,Int) -> Doc
nodeDoc (n,e,c,i,f) = text ("node_"++(show n)) <+>
                      attr [("label","\"" ++pprint e ++ "\n" ++(show i) ++ "-" ++ (show f) ++"\""), ("color",ppcolor c)]<+>
                      semi
edgeListDoc :: [(Int, Int,NColor)] -> Doc
edgeListDoc xs = vcat (map f xs)
   where
      f (n,k,c) = text ("node_"++(show n)) <+> text "->" <+> text ("node_"++(show k)) <+>
                  attr [("color",ppcolor c)] <+> semi


attr :: [(String,String)] -> Doc
attr xs = lbrack <+>
          (hcat $ intersperse comma $ map (\(a,v) -> (text a) HPJ.<> equals HPJ.<> (text v)) xs) <+>
          rbrack


mkDotDoc:: PTree -> Doc
mkDotDoc t = (treeDoc (numberNodes t))


quotedField :: String -> String -> Doc
quotedField f v = (doubleQuotes (text f)) <+> (text ":") <+> (doubleQuotes (text v)) 

field :: String -> String -> Doc
field f v = (doubleQuotes (text f)) <+> (text ":") <+> (text v)

fieldd :: String -> Doc -> Doc
fieldd f v = (doubleQuotes (text f)) <+> (text ":") <+> v

ppcolor :: NColor -> String
ppcolor Green = "green"
ppcolor Red = "red"

qnode :: Bool -> String ->  (E, NColor, Int, Int) -> [PTree] -> Doc
qnode b p (e, c, i, f) xs
   = vcat ([quotedField "name" (pprint e) HPJ.<> comma ,
            quotedField "parent" p HPJ.<> comma ,
            quotedField "type" (ppcolor c) HPJ.<> comma ,
            field "from" (show i) HPJ.<> comma ,
            field "to" (show f) HPJ.<> (if (null xs) then empty else comma),
            if null xs
                then empty
                else fieldd (if b then "children" else "_children") (vcat (map (pptree False (pprint e)) xs)) ])

pptree ::  Bool -> String -> PTree -> Doc
pptree b p (Node d xs)
        = braces (qnode b p d xs)

srun :: G -> E -> String -> [IState]
srun g e s = cState2iState (map simp (run g e s))

runTree :: G -> E -> String -> PTree
runTree g e s = head (mkT (reverse $ srun g e s) [])

makeTree  :: G -> E -> String -> String
makeTree g e s = render (pptree True "null"  (runTree g e s))

runDot :: G -> E -> String -> String
runDot g e s = render (mkDotDoc $ runTree g e s)
