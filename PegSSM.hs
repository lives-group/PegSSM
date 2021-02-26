
module PegSSM where
import Data.List

data D = Up  | Dw   deriving Show
data R = Top | Bot deriving Show

data E = Seq E E
       | Alt E E
       | Kle E
       | Not E
       | Var String
       | Lit Char
       | Eps
       | Any
       | NOP
       deriving Show

       

type G    = [(String,E)]
type Zipp = (Int,[Int],String,String)

type State = (G,D,E,[E],Zipp, R)

andE :: E -> E
andE e = Not (Not e)

       
parens :: Bool -> String -> String
parens True s ="(" ++ s ++ ")"
parens False s = s

prec :: E -> Int
prec (Alt _ _) = 7
prec (Seq _ _) = 8
prec (Kle _) = 9    
prec (Not _) = 9
prec (Var _) = 10
prec (Lit _) = 10
prec (Any) = 10
prec (Eps) = 10
prec (NOP) = 10

pprint :: E -> String
pprint (Eps) = "\x03b5"
pprint (Lit c) = c:[]
pprint (Any) = "."
pprint (Alt e1 e2) = (parens (7 >= prec e1) (pprint e1)) ++ "\\" ++ (parens (7 > prec e2) (pprint e2))
pprint (Seq e1 e2) = (parens (8 >= prec e1) (pprint e1)) ++ (parens (8 > prec e2) (pprint e2))
pprint (Not e1) = "!" ++ (parens (9 > prec e1) (pprint e1))
pprint (Kle e1) = (parens (9 > prec e1) (pprint e1)) ++ "*"
pprint (Var x)  = x
pprint (NOP)  = ""

pprintD :: D -> String
pprintD Up = "\x2191"
pprintD Dw = "\x2193"

pprintR :: R -> String
pprintR Top = "T"
pprintR Bot = "F"

ppList :: Show a => String -> [a] -> String
ppList s xs = concat $ intersperse s (map show xs) 

pprintZip :: Zipp -> String
pprintZip (i,stk,xs,ys) = "{"++(show i) ++ 
                          --(concat $ (intersperse ";") $ map show stk)++ 
                           ":>" ++ xs ++ "\x00B7" ++ ys++"}"

pprintState :: State -> String
pprintState (g,d,e,stk,zpp,r) = (pprintD d) ++ (pprint e) ++ " [" ++
                                (concat $ intersperse "," (map pprint stk)) ++ 
                                "] "++ (pprintZip zpp) ++ (pprintR r)


mark :: Zipp -> Zipp
mark (i,ms,xs,ys) = (i,i:ms,xs,ys)

dismiss :: Zipp -> Zipp 
dismiss (i,[],xs,zs) = (i,[],xs,zs)
dismiss (i,(m:ms),xs,zs) = (i,ms,xs,zs)

adv ::  Zipp -> Zipp
adv (i,ms,xs,[]) = (i,ms,xs,[])
adv (i,ms,xs,y:ys) = (i+1,ms,y:xs,ys)

restore :: Zipp -> Zipp
restore (i,[],xs,ys) = (i,[],xs,ys)
restore (i,(p:ms),xs,ys) = (p,ms,drop (i-p) xs, (reverse $ take (i-p) xs) ++ ys)


step :: State -> State 
step (g,Dw, NOP, es,z,r) = (g,Up, NOP, es,z,r)
step (g,Dw, Eps, es,z,r) = (g,Up, Eps, es,z,Top)
step (g,Dw, Any, es,(i,ms,xs,y:ys),r) = (g,Up, Any, es,(i+1,ms,y:xs,ys),Top)
step (g,Dw, Any, es,(i,ms,xs,[]),r) = (g,Up, Any, es,(i,ms,xs,[]),Bot)
step (g,Dw, (Lit x), es,(i,ms,xs,y:ys),r) 
    | x == y    = (g,Up, (Lit x), es, (i+1,ms,x:xs,ys)  , Top)
    | otherwise = (g,Up, (Lit x), es, (i  ,ms,xs  ,y:ys), Bot)
step (g,Dw, (Lit x), es,(i,ms,xs,[]),r)  = (g, Up, (Lit x), es,(i,ms,xs,[]),Bot) 
step (g,Dw, (Alt e1 e2) , es,z,r)        = (g, Dw, e1, (Alt NOP e2):es, mark z,r)
step (g,Up, e1, (Alt NOP e2):es,z,Bot)    = (g, Dw, e2, (Alt e1 NOP):es, restore z,Bot)
step (g,Up, e2, (Alt e1 NOP):es,z,Bot)    = (g, Up, Alt e1 e2, es, restore z,Bot)
step (g,Up, e1, (Alt NOP e2):es,z,Top)    = (g, Up, Alt e1 e2, es, dismiss z,Top)
step (g,Up, e2, (Alt e1 NOP):es,z,Top)    = (g, Up, Alt e1 e2, es, dismiss z,Top)
step (g,Dw, Seq e1 e2, es, z, b)         = (g, Dw, e1, (Seq NOP e2):es, z, b)
step (g,Up, e1, (Seq NOP e2):es, z, Top) = (g, Dw, e2, (Seq e1 NOP):es, z, Top)
step (g,Up, e1, (Seq NOP e2):es, z, Bot) = (g, Up, Seq e1 e2, es, z, Bot)
step (g,Up, e2, (Seq e1 NOP):es, z, b)   = (g, Up, Seq e1 e2, es, z, b) 
step (g,Dw, Not e1, es, z, b)            = (g, Dw, e1, (Not NOP):es, mark z, b)
step (g,Up, e1, (Not NOP):es, z, Top)    = (g, Up, Not e1, es, (dismiss.restore) z, Bot)
step (g,Up, e1, (Not NOP):es, z, Bot)    = (g, Up, Not e1, es, (dismiss.restore) z, Top)
step (g,Dw, Kle e1, es, z, b)            = (g, Dw, e1, (Kle NOP):es, mark z, b)
step (g,Up, e1, (Kle NOP):es, z, Bot)    = (g, Up, Kle e1, es, restore z, Top)
step (g,Up, e1, (Kle NOP):es, z, Top)    = (g, Dw, e1, (Kle NOP):es, mark z, Top)
step (g,Dw, Var x, es, z, r)             = case (lookup x g) of 
                                                (Just e') -> (g, Dw, e'   , (Var x):es, mark z, r)
                                                Nothing   -> (g, Up, Var x, es, z, Bot)
step (g,Up, _, (Var x):es, z, Bot)     = (g, Up, Var x, es, restore z, Bot)                                                
step (g,Up, _, (Var x):es, z, Top)     = (g, Up, Var x, es, dismiss z, Top)


-- Primitives

peg1 = Lit 'a'

peg2 = Seq (Lit 'a') (Lit 'b')

peg3 = Alt (Lit 'a') (Lit 'b')

peg4 = Kle (Lit 'a')

peg5 = Kle peg2

peg6 = Kle peg3

peg7 = Not (Lit 'a') 

peg8 = Not peg2

peg9 = Not peg3

-- Grammmars

gpeg1 = [("A",Alt peg2 (Var "B")),
         ("B", Lit 'c')]

         
         
anbn = Alt (Seq (Lit 'a') (Seq (Var "A") (Lit 'b'))) Eps
bncn = Alt (Seq (Lit 'b') (Seq (Var "B") (Lit 'c'))) Eps
gpeg2 = [("S", Seq (andE (Seq (Var "A") (Lit 'c'))) (Seq (Seq (Kle (Lit 'a')) (Var "B") ) (Not Any) )),
         ("A", anbn),
         ("B", bncn)]
         
         
stepper :: (State -> a) -> State -> Maybe (a, State)
stepper prj s = fx (step s)
  where
      fx st@(_,Up,NOP,[],_,_)   = Nothing
      fx st@(g,Up,e,[],zpp,r)   = Just (prj st, (g,Dw,NOP,[],zpp,r))
      fx st@(g,dir,e,stk,zpp,r) = Just (prj st, st)
    
run :: G -> E -> String -> [State]
run g e s = (g,Dw,e,[],(0,[],[],s),Top) : unfoldr (stepper id) (g,Dw,e,[],(0,[],[],s),Top)

ppRun :: G -> E -> String -> IO()
ppRun g e s = mapM_ (putStrLn.pprintState) (run g e s)
