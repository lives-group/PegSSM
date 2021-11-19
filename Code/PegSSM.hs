
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


fullyAccepted :: State -> Bool
fullyAccepted (g,d,e,stk,(i,mrk,con,[]), Top) = True
fullyAccepted (g,d,e,stk,(i,mrk,con,inp), r) = False

lambdaAccepted :: State -> Bool
lambdaAccepted (g,d,e,stk,(i,mrk,[],_), Top) = True 
lambdaAccepted (g,d,e,stk,(i,mrk,(_:_),ys), Top) = False

accepted :: State -> Bool 
accepted (g,d,e,stk,(i,mrk,con,inp), Top) = True
accepted (g,d,e,stk,(i,mrk,con,inp), Bot) = False

acceptedLeaving :: String -> State -> Bool 
acceptedLeaving s (g,d,e,stk,(i,mrk,con,inp), Top) = inp == s
acceptedLeaving s (g,d,e,stk,(i,mrk,con,inp), Bot) = False

acceptedJust :: String -> State -> Bool 
acceptedJust s (g,d,e,stk,(i,mrk,con,inp), Top) = (reverse con) == s
acceptedJust s (g,d,e,stk,(i,mrk,con,inp), Bot) = False

andE :: E -> E
andE e = Not (Not e)

       
pparens :: Bool -> String -> String
pparens True s ="(" ++ s ++ ")"
pparens False s = s

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
pprint (Eps) = "e"
pprint (Lit c) = c:[]
pprint (Any) = "."
pprint (Alt e1 e2) = (pparens (7 > prec e1) (pprint e1)) ++ "\\\\" ++ (pparens (7 > prec e2) (pprint e2))
pprint (Seq e1 e2) = (pparens (8 > prec e1) (pprint e1)) ++ (pparens (8 > prec e2) (pprint e2))
pprint (Not e1) = "!" ++ (pparens (9 > prec e1) (pprint e1))
pprint (Kle e1) = (pparens (9 > prec e1) (pprint e1)) ++ "*"
pprint (Var x)  = x
pprint (NOP)  = "\x2609"

pprintD :: D -> String
pprintD Up = "\x2191"
pprintD Dw = "\x2193"

pprintR :: R -> String
pprintR Top = "A"
pprintR Bot = "R"

ppList :: Show a => String -> [a] -> String
ppList s xs = concat $ intersperse s (map show xs) 

pprintZip :: Zipp -> String
pprintZip (i,stk,xs,ys) = "{"++(show i) ++ ":"++
                          (sshead stk)++ 
                          xs ++ "\x00B7" ++ ys++"}"
    where sshead [] = "[_]"
          sshead (x:xs) = "[" ++ show x ++ "]"
          ssstk ws = (concat $ (intersperse ";") $ map show ws)

pprintState :: State -> String
pprintState (g,d,e,stk,zpp,r) = (pprintZip zpp) ++ " "++ (pprintR r) ++ " " ++ (pprintD d) ++ (pprint e) ++ " [" ++
                                (concat $ intersperse "," (map pprint stk)) ++ 
                                "] "


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
step (g,Up, e1, (Alt NOP e2):es,z,Bot)   = (g, Dw, e2, (Alt e1 NOP):es, restore z,Bot)
step (g,Up, e2, (Alt e1 NOP):es,z,Bot)   = (g, Up, Alt e1 e2, es, z,Bot)
step (g,Up, e1, (Alt NOP e2):es,z,Top)   = (g, Up, Alt e1 e2, es, dismiss z,Top)
step (g,Up, e2, (Alt e1 NOP):es,z,Top)   = (g, Up, Alt e1 e2, es,z,Top)
step (g,Dw, Seq e1 e2, es, z, b)         = (g, Dw, e1, (Seq NOP e2):es, z, b)
step (g,Up, e1, (Seq NOP e2):es, z, Top) = (g, Dw, e2, (Seq e1 NOP):es, z, Top)
step (g,Up, e1, (Seq NOP e2):es, z, Bot) = (g, Up, Seq e1 e2, es, z, Bot)
step (g,Up, e2, (Seq e1 NOP):es, z, b)   = (g, Up, Seq e1 e2, es, z, b) 
step (g,Dw, Not e1, es, z, b)            = (g, Dw, e1, (Not NOP):es, mark z, b)
step (g,Up, e1, (Not NOP):es, z, Top)    = (g, Up, Not e1, es, restore z, Bot)
step (g,Up, e1, (Not NOP):es, z, Bot)    = (g, Up, Not e1, es, restore z, Top)
step (g,Dw, Kle e1, es, z, b)            = (g, Dw, e1, (Kle NOP):es, mark z, b)
step (g,Up, e1, (Kle NOP):es, z, Bot)    = (g, Up, Kle e1, es, restore z, Top)
step (g,Up, e1, (Kle NOP):es, z, Top)    = (g, Dw, e1, (Kle NOP):es, mark z, Top)
step (g,Dw, Var x, es, z, r)             = case (lookup x g) of 
                                                (Just e') -> (g, Dw, e'   , (Var x):es, z, r)
                                                Nothing   -> (g, Up, Var x, es, z, Bot)
step (g,Up, _, (Var x):es, z, Bot)     = (g, Up, Var x, es, z, Bot)                                                
step (g,Up, _, (Var x):es, z, Top)     = (g, Up, Var x, es, z, Top)
                 
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

countRun :: G -> E -> String -> (State,Int)
countRun g e s = (last xs, length xs)
    where xs = run g e s

simpleRun ::  G -> E -> String -> State
simpleRun g e s = last (run g e s)

