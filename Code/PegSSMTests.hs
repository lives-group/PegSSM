
module PegSSMTests where

import PegSSM
import Test.Tasty 
import Data.List
import Test.Tasty.HUnit

type Prop = State -> Bool
-- (g,d,e,stk,(i,mrk,con,inp), r) -> Bool


type Alpha a = [a]
type Wrd a   = [a]
type Lang a  = [Wrd a]

type Str = Lang Char
type Chrs = Alpha Char

type NLang a = Int -> Wrd a

--
--  Geração de casos de testes
--

empty :: Lang a
empty = [[]]

eps :: Wrd a
eps = []

sigStarN  :: Eq a => Int -> Alpha a -> Lang a
sigStarN  0 _ = [[]]
sigStarN  n xs = zs  `union` [ x:z| x <- xs, z <- zs] 
    where zs = sigStarN  (n-1) xs

sigStarN' :: Eq a => Int -> Alpha a -> Lang a
sigStarN' 1 xs = [[x] | x <- xs]
sigStarN' n xs = zs  `union` [ x:z| x <- xs, z <- zs] 
    where zs = sigStarN' (n-1) xs
          
          
cat ::  Eq a => Lang a -> Lang a -> Lang a
cat xs ys = nub $ [ x ++ y | x <- xs, y <- ys] 
          
          
klenee :: Eq a => Lang a -> Lang a
klenee xs =  [[]] ++ [ zs ++ x | zs <- klenee xs,  x <- xs] 

alt :: Eq a => Lang a -> Lang a -> Lang a
alt xs ys = xs `union` ys

compl :: Eq a => Alpha a -> Lang a -> Lang a
compl xs ys = (klenee (map (:[]) xs)) \\ ys

repeatN :: a -> NLang a
repeatN x n = replicate n x 

cogen :: NLang a -> NLang a -> NLang a 
cogen l l' n =  (l n) ++ (l' n) 

naturalize :: NLang a -> Int -> Lang a
naturalize l n = [ l k |k <-[n..]] 

{-
  This part does not work as intended !
  It is supposed to careate a generator of coorect acceptable inputs from the Peg grammar itself,
  but obviously it can't be done in this way, can it ? 
  
  Notes for latter check the stubborn grammar S->bSb/Epsilon.
  
peg2gen :: Chrs -> G -> E -> Str
peg2gen _ g Eps = [""]
peg2gen a g Any = map (:[]) a
peg2gen a g (Lit c) = [c:[]]
peg2gen a g (Not e) = compl a (peg2gen a g e)
peg2gen a g (Kle e) = klenee (peg2gen a g e)
peg2gen a g (Alt e1 e2) = alt (peg2gen a g e1) (peg2gen a g e2)
peg2gen a g (Seq e1 e2) = cat (peg2gen a g e1) (peg2gen a g e2)
peg2gen a g (Var s) = case lookup s g of 
                           Nothing -> empty
                           Just e -> peg2gen a g e -}

                           
--S -> b S b | a S a
--S' -> b S' b | Eps
                        
--
--  Instâncias de testes
--

-- ================================================================================================================
-- Primitives: Those are simple PEGs and not Grammars !
-- Each definition contains a simple PEG expressiona with no variables, and it's test cases.  
-- 
--
-- ===============================================================================================================

buildTestCases :: G -> E -> String -> ([String],[String]) -> [TestTree]
buildTestCases g e prfx (acc,rej) = map (\s -> testCase (prfx ++ " accept \"" ++ s ++ "\"") (assertAccep s)) acc ++
                                    map (\s -> testCase (prfx ++ " reject \"" ++ s ++ "\"") (assertReject s)) rej 
    where accept s       = accepted (simpleRun g e s)
          assertAccep  s = assertBool ("rejected:" ++s) (accept s) 
          assertReject s = assertBool ("accepted:" ++s) (not $ accept s) 
          
pegTest :: (State -> Bool) -> String  -> G -> E -> String -> TestTree
pegTest p prfx g e inp = testCase (prfx ++ " on \"" ++ inp ++ "\"") (assertAccep inp)
                                    
    where accept s       = p (simpleRun g e s)
          assertAccep  s = assertBool (" failed on :" ++s) (accept s) 

accTest g e s  = pegTest accepted (pprint e ++ " accept")  g e s           -- accepts a prefix of the input 
accJustTest p g e s  = pegTest (acceptedJust p) (pprint e ++ " accept just \"" ++ p++ "\"") g e s   -- accepts explicitly the given string as prefix
lamTest g e s  = pegTest accepted (pprint e ++ " lambda acc")  g e s       -- accepts without consuming any input 
fullAccTest g e s = pegTest fullyAccepted (pprint e ++ " fully acc") g e s -- accepts consuming the input entirely 
rejTest g e s  = pegTest (not.accepted) (pprint e ++ " reject") g e s      -- Rejects the input
          
          
-- "lambda"          
peg0 = Eps 
peg0tc = map (lamTest [] peg0) (sigStarN 2 "ab")


-- "a"
peg1 = Lit 'a' 
peg1tc =  [(fullAccTest [] peg1 "a"), (accTest [] peg1 "ab"), (accTest [] peg1 "aa"), (rejTest [] peg1 "b"), (rejTest [] peg1 "")]

-- "."
peg2 = Any    
peg2tc =  map (fullAccTest [] peg2) ["a","b"] ++  
          map (accTest [] peg2) ["aa","ab","ba","bb"] ++
          [rejTest [] peg2 ""]

-- "ab"
peg3 = Seq (Lit 'a') (Lit 'b')
ipeg3 = ([], sigStarN  2 "ac")
peg3tc = [fullAccTest [] peg3 "ab"] ++
         map (accJustTest "ab" [] peg3) ["aba","abb","abab"] ++
         map (rejTest [] peg3) ["","a","ba","aa","bb"]

-- "a/b"
peg4 = Alt (Lit 'a') (Lit 'b')
ipeg4 = (sigStarN' 3 "ab",["","c","d"])
peg4tc = buildTestCases [] peg4 (pprint peg4) ipeg4

-- "abc"
peg5 = Seq (Lit 'a') (Seq (Lit 'b') (Lit 'c'))
ipeg5 = (take 3 $ drop 1 $ klenee ["abc"], ( (sigStarN  3 "ab") `alt` (sigStarN  3 "ac")))
peg5tc = buildTestCases [] peg5 (pprint peg5) ipeg5

-- "a/b/c"
peg6 = Alt (Lit 'a') (Alt (Lit 'b') (Lit 'c'))
ipeg6 =  (sigStarN'  3 "abc",sigStarN  2 "de")
peg6tc = buildTestCases [] peg6 (pprint peg6) ipeg6


-- "a(b/c)"
peg7 = Seq (Lit 'a') (Alt (Lit 'b') (Lit 'c'))
ipeg7 = let e1 = cat ["a"] $ take 5 (drop 1 $ klenee (alt ["b"] ["c"])) in ( e1, take 5 $ compl "abc" e1)
peg7tc = buildTestCases [] peg7 (pprint peg7) ipeg7

-- "" 
peg8= Seq  (Alt (Lit 'a') (Lit 'b')) (Lit 'c')
ipeg8 = let e1 = cat (alt ["a"] ["b"]) ["c"] in ( e1, take 5 $ compl "abc" e1)
peg8tc = buildTestCases [] peg8 (pprint peg8) ipeg8

-- "a*"
peg9 = Kle (Lit 'a')
ipeg9 = (sigStarN  4 "a",[])
peg9tc = buildTestCases [] peg9 (pprint peg9) ipeg9

-- "(ab)*"
peg10 = Kle peg3
ipeg10 = (take 7 (klenee ["ab"]),[])
peg10tc = buildTestCases [] peg10 (pprint peg10) ipeg10

-- "(a/b)*"
peg11 = Kle peg4
ipeg11 = (take 10 $ klenee (alt ["a"] ["b"]),[])
peg11tc = buildTestCases [] peg11 (pprint peg11) ipeg11

-- "!a"
peg12 = Not (Lit 'a') 
ipeg12 = (take 15 $ klenee (alt ["a"] ["b"]),[])
peg12tc = buildTestCases [] peg12 (pprint peg11) ipeg12

-- "!(ab)"
peg13 = Not peg3
ipeg13 =  let l = take 3 $ drop 1 $ klenee ["ab"] in (take 5 $ compl "ab" l ,l)
peg13tc = buildTestCases [] peg12 (pprint peg12) ipeg12

-- "!(a/b)"
peg14 = Not peg4
ipeg14 = ([],[])
peg14tc = buildTestCases [] peg14 (pprint peg14) ipeg14

-- Grammmars

--  A -> ab/B 
--  B -> c
--
gpeg1 = [("A",Alt peg3 (Var "B")),
         ("B", Lit 'c')]

-- S -> aSb/eps  
-- 
gpeg2 = [("S",Alt (Seq (Lit 'b') (Seq (Var "S") (Lit 'b'))) (Eps)  )]

-- S -> bbS | epsilon
--
gpeg4 = [("S",Alt (Seq (Lit 'b') (Seq (Lit 'b') (Var "S"))) (Eps)  )]

-- AnBnCn
anbn = Alt (Seq (Lit 'a') (Seq (Var "A") (Lit 'b'))) Eps
bncn = Alt (Seq (Lit 'b') (Seq (Var "B") (Lit 'c'))) Eps

gpeg3 = [("S", Seq (andE (Seq (Var "A") (Lit 'c'))) (Seq (Seq (Kle (Lit 'a')) (Var "B") ) (Not Any) )),
         ("A", anbn),
         ("B", bncn)]
 

pegGroup = testGroup "Simple PEGs" (concat [peg0tc, peg1tc,peg2tc,peg3tc --peg4tc,peg5tc,peg6tc,peg7tc,peg8tc,peg9tc,peg10tc,peg11tc, peg12tc,peg13tc,peg14tc
                                           
                                           ])
 
main :: IO ()
main = defaultMain pegGroup
