
module PegSSMFrame where

import Data.List
import PegSSM
import PegSSMTests
import Text.PrettyPrint.HughesPJ as HPJ

{-    "debugFrames": [
        {
            "input": 0,
            "PEG-position": 1,
            "previousPEG": null,
            "actualPEG": "A",
            "status": "Calling rule A",
            "tag": "i"
        },
-}


-- (Position, Peg, PreviousPEG, Vars, Status,Tag )
type Frame = (Int, E, Maybe E,[String],String, String)

antPeg :: E -> [E] -> Maybe E
antPeg p ((Seq e NOP):_) = Just $ Seq e p
antPeg p ((Seq NOP e):_) = Just $ Seq p e
antPeg p ((Alt e NOP):_) = Just $ Alt e p
antPeg p ((Alt NOP e):_) = Just $ Alt p e
antPeg p ((Kle NOP):_)  = Just $ Kle p
antPeg p ((Not NOP) :_) = Just $ Not p
antPeg p [] = Nothing
antPeg p (x:_) = Just x

hCallStack :: [E] -> [String]
hCallStack  xs = reverse [s | (Var s) <- xs] 

tag :: D -> R -> String
tag Up Top = "s"
tag Up Bot = "f"
tag Dw _ = "i" 

pos :: Zipp -> Int
pos (i,_,_,_) = i 

state2Frame :: State -> Frame
state2Frame (g, d,peg,ctx,z,r) 
    = (pos z,peg,antPeg peg ctx, hCallStack ctx, stat d peg r, tag d r)

stat :: D -> E -> R -> String
stat Dw (Lit x) _ = "Attempting match "++ x:[]
stat Dw (Var x) _ = "Calling rule " ++ x
stat Dw x _ = "Executing " ++ (pprint x)
stat Up (Lit x) r = "match "++ (x:[]) ++ " " ++ (r2Str r)
stat Up (Var x) r = "rule " ++ x ++ " " ++ (r2Str r)
stat Up x r = "Peg " ++ (pprint x) ++ " " ++ (r2Str r)

r2Str :: R -> String
r2Str Top = "succed "
r2Str Bot = "failed "

jlb :: String -> Doc -> Doc 
jlb s d = doubleQuotes (text s)  HPJ.<> colon HPJ.<> d

jmb :: Maybe E -> Doc 
jmb Nothing = doubleQuotes $ text "null" 
jmb (Just e) = doubleQuotes $ text (pprint e)

frame2Doc :: Frame -> Doc
frame2Doc (i,p,pa,stk,msg,tag) 
    = braces (nest 4 $ vcat (punctuate comma 
                                       [ jlb "input" (int i), 
                                         jlb "PEG-position" (int 1),
                                         jlb "previousPEG" (jmb pa),
                                         jlb "actualPEG" (doubleQuotes $ text (pprint p)),
                                         jlb "status" (doubleQuotes $ text msg),
                                         jlb "tag" (doubleQuotes $ text tag)
                                           
                                       ] ) $+$ (text " ") ) 

frames2Doc :: [Frame] -> Doc 
frames2Doc xs = jlb "debugFrames" 
                    (brackets (vcat $ punctuate (comma $+$ text "") (map frame2Doc xs)))

g2Doc :: G -> Doc 
g2Doc g = doubleQuotes $ hcat (punctuate semi (map (\(s,p) -> text (s ++ " -> " ++ pprint p)) g ))
    
runFrames :: G -> E -> String -> Doc 
runFrames g e s 
    = braces (
         nest 4 (
                  vcat (punctuate comma  [ jlb "userInput" (doubleQuotes $ text s),
                                           jlb "code" (g2Doc g),
                                           frames2Doc  (map state2Frame $ run g e s)
                                         ]
                       )
                 )
     )

makeFrames :: G -> E -> String -> String
makeFrames g e s = render (runFrames g e s)
