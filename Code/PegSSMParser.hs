
module PegSSMParser where

import Data.List
import PegSSM


import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String
import Text.Parsec.Char
import Data.Functor.Identity



ident :: Parser String
ident = (letter >>= (\x -> many alphaNum >>= (\xs -> return (x:xs)))) <?> "indentifier"

str :: Parser String
str = do string "\""
         xs <- many (noneOf "'\n\"")
         string "\""
         return xs

lambda :: Parser E
lambda = ( (string "e" <|> string "\x03b5") >> return Eps) <?> "Epsilon"

literal :: Parser E
literal = do string "'"
             c <- anyChar
             string "'"
             return (Lit c)
             

anything :: Parser E
anything =  string "." >> return (Any)

var :: Parser E 
var = do i <- ident 
         return (Var i)
 

parens :: Parser a -> Parser a
parens p = do char '('
              r <- p
              char ')'
              return r

apegTerm :: Parser E
apegTerm = spaces >> (literal <|> lambda <|> anything <|> var <|> (parens apegExp))

apegPre :: Parser E
apegPre = try (spaces >> string "!" >> apegPre >>= (\t -> return (Not t))) <|>
          apegTerm

apegPos :: Parser E
apegPos = apegPre >>= (\t -> option t (try $ spaces >> string "*" >> return (Kle t)))

apegSeq :: Parser E
apegSeq = do xs <- many1 (try apegPos)
             return (foldr1 Seq xs)

apegAlt :: Parser E
apegAlt = do s <- apegSeq
             a <- option s (try $ spaces >> (string "/") >> apegAlt >>= (\t -> return $ Alt s t))
             return a


apegExp :: Parser E          
apegExp = apegAlt
--apegExp = buildExpressionParser apegTable (apegTerm >>= \t -> spaces >> return t)
--         <?> "APEG Expression"

-- apegFact :: Parser E
-- apegFact = buildExpressionParser apegTable (apegTerm >>= \t -> spaces >> return t)
--            <?> "APEG Expression on Not or Klenee"


-- apegTable :: OperatorTable String () Identity E
-- apegTable   = [ [prefix "!" Not, postfix "*" Kle ]
--               ]
--
-- binary :: String -> (a -> a -> a) -> Assoc -> Operator String () Identity a
-- binary  name fun assoc = Infix   (do{ string name; spaces ;return fun }) assoc
--
-- prefix :: String -> (a -> a) -> Operator String () Identity a
-- prefix  name fun       = Prefix  (do{ string name; spaces; return fun })
--
-- postfix :: String -> (a -> a) -> Operator String () Identity a
-- postfix name fun       = Postfix (do{ spaces;string name; spaces; return fun })
--
-- seqOp :: Operator String () Identity E
-- seqOp = Infix   (try (lookAhead apegTerm >> return Seq)) AssocLeft

parseRule :: Parser (String,E)
parseRule = do r <- ident
               spaces
               string "<-"
               spaces
               pexp <- apegExp
               spaces
               string ";"
               return (r,pexp)


sect :: Parser ()
sect = string "----" >>  many (char '-')  >> spaces >> return () 

               
parseGrammar :: Parser G
parseGrammar = do spaces 
                  many1 (parseRule >>= \r -> spaces >> return r) 


parseSpec :: Parser (G,E,String)
parseSpec = do spaces
               g <- option [] parseGrammar
               spaces 
               sect 
               e <- apegExp
               spaces
               sect
               s <- str 
               return (g,e,s)
               
               
               
                  

                      
                 
