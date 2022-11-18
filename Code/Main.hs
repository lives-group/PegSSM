
module Main where

import Data.List
import PegSSM
import PegSSMFrame
import PegSSMParser
import System.Environment
import Text.Parsec.String


                  
main :: IO ()
main  = do fname <- getArgs
           case fname of
                [] -> help 
                ["-i", f] -> parseAndDebug f 
                [f]       -> parseAndRun f
                _         -> help
                              
parseAndRun :: FilePath -> IO ()
parseAndRun f = do g <- parseFromFile parseSpec f
                   case g of
                        Left err -> print err
                        Right x   -> process f x
                        
parseAndDebug :: FilePath -> IO ()
parseAndDebug f = do g <- parseFromFile parseSpec f
                     case g of
                        Left err -> print err
                        Right x   -> debug x

fileStruct :: FilePath -> [String]
fileStruct [] = []
fileStruct xs = ys:fileStruct (etail zs)
    where (ys,zs) = span (/= '/') xs
          etail [] = []
          etail (x:xs) = xs

fileName :: String ->  String
fileName xs = ys
    where (ys,zs) = span (/= '.') xs
          etail [] = []
          etail (x:xs) = xs

process :: FilePath -> (G,E,String) -> IO ()
process f (g,e,s)  = let bpth = fileName f
                         outFile = bpth ++ ".json"
                     in  do writeFile outFile (makeFrames g e s)
                            putStrLn ("output wrote to: " ++ outFile)
                            
debug :: (G,E,String) -> IO ()
debug  (g,e,s) = ppRun g e s
                          

help :: IO ()
help = mapM_ putStrLn ["<(Error)>  Expecting a single file name !",
                       "           usage main -i <filename> or main <filename>",
                       "            Input file must be a text file",
                       "            divided in 3 sections separated by at least four dashes (-).",
                       "            First section is an Peg Grammar",
                       "            Second section is an Peg expression",
                       "            Third section is the input string",
                       "            Example: ",
                       "            A <- 'a'A B / e ;",
                       "            B <- 'b' ;  ",
                       "            -----------------",
                       "            A                ",
                       "            -----------------",
                       "            \"aabb\"  "]
                      


                 
