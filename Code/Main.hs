
module Main where

import Data.List
import PegSSM
import PegSSMFrame
import PegSSMTree
import PegSSMParser
import System.Environment
import Text.Parsec.String


                  
main :: IO ()
main  = do fname <- getArgs
           case fname of
                [] -> help 
                ["-i", f] -> parseAndDebug f 
                ["-t", f] -> parseAndTree f
                ["-g", f] -> parseAndDot f
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

parseAndTree :: FilePath -> IO ()
parseAndTree f = do g <- parseFromFile parseSpec f
                    case g of
                        Left err -> print err
                        Right x   -> tree f x

parseAndDot :: FilePath -> IO ()
parseAndDot f = do g <- parseFromFile parseSpec f
                   case g of
                        Left err -> print err
                        Right x   -> dot f x



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
                            
tree :: FilePath -> (G,E,String) -> IO ()
tree f (g,e,s)  = let bpth = fileName f
                      outFile = bpth ++ ".json"
                  in  do writeFile outFile (makeTree g e s)
                         putStrLn ("output wrote to: " ++ outFile)

dot :: FilePath -> (G,E,String) -> IO ()
dot f  (g,e,s) = let bpth = fileName f
                     outFile = bpth ++ ".dot"
                  in  do writeFile outFile (runDot g e s)
                         putStrLn ("output wrote to: " ++ outFile)


debug :: (G,E,String) -> IO ()
debug  (g,e,s) = ppRun g e s
                          

help :: IO ()
help = mapM_ putStrLn [" Use main [-i | -t] <filename>",
                       " -i : Print the state list of the small stem semantics",
                       " -t : Alternative tree JSON output format  ",
                       " -g : Generate a graph ( DOT file) of the parsing",
                       "-------------------------------------------",
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
                      


                 
