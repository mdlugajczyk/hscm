import System.Environment
import Hscm.Parser

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpr $ args !! 0


