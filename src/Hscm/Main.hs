import System.Environment
import Hscm.Parser
import Hscm.Evaluate

main :: IO ()
main = getArgs >>= print . eval .readExpr . head
