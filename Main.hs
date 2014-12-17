module Main where

import ParseLib.Abstract
import Data.Map (Map)
import qualified Data.Map as L
import Control.Monad (replicateM)
import Data.Char (isSpace)
import System.IO
import Data.List (nub)
import Data.List.Split.Internals (chunksOf)
import Data.Maybe (fromJust)
import Lexer
import Parser

  
main :: IO ()
main = do 
   l <- getLine
   h <- openFile l ReadMode
   c <- hGetContents h
   r <- return (parseArrow $ lexArrow c)
   print r
   print (check r)
   print (programFold environmentAlgebra r)
   print (toEnvironment c)
mainPrint :: IO ()
mainPrint = do   
   l <- getLine
   h <- openFile l ReadMode
   c <- hGetContents h
   r <- return (fst (head (parse parseSpace c)))
   print r
   putStr (spacePrinter r)

   
type Space     =  Map Pos Contents
type Size      =  Int
type Pos       =  (Int, Int)
data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary
	deriving (Show,Eq)

parseSpace :: Parser Char Space
parseSpace =
  do
    (mr,mc)  <-  parenthesised
                   ((,) <$> natural <* symbol ',' <*> natural) <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css      <-  replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ L.fromList $ concat $
             zipWith (\ r cs  ->
             zipWith (\ c d   ->  ((r,c),d)) [0..] cs) [0..] css

spaces :: Parser Char String
spaces = greedy (satisfy isSpace)

contents :: Parser Char Contents
contents =
  choice (Prelude.map (\ (f,c) -> f <$ symbol c) contentsTable) <* spaces

contentsTable :: [(Contents,Char)]
contentsTable =
  [  (Empty,'.'),(Lambda,'\\'),(Debris,'%'),(Asteroid,'O'),(Boundary,'#')]

-- These three should be defined by you
type Ident = String
type Commands = [Cmd]
type Heading = ()

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step  =  Done  Space Pos Heading
           |  Ok    ArrowState
           |  Fail  String

		   
{- Question 4.
The Happy documentation states: "The only reason we used left recursion is that Happy is more efficient at parsing left-recursive rules". The opposite is true for parsers created with parser combinators, they can even get stuck  in an infinite loop because of left-recursion. 
-}

type ProgramAlgebra p r c d a pa = ([r] -> p, 								-- program
						 String -> [c] -> r,								-- Rule
						 (c, c, c, c, d -> c, d -> [a] -> c, String -> c),	-- Cmd
						 (d, d, d),											-- Dir
						 pa -> [c] -> a,									-- Alt
						 (pa, pa, pa, pa, pa, pa))							-- Pat

programFold :: ProgramAlgebra p r c d a pa -> Program -> p
programFold (program, 
			rule, 
			(cmdGo, cmdTake, cmdMark, cmdNothing, cmdTurn, cmdCase, cmdIdent), 
			(dirLeft, dirRight, dirFront), 
			alt, 
			(patEmpty, patLambda, patDebris, patAsteroid, patBoundary, patAny)
		) = fold
			where
				fold (Rules rules) = program (map ruleFold rules)
				ruleFold (Rule s cmds) = rule s (map cmdFold cmds)
				cmdFold GoC = cmdGo
				cmdFold TakeC = cmdTake
				cmdFold MarkC  = cmdMark
				cmdFold NothingC = cmdNothing
				cmdFold (TurnC d) = cmdTurn (dirFold d)
				cmdFold (CaseC d alts) = cmdCase (dirFold d) (map altFold alts)
				cmdFold (IdentC s) = cmdIdent s
				dirFold LeftD = dirLeft
				dirFold RightD = dirRight
				dirFold FrontD = dirFront
				altFold (Match p cmds) = alt (patFold p) (map cmdFold cmds)
				patFold EmptyP = patEmpty
				patFold LambdaP = patLambda
				patFold DebrisP = patDebris
				patFold AsteroidP = patAsteroid
				patFold BoundaryP = patBoundary
				patFold AnyP = patAny
				
check :: Program -> Bool
check program = checkUndefinedRules && checkHasStart && checkDoubleRules && checkAlts
	where
		checkUndefinedRules = and (map ((flip elem) defRules) callRules)		
		checkHasStart = "start" `elem` defRules		
		checkDoubleRules = not (hasDuplicate defRules)
		checkAlts = and $ map altsSatisfied defAlts
		
		defRules = definedRules program
		callRules = calledRules program
		defAlts = definedAlts program
		
hasDuplicate xs = nub xs /= xs

altsSatisfied alts | AnyP `elem` alts = True
				   | EmptyP `elem` alts && 
				     LambdaP `elem` alts &&
				     DebrisP `elem` alts &&
				     AsteroidP `elem` alts &&
				     BoundaryP `elem` alts = True
				   | otherwise = False

definedRules = programFold definedRulesAlgebra
calledRules = programFold calledRulesAlgebra
definedAlts = programFold definedAltsAlgebra
		
definedRulesAlgebra :: ProgramAlgebra [String] String () () () ()
definedRulesAlgebra =  (\xs -> xs,
						\s _ -> s,
						((), (), (), (), \_ -> (), \_ _-> (), \_ -> ()),
						((), (), ()),
						\_ _ -> (),
						((), (), (), (), (), ())
						)

calledRulesAlgebra :: ProgramAlgebra [String] [String] [String] () [String] ()
calledRulesAlgebra = (\xs -> concat xs,
					  \_ xs -> concat xs,
					  ([], [], [], [], \_ -> [], \_ xs -> concat xs, \x -> [x]),
					  ((), (), ()),
					  \_ xs -> concat xs,
					  ((), (), (), (), (), ())
					  )
			   
definedAltsAlgebra :: ProgramAlgebra [[Pat]] [[Pat]] [[Pat]] () (Pat, [[Pat]]) Pat
definedAltsAlgebra = (\xs -> concat xs,
					  \_ xs -> concat xs,
					  ([],[],[],[],\_ ->[], \_ x_xs -> map fst x_xs : concat (map snd x_xs)  , \_ -> []),
					  ((),(),()),
					  \x xs -> (x, concat xs),
					  (EmptyP, LambdaP, DebrisP, AsteroidP, BoundaryP, AnyP)
					  )
-- Question 7.
spacePrinter :: Space -> String
spacePrinter s = printSpace 
				where
					((maxx,maxy),_) = L.findMax s
					keys = [(x,y)|x<-[0..maxx], y <-[0..maxy]]
					valuesList = chunksOf (maxy+1) (map (\key -> s L.! key) keys)
					stringList = map (\x -> map showContent x) valuesList
					printSpace = show (maxx,maxy) ++ "\n" ++ foldr (\x y-> x ++ "\n" ++ y) "" stringList

showContent :: Contents -> Char
showContent pat = fromJust (lookup pat contentsTable)

-- Question 8.
toEnvironment :: String -> Environment
toEnvironment s = toEnvironment' (parseArrow $ lexArrow s)
					where 
						toEnvironment' program | check program = L.fromList (programFold environmentAlgebra program)
											   | otherwise = error "Program did not pass check, check syntax."

		
environmentAlgebra :: ProgramAlgebra [(String,[Cmd])] (String,[Cmd]) Cmd Dir Alt Pat
environmentAlgebra =  (\xs -> xs,
						\s cs -> (s,cs),
						(GoC, TakeC, MarkC, NothingC, \d -> TurnC d, \d as-> CaseC d as, \s -> IdentC s),
						(LeftD, RightD, FrontD),
						\p cs -> Match p cs,
						(EmptyP, LambdaP, DebrisP, AsteroidP, BoundaryP, AnyP)
						)

---- Program Datatype
--data Program = Rules [Rule]
--	deriving Show
--data Rule    = Rule String [Cmd]
--	deriving Show
--data Cmd     = GoC | TakeC | MarkC | NothingC | TurnC Dir | CaseC Dir [Alt] | IdentC String 
--	deriving Show
--data Dir     = LeftD | RightD | FrontD
--	deriving Show
--data Alt     = Match Pat [Cmd]
--	deriving Show
--data Pat     = EmptyP | LambdaP | DebrisP | AsteroidP | BoundaryP | AnyP
--	deriving (Show, Eq)





