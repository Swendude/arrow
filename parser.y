{
module Parser (parseArrow, Program(..), Rule(..), Cmd(..), Dir(..), Alt(..), Pat(..)) where

import Lexer
}
%name parseArrow
%tokentype { Token }
%error { parseError }

%token
	'->'		{ ArrowT }
	'.'			{ PointT }
	','			{ CommaT }
	go			{ GoT }
	take		{ TakeT }
	mark		{ MarkT }
	nothing		{ NothingT }
	turn		{ TurnT }
	case 		{ CaseT }
	of			{ OfT }
	end			{ EndT }
	left 		{ LeftT }
	right		{ RightT }
	front 		{ FrontT }
	';'			{ SemicolonT }
	Empty		{ EmptyT }
	Lambda 		{ LambdaT }
	Debris		{ DebrisT }
	Asteroid	{ AsteroidT }
	Boundary    { BoundaryT }
	'_'			{ AnyT }
	Ident		{ IdentT $$ }

%%

Program : Rules						{ Rules $1 }
Rules 	: {- empty -} 				{ [] }
		| Rule Rules 				{ $1 : $2 }
Rule 	: Ident '->' Cmds '.'		{ Rule $1 $3 }
Cmds	: {- empty -}				{ [] }	
		| Cmdsl			    		{ $1 }
Cmdsl   : Cmd ',' Cmdsl             { $1 : $3 }
		| Cmd                       { [$1] }
Cmd 	: go 						{ GoC }
		| take 						{ TakeC }
		| mark 						{ MarkC }
		| nothing 					{ NothingC }
		| turn Dir					{ TurnC $2 }
		| case Dir of Alts end		{ CaseC $2 $4 } 
		| Ident 					{ IdentC $1 }
Dir 	: left 						{ LeftD }
		| right 					{ RightD }
		| front 					{ FrontD }
Alts	: {- empty -} 				{ [] }
		| Altsl  					{ $1 }
Altsl   : Alt                       { [$1] }
		| Alt ';' Altsl				{ $1 : $3 }
Alt 	: Pat '->' Cmds				{ Match $1 $3 }
Pat 	: Empty 					{ EmptyP }
		| Lambda 					{ LambdaP }
		| Debris                    { DebrisP }
		| Asteroid 					{ AsteroidP }
		| Boundary 					{ BoundaryP }
		| '_' 						{ AnyP }


{ 
parseError :: [Token] -> a
parseError _ = error "Parse error" 

-- Program Datatype
data Program = Rules [Rule]
	deriving Show
data Rule    = Rule String [Cmd]
	deriving Show
data Cmd     = GoC | TakeC | MarkC | NothingC | TurnC Dir | CaseC Dir [Alt] | IdentC String 
	deriving Show
data Dir     = LeftD | RightD | FrontD
	deriving Show
data Alt     = Match Pat [Cmd]
	deriving Show
data Pat     = EmptyP | LambdaP | DebrisP | AsteroidP | BoundaryP | AnyP
	deriving (Show, Eq)
}