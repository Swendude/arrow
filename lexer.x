{
module Lexer (lexArrow, Token(..)) where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  \-\>            {const ArrowT}
  \.            {const PointT}
  \,            {const CommaT}
  go            {const GoT}
  take          {const TakeT}
  mark          {const MarkT}
  nothing       {const NothingT}
  turn          {const TurnT}
  case          {const CaseT}
  of            {const OfT}
  end           {const EndT}
  left          {const LeftT}
  right         {const RightT}
  front         {const FrontT}
  \;             {const SemicolonT}
  Empty         {const EmptyT}
  Lambda        {const LambdaT}
  Debris        {const DebrisT}
  Asteroid      {const AsteroidT}
  Boundary      {const BoundaryT}
  \_            {const AnyT}
  [$alpha $digit \+ \-]+  {\s -> IdentT s}

{
  -- The token type:
data Token =
  ArrowT  |
  PointT  |
  CommaT  |
  GoT     |
  TakeT   |
  MarkT   |
  NothingT|
  TurnT   |
  CaseT   |
  OfT     |
  EndT    |
  LeftT   |
  RightT  |
  FrontT  |
  SemicolonT   |
  EmptyT       |
  LambdaT      |
  DebrisT      |
  AsteroidT    |
  BoundaryT     |
  AnyT         |
  IdentT String
  deriving Show

lexArrow = alexScanTokens
}