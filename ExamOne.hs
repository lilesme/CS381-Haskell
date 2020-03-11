module ExamOne where

-----------------------------------------------------------------------------------------
-- EXAMPLE 1
type State = Int
type Val = State -> State

-- i ::= (any integer)
-- s ::= inc i
--    |  reset
-- p ::= s ; p
--   |  ε

-- p is just a list of statements
-- ε is just nothing
-- ; is concret syntax to delimate statements

type I = Int
data S = Inc Int
       | Reset
data P = [S]
-- data P = Prog S P
--       | Epsilon

-- same thing as: data List = Cons Int List | Nil
valS :: S -> State -> State
valS (Inc a) = \s -> s + a
valS Reset   = \s -> 0

valP :: [S] -> Int -> Int
valP []    x = x
valP (s:p) x = valP p (valS s x)
-----------------------------------------------------------------------------------------
-- EXAMPLE 2
data Cmd = Gas
         | Brake
         | Turn

type Prog = [Cmd]

type Pos   = Int

type Speed = Int

data Dir   = Forward
           | Backward
          deriving (Eq,Show)

type State = (Pos, Speed, Dir)

data Result = OK State
            | Crash Pos
          deriving (Eq,Show)

-- Semantic domain for Cmd: State -> Result
cmd :: Cmd -> State -> Result
cmd Gas (p, s, Forward)   = OK (p+s, s+1, Forward)
cmd Gas (p, s, Backward)  = OK (p-s, s+1, Backward)
cmd Brake (p, 0, d)       = OK (p, 0, d)
cmd Brake (p, s, Forward) = OK (p+s, s-1, Forward)
cmd Brake (p, s, Backward)= OK (p-s, s-1, Forward)
cmd Turn (p, 0, Forward)  = OK (p, 0, Backward)
cmd Turn (p, 0, Backward) = OK (p, 0, Forward)
cmd Turn (p, s, d)        = Crash p

prog :: [Cmd] -> State -> Result
prog []     s = OK s
prog (x:xs) s = case cmd x s of
                  OK s' -> prog xs s'
                  _ -> Crash
