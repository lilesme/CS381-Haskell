

module MiniLogo where

--Megan Liles
--lilesme
import Prelude hiding (Num)
import Data.List

--  num	::=	(any natural number)
--  var	::=	(any variable name)
--  macro	::=	(any macro name)

--  prog	::=	ε   |   cmd ; prog	sequence of commands

--  mode	::=	down   |   up	pen status

-- 1. Define the abstract syntax of MiniLogo as a set of Haskell data types.
--You should use built-in types for num, var, and macro.
--(If you want to define a type Num, you will have to hide that name from the Prelude).

--  prog	::=	ε   |   cmd ; prog	sequence of commands
type Prog = [Cmd]


--  mode	::=	down   |   up	pen status
data Mode = Up
          | Down
          deriving (Show, Eq)


type Var = String   -- takes string arguments (i.e. Var "x1")
type Num = Int      -- takes int arguments (i.e. Num n)
type Macro = String  -- takes string arguments (i.e. Define "line")

--  expr	::=	var	variable reference
--  |	num	literal number
--  |	expr + expr	addition expression
data Expr = Var Var
          | Num Int
          | Add Expr Expr
          deriving (Show, Eq)


--  cmd	::=	pen mode	change pen mode
--  |	move ( expr , expr )	move pen to a new position
--  |	define macro ( var* ) { prog }  	define a macro
--  |	call macro ( expr* )	invoke a macro
data Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [Var] Prog
         | Call Macro [Expr]
         deriving (Show, Eq)


-- 2. Define a MiniLogo macro line (x1,y1,x2,y2) that (starting from anywhere
-- on the canvas) draws a line segment from (x1,y1) to (x2,y2).

--First, write the macro in MiniLogo concrete syntax (i.e. the notation defined
--by the grammar and used in the example programs above). Include this
--definition in a comment in your submission.

-- define line (x1, y1, x2, y2) {
--    pen up; move (x1,y1);
--    pen down; move (x2,y2);
--    pen up;
-- }

--Second, encode the macro definition as a Haskell value using the data Vars
--defined in Task 1. This corresponds to the abstract syntax of MiniLogo.
--Your Haskell definition should start with something like line = Define "line" ...
--CAPITALIZATION QUES.
line = Define "line" ["x1", "y1", "x2", "y2"][Pen Up, Move (Var "x1") (Var "y1"), Pen Down, Move (Var "x2") (Var "y2")]

-- 3. Use the line macro you just defined to define a new MiniLogo macro nix
--(x,y,w,h) that draws a big “X” of width w and height h, starting from
--position (x,y). Your definition should not contain any move commands.


-- First, write the macro in MiniLogo concrete syntax and include this
-- definition in a comment in your submission.

-- define nix (x, y, w, h){
--   call line (x, y, x+w, y+h);
--   call line (x+w, y, x, y+h);
-- }

--Second, encode the macro definition as a Haskell value, representing the
--abstract syntax of the definition.
nix = Define "nix" ["x", "y", "w", "h"][Call "line" [Var "x", Var "y", Add (Var "x") (Var "w"), Add (Var "y") (Var "h")], Call "line" [Add (Var "x") (Var "w"), Var "y", Var "x", Add (Var "y") (Var "h")]]

-- 4. Define a Haskell function steps :: Int -> Prog that constructs a MiniLogo
--program that draws a staircase of n steps starting from (0,0). Below is a
--visual illustration of what the generated program should draw for a couple
--different applications of steps. You may assume that n ≥ 0.

--Line (x1, y1, x2, y2)
--Line (n, n, n-1, n) horizontal line
--Line (n, n , n, n-1) vertical line

steps :: Int -> Prog
steps 0 = []
steps n = [Call "line" [Num n, Num n, Num (n-1), Num n], Call "line" [Num n, Num n, Num n, Num (n-1)]] ++ steps (n-1)


--5. Define a Haskell function macros :: Prog -> [Macro] that returns a list of
--the names of all of the macros that are defined anywhere in a given MiniLogo
--program. Don’t worry about duplicates—if a macro is defined more than once,
--the resulting list may include multiple copies of its name.

macros :: Prog -> [Macro]
macros [] = []
macros (x:xs) = case x of
  Define m _ _ -> m:macros xs  --add macro m to list and call macros on the rest of the list
  otherwise -> macros xs  --no macro found so call macros on the rest of the list

--6. Define a Haskell function pretty :: Prog -> String that pretty-prints a
--MiniLogo program. That is, it transforms the abstract syntax (a Haskell value)
--into nicely formatted concrete syntax (a string of characters). Your
--pretty-printed program should look similar to the example programs given
--above; however, for simplicity you will probably want to print just one
--command per line.

--You may find the functions intersperse or intercalate in Data.List useful for
--inserting commas in your implementation of pretty.

helper :: Expr -> String
helper (Num x)    = "x"  -- Num "x" = x
helper (Var x)    = "x"  -- Var "x" = x
helper (Add x y)  = helper x ++ "+" ++ helper y --Add (Var "x") (Var "y")  -> x + y

-- Pen Up -> pen up;
-- Pen Down -> pen down;
-- Move (Var "x1") (Var "y1") -> move (x1, y1);
--Call "line" [Add (Var "x") (Var "w"), Var "y", Var "x", Add (Var "y") (Var "h")]
-- -> call line (x+w, y, x, y+h)
--Define "nix" ["x", "y", "w", "h"] [...] -> define nix (x, y, w, h){..}

pretty :: Prog -> String
pretty (Pen Up:xs)         = "pen up; " ++ pretty xs
pretty (Pen Down:xs)       = "pen down; " ++ pretty xs
pretty ((Move n m):xs)     = "move (" ++ (helper n) ++ ", " ++ (helper m) ++ "); " ++ pretty xs
pretty ((Call n m):xs)     = "call " ++ n ++ " (" ++ intercalate ", " (map helper m) ++ "); " ++ pretty xs
pretty ((Define n m x):xs) = "define " ++ n ++ " (" ++ intercalate ", " m ++ "){ " ++ pretty x ++ pretty xs ++ "}"

--BONUS PROBLEMS

-- 7. Define a Haskell function optE :: Expr -> Expr that partially evaluates
--expressions by replacing any additions of literals with the result. For
--example, given the expression (2+3)+x, optE should return the expression 5+x.
optE :: Expr -> Expr
optE (Var x)               = Var x
optE (Num x)               = Num x
optE (Add (Num x)(Num y))  = Num (x + y)
optE (Add (Var x)(Num y))  = Add (Var x) (Num y)
optE (Add (Num x)(Var y))  = Add (Num x) (Var y)
optE (Add x y)             = Add (optE x) (optE y)

--8. Define a Haskell function optP :: Prog -> Prog that optimizes all of the
--expressions contained in a given program using optE.

help :: Cmd -> Cmd
help (Pen x)    =  Pen x
help (Move x y) = Move (optE x)(optE y)
help (Call x y) = Call x (map optE y)

optP :: Prog -> Prog
optP []     = []
optP (x:xs) = help x : optP xs
