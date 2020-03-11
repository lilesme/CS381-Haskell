module HW3 where

import MiniMiniLogo
import Render


--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--  * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--    functions for generating MiniMiniLogo programs. It contains the type
--    definitions for Mode, Cmd, and Prog.
--  * Render.hs contains code for rendering the output of a MiniMiniLogo
--    program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))


-- type Point = (Int,Int)
-- type State = (Mode,Point)
-- type Line = (Point,Point)
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen m) (_, (x1, y1))      = ((m, (x1, y1)), Nothing)
cmd (Move x2 y2) (m, (x1, y1)) = case m of
                                  Up -> ((Up, (x2, y2)), Nothing)
                                  Down -> ((Down, (x2, y2)), Just ((x1, y1), (x2, y2)))


-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
prog :: Prog -> State -> (State, [Line])
prog [] s = (s, [])
prog (x:xs) s = case cmd x s of
                  (s', Just l) -> (\(s, xs) -> (s, l:xs)) (prog xs s')
                  (s', Nothing) -> prog xs s'


--
-- * Extra credit

rarms :: Int -> Int -> Prog
rarms x y = [Pen Up, Move x y, Pen Down, Move (x+4) y, Pen Up, Move (x+3) y, Pen Down, Move (x+3) (y+1), Move (x+3) y, Move (x+4) (y+1), Move (x+3) y, Move (x+4) (y-1), Move (x+3) y, Move (x+3) (y-1)]

larms :: Int -> Int -> Prog
larms x y = [Pen Up, Move x y, Pen Down, Move (x-4) y, Pen Up, Move (x-3) y, Pen Down, Move (x-3) (y+1), Move (x-3) y, Move (x-4) (y+1), Move (x-3) y, Move (x-4) (y-1), Move (x-3) y, Move (x-3) (y-1)]

lilbox :: Int -> Int -> Prog
lilbox x y = [Pen Up, Move x y, Pen Down,
          Move (x+1) y, Move (x+1) (y+1), Move x (y+1), Move x y]

bigbox :: Int -> Int -> Prog
bigbox x y = [Pen Up, Move x y, Pen Down,
           Move (x+13) y, Move (x+13) (y+13), Move x (y+13), Move x y]

medbox :: Int -> Int -> Prog
medbox x y = [Pen Up, Move x y, Pen Down,
          Move (x+9) y, Move (x+9) (y+9), Move x (y+9), Move x y]
nose :: Int -> Int -> Prog
nose x y = [Pen Up, Move x y, Pen Down, Move (x+2) y, Move x (y+1), Move x y]

mouth :: Int -> Int -> Prog
mouth x y = [Pen Up, Move x y, Pen Down, Move (x+3) y]



smolbox :: Int -> Int -> Prog
smolbox x y = [Pen Up, Move x y, Pen Down,
          Move (x+5) y, Move (x+5) (y+5), Move x (y+5), Move x y]
-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
amazing :: Prog
amazing = bigbox 30 0 ++ medbox 32 13 ++ smolbox 34 22 ++ rarms 41 17 ++ larms 32 17 ++ lilbox 35 25 ++ lilbox 37 25 ++ lilbox 36 17 ++ lilbox 36 14 ++ lilbox 36 11 ++ nose 36 24 ++ mouth 35 23
