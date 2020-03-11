-- know the difference between bindings, declarations and references
--in haskell you can never have a decalration without a binding
nats :: [Int]  --declaration
nats = 1 : map (+1) nats

primes :: [Int] --declaration
primes =  primes' nats --reference
  where
      primes' (x:xs) = x : (primes' $ filter ((!= 0) (`mod` x)) xs) --binding
      -- primes' on left is a binding /declaration
      -- mod filter x xs primes are all references


-- CIRCLE NAMES, UNDERLINE REFERENCES (LOOK UP NAMES!!!)
-- Lists are dynamically scoped
-- +, ->, maybe, are all names


ex = Add (Get "x") (Lit 2) -- Env -> Maybe Val
---- SEMANTIC DOMAINS
-- type Env = Map Name Val
-- type Env = [(Name, Val)]
-- type Env = Name -> Val
-- The meaning depends on the Environment

-- Domain: Env -> Val
-- Environment: Name -> Val

--semantic domain Stack -> Maybe Stack
data Cmd
  = PushInt Int
  | Pop
  | AddCmd

type Stack = [Int]
type Domain = Stack -> Maybe Stack

-- There are not 2 expr next to the add because in a stack based language the two values are on the stack
