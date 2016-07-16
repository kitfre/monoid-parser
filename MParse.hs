module MParse where
-- Implementation of algorithm for recognizing regular languges presented in Monoid Machines: a O(logn) parser for regular languages
-- By Armando B. Matos, 2006
-- Code by Kit Freddura, 2016
-- Idiomatic Haskell revisions and parllel implementation aided by Samuel G. Schlesinger

{- Definitions:
Let (S, *) be a finite monoid, Y a finite alphabet, f: Y -> S and g: S -> {0, 1} be the input and output functions.
f is naturally extended to a monoid homomorphism f: Y* -> S. 
Def: A monoid machine is the 5-tuple (S, *, Y, f, g)
    - A monoid machine recognizes a language L over Y if for all x in Y*, g(f(x)) = 1 iff x in L

Def: f and g
    Given a DFA (Y, S, I, F, D),  the corresponding monoid S
    - for all a in Y let f(a) = Da(q) i.e the function that maps q to D(a,q)
    - for all b in S let g(b) = 1 if b(qo) in F else 0
-}

{- Sequential algorithm 
    Input: DFA recognizing L, a word x = a1a2...an in Y*
    Output: 1 if x in L else 0

    From DFA define:
        - monoid S and it's table
        - the functions g and g
    compute x' = f(a1)*f(a2)*...*f(a3)
    while |x'| >= 2
        select 1 <= 1 < |x'|
        replate aia(i+1) by (ai) * (a(i+1))
    return g(x')
-}


{- example:
 DFA:
    Y = {a,b}
    S = {1, 2}
    I = F = {1}
    D = {(1,b,1), (1,a,2), (2,a,2), (2,b,1)}

S:
    (a, b)
    M(Y) = { fw | w in Y* }
    fe = (1 2)
    fa = (2 2)
    fb = (1 1) 

    table:
        a       b
    a   a       b
    b   a       b

    let x = aaab
    x' = (2 2)(2 2)(2 2)(1 1)
       = (2 2)(2 2)(1 1) : aa -> a
       = (2 2)(1 1): aa -> a
       = (1 1) : ab -> b
    g(b) = fb(1) = 1 -> ACCEPT

    let x = aaba
    x' = (2 2)(2 2)(1 1)(2 2)
       = (2 2)(1 1)(2 2) : aa -> a
       = (1 1)(2 2) : ab -> b
       = (2 2) : ba -> a
    g(a) = fa(1) = 2 -> REJECT
-}
-- CODE --
import Control.Monad
import Control.Parallel.Strategies

type Elem state = state -> Maybe state

-- implement DFA for parsing
data DFA symbol state = DFA {
    states :: [state],
    start :: state,
    finals :: [state],
    delta :: symbol -> state -> Maybe state, 
    accept :: Elem state -> Bool 
}

-- creates accept function from finals state
-- convenience function for defining and accept function
-- for a DFA with a list of finals
createAccept' :: (Eq state) => [state] -> state -> (Elem state -> Bool)
createAccept' fs start = (\fa -> (fa start) `elem` (map Just fs))

-- given an element of the alphabet a construct fa in monoid
createElem :: DFA symbol state -> symbol -> Elem state 
createElem dfa s = (delta dfa) s

-- converts input word to monoid representation
fword :: [symbol] -> DFA symbol state -> [Elem state]
fword xs dfa = map (createElem dfa) xs

-- SEQUENTIAL ALGORITHM
-- parses an input word according to the above algorithm
-- runs currently in O(n + m) time where n is the size of the input word
-- and m is the size of the elements in the monoid
mparse :: [Elem state] -> DFA symbol state -> Bool
mparse [x] dfa = (accept dfa) x
mparse (x:y:xs) dfa = mparse ((x >=> y):xs) dfa

-- mother function, parses from dfa
parse :: [symbol] -> DFA symbol state -> Bool
parse s dfa = mparse (fword s dfa) dfa

-- PARALLEL implementation - implemented by Samuel G. Schlesinger
preduce :: [Elem e] -> Maybe (Elem e)
preduce [] = Nothing
preduce [x] = Just x
preduce es = return $ runEval $ do
    let (xs, ys) = splitAt (length es `div` 2) es
    (Just x) <- rpar (preduce xs)
    (Just y) <- rpar (preduce ys)
    return (x >=> y)

-- parallel parsing method which approaches O(logn) as number of processes increases
pmparse :: [Elem state] -> DFA symbol state -> Bool
pmparse es dfa = case preduce es of 
    Just e -> (accept dfa) e
    Nothing -> False

-- mother function, parses from input word and DFA in parallel
pparse :: [symbol] -> DFA symbol state -> Bool
pparse s dfa = pmparse (fword s dfa) dfa


-- Uncomment below for test DFA which accepts strings ending in b from the alphabet {a,b}+

testdel 'a' 1 = Just 2
testdel 'a' 2 = Just 2
testdel 'b' 1 = Just 1
testdel 'b' 2 = Just 1
testdel _ _   = Nothing

dfa = DFA [1,2] 1 [1] testdel (createAccept' [1] 1)

