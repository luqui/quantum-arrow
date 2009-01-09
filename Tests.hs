{-# LANGUAGE Arrows #-}

import Test.QuickCheck
import Data.Complex
import Control.Arrow
import QuantumArrow.Quantum
import Control.Monad
import Control.Monad.Random
import Text.Printf

type Q = Quantum (Rand StdGen)

main = forM_ tests $ \(s,a) -> do
    printf "%-25s: " s >> quickCheck a

tests = ["simple443"     --> prop_simple443
        ,"interfere1"    --> prop_interfere1
        ,"interfere2"    --> prop_interfere2
        ,"interfere3"    --> prop_interfere3
        ,"perlExample"   --> prop_perlExample
        ,"nonCollapse"   --> prop_nonCollapse
        ]
    where
    (-->) = (,)

qArbitrary :: (Eq a) => Q () a -> Gen a
qArbitrary q = do
    gen <- rand
    return $ evalRand (execQuantum q ()) gen


states443 :: Q () Int
states443 = proc () -> do
    entangle -< [ (4,    1  :+ 0)
                , (-4, (-1) :+ 0)
                , (3,    0  :+ 1)
                ]

prop_simple443 = forAll (qArbitrary states443) $ \x ->
    any (== x) [4,-4,3]


-- it's the double slit experiment!  (spiritually)

interfere1 :: Q () Int
interfere1 = proc () -> do
    x <- states443 -< ()
    returnA -< x^2

prop_interfere1 = forAll (qArbitrary interfere1) $ \x ->
    x == 9

-- hmm, that's strange.  Let's try to see what's going on

interfere2 :: Q () (Int,Int)
interfere2 = proc () -> do
    x <- states443 -< ()
    returnA -< (x,x^2)

-- we're testing that it actually didn't interfere here because
-- if it only returned 9, it would report "arguments exhausted",
-- not "passed"
prop_interfere2 = forAll (qArbitrary interfere2) $ \(x,xx) ->
    xx /= 9  ==>  (x == 4 && xx == 16 || x == -4 && xx == 16)

-- odd, let's try it another way

interfere3 :: Q () Int
interfere3 = proc () -> do
    x <- states443 -< ()
    _ <- qLift return -< x  -- stand-in for an observation like print
    returnA -< x^2

-- see reasoning on prop_interfere2
prop_interfere3 = forAll (qArbitrary interfere3) $ \x ->
    x /= 9 ==> x == 16

-- an example showing that we're at least as good as
-- Perl's Quantum::Entanglement

perlExample :: Q () (Int,Int,Int)
perlExample = proc () -> do
    c <- entangle -< [(0, 1 :+ 0), (1, 0 :+ 1)] -- c = |0> + i|1>
    d <- entangle -< [(0, 1 :+ 0), (1, 1 :+ 0)] -- d = |0> +  |1>
    let e = c * d
    e' <- if e == 1
             then qLift return -< e   -- observation on e
             else qLift return -< e   -- observation on e
    returnA -< (e',c,d)

prop_perlExample = forAll (qArbitrary perlExample) $ \(e,c,d) ->
     e == 1 && c == 1 && d == 1
  || e == 0 && (c == 0 || d == 0)

-- an example showing how we're better! :-)

nonCollapse :: Q () Int
nonCollapse = proc () -> do
    x <- entangle -< [(1, 1 :+ 0), (2, (-1) :+ 0), (3, 0 :+ 1)]
    y <- if x == 2
            then returnA -< 1
            else returnA -< x
    -- the cases x == 1 and x == 2 cancel each other out, so 
    -- y should only ever be 3
    returnA -< y

prop_nonCollapse = forAll (qArbitrary nonCollapse) $ \y ->
    y == 3
