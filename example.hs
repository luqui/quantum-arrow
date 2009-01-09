{-# LANGUAGE Arrows #-}

import Control.Arrow
import QuantumArrow.Quantum
import Data.Complex


makeStates :: Quantum IO () Int
makeStates = proc () -> 
    entangle -< [( 4,   1  :+ 0)
                ,(-4, (-1) :+ 0)
                ,( 3,   0  :+ 1)
                ]

-- Run these example procedures like: execQuantum detangle ()

-- This will output 4, -4, and 3 with equal probability.
detangle :: Quantum IO () ()
detangle = proc () -> do
    x <- makeStates -< ()
    qLift print -< x

-- This will only ever output 9, because 4^2 == (-4)^2 and
-- their probability amplitudes (1 and -1, resp.) cancel each
-- other out.
detangle2 :: Quantum IO () ()
detangle2 = proc () -> do
    x <- makeStates -< ()
    qLift print -< x^2

-- This is an example from the Quantum::Entanglement perl module
-- documentation that inspired this module.
perlExample :: Quantum IO () ()
perlExample = proc () -> do
    c <- entangle -< [(0, 1 :+ 0), (1, 0 :+ 1)] -- c = |0> + i|1>
    d <- entangle -< [(0, 1 :+ 0), (1, 1 :+ 0)] -- d = |0> + |1>
    let e = c * d  -- e = |0*0> + i|0*1> + |1*0> + i|1*1>
    if e == 1
        then qLift_ (putStrLn "e = 1") -< ()
        else qLift_ (putStrLn "e = 0") -< ()
    qLift putStrLn -< "(c,d) = " ++ show (c,d)

-- This demonstrates that conditionals do *not* collapse the state
-- space, unless they must (for example if I/O is done within a branch)
nonCollapsingConditional :: Quantum IO () ()
nonCollapsingConditional = proc () -> do
    x <- entangle -< [(1, 1 :+ 0), (2, (-1) :+ 0), (3, 0 :+ 1)]
    let y = if x == 2 then 1 else x
    qLift print -< y
