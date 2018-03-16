-- problem two: The Towers of Hanoi --

type Peg = String
type Move = (Peg, Peg)

-- First, we move n-1 from peg1 to peg2 with the help of peg3.
-- And then, move the last one from peg1 to peg3.
-- Then, with the help of peg1, we move n-1 from peg2 to peg3.

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 from buffer to = [(from, to)]
hanoi n from buffer to = 
    hanoi (n-1) from to buffer ++
    hanoi 1 from buffer to ++
    hanoi (n-1) buffer from to


-- if we use four pegs, the result is depended on the value of k.
-- First, we move n-k from peg1 to peg2 with peg3 and peg4 being buffer
-- Then, move k from peg1 to peg4 and not use peg2
-- Last, move n-k from peg2 to peg4 with four pegs

k :: Integer
k = 3

hanoiFour :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiFour 0 from buffer1 buffer2 to = []
hanoiFour 1 from buffer1 buffer2 to = [(from, to)]
hanoiFour n from buffer1 buffer2 to =
    hanoiFour (n-k) from buffer2 to buffer1 ++
    hanoi k from buffer2 to ++
    hanoiFour (n-k) buffer1 from buffer2 to
