nextline [] = [1]
nextline rx = [a+b | (a,b) <- zip (0 : rx ++ [0]) (tail (0 : rx ++ [0]))]

triangle 0 = [nextline []]
-- triangle 1 = triangle 0 ++ [nextline (last (triangle 0))]
triangle n = triangle (n-1) ++ [nextline (last (triangle (n-1)))]

coeff 0 = [1]
coeff n = nextline (coeff (n-1))