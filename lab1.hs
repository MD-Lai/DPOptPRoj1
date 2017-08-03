-- Der Hann Marvin Lai 754672 
-- Optional Project for Comp30020 Declarative Programming

-- Question one
-- Substitute a value for another in an array of arbitrary with equatable types
subst :: Eq t => t -> t -> [t] -> [t]
subst _   _   (x : []) = [x]
subst old new (x : xs)
    |x == old = new : subst old new xs
    |otherwise = x : subst old new xs

-- Question two
-- Interleave one array into the other
interleave :: [t] -> [t] -> [t]
interleave [] [] = []
interleave [] x  = x
interleave x []  = x
interleave (x:xs) (y:ys) = x : y : interleave xs ys