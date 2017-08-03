-- Der Hann Marvin Lai 754672 
-- Optional Project for Comp30020 Declarative Programming

-- Function one
-- Substitute a value for another in an array of arbitrary with equatable types

subst :: Eq t => t -> t -> [t] -> [t]
subst _   _   (x : []) = [x]
subst old new (x : xs)
    |x == old = new : subst old new xs
    |otherwise = x : subst old new xs

