-- 1. Give examples for functions with the following types:
-- (Float −> Float) −> Float
applyToZero :: (Float -> Float) -> Float
applyToZero f = f 0

-- Float −> (Float −> Float)
add' :: Float -> (Float -> Float)
add' = (+)

-- (Float −> Float) −> (Float −> Float)
id' :: (Float -> Float) -> (Float -> Float)
id' f = f

-- 2. Define the function concat using foldr.
concat' :: [[a]] -> [a]
concat' = foldr (++) []

-- 3. Define the functions inits and tails using foldr.

-- 4. The function filter can be defined in terms of concat and map:
-- filter p = concat . map box
--   where box x = _
-- Complete the definition of box.

-- 5. Function composition first applies the latter of the supplied functions to the argument, the former thereafter.
-- Write a function before that can be used to rewrite f . g . h to h `before` g `before` f. What can you say about associativity of (.) and before?

-- 6. We have seen that [...] is a type function that maps types to types. Similarly because −> is a type constructor mapping two types to a type,
-- for some c also c −> is a type function mapping a type a to c −> a. Rewrite the type of map by substituting the type function [...] by c −>.
-- Can you derive an implementation from the resulting type?

-- 7. The function map can be applied on functions. Its result is a function as well (with a different type). There are no restrictions on the
-- function type on which map is applied, it might even be applied to map itself! What is the type of the expression map map?