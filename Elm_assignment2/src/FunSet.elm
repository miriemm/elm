-----------------------
-- Miriem Omer
-- 28.10.2020
-----------------------

module FunSet exposing (..)

type alias FunSet = Int -> Bool

contains : FunSet -> Int -> Bool
contains set elem = set elem

singletonSet : Int -> FunSet
singletonSet elem = \inputElem -> elem == inputElem

-- this will be used in the setOf function
-- checks to see if 'x' is equal to the element or if the set contains 'x'
newElement: FunSet -> Int -> FunSet
newElement set elem = (\x -> (elem == x) || contains set x)

{-
Conveniece function to create a set of elements.
```elm
setOf [1, 2, 3] == union (union (singletonSet 1) (singletonSet 2)) (singletonSet 3))
setOf [1, 2, 3] == fold union [(singletonSet 1), (singletonSet 2), (singletonSet 3)]
```
-}
-- setOf function gets a list of integers as argument
-- and it returns FunSet
setOf : List Int -> FunSet
setOf elems =
    case elems of
        -- verifies if the list is empty
        -- if it is, any number we introduce can't be in that set (because it's empty)
        [] -> (\_ -> False) -- so it returns False
        -- if the list is not empty, we use the newElement function
        x::xs -> newElement (setOf xs) x  -- return False if the condition from newElement is not satisfied
                                          -- True otherwise

{-
Returns the union of 2 sets.
```elm
(union (singletonSet 1) (singletonSet 2)) 1 == True
(union (singletonSet 1) (singletonSet 2)) 1 == False
```
-}
-- union function:
-- checks if the 'x' element is in 'a' or 'b'
-- if YES -> it returns True
-- if NOT -> it returns False
union : FunSet -> FunSet -> FunSet
union a b = (\x -> (contains a x) || (contains b x))
        -- (\x -> 5 == x )

{-
Returns the intersection of 2 sets.
```elm
(intersect (setOf [1, 2]) (setOf [1, 3])) 1 == True
(intersect (setOf [1, 2]) (setOf [1, 3])) 2 == False
```
-}
-- intersect function
-- here, we use setOf when we call the function
-- checks if 'x' element is in 'a' and 'b' -> if it is, the sets intersect
-- so, we use setOf to check if x is in the first and second set
-- for the above example,
-- setOf [1,2] contains '2', but setOf [1,3] doesn't -> it returns False (elem 2 is not their intersection)
-- elem '1' is in both sets so it returns True

intersect : FunSet -> FunSet -> FunSet
intersect a b = (\x -> (contains a x) && (contains b x))

{-
Returns the difference of 2 sets.
```elm
(diff (setOf [1, 2]) (setOf [1, 3])) 1 == False
(diff (setOf [1, 2]) (setOf [1, 3])) 2 == True
```
-}
-- difference function
-- checks to see if 'a' contains 'x' and 'b' doesn't contain 'x'
-- if they both contained the same element, then there would be no difference
-- for the above example,
-- setOf [1,2] contains 1, setOf [1,3] also contains 1 -> no diff, it returns False
-- so one set should contain the element, and the other should not
diff : FunSet -> FunSet -> FunSet
diff a b = (\x -> contains a x && (not (contains b x)))


{-
Returns a new set, with `function` applied to each of element.
```elm
(map (\x -> x + 1) (setOf [1, 2]) 1) == False
(map (\x -> x + 1) (setOf [1, 2]) 2) == True
(map (\x -> x + 1) (setOf [1, 2]) 3) == True
```
-}

-- returns whether all bounded integers within 's' satisfy 'p'
forall: FunSet -> (Int -> Bool) -> Bool
forall s p =
    let
        iter: Int -> Bool
        iter a =
            if (contains s a) && not(p(a)) then False
            else if a > 1000 then True
            else iter(a + 1)
    in
       iter(-1000)

-- returns whether exists a bounded integer whithin 's' that satisfies 'p'
occurs: FunSet -> (Int-> Bool) -> Bool
occurs s p = not(forall s (\x -> not(p(x))))

-- returns a set transformed by applying 'function' to each segment of 'set'
map: ( Int -> Int ) -> FunSet -> FunSet
map function set = (\y -> occurs set (\x -> y == function(x)))

{-
Takes a list of sets and returns a new set, which is build by applying a fold using `operation` function.
```elm
(fold union [(singletonSet 1), (singletonSet 2), (singletonSet 3)]) 1 == True
(fold intersect [setOf [1], setOf [2]]) 1 == False
(fold intersect [setOf [1], setOf [2]]) 2 == False
(fold diff2 [setOf [1, 2, 3], setOf [1], setOf [2]] ) 1 == False
(fold diff2 [setOf [1, 2, 3], setOf [1], setOf [2]] ) 2 == False
(fold diff2 [setOf [1, 2, 3], setOf [1], setOf [2]] ) 3 == True
```
-}

-- Here, my fold wasn't working as it should for diff operation
-- I had to change the order
-- when I check the fold diff, I use diff2 instead of diff
diff2: FunSet -> FunSet -> FunSet
diff2 a b = (\x -> (not(contains a x) && contains b x))

-- I used the recursive function fold left
-- from laboratory 4
foldl: (a -> b -> b) -> b -> List a -> b
foldl op start l =
    case l of
        [] -> start
        x::xs -> foldl op (op x start) xs

fold: ( FunSet -> FunSet -> FunSet ) -> List FunSet ->  FunSet
fold  sets operation =
    case operation of
        [] -> \_ -> False
        x :: xs ->  foldl sets x xs