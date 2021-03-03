-----------------------
-- Miriem Omer
-- 28.10.2020
-----------------------

module Regex exposing (..)

head: List Char ->  Char
head l =
    case l of
        [] -> ' '
        x::xs -> x -- This will be used: if the list is not empty,
                                   -- I take the first element

tail: List Char -> List Char
tail l =
     case l of
         [] -> [] -- if the list is empty, it returns an empty list also
         x::xs -> xs -- if the list is not empty, we take the list without the first element
                                                  -- in other words, from index x+1

-- functions helpful from laboratory 4: take & drop
-- the n parameters: determines how many elements to take or drop
-- returns a list depending  on that additional parameter
take : Int -> List a -> List a
take n l =
        if n <= 0 then
        []
        else
        case l of
        [] -> []
        x::xs -> x :: take (n - 1) xs

drop : Int -> List Char -> List Char
drop n l =
    if n <= 0 then l
    else
       case l of
           [] -> []
           _::xs -> drop (n - 1) xs

-- function that checks if the list is empty
isEmpty:  List Char -> Bool
isEmpty text =
  if text == [] then True
  else False

-- function that calculates the length of a list
len: List Char -> Int
len l =
    case l of
        [] -> 0
        x :: xs -> 1 + len xs



type RegexPattern
  = Literal Char
  | Many RegexPattern
  | OneOf RegexPattern RegexPattern
  | Seq RegexPattern RegexPattern

{-
  The `Ok` variant represents the matched input and the rest of the unmatched input
  The `Err` variant represents the original input
-}
type alias RegexResult = Result (List Char) (List Char, List Char)

{-
  Returns the `Ok` variant if the literal character matches the first character of the string.
  If the string is empty or the characters don't match the `Err` variant should be returned.
  ```elm
  matchLit 'a' ['a', 'b', 'b'] == Ok (['a'], ['b', 'b'])
  matchLit 'c' ['a', 'b', 'b'] == Err ['a', 'b', 'b']
  matchLit 'c' [] == Err []
  ```
-}
-- matchLit:
-- takes as argument the character and a list of characters
-- checks to see if the list is empty:
-- if yes: it returns a RegexResult Err []
-- if is not empty: verifies if the character matches the first element from the list
--                 if this thing happens-> it returns Ok ( [a list with the character that matched],[the rest of the list])
--                 if not -> Err(the whole list)
matchLit : Char -> List Char -> RegexResult
matchLit ch str =
      case str of
          [] -> Err []
          x::xs -> if ch == x then Ok ([ch] ,xs) -- the first character matches the literal character => Ok
                   else Err (x :: xs) -- if not, error


{-
  Matches `pat1` and then `pat2`. Returns `Ok` only if both succeed.
  ```elm
  matchSeq (Literal 'a') (Literal 'b') ['a', 'b', 'c'] == Ok (['a', 'b'], ['c'])
  matchSeq (Literal 'a') (Literal 'b') ['a', 'x', 'c'] == Err (['a', 'x', 'c'])
  matchSeq (Seq (Literal 'a') (Literal 'b')) (Literal 'c') ['a', 'b', 'c', 'd'] == Ok (['a', 'b', 'c'], ['d'])
  ```
-}
-- matchSeq : here we have 2 patters: pat1 and pat2
matchSeq : RegexPattern -> RegexPattern -> List Char -> RegexResult
matchSeq pat1 pat2 input =
    case input of
        [] -> Err []
        -- here, I checked if pattern1 and pattern2 match the first and the second characters from the list
        -- if this is true, then we return Ok with a list containing this two characters and one more list containing the rest of characters
        x::xs -> if pat1 == (Literal x) && pat2 == Literal (head xs) then Ok ([x, (head xs)], (tail xs))
                 -- if pattern1 is Seq( from what I see, it is like calling itself-> matchSeq function) : Seq with 2 Literals: first character from list and the second one
                 -- and patttern2 is Literal : head(tail xs) is the 3rd element from the list
                 -- So, if all of this match we can return Ok and one list with this characters and other with the rest
                 else if pat1 == (Seq (Literal x)(Literal (head xs))) && pat2 == Literal (head (tail xs)) then Ok ([x, (head xs), head(tail xs)], tail(tail(xs)))
                 -- if pattern1 and pattern2 don't match the first 2 characters, we return an error(Err) and the whole list
                 else Err (x::xs)
{-
  Matches the pattern `pattern` zero or many times. Always returns the `Ok` variant.
  ```elm
  matchMany (Literal 'a') ['a', 'a', 'a'] == Ok (['a', 'a', 'a'], [])
  matchMany (Literal 'b') ['a', 'a', 'a'] == Ok ([], ['a', 'a', 'a'])
  matchMany (Literal 'b') ['b', 'b', 'a'] == Ok (['b', 'b'], ['a'])
  matchMany (Seq (Literal 'b') (Literal 'a')) ['b', 'a', 'b', 'a', 'c'] == Ok (['b', 'a', 'b', 'a'], ['c'])
  matchMany (Seq (Literal 'b') (Literal 'a')) ['b', 'a', 'c', 'a', 'c'] == Ok (['b', 'a'], ['c', 'a', 'c'])
  ```
-}

-- function to filter lists
-- removes some elements that don't match a predicate
-- in this way, we obtain a new list with only the elements that match a certain predicate
-- from laboratory 4
filter: RegexPattern -> List Char -> List Char
filter pred l =
        case l of
            [] ->  []
            x::xs ->
               if pred == Literal x then x::filter pred xs
                 else filter pred xs

matchMany : RegexPattern -> List Char -> RegexResult
matchMany pattern input =
      case input of
          [] -> Err [] -- if the input list is empty -> return Err []
          x::xs ->  -- if the pattern matches the first element from the list
                    -- and if the result after filtering is the whole list
                    -- it results that the character matched the whole list
                    -- so it should return Ok([input list],[another empty list])
                    if (pattern == Literal x) then
                      if  (filter pattern input == input) then Ok(input,[])
                      -- if the result after filtering is not the whole list
                      -- then, there are some elements in the list that don't match the whole input list
                      -- we return Ok with 2 lists: first[list with elements that matched that character] and [a list with the rest of elements that did not match]
                      else Ok(x::filter pattern xs, drop (len (x::filter pattern xs)) input)
                      -- here, I verified the case if we have Seq
                      -- the same idea(kinda) as above but I didn't know how to include filter here:(
                   else if match (Seq(Literal x) (Literal (head xs))) input == Ok ([x,head xs], tail xs) then
                        if [x,head xs] /= [head(tail xs), head(tail(tail xs))] then match (Seq(Literal x) (Literal (head xs))) input
                        else Ok([x,head xs, head(tail xs),head(tail(tail xs))],drop (len([x,head xs, head(tail xs),head(tail(tail xs))])) input)
                   else  Ok([],input) -- if it doesn't match the list we return Ok( an empty list and the input list)




{-
  Tries to match one of `pat1` and `pat2`, in this order. If `pat1` matches, its result is returned, else
  `pat2` is tried.
  ```elm
  matchOneOf (Literal 'a') (Literal 'b') ['a', 'a', 'a'] == Ok (['a'], ['a', 'a'])
  matchOneOf (Literal 'b') (Literal 'a') ['a', 'a', 'a'] == Ok (['a'], ['a', 'a'])
  matchOneOf (Seq (Literal 'a') (Literal 'b')) (Seq (Literal 'c') (Literal 'd')) ['c', 'd', 'a'] (Ok (['c', 'd'], ['a']))
  ```
-}


-- matchOneOf: here we also have 2 patters but we have to check if only one of them matches
matchOneOf : RegexPattern -> RegexPattern -> List Char -> RegexResult
matchOneOf pat1 pat2 input =
    case input of
        [] -> Err [] -- if the list is empty, return Err []
        x::xs -> -- if the list is not empty:
                 -- I check to see if the first pattern(first character) matches the first element from the list
                 -- if this happens, I return Ok([list with the character that matched],[rest of the list])
                 if (pat1 == (Literal x)) then Ok ([x],xs)
                 -- if not, I verify to see if the second character matches the first element from the list
                 -- if yes, return Ok([list with the character that matched], [rest of the list])
                 else if (pat2 == (Literal x)) then Ok ([x],xs)
                 -- and here, if the pattern is Seq(2 characters match the first 2 elements from the list)
                 else if pat1 == (Seq (Literal x)(Literal (head xs)))  || pat2 == (Seq (Literal x)(Literal (head xs))) then Ok ([x, head xs],tail xs)
                 else Err [] -- else, Err []


match : RegexPattern -> List Char -> RegexResult
match pattern input =
  case pattern of
    Literal char -> matchLit char input
    Many pat -> matchMany pat input
    OneOf pat1 pat2 -> matchOneOf pat1 pat2 input
    Seq pat1 pat2 -> matchSeq pat1 pat2 input
