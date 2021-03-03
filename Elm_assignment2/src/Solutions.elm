-----------------------
-- Miriem Omer
-- 28.10.2020
-----------------------

module Solutions exposing (..)
import List exposing (filter)
import String exposing (foldr, words)

-- Exercise 2.2.1
type Face = Ace
          | Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | King
          | Queen

--four suits: clubs, diamonds, hearts and spades
type Suit = Hearts | Diamonds | Spades | Clubs
type Card = Card Face Suit

-- generates the list of cards in a 52 card deck
deck : List Card
deck  =
    let
        listFace = [ Ace, Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,King,Queen]
        listSuit = [Hearts, Diamonds, Spades, Clubs]

        deckHelper: List a -> List (a->b) -> List b
        deckHelper l1 =
            List.map (\b -> l1 |> List.map b)
                        >> List.concat

    in
        Card
        |> List.singleton
        |> deckHelper listFace
        |> deckHelper listSuit


-- Exercise 2.2.2


cardValue : Card -> List Int
cardValue card =
     case card of
         Card Ace _ -> [1,11]
         Card Two _ -> [2]
         Card Three _ -> [3]
         Card Four _ -> [4]
         Card Five _ -> [5]
         Card Six _ -> [6]
         Card Seven _ -> [7]
         Card Eight _ -> [8]
         Card Nine _ -> [9]
         Card Ten _ -> [10]
         Card Jack _ -> [10]
         Card King _ -> [10]
         Card Queen _ -> [10]


--Exercise 2.2.3

-- for this problem, I used quicksort algorithm from laboratory 4
partition : comparable -> List comparable -> (List comparable, List comparable)
partition pivot l =
    (filter (\x -> x < pivot) l, filter (\x -> x >= pivot) l)
quicksort : List comparable -> List comparable
quicksort l =
    case l of
        [] -> []
        x::xs ->
            let
                (less, greater) = partition x xs
            in
               (quicksort less) ++ [x] ++ (quicksort greater)

len: List Int -> Int
len l =
    case l of
        [] -> 0
        x :: xs -> 1 + len xs

-- k smallest numbers from list
-- I checked to see if k is greater than the number of elements in that list
-- if is greater than I just sort the list (using quicksort)
-- if is smaller, I used "take" function to take the k smallest elements
-- I thought that it would be easier if the list would be sorted, and I just take the first k elements
-- so, that is why i used take k (quicksort list) -> k being the number of elements needed from the list (smallest)
                                           ---    -> and the sorted list

smallestK: Int -> List Int -> List Int
smallestK k list =

   if k >= len(list) then quicksort list
          else
              let
                  take n l =
                      if n <= 0 then
                        []
                       else
                         case l of
                             [] -> []
                             x::xs -> x :: take ( n - 1) xs

              in
                take k (quicksort list)


-- Exercise 2.2.4

--Function that checks if the list is empty or not
isEmpty:  List Char -> Bool
isEmpty list =
  if list == [] then True
  else False

head: List Char ->  Char
head l =
    case l of
        [] -> ' ' -- empty list -> no char
        x::xs -> x -- I am taking the first character (head of the list)

tail: List Char -> List Char
tail l =
     case l of
         [] -> [] -- if the list is empty, it returns an empty list also
         x::xs -> xs -- if the list is not empty, we take the list without the first element
                                                  -- in other words, from index x+1

-- This function is used to convert a string to a list of characters
toList : String -> List Char
toList string =
  foldr (::) [] string


-- my function, it gets a String as argument and it returns Bool (True/False)
-- this function recursively checks if a string contains matching amount
-- of opening and closing parentheses (using check fct)

-- expectancy of parentheses in the string is kept in a balance indicator count
-- positives indicate amount of needed ')' and negatives amount of '('
-- initial balance is 0

-- when recursion reaches end of a string it checks if balance is ok (count == 0)
-- => matching amount of parentheses seen


balanced: String -> Bool
balanced lista =
    let

         check: List Char -> Int -> Bool
         check lista2 count =
             if isEmpty lista2 then count == 0
             else
                 if head lista2 == '(' then check (tail lista2) (count + 1)
             else
                 -- to ensure that ')' wasn't encountered before '('
                 if head lista2 == ')' then
                     if count == 0 then False
                    else check (tail lista2)(count - 1)
             else
                 check (tail lista2) count

    in
        check (toList lista) 0



-- Exercise 2.2.5


-- head of the list, the first element
-- This will be used in my coinChange function
-- Usually, this kind of functions return Maybe but in my function, I wanted to return an integer
headForCoins: List Int -> Int
headForCoins l =
    case l of
        [] -> 0 -- I will not use this, I put 0 to not have an error
        x::xs -> x -- This will be used: if the list is not empty,
                                   -- I take the first element
-- Second function needed
tailForCoins: List Int -> List Int
tailForCoins l =
    case l of
        [] -> [] -- if the list is empty, it returns an empty list also
        x::xs -> xs -- if the list is not empty, we take the list without the first element
                                                 -- in other words, from index x+1


-- the idea: to use our coins and subtract it from the current amount of money
-- Eventually, the amount of money will be either:
--      - 0
--      - some negative number (meaning this combination of coins failed)
--      - some positive number (meaning that we can still subtract more with the coins we have)
-- coinChange (money - headForCoins(coins)) coins : will exhaust all combination subtracting the first coin from the money
-- coinChange money  (tailForCoins(coins)): exhausts all combinations using all other coins only
-- they are added together (+)
-- the result: how many different ways we can make change for an amount of money

coinChange: Int -> List Int -> Int
coinChange money coins =
    if money == 0 then 1 -- it doesn't work without this condition
     else if  coins == [] || money < 0 then 0
     else coinChange (money - headForCoins(coins)) coins  + coinChange money  (tailForCoins(coins))