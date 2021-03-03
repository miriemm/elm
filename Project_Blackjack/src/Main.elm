-----------------------
-- Miriem Omer
-- 16.11.2020
-----------------------

module Main exposing (main, getValue,filter,calculateScoreHelper,possibleScores,calculateScore)

import Browser
import Html exposing (..)
import Html.Attributes exposing (disabled, hidden, style)
import Html.Events exposing (..)
import List exposing (foldl)
import Random
import Debug exposing (toString)


import Card exposing (..)


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type alias Model =
  { hand: List Card,
    deck: List Card,
    showDeck: Bool
  }

startingModel : Model
startingModel =
    Model [] Card.deck True

init : () -> (Model, Cmd Msg)
init _ =
  ( startingModel
  , Cmd.none
  )


type Msg
  = Draw
  | NewCard Card
  | ToogleDeck


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Draw ->
      ( model
      , drawCard model
      )
    
    -- Add the new card to the player's hand (`hand`) and remove it from the `deck`
    NewCard newCard ->
      ( -- here, I add the newCard to the player's hand
        (Model (newCard :: model.hand)
        -- I use List.filter to filter out (remove) the card from the deck
        -- in other words, I make a new list based on an input list,
        -- filtering out elements that don't match a predicate
       (List.filter (\x -> x /= newCard) model.deck) -- the cards that have been drawn are removed from the deck
       model.showDeck)
      , Cmd.none
      )


    -- Toggle (if it's `True` set it `False`, if it's `False` set it to `True`) the `showDeck` member of the `Model`
    ToogleDeck ->
      (
        if (model.showDeck == True) then
           {model |
             showDeck = False}
        else
           {model |
              showDeck = True}
      , Cmd.none
      )

drawCard : Model -> Cmd Msg
drawCard model =
  case model.deck of
    (first::rest) -> Random.generate NewCard (Random.uniform first rest)
    _ -> Cmd.none

{-
  1. Get the value of each card (use `cardValue`)
  2. Generate a list of all possible scores
  3. Return the score closest to 21 (less than 21!), if one exists, else the smallest score over 21
  ```elm
  calculateScore [Card King Hearts] == 10
  calculateScore [Card Two Hearts] == 2
  calculateScore [Card Two Hearts, Card King Spades] == 12
  calculateScore [Card Ace Hearts, Card King Spades] == 21
  calculateScore [Card Ace Hearts, Card Five Hearts, Card Seven Spades] == 13
  calculateScore [Card King Hearts, Card Five Hearts, Card Seven Spades] == 22
  calculateScore [Card King Hearts, Card Ten Clubs, Card Ace Spades] == 21
  calculateScore [Card Ace Spades, Card Ace Clubs, Card Ten Clubs, Card King Clubs] == 22
  ```
-}

-- function that gets the value of each card
-- and those values are returned in a list
getValue : List Card -> List Int
getValue card =
   case card of
       [] -> []
       x::xs -> cardValue x ++ getValue xs

-- function to add the scores
-- takes as argument a List of Integers ( representing the values of cards)
-- and returns an integer representing the sum => total score
addScores : List Int -> Int
addScores lista =
    -- I used foldl to calculate this sum
    foldl (+) 0 lista

-- filter function : takes as argument the list of cards
-- and returns an integer representing the number of 'Aces' found
-- in other words, I filtered my list and count the number of Aces
filter : List Card -> Int
filter l =
    case l of
        -- if the list is empty, we return 0
        [] -> 0
        -- if the list is not empty, we go through the list
        -- and check if we find Aces in the list
        x::xs -> -- if we find, we add 1 and continue checking the rest of the list
               if x == Card Ace Hearts ||
                x == Card Ace Spades ||
                x == Card Ace Diamonds ||
                x == Card Ace Clubs then 1 + filter xs
                -- if we don't find Aces, we filter the rest of the list without adding anything
               else filter xs

-- helper function for calculating the score
-- takes as argument a list of cards
-- and returns an integer representing the score
calculateScoreHelper : List Card -> Int
calculateScoreHelper val =
      case val of
          -- if the list is empty, we return 0
          [] -> 0
          -- if it is not empty, we go through the list of cards
          -- and check the value of each card (using cardValue)
          x :: xs -> -- if the value is [1,11] (so the card is Ace)
                     -- I added 11 for that card and continue with the rest of the list
                     -- (Ace can take 2 values '1' and '11', so I chose to add 11)
                     if cardValue x == [1,11] then 11 + calculateScoreHelper xs
                     -- if it is not Ace (it is any other card)
                     -- we calculate the score with its value (cardValue x)
                     else addScores (cardValue x) + calculateScoreHelper xs


-- function to get the possible scores
-- takes as argument a list of cards
-- ands returns a list of integers
-- if there is an Ace in the list of cards -> we will have a list with 2 elements (possible scores)
-- example: Card Ace Hearts, Card Two Spades -> possible scores: [3,13]
-- otherwise, we will have a list with only one element
possibleScores : List Card -> List Int
possibleScores cards =
   case cards of
    -- if the list is empty, returns an empty list
    []-> []
    -- if it is not empty
    -- we go through the list of cards
    x :: xs ->  -- if the number of Aces is larger than 1 (there are Aces in the list)
                if filter cards >= 1  then
                                  -- we calculate the possible scores :
                                  -- first element of the list will be the score when Ace has the value 1 (Ace = 1)
                                  -- for this, I subtracted 10 * filter cards meaning 10 * number of Aces
                                  -- and I appended the second element: the score when Ace has the value 11
                                 [(calculateScoreHelper cards) - 10 * filter cards] ++ [calculateScoreHelper cards]
                -- if there are no Aces, we just calculate the score using calculateScoreHelper function
               else [calculateScoreHelper cards]

-- calculateScore function: takes as argument a list of cards and returns the final score
-- this function returns the score closest to 21 (less than 21!), if one exists,
-- else the smallest score over 21
calculateScore : List Card -> Int
calculateScore cards =
    case cards of
        -- if the list is empty, returns 0
        [] -> 0
        -- if the list is not empty, we go through the list of cards
        x::xs ->  -- if the number of Aces is larger than 1
                  -- and the score calculated is larger than 22
                  -- we return the score calculated (with Ace having the value 1)
                   if filter cards >= 1 && (calculateScoreHelper cards > 22) then
                                          (calculateScoreHelper cards) - 10 * filter cards
                   -- if there are no Aces, we simply calculate the score
                  else calculateScoreHelper cards


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

{-
  Use the `viewCard` function for showing the player's hand and the `cardToUnicode` function for the remaining deck.
-}
view : Model -> Html Msg
view model =
  let
    appName = "Blackjack"

    -- function to return the status of the game: Win or Lose
    winStatus : List Card -> String
    winStatus score =
        -- if the score is 21 then it is WIN
        if calculateScore score == 21 then "Win"
        -- if the score is less or above 21 then it is LOSE
        else "Lose"

    -- function to show the player's hand
    showHand : List Card -> List (Html msg)
    showHand cards =
        case cards of
            -- if the list is empty, returns an empty list
            [] -> []
            -- if the list is not empty
            -- we go through the list and we use the viewCard function (for showing the cards)
            x::xs -> viewCard x :: showHand xs

     -- function to show the remaining deck
    showRemainingDeck =
        model.deck
        -- Here, I used cardToUnicode function
        |> List.map cardToUnicode
        -- and this is used to make String -> Html msg
        |> List.map text

    -- hideDeck function will be used to toggle the deck visibility
    -- I used it below with the 'hidden' attribute ( hidden indicates the relevance of an element)
    hideDeck =
         -- the showDeck member of the model
        -- if it is True , set it False
        if model.showDeck == True then False
        -- if it is False, set it True
        else True

    -- function used to disable the draw button
    -- when used with 'disabled'
    -- if we obtain 21 or above, we cannot draw anymore
    disableDraw =
        if calculateScore model.hand >= 21 then True
        else False


  in
  -- I added some colors and font sizes to make my web page look more aesthetic
    div [style "background-color" "yellow"
         ,style "height" "90px"
         ,style "width" "100%"]
      [div [style "font-size" "2em"] [ h1 [] [text appName] ]
         -- button used to draw a card, it is disabled when score of model.hand >= 21
       , button [ onClick Draw, style "background-color" "green", style "font-size" "1.5em", disabled disableDraw] [ text "Draw a card" ]
         -- button used to toggle the deck visibility ( hidden or shown)
       , button [ onClick ToogleDeck, style "background-color" "red", style "font-size" "1.5em"] [text "Toggle the deck visibility"]
       -- here, I used the boolean attribute 'hidden' : to hide the remaining deck ( when I press the button from above)
       , div [style "font-size" "3em", hidden hideDeck]  showRemainingDeck
       -- The score is calculated for the drawn cards : calculateScore model.hand
       -- and it is displayed
       , div [ style "font-size" "1.5em"] [text "Score: ", text <| toString <| (calculateScore model.hand)]
       -- Here, I used the function created above 'winStatus' => check the status for the player's hand (model.hand)
       -- Win or Lose text: depending on the score (if it is 21- Win) , a Win or Lose text is displayed
       , div [ style "font-size" "1.5em"] [text "Win or Lose?  ", text <| toString <| winStatus model.hand]
       -- The Player's hand is displayed ( what cards did he draw)
       -- here, I used the showHand function implemented above with the argument model.hand ( the player's hand)
       , div [style "font-size" "1em"] (showHand model.hand)
      ]


