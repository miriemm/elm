-----------------------
-- Miriem Omer
-- 16.11.2020
-----------------------

module Card exposing (Card(..), Face(..), Suit(..), cardValue, viewCard, cardToString, deck, suitOfCard, faceOfCard,cardToUnicode)

import Html exposing (..)
import Html.Attributes exposing (style)

{-
  Replace with your definitions from assignment 1
-}
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

faceToString : Face -> String
faceToString face =
    case face of
        Jack -> "Jack"
        King -> "King"
        Queen -> "Queen"
        Ace -> "Ace"
        Two -> "Two"
        Three -> "Three"
        Four -> "Four"
        Five -> "Five"
        Six -> "Six"
        Seven -> "Seven"
        Eight -> "Eight"
        Nine -> "Nine"
        Ten -> "Ten"

suitToString: Suit -> String
suitToString suit =
    case suit of
        Hearts -> "Hearts"
        Diamonds -> "Diamonds"
        Spades -> "Spades"
        Clubs -> "Clubs"

cardToString: Card -> String
cardToString card =
    case card of
        (Card face suit) -> faceToString(face) ++ " of " ++ suitToString(suit)

cardValue : Card -> List Int
cardValue card =
     case card of
         Card Ace _ -> [1,11]
         Card Two _-> [2]
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

deck : List Card
deck =
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

{-
  Modify this function (if needed) to work with your `Card` definition
-}
cardToUnicode : Card -> String
cardToUnicode (Card face suit) =
   case face of
     Ace -> case suit of
       Spades ->"🂡"
       Hearts -> "🂱"
       Clubs ->  "🃑"
       Diamonds -> "🃁"
     Two -> case suit of
       Spades ->"🂢"
       Hearts -> "🂲"
       Clubs ->  "🃒"
       Diamonds -> "🃂"
     Three -> case suit of
       Spades ->"🂣"
       Hearts -> "🂳"
       Clubs ->  "🃓"
       Diamonds ->"🃃"
     Four -> case suit of
       Spades ->"🂤"
       Hearts -> "🂴"
       Clubs ->  "🃔"
       Diamonds -> "🃄"
     Five -> case suit of
       Spades ->"🂥"
       Hearts -> "🂵"
       Clubs ->  "🃕"
       Diamonds -> "🃅"
     Six -> case suit of
       Spades ->"🂦"
       Hearts -> "🂶"
       Clubs ->  "🃖"
       Diamonds -> "🃆"
     Seven -> case suit of
       Spades ->"🂧"
       Hearts -> "🂷"
       Clubs ->  "🃗"
       Diamonds -> "🃇"
     Eight -> case suit of
       Spades -> "🂨"
       Hearts ->  "🂸"
       Clubs ->   "🃘"
       Diamonds ->  "🃈"
     Nine -> case suit of
       Spades -> "🂩"
       Hearts ->  "🂹"
       Clubs ->   "🃙"
       Diamonds ->  "🃉"
     Ten -> case suit of
       Spades ->"🂪"
       Hearts -> "🂺"
       Clubs ->  "🃚"
       Diamonds -> "🃊"
     Jack -> case suit of
       Spades ->"🂫"
       Hearts -> "🂻"
       Clubs ->  "🃛"
       Diamonds -> "🃋"
     Queen -> case suit of
       Spades ->"🂭"
       Hearts -> "🂽"
       Clubs ->  "🃝"
       Diamonds -> "🃍"
     King -> case suit of
       Spades -> "🂮"
       Hearts -> "🂾"
       Clubs ->  "🃞"
       Diamonds -> "🃎"



suitOfCard : Card -> Suit
suitOfCard (Card face suit) =
     suit

faceOfCard : Card -> Face
faceOfCard (Card face suit) =
    face

{-
  Modify this function (if needed) to work with your `Card` definition
-}
viewCard : Card -> Html msg
viewCard card =
   let
     suit = suitOfCard card
     face = faceOfCard card
     faceName = faceToString face
     suitName = suitToString suit
     suitColor s =
       case s of
         Diamonds -> "red"
         Spades -> "black"
         Hearts -> "red"
         Clubs -> "black"
     unicode = cardToUnicode card
   in
     div [style "display" "inline-block"] [
       div [style "font-size" "12em", style "color" (suitColor suit)] [text unicode],
       div [style "font-size" "0.8em"]  [text (cardToString card)]
     ]

