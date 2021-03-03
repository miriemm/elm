-----------------------
-- Miriem Omer
-- 12.10.2020
-----------------------



--Exercise 1.2.1 & 1.2.2


module Solutions exposing (..)



type Face = Ace
          | Two
          | Three
          | Four
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

faceToString:  Face -> String
faceToString face =
    case face of
        Jack -> "Jack"
        King -> "King"
        Queen -> "Queen"
        Ace -> "Ace"
        Two -> "Two"
        Three -> "Three"
        Four -> "Four"
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


-- Card face suit -> " {Face} of {Suit}"
cardToString: Card -> String
cardToString card =
    case card of
        (Card face suit) -> faceToString(face) ++ " of " ++ suitToString(suit)



-- Exercise 1.2.3


type alias Point = {x: Float, y: Float}
type alias Segment = { start: Point, end: Point }

linesIntersect: Segment-> Segment -> String
linesIntersect firstSegment secondSegment =
    let

        deltaX = firstSegment.end.x - firstSegment.start.x
        deltaY = firstSegment.end.y - firstSegment.start.y
        slope1 = deltaY/deltaX
        deltaXnew = secondSegment.end.x - secondSegment.start.x
        deltaYnew = secondSegment.end.y - secondSegment.start.y
        slope2 = deltaYnew/deltaXnew

        xIntersect = ((firstSegment.start.x * firstSegment.end.y - firstSegment.start.y * firstSegment.end.x)  * (secondSegment.start.x - secondSegment.end.x) -
                     (firstSegment.start.x - firstSegment.end.x) * (secondSegment.start.x * secondSegment.end.y - secondSegment.start.y * secondSegment.end.x))
                     /((firstSegment.start.x - firstSegment.end.x) * (secondSegment.start.y - secondSegment.end.y) -
                     (firstSegment.start.y - firstSegment.end.y) * (secondSegment.start.x - secondSegment.end.x))

        yIntersect = ((firstSegment.start.x * firstSegment.end.y - firstSegment.start.y * firstSegment.end.x)  * (secondSegment.start.y - secondSegment.end.y) -
                     (firstSegment.start.y - firstSegment.end.y) * (secondSegment.start.x * secondSegment.end.y - secondSegment.start.y * secondSegment.end.x))
                     /((firstSegment.start.x - firstSegment.end.x) * (secondSegment.start.y - secondSegment.end.y) -
                     (firstSegment.start.y - firstSegment.end.y) * (secondSegment.start.x - secondSegment.end.x))


    in
       if slope1 == slope2 then "Segmente paralele"
       else
           if xIntersect >= firstSegment.start.x
           && xIntersect <= firstSegment.end.x
           && xIntersect >= secondSegment.start.x
           && xIntersect <= secondSegment.end.x
           && yIntersect >= firstSegment.start.y
           && yIntersect <= firstSegment.end.y
           && yIntersect >= secondSegment.start.y
           && yIntersect <= secondSegment.end.y
           then "Segmentele se intersecteaza"
           else
               "Segmentele nu se intersecteaza"



-- Exercise 1.2.4

trailingZeros: Int -> Int
trailingZeros n =
    let
       iterate: Int -> Int -> Int
       iterate result x =
           if x == 0 then
               result
           else
               iterate(result + x // 5)(x//5)
    in
    iterate 0 n



















