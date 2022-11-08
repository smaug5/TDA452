module BlackJack where
import Cards
import RunGame
import Test.QuickCheck


hand3 = Add (Card Ace Hearts)
            (Add (Card Ace Spades)
            (Add (Card (Numeric 10) Spades) Empty))

hand2 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)

hand1 = Add (Card Ace Spades) Empty


sizeSteps :: [Integer]

sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))
            , 1 + size (Add (Card Jack Spades) Empty)
            , 1 + 1 + size Empty
            , 1 + 1 + 0
            , 2 ]

displayCard :: Card -> String
displayCard (Card (Numeric number) suit) = show number ++ " of " ++ show suit
displayCard (Card rank suit) = show rank ++ " of " ++ show suit

-- display with patternmatching so it doesn't say "Hand is empty" when there are cards
display :: Hand -> String
display Empty = "Hand is empty"
display (Add card Empty) = displayCard card
display (Add card hand)  = displayCard card ++ ", " ++ display hand


-- display with a guard to prevent "Hand is empty" when hand isn't empty
display2 :: Hand -> String
display2 Empty = "Hand is empty"
display2 (Add card hand)
    | hand == Empty = displayCard card
    | otherwise     = displayCard card ++ ", " ++ display2 hand



----------------------------------A2--------------------------------


value :: Hand -> Integer
value hand 
    | initialValue hand > 21 = initialValue hand - numberOfAces hand * 10
    | otherwise = initialValue hand

numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace _) hand) = 1 + numberOfAces hand
numberOfAces (Add _ hand) = 0 + numberOfAces hand

initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add (Card Ace _) hand) = 11 + value hand
initialValue (Add (Card (Numeric num) _) hand) = num + value hand
initialValue (Add _ hand) = 10 + value hand


-------------------A3-------------------------------------------


gameOver :: Hand -> Bool
gameOver hand
    | value hand > 21 = True
    | otherwise = False


---------------------A4-----------------------------------------

winner :: Hand -> Hand -> Player
winner hand1 hand2
    | value hand1 <= value hand2 = Bank
    | otherwise = Guest


----------------------B1---------------------------------------


(<+) :: Hand -> Hand -> Hand
(<+) h1 Empty = h1
(<+) h1 (Add card Empty) = Add card h1
(<+) h1 (Add card h2) = Add card (h1 <+ h2)