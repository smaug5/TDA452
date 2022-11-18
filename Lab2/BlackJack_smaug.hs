module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import System.Random

hand4 = Add (Card Jack Spades)
            (Add (Card Queen Hearts)
            (Add (Card (Numeric 5) Diamonds) Empty))

hand3 = Add (Card Ace Hearts)
            (Add (Card Ace Spades)
            (Add (Card (Numeric 10) Spades) Empty))

hand2 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)

hand1 = Add (Card Ace Spades) Empty

testCard = Card Ace Spades


sizeSteps :: [Integer]

sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))
            , 1 + size (Add (Card Jack Spades) Empty)
            , 1 + 1 + size Empty
            , 1 + 1 + 0
            , 2 ]

displayCard :: Card -> String
displayCard (Card (Numeric number) suit)    = show number ++ " of " ++ show suit
displayCard (Card rank suit)                = show rank ++ " of " ++ show suit

-- display with patternmatching so it doesn't say "Hand is empty" when there are cards
display :: Hand -> String
display Empty = "Hand is empty"
display (Add card Empty) = displayCard card
display (Add card hand)  = displayCard card ++ ", " ++ display hand


-- display with a guard to prevent "Hand is empty" when hand isn't empty
display2 :: Hand -> String
display2 Empty              = "Hand is empty"
display2 (Add card hand)
    | hand == Empty         = displayCard card
    | otherwise             = displayCard card ++ ", " ++ display2 hand

{- valueRank :: Rank -> Integer
valueRank (Card Numeric rank _) = rank
valueRank (Card Ace _)          = 11
valueRank Card                  = 10 -}

----------------------------------A2--------------------------------


value :: Hand -> Integer
value hand 
    | initialValue hand > 21 = initialValue hand - numberOfAces hand * 10
    | otherwise              = initialValue hand

numberOfAces :: Hand -> Integer
numberOfAces Empty                   = 0
numberOfAces (Add (Card Ace _) hand) = 1 + numberOfAces hand
numberOfAces (Add _ hand)            = 0 + numberOfAces hand

initialValue :: Hand -> Integer
initialValue Empty                              = 0
initialValue (Add (Card Ace _) hand)            = 11 + value hand
initialValue (Add (Card (Numeric num) _) hand)  = num + value hand
initialValue (Add _ hand)                       = 10 + value hand


-------------------A3-------------------------------------------


gameOver :: Hand -> Bool
gameOver hand
    | value hand > 21 = True
    | otherwise       = False


---------------------A4-----------------------------------------

winner :: Hand -> Hand -> Player
winner guestHand bankHand
    | (value guestHand > value bankHand) && not (gameOver guestHand) = Guest
    | gameOver bankHand && not (gameOver guestHand)= Guest
    | otherwise = Bank


----------------------B1---------------------------------------
-- <+ is an operator which adds a hand to the top of another hand.

(<+) :: Hand -> Hand -> Hand
(<+) h1 Empty = h1
(<+) h1 (Add card h2) = Add card (h1 <+ h2)

--- Checks that the operator <+ is associative
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3


--- Checks that the size of a new hand from two hands 
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = numberOfCards(h1 <+ h2) == numberOfCards h1  + numberOfCards h2


numberOfCards :: Hand -> Integer
numberOfCards Empty = 0
numberOfCards (Add _ hand) = 1 + numberOfCards hand

---------------------B2----------------------------------------

fullDeck :: Hand
fullDeck = addCardsToHand allCards


addCardsToHand :: [Card] -> Hand
addCardsToHand [] = Empty
addCardsToHand (card : cards) = Add card (addCardsToHand cards)


allCards :: [Card]
allCards = [Card rank suit| suit <- suits, rank <- ranks]
    where ranks = (map Numeric [2..10] ++ [Jack, Queen, King, Ace])
          suits = [Hearts, Spades, Diamonds, Clubs]

----------------------B3-----------------------------------------

first :: Hand -> Card
first (Add card hand) = card


draw :: Hand -> Hand -> (Hand, Hand)
draw Empty hand = error "Draw: the deck is empty."
draw (Add card deck) hand = (deck, Add card hand)


-----------------------B4----------------------------------------
type Deck = Hand

playBank :: Deck -> Hand
playBank deck = playBankHelper deck Empty

playBankHelper :: Deck -> Hand -> Hand
playBankHelper deck bankHand
                | value bankHand > 16 = bankHand
                | otherwise = playBankHelper smallerDeck biggerHand
                where (smallerDeck, biggerHand) = draw deck bankHand


--------------------B5-----------------------------------------

shuffleDeck :: StdGen -> Deck -> Deck
shuffleDeck gen Empty = Empty
shuffleDeck gen (Add card Empty) = Add card Empty
shuffleDeck gen deck             = (shuffleDeck gen' smallerDeck) <+ (Add card Empty)
    where
        (n, gen')                = (randomR (1,size deck) gen)
        (smallerDeck, card)      = (removeNth deck n)


removeNth :: Deck -> Integer -> (Deck, Card)
removeNth deck 1 = (smallerDeck, card)
    where 
        (smallerDeck, (Add card h)) = draw deck Empty
removeNth deck n                    = (evenSmallerDeck <+ hand, card)
    where
        (smallerDeck, hand)         = draw deck Empty
        (evenSmallerDeck, card)     = removeNth smallerDeck (n-1)




belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h


prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffleDeck g h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle gen deck = size deck == size (shuffleDeck gen deck)
---------------------------B6----------------------------------------------
implementation = Interface
  { iFullDeck = fullDeck
  , iValue    = value
  , iDisplay  = display
  , iGameOver = gameOver
  , iWinner   = winner 
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffleDeck
  }

main :: IO ()
main = runGame implementation