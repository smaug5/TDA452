module BlackJack where
import Cards
import RunGame
import System.Random

-----------------A0----------------------------
{-Checking the size function-}
testHand1 :: Hand
testHand1 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)

hand4 :: Hand
hand4 = Add (Card Jack Spades)
            (Add (Card Queen Hearts)
            (Add (Card (Numeric 5) Diamonds) Empty))

hand3 :: Hand
hand3 = Add (Card Ace Hearts)
            (Add (Card Ace Spades)
            (Add (Card (Numeric 10) Spades) Empty))

hand2 :: Hand
hand2 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)

sizeSteps :: [Integer]
sizeSteps = [ size testHand1
            , size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))
            -- ... add the remaining steps here
            , 1 + size (Add (Card Jack Spades) Empty)
            , 1 + 1 + size Empty
            , 1 + 1 + 0
            , 2]


-----------------A1----------------------------
{-displayCard shows a given cards rank and suit-}
displayCard :: Card -> String
displayCard (Card (Numeric rInt) s) = show rInt ++ " of " ++ show s
displayCard (Card r s) = show r ++ " of " ++ show s

{-displayHand recursively uses displayCard to display all the cards in a hand-}
display :: Hand -> String
display Empty = "The hand is empty"
display (Add card Empty) = displayCard card
display (Add card hand)  = displayCard card ++ ", " ++ display hand


-----------------A2----------------------------
-- returns the value of a given Rank. gives different value of Ace depending on Bool
valueRank :: Card -> Bool -> Integer
valueRank (Card (Numeric rInt) _) _ = rInt
valueRank (Card Ace _) b        | not b = 11
                                | otherwise = 1
valueRank (Card r _) _          = 10

-- calculates the value of the hand recursively calling on valueRank, passing
-- along the Bool for whether or not aces should be counted as 11 or 1.
initialValue :: Hand -> Bool -> Integer
initialValue Empty _             = 0
initialValue (Add card Empty) b  = valueRank card b
initialValue (Add card hand) b   = valueRank card b + initialValue hand b

-- if initialValue hand False > 21, then do initialValue hand True instead
-- True means that aces now count as 1 instead of 11. Test with testHand3 and testHand4
value :: Hand -> Integer
value hand   
        | initialValue hand False > 21 = initialValue hand True
        | otherwise = initialValue hand False


-----------------A3----------------------------
-- very simple method. if value of hand is over 21 returns true, otherwise false
gameOver :: Hand -> Bool
gameOver hand 
            | value hand > 21   = True
            | otherwise         = False


-----------------A4----------------------------
-- winner checks what hand has won the round of Blackjack, guestHand or bankHand
-- two guards for cases when the guest can win
-- first guard is when the value of guestHand is higher than bankHand but not bust. 
-- second guard is when the bankHand is bust and not the guesthand
-- bank wins in all other cases
winner :: Hand -> Hand -> Player
winner guestHand bankHand 
                | (value guestHand > value bankHand) 
                    && not (gameOver guestHand) 
                    = Guest
                | gameOver bankHand && not (gameOver guestHand)
                    = Guest
                | otherwise = Bank


----------------------B1---------------------------------------
-- <+ is an operator which adds a hand to the top of another hand.

(<+) :: Hand -> Hand -> Hand
(<+) Empty h1 = h1
(<+) (Add card h1) h2 = Add card (h1 <+ h2)

--- Checks that the operator <+ is associative
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3


--- Checks that the size of a new hand from two hands is the same as the two separate added.
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = numberOfCards(h1 <+ h2) == numberOfCards h1  + numberOfCards h2

--- Calculates the number of cards in a hand
numberOfCards :: Hand -> Integer
numberOfCards Empty = 0
numberOfCards (Add _ hand) = 1 + numberOfCards hand

---------------------B2----------------------------------------

fullDeck :: Hand
fullDeck = addCardsToHand allCards

--- Helper function which adds all cards from a list to a hand
addCardsToHand :: [Card] -> Hand
addCardsToHand cards = foldr Add Empty cards

---- Makes a list of all kind of cards in a standard deck (except Joker)
allCards :: [Card]
allCards = [Card rank suit| suit <- suits, rank <- ranks]
    where ranks = map Numeric [2..10] ++ [Jack, Queen, King, Ace]
          suits = [Hearts, Spades, Diamonds, Clubs]

----------------------B3-----------------------------------------

-- Draws the topcard of a deck
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty hand = error "Draw: the deck is empty."
draw (Add card deck) hand = (deck, Add card hand)


-----------------------B4----------------------------------------
type Deck = Hand

--- Function which draws cards for the bank
playBank :: Deck -> Hand
playBank deck = playBankHelper deck Empty


--- Draws cards for the bank hand until it reaches a value of over 16
playBankHelper :: Deck -> Hand -> Hand
playBankHelper deck bankHand
                | value bankHand >= 16 = bankHand
                | otherwise = playBankHelper smallerDeck biggerHand
                where (smallerDeck, biggerHand) = draw deck bankHand


--------------------B5-----------------------------------------


--- Shuffles a deck using System.Random, with the help of a StdGen
shuffleDeck :: StdGen -> Deck -> Deck
shuffleDeck gen Empty = Empty
shuffleDeck gen (Add card Empty) = Add card Empty
shuffleDeck gen deck             = shuffleDeck gen' smallerDeck <+ Add card Empty
    where
        (n, gen')                = randomR (1,size deck) gen
        (smallerDeck, card)      = removeNth deck n


-- Removes card number 'n' from the deck and then returns the rest of the deck, and
-- also the card
removeNth :: Deck -> Integer -> (Deck, Card)
removeNth deck 1 = (smallerDeck, card)
    where 
        (smallerDeck, Add card h)   = draw deck Empty
removeNth deck n                    = (evenSmallerDeck <+ hand, card)
    where
        (smallerDeck, hand)         = draw deck Empty
        (evenSmallerDeck, card)     = removeNth smallerDeck (n-1)



-- Helper function for prop_shuffle_sameCards
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

-- Checks so that a deck has the same cards before and after shuffling it
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffleDeck g h


-- Checks so that a deck doesn't change it size after being shuffled
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle gen deck = size deck == size (shuffleDeck gen deck)
---------------------------B6----------------------------------------------

-- Interface for running a game through runGame
implementation :: Interface
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