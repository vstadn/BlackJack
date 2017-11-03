{--
Vitalii Stadnyk and Niraj Parajuli
Assignment 7
Functional Programming
BlackJack Project (Project Assignment 7.6) 
--}

import System.Random
import Control.Monad
import Data.Set

data PlayingCard = Card Value Suit
                   deriving (Eq, Ord, Show)
data Suit = Clubs | Diamonds | Hearts | Spades
            deriving (Eq, Ord, Show, Enum)
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
             deriving (Eq, Ord, Show, Enum)
type Deck = [PlayingCard]

-- this function creates initial deck of cards (sorted and unshuffled)
makeDeck :: Deck
makeDeck = [Card value suit | suit <- [Clubs .. Spades], value <- [Two .. Ace]]

-- this function shuffles a deck of cards (using random indexing)
shuffle :: Deck -> Deck -> IO(Deck)
shuffle initialDeck newDeck = do
    if length (initialDeck) == 0
      then
        return newDeck
    else do
      randomGenCards <- newStdGen
      let randomCard = mod (fst (random randomGenCards)) (length initialDeck)
      let shuffledDeck = (initialDeck!!randomCard : newDeck)
      let leftHand = take randomCard initialDeck 
      let rightHand = drop (randomCard + 1) initialDeck
      shuffle (leftHand++rightHand) (shuffledDeck)

-- this function determines which numerical value each card corresponds to and returns it in a list
cardValue :: PlayingCard -> [Int]
cardValue (Card v s)
    | v == Two                                         = [2]
    | v == Three                                       = [3]
    | v == Four                                        = [4]
    | v == Five                                        = [5]
    | v == Six                                         = [6]
    | v == Seven                                       = [7]
    | v == Eight                                       = [8]
    | v == Nine                                        = [9]
    | v == Ten || v == Jack || v == Queen || v == King = [10]
    | v == Ace                                         = [1,11] 

-- this function calculates possible value(s) of player's hand
sumVal :: [Int] -> PlayingCard -> [Int]
sumVal initialSum card = toList (fromList (Prelude.filter (<22) (concat [Prelude.map (\x -> x + y) (cardValue card) | y <- initialSum])))

-- this function displays a card in a readable format
display :: PlayingCard -> String
display (Card v s) = show v ++ " of " ++ show s

-- this functions displays current possible value(s) of player's hand
displaySum :: [Int] -> String -> String -> IO()
displaySum playerSum player1 player2 = do 
    if playerSum == [] then
        putStrLn (player1 ++ " got busted! " ++ player2 ++ " won!")
    else
        if (length(playerSum) > 1) then 
            putStrLn ("The sum is either " ++ show (playerSum!!0) ++ " or " ++ show (playerSum!!1))
        else 
            putStrLn ("The sum is " ++ show (playerSum!!0))

-- this function is responsible for actual Blackjack game
blackJack :: IO()
blackJack = do
    shuffledDeck <- shuffle makeDeck []
    let dealerHand = [shuffledDeck !! i | i <- [0,2]]
    let playerHand = [shuffledDeck !! i | i <- [1,3]]
    let updatedDeck = drop 4 shuffledDeck
    putStrLn ("One of the dealer's card is " ++ display (dealerHand !! 0))
    putStrLn ("Your hand is " ++ display (playerHand !! 0) ++ ", " ++ display (playerHand !! 1))
    let playerSum = sumVal (sumVal [0] (playerHand!!0)) (playerHand!!1)
    let dealerSum = sumVal (sumVal [0] (dealerHand!!0)) (dealerHand!!1)
    displaySum playerSum "You" "Dealer"
    let dealerBlackJack = "The dealer has " ++ display (dealerHand!!0) ++ ", " ++ display (dealerHand!!1) 
    if elem 21 playerSum then do
        putStrLn ("You have blackJack!")
        putStrLn (dealerBlackJack)
        if elem 21 dealerSum then
            putStrLn ("Dealer has a blackJack too! You tied with the Dealer.")
        else
            putStrLn ("You won!")
    else do
        putStrLn ("Would you like to hit or stand? Press 'h' to hit and 's' to stand.")
        option <- getChar
        putStrLn ("")
        if option == 'h' then
            hitUser updatedDeck playerSum dealerHand dealerSum
        else do
            putStrLn ("Dealer has " ++ display (dealerHand!!0) ++ ", " ++ display (dealerHand!!1))
            displaySum dealerSum "Dealer" "You"
            hitDealer updatedDeck playerSum dealerHand dealerSum

-- this function is responsible for handling case when user wants to hit another card
hitUser :: Deck -> [Int] -> Deck -> [Int] -> IO()
hitUser updatedDeck playerSum dealerHand dealerSum = do
    putStrLn ("Your card is " ++ display (updatedDeck!!0))
    let newCard = updatedDeck!!0
    let newDeck = tail updatedDeck
    let newPlayerSum = sumVal playerSum newCard
    displaySum newPlayerSum "You" "Dealer"
    if newPlayerSum == [] then 
        putStrLn ("")
    else do
        putStrLn ("Would you like to hit or stand? Press 'h' to hit and 's' to stand.")
        option <- getChar
        putStrLn ("")
        if option == 'h' then
            hitUser newDeck newPlayerSum dealerHand dealerSum
        else do 
            putStrLn ("Dealer has " ++ display (dealerHand!!0) ++ ", " ++ display (dealerHand!!1))
            if elem 21 dealerSum then 
                putStrLn ("Dealer has a blackJack with 2 cards. You lost...")
            else
                hitDealer newDeck newPlayerSum dealerHand dealerSum

-- this function is responsible for handling case until dealer exceeds 16
hitDealer :: Deck -> [Int] -> Deck -> [Int] -> IO()
hitDealer updatedDeck playerSum dealerHand dealerSum = do
    if dealerSum == [] then
        putStrLn ("")
    else 
        if maximum dealerSum < 17 then do 
            let newCard = updatedDeck!!0
            putStrLn ("Dealer was dealt " ++ display newCard)
            let newDeck = tail updatedDeck
            let newDealerSum = sumVal dealerSum newCard
            displaySum newDealerSum "Dealer" "You"
            hitDealer newDeck playerSum dealerHand newDealerSum    
        else 
            if (maximum playerSum) > (maximum dealerSum) then
                putStrLn ("Your hand is better! You won!")
            else 
                if (maximum playerSum) < (maximum dealerSum) then
                    putStrLn ("Dealer has a better hand! You lost...")
                else
                    putStrLn ("You tied with the Dealer.")

main = blackJack
