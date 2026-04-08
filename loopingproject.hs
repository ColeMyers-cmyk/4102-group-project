import System.Random.Shuffle (shuffleM)
import System.Exit (exitSuccess) -- Safetly terminates Program 
import Text.Read (readMaybe) -- Important for Exception Handling

getSpecificElements :: [Int] -> [a] -> [a]
getSpecificElements indices list = [list !! i | i <- indices]

shuffle :: [a] -> IO [a] -- ordered set of 52 cards to shuffled cards
shuffle play_deck = shuffleM play_deck

deal :: [a] -> ([a], [a], [a]) -- deck of cards to players two, dealers two, and the rest of the deck --
deal play_deck =
    (players_cards, dealers_cards, remaining_deck)
  where
    players_cards = getSpecificElements [0, 2] play_deck
    dealers_cards = getSpecificElements [1, 3] play_deck
    remaining_deck = drop 4 play_deck

cardValue :: String -> Int -- takes a card and returns its blackjack value
cardValue card =
    let rank = takeWhile (/= '-') card
    in case rank of
        "A"  -> 11
        "K"  -> 10
        "Q"  -> 10
        "J"  -> 10
        _    -> read rank

handValue :: [String] -> Int -- take a hand and return total value
handValue cards = adjustForAces total aces
  where
    values = map cardValue cards
    total = sum values
    aces = length (filter (\c -> takeWhile (/= '-') c == "A") cards)

    adjustForAces t 0 = t
    adjustForAces t a
        | t > 21 = adjustForAces (t - 10) (a - 1)
        | otherwise = t

play :: Int -> [String] -> [String] -> ([String], [String]) -- take the players choice and their current cards then return their new cards and the updated deck
play decision players_cards play_deck
    | decision == 1 = (players_cards, play_deck)
    | decision == 2 =
        case play_deck of
            [] -> (players_cards, play_deck)
            (x:xs) -> (players_cards ++ [x], xs)
    | decision == 3 =
        case play_deck of
            [] -> (players_cards, play_deck)
            (x:xs) -> (players_cards ++ [x], xs)
    | otherwise = (players_cards, play_deck)

-- Dealer automatically draws cards until reaching 17
-- Demonstrates recursion as a control structure (replacing loops like while)
-- Uses pattern matching to safely handle the deck
dealerPlay :: [String] -> [String] -> ([String], [String]) -- dealer hits until 17 or more
dealerPlay dealers_cards play_deck
    | handValue dealers_cards < 17 =
        case play_deck of
            [] -> (dealers_cards, play_deck)
            (x:xs) -> dealerPlay (dealers_cards ++ [x]) xs
    | otherwise = (dealers_cards, play_deck)

-- Displays a player's or dealer's hand
-- Demonstrates IO operations and use of mapM_ for iterating over lists
-- Uses built-in functions to process and display data

showHand :: String -> [String] -> IO ()
showHand label cards = do
    putStrLn label
    mapM_ putStrLn cards
    putStrLn $ "Value: " ++ show (handValue cards)
-- Handles the player's turn and returns updated hand and deck
-- Demonstrates IO operations and state passing in functional programming
playerTurn :: [String] -> [String] -> IO ([String], [String])
playerTurn players_cards play_deck = do
    showHand "You have: " players_cards
-- Checks if player has busted (value > 21)
-- Demonstrates conditional control structure
    if handValue players_cards > 21 then
        return (players_cards, play_deck)
    else do
        putStrLn "What do you want to do?\n 1. stand \n 2. hit\n 3. double down"
        decisionStr <- getLine

        case readMaybe decisionStr :: Maybe Int of
            Nothing -> do
                putStrLn "wrong input\n What do you want to do?\n 1. stand \n 2. hit\n 3. double down"
                playerTurn players_cards play_deck
            Just decision ->
                if decision == 1 then
                    return (players_cards, play_deck)
                else if decision == 2 then do
                    let (new_players_cards, new_play_deck) = play decision players_cards play_deck
                    playerTurn new_players_cards new_play_deck
                else if decision == 3 then do
                    let (new_players_cards, new_play_deck) = play decision players_cards play_deck
                    return (new_players_cards, new_play_deck)
                else do
                    putStrLn "wrong input\n What do you want to do?\n 1. stand \n 2. hit\n 3. double down"
                    playerTurn players_cards play_deck


gameloop :: [string] -> IO
gameloop play_deck
    if length play_deck < 4
        do
        play_deck <- shuffle deck
    do
        let (players_cards, dealers_cards, remaining_deck) = deal play_deck

            putStrLn $ "you got the " ++ (players_cards !! 0)
            putStrLn $ "you got the " ++ (players_cards !! 1) ++ "\n"

            let dealer_show = [head dealers_cards]
            putStrLn $ "Dealer shows: " ++ head dealer_show

            (final_players_cards, final_play_deck) <- playerTurn players_cards remaining_deck

            if handValue final_players_cards > 21 then
                putStrLn "You lose"
            else do
                if handValue final_players_cards == 21 then
                    putStrLn "You have 21!"
                else
                    putStrLn "Player stands."

                let (final_dealers_cards, _) = dealerPlay dealers_cards final_play_deck

                putStrLn "\nDealer's hand:"
                mapM_ putStrLn final_dealers_cards
                putStrLn $ "Dealer value: " ++ show (handValue final_dealers_cards)

                if handValue final_dealers_cards > 21 then
                    putStrLn "You win!!!"
                else if handValue final_players_cards > handValue final_dealers_cards then
                    putStrLn "You win!!!"
                else if handValue final_players_cards < handValue final_dealers_cards then
                    putStrLn "Dealer wins"
                else
                    putStrLn "Push (tie)"




-- Entry point of the program
-- Demonstrates IO operations and overall program control
main :: IO ()
main = do

-- Represents a full deck of cards using a list data structure
-- Demonstrates use of lists to store and manage data

    let deck = ["A-H", "2-H", "3-H", "4-H", "5-H", "6-H", "7-H", "8-H", "9-H", "10-H", "J-H", "Q-H", "K-H",
                "A-S", "2-S", "3-S", "4-S", "5-S", "6-S", "7-S", "8-S", "9-S", "10-S", "J-S", "Q-S", "K-S",
                "A-D", "2-D", "3-D", "4-D", "5-D", "6-D", "7-D", "8-D", "9-D", "10-D", "J-D", "Q-D", "K-D",
                "A-C", "2-C", "3-C", "4-C", "5-C", "6-C", "7-C", "8-C", "9-C", "10-C", "J-C", "Q-C", "K-C"]

    putStrLn "Hello, are you ready to play a game? [Y/N]"
-- Reads user input and controls program flow using if-then-else
-- Demonstrates conditional control structures
    ans <- getLine

    if ans == "Y" || ans == "y" then
        do        
            putStrLn "let's begin\n ---shuffling deck---"
-- Shuffles the deck using an IO operation
-- Demonstrates handling of randomness in Haskell
            play_deck <- shuffle deck
            putStrLn "---dealing cards---"
-- Splits the deck into player cards, dealer cards, and remaining deck
-- Demonstrates tuple unpacking and list manipulation
            gameloop(play_deck)
            
    else if ans == "N" || ans == "n" then
        do
            putStrLn "Have a nice day."
            exitSuccess
    else
        putStrLn "incorrect input"