import System.Random.Shuffle (shuffleM)
import System.Exit (exitSuccess)
import Text.Read (readMaybe)

getSpecificElements :: [Int] -> [a] -> [a]
getSpecificElements indices list = [list !! i | i <- indices]

shuffle :: [a] -> IO [a]
shuffle play_deck = shuffleM play_deck

deal :: [a] -> ([a], [a], [a])
deal play_deck =
    (players_cards, dealers_cards, remaining_deck)
  where
    players_cards = getSpecificElements [0, 2] play_deck
    dealers_cards = getSpecificElements [1, 3] play_deck
    remaining_deck = drop 4 play_deck

cardValue :: String -> Int
cardValue card =
    let rank = takeWhile (/= '-') card
    in case rank of
        "A"  -> 11
        "K"  -> 10
        "Q"  -> 10
        "J"  -> 10
        _    -> read rank

handValue :: [String] -> Int
handValue cards = adjustForAces total aces
  where
    values = map cardValue cards
    total = sum values
    aces = length (filter (\c -> takeWhile (/= '-') c == "A") cards)

    adjustForAces t 0 = t
    adjustForAces t a
        | t > 21 = adjustForAces (t - 10) (a - 1)
        | otherwise = t

play :: Int -> [String] -> [String] -> ([String], [String])
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

dealerPlay :: [String] -> [String] -> ([String], [String])
dealerPlay dealers_cards play_deck
    | handValue dealers_cards < 17 =
        case play_deck of
            [] -> (dealers_cards, play_deck)
            (x:xs) -> dealerPlay (dealers_cards ++ [x]) xs
    | otherwise = (dealers_cards, play_deck)

showHand :: String -> [String] -> IO ()
showHand label cards = do
    putStrLn label
    mapM_ putStrLn cards
    putStrLn $ "Value: " ++ show (handValue cards)

playerTurn :: [String] -> [String] -> IO ([String], [String])
playerTurn players_cards play_deck = do
    showHand "You have:" players_cards
    if handValue players_cards > 21 then
        return (players_cards, play_deck)
    else do
        putStrLn "What do you want to do?"
        putStrLn "1. stand"
        putStrLn "2. hit"
        putStrLn "3. double down"
        decisionStr <- getLine

        case readMaybe decisionStr :: Maybe Int of
            Nothing -> do
                putStrLn "Wrong input."
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
                    putStrLn "Wrong input."
                    playerTurn players_cards play_deck

gameloop :: [String] -> [String] -> IO ()
gameloop deck play_deck = do
    current_deck <-
        if length play_deck < 4
            then do
                putStrLn "\nNot enough cards left. Reshuffling a new deck..."
                shuffle deck
            else return play_deck

    let (players_cards, dealers_cards, remaining_deck) = deal current_deck

    putStrLn "\n--- New Round ---"
    putStrLn $ "You got the " ++ (players_cards !! 0)
    putStrLn $ "You got the " ++ (players_cards !! 1)

    putStrLn $ "Dealer shows: " ++ head dealers_cards

    (final_players_cards, final_play_deck) <- playerTurn players_cards remaining_deck

    if handValue final_players_cards > 21 then
        putStrLn "You lose"
    else do
        if handValue final_players_cards == 21 then
            putStrLn "You have 21!"
        else
            putStrLn "Player stands."

        let (final_dealers_cards, deck_after_dealer) = dealerPlay dealers_cards final_play_deck

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

        askReplay deck deck_after_dealer
        return ()

    askReplay deck final_play_deck

askReplay :: [String] -> [String] -> IO ()
askReplay deck next_deck = do
    putStrLn "\nPlay again? [Y/N]"
    ans <- getLine
    if ans == "Y" || ans == "y" then
        gameloop deck next_deck
    else if ans == "N" || ans == "n" then do
        putStrLn "Have a nice day."
        exitSuccess
    else do
        putStrLn "Incorrect input."
        askReplay deck next_deck

main :: IO ()
main = do
    let deck =
            [ "A-H", "2-H", "3-H", "4-H", "5-H", "6-H", "7-H", "8-H", "9-H", "10-H", "J-H", "Q-H", "K-H"
            , "A-S", "2-S", "3-S", "4-S", "5-S", "6-S", "7-S", "8-S", "9-S", "10-S", "J-S", "Q-S", "K-S"
            , "A-D", "2-D", "3-D", "4-D", "5-D", "6-D", "7-D", "8-D", "9-D", "10-D", "J-D", "Q-D", "K-D"
            , "A-C", "2-C", "3-C", "4-C", "5-C", "6-C", "7-C", "8-C", "9-C", "10-C", "J-C", "Q-C", "K-C"
            ]

    putStrLn "Hello, are you ready to play a game? [Y/N]"
    ans <- getLine

    if ans == "Y" || ans == "y" then do
        putStrLn "Let's begin."
        putStrLn "--- Shuffling deck ---"
        play_deck <- shuffle deck
        putStrLn "--- Dealing cards ---"
        gameloop deck play_deck
    else if ans == "N" || ans == "n" then do
        putStrLn "Have a nice day."
        exitSuccess
    else
        putStrLn "Incorrect input."