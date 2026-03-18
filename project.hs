import System.Random.Shuffle (shuffleM)

getSpec7ficElements indices list = [list !! i | i <- indices]

shuffle :: [a] -> IO [a] --ordered set of 52 cards to shuffled cards
shuffle play_deck = shuffleM deck

deal :: [a] -> ([a], [a], [a])--deck of cards to players two, dealers two, and the rest of the deck --
deal play_deck =
        (players_cards, dealers_cards, play_deck )
    where
        players_cards = getSpecificElements [0,2] play_deck
        dealers_cards = getSpecificElements [1,3] play_deck
        play_deck = getSpecificElements [4:52] play_deck

play :: int -> [a] -> ([a], [a])-- take the players choice and their current cards then return their new cards and the updated deck
play decision players_cards = (players_cards, play_deck)
    where
        
        if ans == 2 then
            players_cards.add(play_deck.pop(0))
        else if ans == 3 then
            players_cards.add(play_deck.pop(0))
            players_cards.add(play_deck.pop(1))
        else if ans != 1 then
            do
                putStrLn "wrong input\n What do you want to do?\n 1. stand \n 2. hit\n 3. double down"
                decision <- getline
                play(decision, players_cards)
    
        putstrLn "You have: "
        mapM_ putstrLn players_deck



main :: IO ()
main = do
    
    let deck = ["A-H", "1-H", "2-H", "3-H", "4-H", "5-H", "6-H", "7-H", "8-H", "9-H", "10-H", "J-H", "Q-H", "K-H","A-S", "1-S", "2-S", "3-S", "4-S", "5-S", "6-S", "7-S", "8-S", "9-S", "10-S", "J-S", "Q-S", "K-S","A-D", "1-D", "2-D", "3-D", "4-D", "5-D", "6-D", "7-D", "8-D", "9-D", "10-D", "J-D", "Q-D", "K-D","A-C", "1-C", "2-C", "3-C", "4-C", "5-C", "6-C", "7-C", "8-C", "9-C", "10-C", "J-C", "Q-C", "K-C"]

    putStrLn "Hello, are you ready to play a game? [Y/N]"
    ans <- getLine
    
    if ans == "Y" || ans == "y"then
        do
        putStrLn "let's begin\n ---shufflng deck---"
        let play_deck = shuffle(deck)
        putStrLn"---dealing cards---"
        let (players_cards, dealers_cards, play_deck) = deal(play_deck)
        putStrLn $ "you got the " ++ player_cards[0] ++ "\n you got the "++ player_cards[1] ++"\n"

        putStrLn "What do you want to do?\n 1. stand \n 2. hit\n 3. double down"
        decision <- getLine
        let (players_deck, play_deck) = play(decision, players_cards)
        if value > 21 then
            putstring "You lose"
        else 
            do 
                if players_cards == 21 do
                    putstruing "You win!!!"
                else do 
                    putStrLn "What do you want to do?\n 1. stand \n 2. hit"
                    decision <- getLine
                    play(decision, players_cards)
        if players_cards == 21 do
            putstruing "You win!!!"
        else
            While dealers_cards < 21 do
                play(2, dealers_cards)
                if dealers_cards > 21 do
                    putstring "You win!!!"
                else if players_cards < dealers_cards do
                    putstring "Dealer wins"


    else if ans == "N" || ans == "n" then
        putStrLn "Have a nice day."
        exit()
    else 
        putstring "incorrect input"


