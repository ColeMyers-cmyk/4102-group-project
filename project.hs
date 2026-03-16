shuffle :: list -> list --ordered set of 52 cards to shuffled cards

deal :: list -> 3 lists --deck of cards to players two, dealers two, and the rest of the deck --

play :: int -> list -> 2 lists -- take the players choice and their current cards then return their new cards and the updated deck


main :: IO ()
main = do
    putStrLn "Hello, are you ready to play a game? [Y/N]"
    ans <- getLine
    
    if ans == "Y" || ans == "y"then
        do
        putStrLn "let's begin\n ---shufflng deck---"
        play_deck = shuffle(deck)
        putStrLn"---dealing cards---"
        players_cards, dealers_cards, play_deck = deal(play_deck)
        putStrLn "you got the {player_cards[0]}\n you got the {player_cards[1]}"

        putStrLn "What do you want to do?\n 1. stand \n 2. hit\n 3. double down"
        decision <- getLine
        players_deck, play_deck = play(decision, players_cards)
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


