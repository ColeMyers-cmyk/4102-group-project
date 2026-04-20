# 4102-group-project


Overview

This project is a command-line card game written in Haskell that simulates a standard 52-card deck. The program demonstrates core functional programming concepts such as list manipulation, randomness, user input handling, and control flow. The game initializes a full deck, shuffles it, and begins gameplay based on user interaction.

Features
	•	Full 52-card deck representation using lists
	•	Deck shuffling using randomness
	•	Interactive user input (Y/N prompt)
	•	Game loop for continuous play
	•	Separation of logic for deck creation, shuffling, and gameplay

Concepts Demonstrated
	•	Functional programming in Haskell
	•	Use of IO for user interaction
	•	Lists as primary data structures
	•	Conditional logic using if-then-else
	•	Monadic operations using do notation
	•	Basic game state handling

Project Structure

loopingproject.hs – Main program file
README.md – Project documentation

How to Run
	1.	Install Haskell if not already installed
Download from https://www.haskell.org/platform/
	2.	Compile the program
ghc loopingproject.hs
	3.	Run the executable
./loopingproject

How It Works

The program creates a full deck of cards consisting of four suits: Hearts, Spades, Diamonds, and Clubs. Each suit contains thirteen cards ranging from Ace through King.

The user is prompted to start the game by entering Y or N. If the user selects Y, the deck is shuffled and the game begins. If the user selects N, the program exits.

The main game logic is handled through a game loop function, which processes the shuffled deck and manages gameplay.

Example Deck Format

“A-H” represents Ace of Hearts
“10-S” represents Ten of Spades
“K-D” represents King of Diamonds

Dependencies

If required, include the following imports in the Haskell file:

import System.Exit (exitSuccess)

Note that Haskell does not include a built-in shuffle function, so a custom shuffle implementation or external library may be required.

Future Improvements
	•	Add a scoring system
	•	Implement specific card game rules such as Blackjack or War
	•	Improve input validation and error handling
	•	Add multiplayer functionality
	•	Develop a graphical user interface
