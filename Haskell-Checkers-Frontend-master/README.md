# System

This provides a front-end for a checkers game written in Haskell
using the Brick library. It is meant for UCalgary CPSC449 course in
programming paradigms. This is a TUI (a textual user interface), and should
work over SSH or PuTTY.

## Setting up and running the program

To set up the program, you must have 
[stack installed](https://www.haskellstack.org "stack-download-link")
(click on the above link to download and install stack). The following set of
bash commands will clone the git repository and build the stack project.

``` shell
git clone https://github.com/benjamin-macadam/Haskell-Checkers-Frontend.git
cd Haskell-Checkers-Frontend
stack build
```
You can also download the project as a zip file from the github page and run
stack build once you've entered the Haskell-Checkers-Frontend.
Once you've run the build command, 
stack will then setup an appropriate environment to run this project.
This may take 20 minutes your first time as stack will need to download
and install a sandboxed GHC compiler and all of the necessary packages.
To run this program, enter

``` shell
stack run
```

and the program will run (note that at this point, all that will happen
is a simple human vs. human game will be initiated). To debug the program,
you will want to use the stack environments ghci.

``` shell
stack ghci run
```

## Game Loop

The game loop is split into two parts, human and AI.

### Human move

The human controls the cursor and builds a move. This is activated
when the gamestate has a "Human" player making a move.

-   pressing the arrow keys navigates the board
-   pressing the space key adds the current square to the move.
-   pressing enter "applies the move".
-   if the move is legal, then the move should be applied and the next players turn begins.
-   if the move is illegal, then the game must tell them they made an illegal move and ask them to try again.

### AI move

When AI's move will be displayed in the "move" bracket of the game status menu. 
The user must press enter for the move to be applied.

