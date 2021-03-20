# tic-tac-roll

# Game Description

Please refer to the game description from [Assignment 1 Wiki](https://git.gvk.idi.ntnu.no/course/prog2006/prog2006-2021/-/wikis/Tasks/Assignment-1:-tic-tac-roll)

# Usage

* Run the game with `stack run -- -h ` to see the command line options.
* The default logger logs the game session in `debug.log` file. You can change the filename with `-d filename` flag.
* The default mode is human vs computer, and you can use the `-f` flag to make the computer to take the first move. 
  By default, computer waits for the human (opponent) to make the first move.

  
# Computer vs. Computer

To run the game in computer vs. computer mode, run:
```
mkfifo pipe1
mkfifo pipe2

// in one terminal
cat pipe1 | stack run &> pipe2

// in another terminal
cat pipe2 | stack run -- -f -d debug2.log &>pipe1
```

The programs should quit by themselves, and you should have a transcript 
of the game in two log files: `debug.log` and 
`debug2.log`, as seen from two player's perspectives. Observe, that in both files, 
the marks are swapped because in both the first player is always `X`.  


# Simple AI

Currently, the game manages a simple game heuristic, with a single move look-ahead.

1. If there is a winning move, take it.
2. If there is a winning move for the opponent, take it to prevent oponent making that move.
3. If the middle of the board is free, take it.
4. Otherwise, make a random move. 


# Comments

The code is heavily commented, beyond reasonable approach. 
This is done to highlight some general thinking and
some of the modelling decisions/aspects. 
Normally, the code should NOT be commented that much, and the function types and names
should in most cases work as documentation alone (comments should not be necessary).


## Credits

Some parts of the original codebase for the game have been inspired by the [Dwayne Crooks implementation]
(https://github.com/dwayne/xo-haskell) in particular, the win condition matching. 
Note however, that this implementation is using different approach to solve most of the other tasks.
