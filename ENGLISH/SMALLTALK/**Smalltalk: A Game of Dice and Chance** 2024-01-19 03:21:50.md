```smalltalk
"Define the variables needed for the program"
numPlayers := 4.
players := OrderedCollection new: numPlayers.
scores := OrderedCollection new: numPlayers.

"Initialize the players and scores"
1 to: numPlayers do: [ :i |
    players add: ('Player' , i printString).
    scores add: 0.
].

"Display the initial state of the game"
Transcript show: 'Initial State of the Game:'. cr.

players do: [ :player |
    Transcript show: player , ' has a score of ' , scores at: (players indexOf: player), ' points.'. cr.
].

"Run the game until someone reaches a score of 100"
[scores max > 99] whileFalse: [
    "Get the player and score for this round"
    player := players atRandom.
    score := Random new randomBetween: 1 and: 10.

    "Update the player's score"
    scores at: (players indexOf: player) put: (scores at: (players indexOf: player)) + score.

    "Display the updated score"
    Transcript show: player , ' scored ' , score , ' points and now has a score of ' , scores at: (players indexOf: player), ' points.'. cr.
].

"Display the final state of the game"
Transcript show: 'Final State of the Game:'. cr.

players do: [ :player |
    Transcript show: player , ' has a score of ' , scores at: (players indexOf: player), ' points.'. cr.
].
```

**Explanation:**

* The code begins by defining the number of players in the game (`numPlayers`) and creates two `OrderedCollection` instances, `players` and `scores`, to store the names of the players and their scores, respectively.


* The `players` and `scores` collections are initialized with the names of the players and initial scores of 0 for each player.


* The program then displays the initial state of the game by printing the names of the players along with their scores to the Transcript window.


* The game loop begins, which runs until one of the players reaches a score of 100.


* In each iteration of the game loop:
    1. A random player is selected (`player`) using `atRandom` and a random score is generated (`score`).
    2. The score of the selected player is updated by adding the random score.
    3. The updated score is displayed in the Transcript window.


* After the game loop, the final state of the game is displayed by printing the names of the players and their final scores to the Transcript window.