# Tennis Kata

This Kata is about implementing a simple tennis game. I came up with it while thinking about Wii tennis, where they have simplified tennis, so each set is one game.

The scoring system is rather simple:

    Each player can have either of these points in one game “love” “15” “30” “40”
    If you have 40 and you win the point you win the game, however there are special rules.
    If both have 40 the players are “deuce”.
    If the game is in deuce, the winner of a point will have advantage
    If the player with advantage wins the ball he wins the game
    If the player without advantage wins they are back at deuce.

## Development
1. Install [stack](https://docs.haskellstack.org/en/stable/README/)
1. Install [ghcid]: `$ stack install ghcid`
1. Run the compiler and tests on watch: `$ ghcid --command="stack ghci test/Main.hs" --test=main`