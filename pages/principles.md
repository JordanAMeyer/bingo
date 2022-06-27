# Principles of the `bingo` Package

The `bingo` package can be used to run simulations of various bingo configurations to predict, for example, the odds of winning or the number of balls it will take for a bingo to be called. While the base R packages provide excellent functionality for creating "cards" (in the form of matrices) and "calling" numbers (through random sampling), these packages are less conducive to evaluating the many patterns possible in a typical bingo game. Even something as simple as a single straight line has 13 different possible winning patterns. In response to this, the `bingo` package implements a series of functions that make programming these patterns much easier.

## Building Configurations

Configurations created using the `bingo` package as stored in `bingo.config` objects. There are a handful of functions for some of the most common bingo game configurations. For a single straight line game of bingo, creating this object is as simple as:

`x <- bingo.line()`

Of course, most bingo games are more complex than a single straight line and may require a combination of conditions to win. Multiple conditions can be "added" together to make this possible. For example, a configuration that requires a straight line and four corners could be declared as:

`x <- bingo.line() + bingo.corners()`

If multiple types of patterns can win a game (e.g., *either* a straight line *or* four corners counts as a win), this can be expressed as:

`x <- bingo.either(bingo.line(), bingo.corners())`

These either-or combinations can be combined with other patterns. For example, a game that requires a straight line or four corners in combination with a corner cluster ("postage stamp"):

`x <- bingo.either(bingo.line(), bingo.corners()) + bingo.stamp()`

Some configurations may require a more complex set-up involving specific spaces on the card. These individual spaces can be evoked directly through the `bingo.space()` function. For example, a "small I" configuration (top and bottom I, top and bottom G, and the entire N column) could be declared as follows:

`x <- bingo.space("I1") + bingo.space("I5") + bingo.space("G1") + bingo.space("G5") + bingo.line("N")`

Notice that the above also passes an optional argument to the `bingo.line()` function that references a specific column. In short, the `bingo` package offers a relatively simple and flexible syntax for specifying the bingo configurations that it uses in simulations.

## Running Simulations

Simulations are incredibly simple to run using the `bingo` package. The core function allows for specifying the configuration, number of cards in play, and number of simulations to run. Using this information, it returns stats on how many numbers were called on average, how many simultaneous bingos there typically were (i.e., multiple winners), and what the odds are that any given card will win.
