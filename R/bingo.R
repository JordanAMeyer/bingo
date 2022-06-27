# create a bingo.card object
bingo.getcard <- function() {
  ## numeric card
  x <- matrix(c(sample(1:15,
                       5),
                sample(16:30,
                       5),
                sample(31:45,
                       5),
                sample(46:60,
                       5),
                sample(61:75,
                       5)),
              5,
              5)
  x[3, 3] <- NA
  
  ## logical card
  y <- matrix(F,
              5,
              5)
  y[3, 3] <- T
  
  ## visual card
  z <- x
  
  card <- list(n = x,
               l = y,
               v = z)
  class(card) <- "bingo.card"
  
  return(card)
}