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
  
  # combine matrices and return
  card <- list(n = x,
               l = y)
  class(card) <- "bingo.card"
  return(card)
}

# S3 print method for bingo.card objects
print.bingo.card <- function(x) {
  x$n[x$l] <- "*"
  dimnames(x$n) <- list(rep("",
                            times = 5),
                        c("B",
                          "I",
                          "N",
                          "G",
                          "O"))
  print(x$n,
        quote = F)
}