bingo <- function(n,
                  config,
                  reps = 1000) {
  n.balls <- c()
  winners <- c()
  for(i in 1:reps) {
    game <- bingo.simulate(n,
                           config)
    n.balls <- append(n.balls,
                      game$n.balls)
    winners <- append(winners,
                      game$winners)
  }
  cat(mean(n.balls),
      "Balls on Average with",
      mean(winners),
      "winners.\n")
}

# check whether a card matches a configuration
bingo.checkcard <- function(card,
                            config) {
  for(i in config) {
    for(j in 1:length(i[[1]])) {
      if(!card$l[i[[1]][j],i[[2]][j]]) break
      if(j == length(i[[1]])) return(T)
    }
  }
  return(F)
}

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
  x[3, 3] <- 0
  
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

# straight line configuration
bingo.line <- function(type = "all") {
  bingo.line1 <- list(rep(1,
                          5),
                      1:5)
  bingo.line2 <- list(rep(2,
                          5),
                      1:5)
  bingo.line3 <- list(rep(3,
                          5),
                      1:5)
  bingo.line4 <- list(rep(4,
                          5),
                      1:5)
  bingo.line5 <- list(rep(5,
                          5),
                      1:5)
  bingo.lineb <- list(1:5,
                      rep(1,
                          5))
  bingo.linei <- list(1:5,
                      rep(2,
                          5))
  bingo.linen <- list(1:5,
                      rep(3,
                          5))
  bingo.lineg <- list(1:5,
                      rep(4,
                          5))
  bingo.lineo <- list(1:5,
                      rep(5,
                          5))
  bingo.lined1 <- list(1:5,
                       1:5)
  bingo.lined2 <- list(5:1,
                       1:5)
  if(type == "all") {
    x <- list(bingo.line1,
              bingo.line2,
              bingo.line3,
              bingo.line4,
              bingo.line5,
              bingo.lineb,
              bingo.linei,
              bingo.linen,
              bingo.lineg,
              bingo.lineo,
              bingo.lined1,
              bingo.lined2)
  }
  class(x) <- "bingo.config"
  return(x)
}

# mark a number on a card
bingo.mark <- function(x, card) {
  if(x %in% 1:15) z <- 1
  else if(x %in% 16:30) z <- 2
  else if(x %in% 31:45) z <- 3
  else if(x %in% 46:60) z <- 4
  else if(x %in% 61:75) z <- 5
  else stop("x is not a valid bingo number")
  
  for(y in 1:5) {
    if(card$n[y, z] == x) {
      card$l[y, z] <- T
      break
    }
  }
  
  return(card)
}

# simulate a single bingo game
bingo.simulate <- function(n,
                           config) {
  cards <- list()
  for(i in 1:n) {
    cards[[i]] <- bingo.getcard()
  }
  balls <- sample(1:75,
                  75)
  for(i in 1:length(balls)) {
    cards <- lapply(cards,
                    function(x) bingo.mark(balls[i],
                                           x))
    winners <- sapply(cards,
                      function(x) bingo.checkcard(x,
                                                  config))
    if(sum(winners) > 0) {
      break
    }
  }
  z <- list(n.balls = i,
            winners = sum(winners),
            winners.l = winners,
            cards = cards)
  class(z) <- "bingo.game"
  return(z)
}

# create a bingo.config object referencing a single space
bingo.space <- function(space) {
  x <- as.numeric(substr(space,
                         2,
                         2))
  
  y.char <- substr(space,
                   1,
                   1)
  if(y.char == "b") y <- 1
  else if(y.char == "i") y <- 2
  else if(y.char == "n") y <- 3
  else if(y.char == "g") y <- 4
  else if(y.char == "o") y <- 5
  
  z <- list(list(c(x),
                 c(y)))
  class(z) <- "bingo.config"
  return(z)
  
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