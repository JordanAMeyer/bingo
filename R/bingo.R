`+.bingo.config` <- function(x,
                             y) {
  z <- list()
  for(i in 1:length(x)) {
    for(j in 1:length(y)) {
      if(setequal(x[[i]][[1]], y[[j]][[1]]) &
         setequal(x[[i]][[2]], y[[j]][[2]])) next
      else {
        z1 <- append(x[[i]][[1]],
                     y[[j]][[1]])
        z2 <- append(x[[i]][[2]],
                     y[[i]][[2]])
        z <- append(z,
                    list(list(z1,
                              z2)))
      }
    }
  }
  class(z) <- "bingo.config"
  return(z)
}

# function for combining multiple possible winning patterns
bingo.either <- function(...) {
  x <- list()
  for(i in 1:...length()) {
    x <- append(x,
                ...elt(i))
  }
  class(x) <- "bingo.config"
  return(x)
}

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
  bingo.line1 <- bingo.space("b1") +
    bingo.space("i1") +
    bingo.space("n1") +
    bingo.space("g1") +
    bingo.space("o1")
  bingo.line2 <- bingo.space("b2") +
    bingo.space("i2") +
    bingo.space("n2") +
    bingo.space("g2") +
    bingo.space("o2")
  bingo.line3 <- bingo.space("b3") +
    bingo.space("i3") +
    bingo.space("n3") +
    bingo.space("g3") +
    bingo.space("o3")
  bingo.line4 <- bingo.space("b4") +
    bingo.space("i4") +
    bingo.space("n4") +
    bingo.space("g4") +
    bingo.space("o4")
  bingo.line5 <- bingo.space("b5") +
    bingo.space("i5") +
    bingo.space("n5") +
    bingo.space("g5") +
    bingo.space("o5")
  bingo.lineb <- bingo.space("b1") +
    bingo.space("b2") +
    bingo.space("b3") +
    bingo.space("b4") +
    bingo.space("b5")
  bingo.linei <- bingo.space("i1") +
    bingo.space("i2") +
    bingo.space("i3") +
    bingo.space("i4") +
    bingo.space("i5")
  bingo.linen <- bingo.space("n1") +
    bingo.space("n2") +
    bingo.space("n3") +
    bingo.space("n4") +
    bingo.space("n5")
  bingo.lineg <- bingo.space("g1") +
    bingo.space("g2") +
    bingo.space("g3") +
    bingo.space("g4") +
    bingo.space("g5")
  bingo.lineo <- bingo.space("o1") +
    bingo.space("o2") +
    bingo.space("o3") +
    bingo.space("o4") +
    bingo.space("o5")
  bingo.lined1 <- bingo.space("b1") +
    bingo.space("i2") +
    bingo.space("n3") +
    bingo.space("g4") +
    bingo.space("o5")
  bingo.lined2 <- bingo.space("b5") +
    bingo.space("i4") +
    bingo.space("n3") +
    bingo.space("g2") +
    bingo.space("o1")
  if(type == "row1") x <- bingo.line1
  else if(type == "row2") x <- bingo.line2
  else if(type == "row3") x <- bingo.line3
  else if(type == "row4") x <- bingo.line4
  else if(type == "row5") x <- bingo.line5
  else if(type == "colb") x <- bingo.lineb
  else if(type == "coli") x <- bingo.linei
  else if(type == "coln") x <- bingo.linen
  else if(type == "colg") x <- bingo.lineg
  else if(type == "colo") x <- bingo.lineo
  else if(type == "diag1") x <- bingo.lined1
  else if(type == "diag2") x <- bingo.lined2
  else if(type == "all") x <- bingo.either(bingo.line1,
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

# S3 print method for bingo.config object
print.bingo.config <- function(x) {
  for(i in x) {
    y <- bingo.getcard()
    y$l[3, 3] <- F
    y$n[3, 3] <- "X"
    for(j in 1:length(i[[1]])) {
      y$l[i[[1]][j], i[[2]][j]] <- T
    }
    print(y)
    cat("\n")
  }
}