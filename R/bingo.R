require(parallel)

`+.bingo.config` <- function(e1,
                             e2) {
  for(i in e1) {
    for(j in e2) {
      i$spaces <- i$spaces[order(i$spaces$x,
                                 i$spaces$y),]
      j$spaces <- j$spaces[order(j$spaces$x,
                                 j$spaces$y),]
      
      if(identical(i$spaces,
                   j$spaces)) next
      else if(!i$interlocking & !j$interlocking &
              nrow(merge(i$spaces,
                         j$spaces)) > 0) next
      
      df <- rbind(i$spaces,
                  j$spaces)
      df <- df[!duplicated(df),]
      
      if(identical(i$spaces, df)) next
      
      df <- df[order(df$x,
                     df$y),]
      rownames(df) <- 1:nrow(df)
      
      interlocking <- i$interlocking | j$interlocking
      
      if(!exists("x",
                 inherits = F)) x <- list(list(spaces = df,
                                               interlocking = interlocking))
      else x <- append(x,
                       list(list(spaces = df,
                                 interlocking = interlocking)))
    }
  }
  x <- x[!duplicated(x)]
  class(x) <- "bingo.config"
  return(x)
}

`*.bingo.config` <- function(x,
                             y) {
  type.error <- "bingo.config objects can only be multiplied by a whole number"
  if((!is.numeric(x) & !is.numeric(y))) stop(type.error)
  else if(is.numeric(x)) {
    if(x %% 1 != 0) stop(type.error)
    num <- x
    config <- y
  }
  else if(is.numeric(y)) {
    if(y %% 1 != 0) stop(type.error)
    config <- x
    num <- y
  }
  
  z <- config
  for(i in 1:(num - 1)) {
    z <- z + config
  }
  return(z)
}

bingo <- function(n,
                  config,
                  player.cards = 12,
                  reps = 1000,
                  cores = 1,
                  odd.even = F) {
  run <- function(rep.n,
                  n,
                  config,
                  odd.even) {
    source("R/bingo.R")
    x <- bingo.simulate(n = n,
                        config = config,
                        odd.even = odd.even)
    return(x)
  }
  cl <- makeCluster(cores)
  x <- clusterApplyLB(cl,
                      1:reps,
                      run,
                      n = n,
                      config = config,
                      odd.even = odd.even)
  stopCluster(cl)
  n.balls <- c()
  winners <- c()
  player.wins <- c()
  for(i in x) {
    n.balls <- append(n.balls,
                      i$n.balls)
    winners <- append(winners,
                      i$winners)
    player.wins <- append(player.wins,
                          T %in% i$winners.l[1:player.cards])
  }
  y <- list(n.balls = n.balls,
            winners = winners,
            player.wins = player.wins)
  class(y) <- "bingo.sim"
  return(y)
}

# simulate a single bingo game
bingo.simulate <- function(n,
                           config,
                           odd.even = F) {
  cards <- list()
  for(i in 1:n) {
    cards[[i]] <- card.get()
  }
  balls <- sample(1:75,
                  75)
  if(odd.even & balls[1] %% 2 != 0) {
    balls.odd <- balls[balls %% 2 != 0]
    for(i in balls.odd) {
      cards <- lapply(cards,
                      function(x) card.mark(i,
                                            x))
    }
    balls <- balls[balls %% 2 == 0]
  }
  else if(odd.even & balls[1] %% 2 == 0) {
    balls.even <- balls[balls %% 2 == 0]
    for(i in balls.even) {
      cards <- lapply(cards,
                      function(x) card.mark(i,
                                            x))
    }
    balls <- balls[balls %% 2 != 0]
  }
  for(i in 1:length(balls)) {
    cards <- lapply(cards,
                    function(x) card.mark(balls[i],
                                          x))
    winners <- sapply(cards,
                      function(x) card.check(x,
                                             config))
    if(sum(winners) > 0) {
      break
    }
  }
  z <- list(n.balls = i,
            winners = sum(winners),
            winners.l = winners)
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
  
  z <- list(list(spaces = data.frame(x = x,
                                     y = y),
                 interlocking = T))
  class(z) <- "bingo.config"
  return(z)
  
}

# check whether a card matches a configuration
card.check <- function(card,
                       config) {
  for(i in config) {
    for(j in 1:nrow(i$spaces)) {
      if(!card$l[i$spaces$x[j],i$spaces$y[j]]) break
      
      if(j == nrow(i$spaces)) return(T)
    }
  }
  return(F)
}

# create a bingo.card object
card.get <- function() {
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

# mark a number on a card
card.mark <- function(x, card) {
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

# function for combining multiple possible winning patterns
either <- function(...) {
  x <- list()
  for(i in 1:...length()) {
    x <- append(x,
                ...elt(i))
  }
  class(x) <- "bingo.config"
  return(x)
}

# cluster configuration
pattern.cluster <- function(type = "any") {
  if(type == "any") {
    either(bingo.space("b1") + bingo.space("i1") + bingo.space("b2") +
             bingo.space("i2"),
           bingo.space("b2") + bingo.space("i2") + bingo.space("b3") +
             bingo.space("i3"),
           bingo.space("b3") + bingo.space("i3") + bingo.space("b4") +
             bingo.space("i4"),
           bingo.space("b4") + bingo.space("i4") + bingo.space("b5") +
             bingo.space("i5"),
           bingo.space("i1") + bingo.space("n1") + bingo.space("i2") +
             bingo.space("n2"),
           bingo.space("i2") + bingo.space("n2") + bingo.space("i3") +
             bingo.space("n3"),
           bingo.space("i3") + bingo.space("n3") + bingo.space("i4") +
             bingo.space("n4"),
           bingo.space("i4") + bingo.space("n4") + bingo.space("i5") +
             bingo.space("n5"),
           bingo.space("n1") + bingo.space("g1") + bingo.space("n2") +
             bingo.space("g2"),
           bingo.space("n2") + bingo.space("g2") + bingo.space("n3") +
             bingo.space("g3"),
           bingo.space("n3") + bingo.space("g3") + bingo.space("n4") +
             bingo.space("g4"),
           bingo.space("n4") + bingo.space("g4") + bingo.space("n5") +
             bingo.space("g5"),
           bingo.space("g1") + bingo.space("o1") + bingo.space("g2") +
             bingo.space("o2"),
           bingo.space("g2") + bingo.space("o2") + bingo.space("g3") +
             bingo.space("o3"),
           bingo.space("g3") + bingo.space("o3") + bingo.space("g4") +
             bingo.space("o4"),
           bingo.space("g4") + bingo.space("o4") + bingo.space("g5") +
             bingo.space("o5"))
  } else if(type == "corner") {
    either(bingo.space("b1") + bingo.space("b2") + bingo.space("i1") +
             bingo.space("i2"),
           bingo.space("b4") + bingo.space("b5") + bingo.space("i4") +
             bingo.space("i5"),
           bingo.space("g1") + bingo.space("g2") + bingo.space("o1") +
             bingo.space("o2"),
           bingo.space("g4") + bingo.space("g5") + bingo.space("o4") +
             bingo.space("o5"))
  } else stop("type not recognized")
}

pattern.corners <- function() {
  return(bingo.space("b1") + bingo.space("b5") + bingo.space("o1") +
           bingo.space("o5"))
}

pattern.coverall <- function() {
  return(pattern.line("colb") + pattern.line("coli") + pattern.line("coln") +
           pattern.line("colg") + pattern.line("colo"))
}

# picture frame configuration
pattern.frame <- function(type = "large") {
  if(type == "large") {
    x <- bingo.space("b1") + bingo.space("i1") + bingo.space("n1") +
      bingo.space("g1") + bingo.space("o1") + bingo.space("o2") +
      bingo.space("o3") + bingo.space("o4") + bingo.space("o5") +
      bingo.space("g5") + bingo.space("n5") + bingo.space("i5") +
      bingo.space("b5") + bingo.space("b4") + bingo.space("b3") +
      bingo.space("b2")
    return(x)
  }
  else if(type == "small") {
    x <- bingo.space("i2") + bingo.space("n2") + bingo.space("g2") +
      bingo.space("g3") + bingo.space("g4") + bingo.space("n4") +
      bingo.space("i4") + bingo.space("i3")
    return(x)
  }
  else stop("frame type not recognized")
}

# straight line configuration
pattern.line <- function(type = "all") {
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
  else if(type == "all") x <- either(bingo.line1,
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

no.interlock <- function(x) {
  if(class(x) != "bingo.config") stop("x must be a bingo.config object")
  
  for(i in x) {
    if(!exists("y")) y <- list(list(spaces = i$spaces,
                                    interlocking = F))
    else y <- append(y,
                     list(list(spaces = i$spaces,
                               interlocking = F)))
  }
  class(y) <- "bingo.config"
  return(y)
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
    y <- card.get()
    y$l[3, 3] <- F
    y$n[3, 3] <- "X"
    for(j in 1:nrow(i$spaces)) {
      y$l[i$spaces$x[j], i$spaces$y[j]] <- T
    }
    print(y)
    cat("\n")
  }
}

print.bingo.sim <- function(x) {
  cat("\nMean Number of Balls until Win:",
      mean(x$n.balls),
      "\nMean Number of Winners per Game:",
      mean(x$winners),
      "\n% Player Wins:",
      round(mean(x$player.wins) * 100,
            1))
}