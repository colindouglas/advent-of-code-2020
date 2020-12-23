library(tidyverse)

input <- read_lines("day22-input.txt")

hand_starts <- which(str_detect(input, "Player "))

hand_p1 <- as.numeric(input[(hand_starts[1] + 1):(hand_starts[2] - 2)])
hand_p2 <-  as.numeric(input[(hand_starts[2] + 1):length(input)])



play_hand <- function(state) {
  
  if (length(state$p1_hand) == 0 | length(state$p2_hand) == 0) {
    message("GAME IS OVER")
    return(state)
  }
  
  p1_card <- state$p1_hand[1]
  if (length(state$p1_hand) < 2) {
    state$p1_hand <- c()
  } else {
    state$p1_hand <- state$p1_hand[2:length(state$p1_hand)]
  }
  
  p2_card <- state$p2_hand[1]
  if (length(state$p2_hand) < 2) {
    state$p2_hand <- c()
  } else {
    state$p2_hand <- state$p2_hand[2:length(state$p2_hand)]
  }
  
  if (p1_card > p2_card) {
    #message("P1 wins: ", p1_card, " ", p2_card)
    state$p1_hand <- c(state$p1_hand, p1_card, p2_card)
  }
  if (p1_card < p2_card) {
    #message("P2 wins: ", p1_card, " ", p2_card)
    state$p2_hand <- c(state$p2_hand, p2_card, p1_card)
  }
  if (p1_card == p2_card) {
    message("TIE??")
  }
  
  return(state)
  
}

state <- list(p1_hand = hand_p1,
              p2_hand = hand_p2)

play_game <- function(state) {
  
  old_state <- list()
  i <- 0
  while(!identical(state, old_state)) {
    i <- i + 1
    old_state <- state
    state <- play_hand(state)
  }
  state
}

state <- play_game(state)

sum(state$p1_hand * length(state$p1_hand):1) + sum(state$p2_hand * length(state$p2_hand):1)



# Part 2 ------------------------------------------------------------------

library(tidyverse)

input <- read_lines("day22-validation.txt")

hand_starts <- which(str_detect(input, "Player "))

hand_p1 <- as.numeric(input[(hand_starts[1] + 1):(hand_starts[2] - 2)])
hand_p2 <-  as.numeric(input[(hand_starts[2] + 1):length(input)])

# Define the starting state

state <- list(p1_hand = hand_p1,
              p2_hand = hand_p2,
              visited = c())
level <- 0

play_hand <- function(state) {

  hand_order <- paste(c(state$p1_hand, 0, state$p2_hand), collapse = ",")
  if (hand_order %in% state$visited) {
    message("P1 WINS BY LOOP")
    state$winner <- "p1"
    level <<- level - 1
    return(state)
  } else {
    state$visited <- c(state$visited, hand_order)
  } 
  
  
  # If anyone's hand is empty, we've got a winner
  if (length(state$p1_hand) == 0) {
    level <<- level - 1
    state$winner <- "p2"
    message("P2 WIN (Level ", level, ", states:", length(state$visited), ")")
    return(state)
  } else if (length(state$p2_hand) == 0) {
    level <<- level - 1
    state$winner <- "p1"
    message("P1 WIN (Level ", level, ", states:", length(state$visited), ")")
    return(state)
  }
  
  
  # If there are no empty hands, deal the next cards
  # And reduce the hands
  p1_card <- state$p1_hand[1]
  if (length(state$p1_hand) < 2) {
    state$p1_hand <- c()
  } else {
    state$p1_hand <- state$p1_hand[2:length(state$p1_hand)]
  }
  
  p2_card <- state$p2_hand[1]
  if (length(state$p2_hand) < 2) {
    state$p2_hand <- c()
  } else {
    state$p2_hand <- state$p2_hand[2:length(state$p2_hand)]
  }
  
  # Check if recursion criteria are met
  
  # If both players have at least as many cards remaining in their deck as the 
  # value of the card they just drew, the winner of the round is determined by 
  # playing a new game of Recursive Combat (see below).
  
  if ((p1_card <= length(state$p1_hand)) & (p2_card <= length(state$p2_hand))) {
    level <<- level + 1
    message("RECURSION (Level ", level, ", states:", length(state$visited), ")")

    sub_state <- list(
      p1_hand = state$p1_hand[1:p1_card],
      p2_hand = state$p2_hand[1:p2_card],
      visited = list(c())
    )
    sub_state <- play_game(sub_state)
    winner <- sub_state$winner
  } else {
    
    # Check for winner
    if (p1_card > p2_card) {
      winner <- "p1"
    } else
    if (p1_card < p2_card) {
      winner <- "p2"
    } else
    if (p1_card == p2_card) {
      message("TIE??")
    }
  }
  
  if (winner == "p1") {
    state$p1_hand <- c(state$p1_hand, p1_card, p2_card)
  } else if (winner == "p2") {
    state$p2_hand <- c(state$p2_hand, p2_card, p1_card)
  }
  
  return(state)
}


play_game <- function(state) {
  
  old_state <- list()
  while(!identical(state, old_state)) {
    old_state <- state
    state <- play_hand(state)
  }
  
  if (length(state$p1_hand) == 0) {
    state$winner <- "p2"
  } else if (length(state$p2_hand) == 0) {
    state$winner <- "p1"
  } else {
    state$winner <- "p1"
  }
  return(state)
  
}

state <- play_game(state)

if (state$winner == "p1") {
  o <- sum(state$p1_hand * length(state$p1_hand):1)
} else if (state$winner == "p2") {
  o <- sum(state$p2_hand * length(state$p2_hand):1)
} else {
  message("WHO WON?!")
}

print(o)

# too low 16092
# 10375 haven't tried
# too high 35661