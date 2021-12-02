# library(tidyverse)
# input <- "389547612"
# #input <- "389125467" # Validation
# 
# circle <- str_split(input, pattern = "") %>% unlist() %>% as.numeric()
# 
# play_game <- function(n) {
#   
#   
#   # Before the crab starts, it will designate the first cup in your list as the current cup. The crab is then going to do 100 moves.
#   current_cup <- circle[1]
#   current_pos <- 1
#   move <- 1
#   
#   
#   for (nothing in 1:n) {
#     ## START LOOP
#     circle_print <- paste(circle, collapse = " ")
#     circle_print <- str_replace(circle, pattern = as.character(current_cup), replacement = "(\\0)")
#     message("start of move ", move, " // cups ", circle_print)
#     # Each move, the crab does the following actions:
#     
#     # The crab picks up the three cups that are immediately clockwise of the current cup. 
#     # They are removed from the circle; cup spacing is adjusted as necessary to maintain the circle.
#     
#     pickup_index <- (current_pos + 1:3 - 1) %% length(circle) + 1
#     
#     cups_pickedup <- circle[pickup_index]
#     circle <- circle[-pickup_index]
#     message("move ", move, " // picked up ", cups_pickedup)
#     
#     # The crab selects a destination cup: the cup with a label equal to the current cup's label minus one. 
#     # If this would select one of the cups that was just picked up, the crab will keep subtracting one until 
#     # it finds a cup that wasn't just picked up.
#     dest_cup <- suppressWarnings(max(circle[circle < current_cup])) # Suppress Inf warning
#     # If at any point in this process the value goes below the lowest  value on any cup's label, it 
#     # wraps around to the highest value on any cup's label instead.
#     if ((length(dest_cup) == 0) | is.infinite(dest_cup)) {
#       dest_cup <- max(circle)
#     }
#     message("move ", move, " // destination ", dest_cup)
#     dest_pos <- which(circle == dest_cup)
#     
#     # The crab places the cups it just picked up so that they are immediately clockwise of the destination cup. 
#     # They keep the same order as when they were picked up.
#     move <- move + 1
#     circle <- append(circle, cups_pickedup, after = dest_pos)
#     
#     
#     # The crab selects a new current cup: the cup which is immediately clockwise of the current cup.
#     current_pos <- (which(circle == current_cup) %% length(circle)) + 1
#     current_cup <- circle[current_pos]
#     
#     circle_print <- paste(circle, collapse = " ")
#     circle_print <- str_replace(circle, pattern = as.character(current_cup), replacement = "(\\0)")
#     message("end of move ", move -1, " // cups ", circle_print)
#   }
#   start_position <- which(circle == 1)
#   # 6 7 8 9 1 2 3 4 5
#   paste(
#     circle[((start_position):(start_position + length(circle) - 2) %% length(circle))+1],
#     collapse = ""
#   )
# }
# 
# play_game(100)


# Part 2 ------------------------------------------------------------------

# We have to re-write this in such a way that we don't need to rewrite the whole vector
# when we re-arrange the circle

# Therefore, let's make a vector called "next_cup" where
# next_cup[x] is the cup after 'x'

library(tidyverse)
input <- "389547612"
#input <- "389125467" # Validation

# Make the circle of cups
circle <- as.integer(unlist(str_split(input, pattern = "")))
total_cups <- 1e6  # Add on the extra part 2 cups
circle <- c(circle, (length(circle) + 1):total_cups)

# Create the next_cup vector ahead of time at the right length (because it's huge)
next_cup <- vector(mode = "integer", length = length(circle))

for (i in seq_along(circle)) {
  next_cup[circle[i]] <- as.integer(circle[i + 1])
}

next_cup[tail(circle, 1)] <- circle[1]  # Loop around to the start


play_game <- function(n) {
  
  # Before the crab starts, it will designate the first cup in your list as the current cup. 
  current_cup <- circle[1]
  
  
  ## START LOOP
  for (nothing in seq_len(n)) {
    if (nothing %% 10000 == 0) {
      tictoc::toc()
      tictoc::tic(paste("Iteration", nothing))
    }
    # Each move, the crab does the following actions:
    
    # The crab picks up the three cups that are immediately clockwise of the current cup. 
    # They are removed from the circle; cup spacing is adjusted as necessary to maintain the circle.
    cup1 <- next_cup[current_cup]
    cup2 <- next_cup[cup1]
    cup3 <- next_cup[cup2]
    cup4 <- next_cup[cup3]
    
    # The crab selects a destination cup: the cup with a label equal to the current cup's label minus one. 
    # If this would select one of the cups that was just picked up, the crab will keep subtracting one until 
    # it finds a cup that wasn't just picked up.
    
    dest_cup <- current_cup - 1L
    
    # If at any point in this process the value goes below the lowest  value on any cup's label, it 
    # wraps around to the highest value on any cup's label instead.
    #i <- 0
    while(dest_cup %in% c(cup1, cup2, cup3) | (dest_cup < 1)) {
      #i <- i + 1
      #message("While loop")
      #if (i > 1000) break
      dest_cup <- dest_cup - 1L
      if (dest_cup < 1) dest_cup <- length(next_cup)
    }
    
    # The crab places the cups it just picked up so that they are immediately clockwise of the destination cup. 
    # They keep the same order as when they were picked up.
    dest_cup_other_side <- next_cup[dest_cup]
    
    # Skip over triplet
    next_cup[current_cup] <- cup4
    
    # Insert triple
    next_cup[dest_cup] <- cup1
    next_cup[cup3] <- dest_cup_other_side
    
    # The crab selects a new current cup: the cup which is immediately clockwise of the current cup.
    current_cup <- next_cup[current_cup]

  }


  return(next_cup[1] * next_cup[next_cup[1]])
}

tictoc::tic("All of it")
out <- play_game(10000000)
tictoc::toc()
out
