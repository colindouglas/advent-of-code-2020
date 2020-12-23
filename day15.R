library(tidyverse)
library(progress)

input <- "0,6,1,7,2,19,20"
#input <- "0,3,6" # Validation

input <- str_split(input, pattern = ",") %>% unlist() %>% as.numeric()

next_number <- function(v) {

  last_number <- tail(v, 1)
  if (!(last_number %in% head(v, length(v) - 1))) {
    return(0)
  }
  diff(tail(which(v == last_number), 2))
  
}

add_next_number <- function(v) {
  c(v, next_number(v))
}

add_n_number <- function(v, n) {
  i <- 0
  pb <- progress_bar$new(total = n)
  for (nothing in 1:n) {
    i <- i + 1
    pb$tick()
    if ((i %% 10000) == 0) message(i, " (", round(i/n * 100, 1), "%)")
    v <- c(v, next_number(v))
  }
  v
}
  


part1 <- add_n_number(input, 2020)
part1[2020]


# Part 2 ------------------------------------------------------------------

# Part 1 method takes too long
# Needs to be more efficient
# Profiling:
#   - Progress bar takes too much time
#   - %in% takes too much time
#   - which() and diff() also fairly slow

get_n_number <- function(v, n) {
  # v <- input; n <- 2020
  n <- n -1 # Because the output of the function is the _next_ number
  
  # For each number, make a vector that holds the position of the last time we used it
  # Init this all at the start so there's no issues/time with memory allocation
  last_seen <- vector(mode = "double", length = n) # If it's 0, we haven't seen it
  
  # Fill in the values for the starting positions
  # Last seen is indexed from 1
  # Therefore last_seen[1] is the last time we saw 0
  # last_seen[20] is the last time we saw 21
  # last seen(n) is the last time we saw n+1
  
  # We do this for the starting set
  for (number in input[1:(length(input)-1)]) {
    last_seen[number +1] <- max(which(input == number))
  }
  
  # The last number we've seen, starting off the loop
  last_number_seen <- tail(input, 1)
  
  for (i in (length(v)):n) {
    # When we did last seen this number
    when_last_seen <- last_seen[last_number_seen + 1]
    
    # If we last saw this number at '0', that means we haven't seen it
    # We update the last_seen vector with the current iteration at this index
    # Then the next number is 0
    if(when_last_seen == 0) {
      last_seen[last_number_seen + 1] <- i
      last_number_seen <- 0
      
      # If we have seen this before, record the last time we saw the number
      # Update the last seen to say that we last saw the number this round
      # And figure out the next number via subtraction
    } else {
      last_time_we_saw_number <- last_seen[last_number_seen + 1]
      last_seen[last_number_seen + 1] <- i
      last_number_seen <- i - last_time_we_saw_number
    }
  }
  last_number_seen # return
}

get_n_number(input, 2020) == 706


tictoc::tic()
get_n_number(input, 30000000)
tictoc::toc()

