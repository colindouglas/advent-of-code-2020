library(tidyverse)

trans_sn <- function(x, loop = 1) {
  value <- 1
  for (nothing in seq_len(loop)) {
    # Set the value to itself multiplied by the subject number.
    value <- value * x
    
    # Set the value to the remainder after dividing the value by 20201227.
    value <- value %% 20201227
  }
  value
}

trans_sn(7, loop = 8)  # 5764801
trans_sn(7, loop = 11)  # 17807724

trans_sn(17807724, loop = 8) == trans_sn(5764801, loop = 11)


# Part 1 ------------------------------------------------------------------

input <- as.numeric(read_lines("day25-input.txt"))

crack_loops <- function(subject_number, public_key) {
  
  loops <- 0
  
  # To transform a subject number, start with the value 1.
  value <- 1
  while (value != public_key) {
    loops <- loops + 1
    # Set the value to itself multiplied by the subject number.
    value <- value * subject_number
    
    # Set the value to the remainder after dividing the value by 20201227.
    value <- value %% 20201227
    
    if (loops %% 100000 == 0) message(loops)
  }
  loops
}

card_loops <- crack_loops(7, input[[1]])
door_loops <- crack_loops(7, input[[2]])

trans_sn(trans_sn(7, card_loops), door_loops)
