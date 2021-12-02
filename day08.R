library(tidyverse)

ins <- tibble(ins = readLines("day8-input.txt")) %>%
  separate(ins, into = c("op", "mag"), sep = " ") %>%
  mutate(mag = as.numeric(mag))

flip_ins <- function(x) {
  
  if (ins$op[x] == "acc") return(NULL)
  
  # For flipping a single instruction
  if (ins$op[x] == "jmp") { 
    ins$op[x] <- "nop"
  } else if ((ins$op[x] == "nop")) {
    ins$op[x] <- "jmp"
  }
  
  acc <- 0
  i <- 1
  vis <- c()
  
  # Start loop
  while (TRUE) {
    # If we're finished of instructions, return the accumulator value
    if (i > nrow(ins)) return(acc)
    
    # If the instruction is 'acc', modify the accumulator and move to the next row
    # If we've already visited this row, the program is looping and not useful, return NULL
    if (ins$op[i] == "acc") {
      if (i %in% vis) return(NULL)
      vis <- c(vis, i)
      acc <- acc + ins$mag[i]
      i <- i + 1
      
      # If the instruction is "jmp", modify the locator and move to that row
    } else if (ins$op[i] == "jmp") {
      if (i %in% vis) return(NULL)
      vis <- c(vis, i)
      i <- i + ins$mag[i]
      
      # If the instruction is "nop", we don't do anything, move on to the next row
    } else if (ins$op[i] == "nop") {
      if (i %in% vis) return(NULL)
      vis <- c(vis, i)
      i <- i + 1
    } else {
      # Something is broken
      message("huh?")
    }
  }
  return(acc)
}

out <- unlist(map(1:nrow(ins), flip_ins))
out
