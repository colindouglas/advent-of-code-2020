library(tidyverse)

bps <- tibble(
  ins = readLines("day5-input.txt")
) %>%
  mutate(backfront = substring(ins, first = 1, last = 7),
         leftright = substring(ins, first = 8, last = 10))

backfront_total <- 127 # 128 rows total (index from 0)
leftright_total <- 7 # 8 columns total (index from 0)

bin_part <- function(x, nrow) {

  range <- 0:nrow # Index from 0 like a monster
  
  x <- str_split(x, pattern = "", simplify = TRUE) # Split into vector of single chars
  
  for (dir in x) {
    # "Front" and "Left" take the lower half
    if (dir == "F" | dir == "L") {
      range <- range[1:(length(range)/2)]
      next
    }
    
    # "Back" and "Right" take the upper half
    if (dir == "B" | dir == "R") {
      range <- range[((length(range)/2)+1):length(range)]
      next
    }
  }
  range
}

bps_part1 <- bps %>%
  rowwise() %>%
  mutate(row = bin_part(backfront, backfront_total),
         column = bin_part(leftright, leftright_total),
         seat_id = row * 8 + column)

max(bps_part1$seat_id)


# Part 2 ------------------------------------------------------------------

bottom <- min(bps_part1$seat_id) 
top <- max(bps_part1$seat_id)

setdiff(bottom:top, bps_part1$seat_id)
