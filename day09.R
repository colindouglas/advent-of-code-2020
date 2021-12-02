library(tidyverse)

input <- as.numeric(readLines("day9-validation.txt")); preamb <- 5 # Validation
input <- as.numeric(readLines("day9-input.txt")); preamb <- 25 # Test


# Generate all of the combinations of the preamble


check_valid <- function(v) {
  
  o <- vector(mode = "logical", length = length(v))
  o[1:preamb] <- TRUE
  
  for (i in (preamb + 1):length(v)) {
    
    combos <- input[(i-preamb):(i-1)] %>%
      expand.grid(., .)
    
    # Find their sums
    sums <- combos[,1] + combos[,2]
    
    # Take out entries where it's the same number twice
    sums <- sums[-seq(1, length(sums), by = preamb+1)]
    
    # Check if the number is in the sums
    o[i] <- v[i] %in% sums
  }
  v[!o]
}

check_valid(input)


# Part 2 ------------------------------------------------------------------

target <- check_valid(input)

checks <- crossing(
  start = 1:length(input),
  end = 1:length(input)) %>% 
  filter(start < end) %>%
  rowwise() %>%
  mutate(range_sum = sum(input[start:end])) %>%
  filter(range_sum == target)

range <- input[checks$start:checks$end]

min(range) + max(range)

#1068 too low