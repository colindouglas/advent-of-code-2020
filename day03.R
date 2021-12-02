library(tidyverse)

# Read in data, turn into matrix
# Trees are 1
tree_mat <- readLines("day3-input.txt") %>%
  str_replace_all("\\.", "0") %>%
  str_replace_all("#", "1") %>%
  str_split(pattern = "") %>%
  map(as.numeric) %>%
  sapply(., cbind) %>% 
  t()


check_slope <- function(slope) {
  
  start <- c(1, 1)
  total_trees <- 0
  pos <- start
  
  for (i in 1:nrow(tree_mat)) {
    pos <- pos + slope
    if (pos[1] > nrow(tree_mat)) break
    pos[2] <- ((pos[2]-1) %% ncol(tree_mat)) + 1  # Rollover when you're pass the x limit
    total_trees <- total_trees + tree_mat[pos[1], pos[2]]
  }
  
  return(total_trees)
}


# Part 1 ------------------------------------------------------------------

check_slope(c(1, 3))


# Part 2 ------------------------------------------------------------------


# Right 1, down 1.
check_slope(c(1, 1)) *
# Right 3, down 1. (This is the slope you already checked.)
check_slope(c(1, 3)) *
# Right 5, down 1.
check_slope(c(1, 5)) *
# Right 7, down 1.
check_slope(c(1, 7)) *
# Right 1, down 2.
check_slope(c(2, 1))

