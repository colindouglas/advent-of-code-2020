library(tidyverse)

tictoc::tic("Part 1")
input <- as.numeric(readLines("day10-input.txt"))

input <- c(0, sort(input), max(input) + 3)

out <- input[order(input)] %>% diff %>% table()

out[1] * out[2]
tictoc::toc()

# Part 2 but using igraph -------------------------------------------------

nodes <- tidyr::crossing(x = input, y = input) %>%
  filter((y - x) %in% 1:3)

graph <- igraph::graph_from_data_frame(nodes, directed = TRUE)

end <- as.character(max(input))

# Run out of memory calculating this
#paths <- all_simple_paths(graph, from = "0", to = to_vert)



# Part 2 but counting streaks ---------------------------------------------

tictoc::tic("All of part 2")
# Find how many 1's are between each 3
streaks_of_ones <- diff(which(c(3, diff(input)) == 3)) - 2

combos <- case_when(
  # A[111]B can be navigated 7 different ways
  streaks_of_ones == 3 ~ 7,  
  # A[1__]B
  # A[_1_]B
  # A[__1]B
  # A[11_]B
  # A[1_1]B
  # A[_11]B
  # A[111]B
  
  # A[11]B can be navigated 4 different ways
  streaks_of_ones == 2 ~ 4, 
  # A[__]B
  # A[1_]B
  # A[_1]B
  # A[11]B
   
  # A[1]B can be navigated 2 different ways 
  streaks_of_ones == 1 ~ 2, 
  # A[_]B
  # A[1]B
 
  # Everything else does nothing, so multiply by identity
  TRUE ~ 1)

sprintf("%.0f", prod(combos))
tictoc::toc()
