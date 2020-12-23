library(tidyverse)

tictoc::tic("Part 1")
numbers <- as.numeric(readLines("day1-input.txt"))

numbers_1 <- crossing(x = numbers, y = numbers)

sums <- numbers_1 %>%
  mutate(sum = x + y,
         products = x * y) %>%
  filter(sum == 2020)
tictoc::toc()


# Part 2 ------------------------------------------------------------------

tictoc::tic("Part 2")
numbers_2 <- crossing(x = numbers, y = numbers, z = numbers)

sums <- numbers_2 %>%
  mutate(sum = x + y +z,
         products = x * y * z) %>%
  filter(sum == 2020)
tictoc::toc()
