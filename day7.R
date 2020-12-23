library(tidyverse)

rules <- tibble(rule =readLines("day7-input.txt")) %>%
  separate(rule, sep = " contain ", into = c("bag", "contains"), remove = FALSE) %>%
  mutate(contains = str_remove_all(contains, "(\\.)|(bags)|(bag)"),
         bag = str_remove_all(bag, "(\\.)|(bags)|(bag)"),
         contains_list = str_split(contains, pattern = ", ")) %>%
  unnest_longer(contains_list) %>%
  mutate(contains = str_trim(contains_list)) %>%
  select(-contains_list)

rules1 <- rules %>%
  mutate(contains = str_trim(str_remove_all(contains, pattern = "[0-9] ")),
         bag = str_trim(str_remove_all(bag, pattern = "[0-9] ")))

starting_bag <- "shiny gold"

contains_which <- function(x) {
  unlist(map(x, ~ rules1$bag[rules1$contains == .]))
}

new_bags <- contains_which(starting_bag)
bags <- c()

# Repeat until it converges
while (!identical(bags, new_bags)) {
  bags <- new_bags
  new_bags <- unique(c(bags, contains_which(bags)))
}

length(bags)

# Part 2 ------------------------------------------------------------------

rules2 <- rules %>%
  mutate(bag = str_trim(bag)) %>%
  separate(contains, into = c("n", "contains"), sep = " ", extra = "merge") %>%
  filter(contains != "other")



whats_inside <- function(bags) {
  
  bags <- bags[!is.na(bags)]
  if (length(bags) == 0) return(NULL)
  
  out <- unlist(map(bags, ~ rep(rules2$contains[rules2$bag == .], times = rules2$n[rules2$bag == .])))
  
  list(out)
}

# Setup
bag_count <- list()
bag_count <- append(bag_count, starting_bag)

while (length(unlist(tail(bag_count, 1))) != 0) {
  bag_count <- append(bag_count, whats_inside(tail(bag_count, 1)[[1]]))
}

sum(map_dbl(bag_count, length)) - 1  # Because we don't count the original shiny gold bag
