library(tidyverse)

input <- read_lines("day16-input.txt")


# Parsing the rules -------------------------------------------------------
vec_to_ranges <- function(v) {
  stopifnot(length(v) %% 2 == 0)
  
  starts <- v[seq(1, length(v), by = 2)]
  stops <- v[seq(2, length(v), by = 2)]
  
  unlist(map2(starts, stops, ~.x:.y))
}

rules <- input[str_detect(input, "[a-z ]:")]
rules <- rules[!str_detect(rules, "(your ticket)|(nearby tickets)")]
rules <- str_split(rules, pattern = ": ")

rules_names <- map_chr(rules, ~ .[[1]])

rules_ranges <- map_chr(rules, ~ .[[2]])
rules_ranges <- str_split(rules_ranges, pattern = "( or )|(\\-)")
rules_ranges <- map(rules_ranges, ~ as.numeric(.))
rules_ranges <- map(rules_ranges, ~ vec_to_ranges(.))
names(rules_ranges) <- rules_names

# Parsing the nearby tickets ----------------------------------------------

nb_ticks <- which(str_detect(input, "(nearby tickets)"))
nb_ticks <- input[(nb_ticks+1):length(input)] %>%
  str_split(pattern = ",") %>%
  map(as.numeric)

valid_values <- unique(unname(unlist(rules_ranges)))
nb_tickets_vals <- as.numeric(unlist(nb_ticks))

sum(nb_tickets_vals[!map_lgl(nb_tickets_vals, ~ . %in% valid_values)]) # Validation == 71


# Part 2 ------------------------------------------------------------------

invalid_ticks <- map_lgl(nb_ticks, ~ any(!(. %in% valid_values)))

valid_ticks <- nb_ticks[!invalid_ticks]

fields <- map(1:length(valid_ticks[[1]]), function(x) { map_dbl(valid_ticks, function(y) y[[x]]) })

get_field_name <- function(field) {
  names(rules_ranges)[map_lgl(rules_ranges, ~ all(field %in% .))]
}

# This is where we store the correct answers
true_fields <- vector(mode = "character", length = 20)

# Make a list of potential fields
potential_fields <- map(fields, ~ get_field_name(.))
i <- 0

while ("" %in% true_fields) {
  i <- i + 1
  tictoc::tic(i)
  # Find the name and position of the position that only has one potential field
  true_field <- unlist(potential_fields[map_dbl(potential_fields, length) == 1])
  true_field_position <- which(map_lgl(potential_fields, ~ all(. == true_field) & (length(.) > 0)))
  
  # Store that field in our results vector
  true_fields[true_field_position] <- unlist(true_field)
  
  # Remove the newly-discovered field from the list of potentials
  potential_fields <- map(potential_fields, ~ .[. != true_field])
  
  # Start again?
  tictoc::toc()
}

my_ticket <- input[which(str_detect(input, "(your ticket)")) + 1] %>%
  str_split(pattern = ",") %>% 
  unlist() %>%
  as.numeric()

names(my_ticket) <- true_fields

my_ticket[str_detect(names(my_ticket), "^departure")] %>% prod()
