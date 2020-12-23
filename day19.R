library(tidyverse)

input <- read_lines("day19-input.txt")
input <- str_remove_all(input, '\\"')
message_split <- which(input == "")
rules <- input[1:(message_split-1)]
messages <- input[(message_split+1):length(input)]

rules_split <- str_split(rules, pattern = ": ")

rules <- map_chr(rules_split, ~ .[2]) # The instructions of the rules
rules_number <- as.numeric(map_chr(rules_split, ~ .[1])) # The order of the rules


# Reorder the instructions for they're in the right order
rules <- rules[order(rules_number)]


# Put them all in brackets to fix OoO
rules <- paste0("(", rules, ")")

# Now we start substituting 
subbed_rules <- rules

# We add parens around either side of an "|" because otherwise OoO messages us up
subbed_rules <- str_replace_all(subbed_rules, pattern = "[0-9 ]+(?= \\|)", replacement = "\\( \\0 \\)")
subbed_rules <- str_replace_all(subbed_rules, pattern = "(?<=\\|)[0-9 ]+", replacement = "\\( \\0 \\)")

# Add spaces around each number
subbed_rules <- str_replace_all(subbed_rules, pattern = "(?<=\\()[0-9]", replacement = "\\( \\0")
subbed_rules <- str_replace_all(subbed_rules, pattern = "[0-9](?=\\))", replacement = "\\0 \\)")

iter <- 0
while (any(str_detect(subbed_rules, pattern = "[0-9]"))) {
#for (nothing in 1:length(rules)) {
  iter <<- iter + 1
  message(iter)
  terminal_rules <- which(!str_detect(subbed_rules, "[0-9]")) - 1
  
  for (i in 1:length(terminal_rules)) {
    subbed_rules <- str_replace_all(subbed_rules, 
                                    pattern = paste0(" ", as.character(terminal_rules[i]), " "), 
                                    replacement = paste0(" ", rules[terminal_rules[i]+1], " ")
    )
    # Add spaces around each number
    subbed_rules <- str_replace_all(subbed_rules, pattern = "(?<=\\()[0-9]", replacement = "\\( \\0")
    subbed_rules <- str_replace_all(subbed_rules, pattern = "[0-9](?=\\))", replacement = "\\0 \\)")
  }
}

regex <- str_remove_all(subbed_rules, pattern = " ")

check_message <- function(this_message, regex) {
  
  extract <- replace_na(str_extract(this_message, regex), 0)
  nchar(this_message) == nchar(extract)
  
}

out <- tibble(
  message = messages) %>%
  rowwise() %>%
  mutate(valid = check_message(message, regex[[1]]))

worked_in_pt1 <- out$message[out$valid]


# Part 2 ------------------------------------------------------------------
library(tidyverse)
new_8 <- "X"
new_11 <- "Y"

input <- read_lines("day19-input.txt")
input <- str_remove_all(input, '\\"')
message_split <- which(input == "")
rules <- input[1:(message_split-1)]
messages <- input[(message_split+1):length(input)]

rules_split <- str_split(rules, pattern = ": ")

rules <- map_chr(rules_split, ~ .[2]) # The instructions of the rules
rules_number <- as.numeric(map_chr(rules_split, ~ .[1])) # The order of the rules


# Reorder the instructions for they're in the right order
rules <- rules[order(rules_number)]


# Put them all in brackets to fix OoO
rules <- paste0("(", rules, ")")

rules[9] <- new_8
rules[12] <- new_11
# Now we start substituting 
subbed_rules <- rules

# We add parens around either side of an "|" because otherwise OoO messages us up
subbed_rules <- str_replace_all(subbed_rules, pattern = "[0-9 ]+(?= \\|)", replacement = "\\( \\0 \\)")
subbed_rules <- str_replace_all(subbed_rules, pattern = "(?<=\\|)[0-9 ]+", replacement = "\\( \\0 \\)")

# Add spaces around each number
subbed_rules <- str_replace_all(subbed_rules, pattern = "(?<=\\()[0-9]", replacement = "\\( \\0")
subbed_rules <- str_replace_all(subbed_rules, pattern = "[0-9](?=\\))", replacement = "\\0 \\)")

# Make (a) and (b) just 'a' and 'b'



iter <- 0
while (any(str_detect(subbed_rules, pattern = "[0-9]"))) {
  #for (nothing in 1:length(rules)) {
  iter <<- iter + 1
  message(iter)
  terminal_rules <- which(!str_detect(subbed_rules, "[0-9]")) - 1
  
  for (i in 1:length(terminal_rules)) {
    subbed_rules <- str_replace_all(subbed_rules, 
                                    pattern = paste0(" ", as.character(terminal_rules[i]), " "), 
                                    replacement = paste0(" ", rules[terminal_rules[i]+1], " ")
    )
    # Add spaces around each number
    subbed_rules <- str_replace_all(subbed_rules, pattern = "(?<=\\()[0-9]", replacement = "\\( \\0")
    subbed_rules <- str_replace_all(subbed_rules, pattern = "[0-9](?=\\))", replacement = "\\0 \\)")
    
    # Make (a) and (b) just 'a' and 'b'
    subbed_rules <- str_replace_all(subbed_rules, pattern = "\\(a\\)", replacement = "a")
    subbed_rules <- str_replace_all(subbed_rules, pattern = "\\(b\\)", replacement = "b")
  }
}

subbed_rules <- str_remove_all(subbed_rules, pattern = " ") %>%
  str_replace_all(pattern = "\\(a\\)", replacement = "a") %>%
  str_replace_all(pattern = "\\(b\\)", replacement = "b") 

iterations <- crossing(
  eight = 1:10,
  eleven = 1:10
)

iterate_regex <- function(which, n) {
  
  # 8: 42 | 42 8
  # 11: 42 31 | 42 11 31
  
  stopifnot(which %in% c(8, 11))
  
  if (which == 8) {
    out <- paste(rep(subbed_rules[43], times = n), collapse = "")
  } else if (which == 11) {
    out <- paste(rep(subbed_rules[c(43, 32)], each = n), collapse = "")
  }
  out
}
counter <- 0
iterations <- iterations %>%
  rowwise() %>%
  mutate(regex_8 = iterate_regex(8, n = eight),
         regex_11 = iterate_regex(11, n = eleven),
         regex_all = paste0(regex_8, regex_11))

follows_rule0 <- function(message, regex) {
  #message <- messages[1]; regex <- iterations$regex_all[1]
  ext <- replace_na(str_extract(message, pattern = regex), "")
  nchar(ext) == nchar(message)
}

message_ok <- function(message) {
  counter <<- counter +1
  tictoc::tic(paste("Message", counter, "complete"))
  
  for (reg in iterations$regex_all) {
    if (follows_rule0(message, reg)) {
      tictoc::toc()
      return(TRUE)
    }
  }
  tictoc::toc()
  FALSE
}

# If it worked in part 1, it definitely worked so we don't need to check it again
not_ok_yet <- messages[!(messages %in% worked_in_pt1)]

tictoc::tic("All of part 2")

furrr::future_map_lgl(not_ok_yet, ~ message_ok(.)) -> ok_in_pt2

sum(ok_in_pt2) + length(worked_in_pt1) 

tictoc::toc()
