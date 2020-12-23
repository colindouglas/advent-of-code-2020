library(tidyverse)

input <- read_lines("day18-validation.txt")

test <- input[[2]]

eval_plus <- function(s) {
  
  plus_re <- "[0-9]+ \\+ [0-9]+"
  
  plus <- str_extract(s, plus_re)
  sum <- as.character(eval(parse(text = plus)))
  
  str_replace(s, plus_re, sum)
}

eval_pluses <- function(s) {
  new_s <- s
  s <- ""
  while (!identical(s, new_s)) {
    s <- new_s
    new_s <- eval_plus(s)
  }
  new_s
}

eval_all <- function(s) {
  s <- eval_pluses(s)
  eval(parse(text = s))
}

map_dbl(input, eval_all) == c(26, 437, 12240, 13632)


# Part 1 but with Infix ---------------------------------------------------

# in R, all infix operators have the same precedence
`%slow+%` <- function(x, y) x + y
`%slow*%` <- function(x, y) x * y

slow_ops <- function(s) {
  s <- str_replace_all(s, pattern = "\\*", replacement = "%slow*%")
  s <- str_replace_all(s, pattern = "\\+", replacement = "%slow+%")
  eval(parse(text = s))
}

# Validation
map_dbl(input, slow_ops) == c(26, 437, 12240, 13632)

tictoc::tic("Part 1")
read_lines("day18-input.txt") %>%
  map_dbl(slow_ops) %>% sum() %>% gmp::as.bigz()
tictoc::toc()

# Part 2 ------------------------------------------------------------------

# Is actually how I tried to do part 1 initially

eval_plus <- function(s) {
  
  plus_re <- "[0-9]+ \\+ [0-9]+"
  
  plus <- str_extract(s, plus_re)
  sum <- as.character(eval(parse(text = plus)))
  
  o <- str_replace(s, plus_re, sum)
  #message(o)
  o
}

eval_pluses <- function(s) {
  new_s <- s
  s <- ""
  while (!identical(s, new_s)) {
    s <- new_s
    new_s <- eval_plus(s)
  }
  new_s
}

eval_paren <- function(s) {
  
  # Extract the first statement in parenthesis
  paren_re <- "\\([0-9*+ ]+\\)"
  paren <- str_extract(s, paren_re)
  
  if (is.na(paren)) return(s) # Early return if there's nothing to evaluate
  
  paren <- str_remove_all(paren, pattern = "[\\(\\)]")
  paren <- eval_all(paren)
  evald <- as.character(eval(parse(text = paren)))
  
  o <- str_replace(s, paren_re, evald)
  #message(o)
  o
}

eval_parens <- function(s) {
  new_s <- s
  s <- ""
  while (!identical(s, new_s)) {
    s <- new_s
    new_s <- eval_paren(s)
  }
  new_s
}

eval_all <- function(s) {
  new_s <- s
  s <- ""
  while (!identical(s, new_s)) {
    s <- new_s
    new_s <- eval_paren(s)
    new_s <- eval_pluses(new_s)
  }
  new_s
  eval(parse(text = s))
}

#eval_all(input[[1]]) # 46
#eval_all(input[[2]]) # 1445
#eval_all(input[[3]]) # 669060
#eval_all(input[[4]]) # 23340

tictoc::tic("Part 2")
read_lines("day18-input.txt") %>%
  map_dbl(eval_all) %>% sum() %>% gmp::as.bigz()
tictoc::toc()


# 5019432542701 too low
# 70518821989947