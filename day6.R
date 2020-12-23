library(tidyverse)

customs <- read_file("day6-input.txt") %>% 
  str_split(., pattern = "\n\n", simplify = TRUE) %>%
  str_replace_all("\n", " ")

unique_questions <- function(x) {
  
  o <- as.vector(str_split(x, pattern = "", simplify = TRUE))
  o <- o[o != " "]
  length(unique(o))
}

map_dbl(customs, unique_questions) %>% sum()


# Part 2 ------------------------------------------------------------------


unique_questions_all <- function(x) {
  
  o <- str_split(x, pattern = " ")[[1]]
  o <- o[o != ""] # Fuck yoooouuuu
  passengers <- length(o)
  
  tab <- str_split(o, pattern = "")
  
  length(Reduce(intersect, tab))

}

map_dbl(customs, unique_questions_all) %>% sum()

#3423 too high
#3375 not right

