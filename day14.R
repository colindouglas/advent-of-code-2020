library(tidyverse)

input <- read_lines("day14-input.txt")


bits_to_int <- function(v) {
  # Converts vector of bits to an integer
  # Most significant bit on RHS
  l <- length(v)-1
  sum(v * 2^(l:0))
}

int_to_bits <- function(z) {
  # Converts integer to vector of bits
  # Most significant bit on RHS
  z <- as.numeric(z)
  o <- c()
  for (i in 35:0) {
    o <- c(o, (z %/% 2^i))
    z <- z - (z %/% 2^i) * 2^i
  }
  o
}

mem <- list()

for (line in input) {
  if (str_detect(line, "^mask =")) {
    
    mask <- str_extract(line, "[X01]+")
    change <- suppressWarnings(as.numeric(str_split(mask, pattern = "", simplify = TRUE)))
    change <- !is.na(change)
    mask <- str_split(mask, pattern = "", simplify = TRUE)
    mask <- as.numeric(mask[mask != "X"])
    # Do things to the mask #
    
  } else
    if (str_detect(line, "^mem\\[[0-9]+\\]")) {
      mem_pos <- str_extract(line, "^mem\\[[0-9]+\\]") %>% str_extract("[0-9]+")
      value <- str_extract(line, "[0-9]+$")
      
      value_bit <- int_to_bits(value)
      value_bit[change] <- as.numeric(mask)
      mem[[mem_pos]] <- value_bit
    }
}


sum(map_dbl(mem, bits_to_int)) %>% gmp::as.bigz()

# 12770062798021 # too high


# Part 2 ------------------------------------------------------------------
tictoc::tic("Part 2")
float_mem <- function(mem, mask) {
  # Determines the memory positions defined by a memory (int) and a mask (chr)
  mem <- int_to_bits(as.numeric(mem))
  
  mem <- case_when(
    mask == "0" ~ as.character(mem),
    mask == "1" ~ "1",
    mask == "X" ~ "X"
  )
  X_n <- sum(mem == "X")
  
  floats <- gtools::permutations(n = length(0:1), r = X_n, v = as.character(0:1), repeats.allowed = TRUE)
  o <- list()
  
  for (i in 1:nrow(floats)) {
    subbed_mem <- mem
    subbed_mem[mem == "X"] <- floats[i,]
   o <- append(o, list(subbed_mem))
  }
  
  o <- map(o, as.numeric)
  map_dbl(o, bits_to_int)
}

final_mem <- list()

for (line in input) {
  tictoc::tic(line)
  if (str_detect(line, "^mask =")) {
    
    mask <- str_extract(line, "[X01]+") 
    mask <- str_split(mask, pattern = "", simplify = TRUE)
    
  } else
    if (str_detect(line, "^mem\\[[0-9]+\\]")) {
      mem_pos <- str_extract(line, "^mem\\[[0-9]+\\]") %>% str_extract("[0-9]+")
      value <- str_extract(line, "[0-9]+$")
      
      to_write <- float_mem(mem_pos, mask)
      
      for (name in to_write) {
        name <- as.character(name)
        final_mem[[name]] <- as.numeric(value)
      }
    }
  tictoc::toc()
}


unlist(final_mem) %>% sum() %>% gmp::as.bigz()

tictoc::toc()

