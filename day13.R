library(tidyverse)
library(gmp)

input <- read_lines("day13-validation.txt")
earliest <- as.numeric(unlist(input[[1]]))

ids <- unlist(str_split(input[2], pattern = ","))

ids <- as.numeric(ids[!is.na(as.numeric(ids))])

first_buses_after_earliest <- ids * (earliest %/% ids + 1)

ids[which(first_buses_after_earliest == min(first_buses_after_earliest))] * (min(first_buses_after_earliest) - earliest)


# Part 2 ------------------------------------------------------------------

ids <- unlist(str_split(input[2], pattern = ","))

check_t <- Vectorize(function(ids, t) {
  for (i in 1:(length(ids))) {
    
    if (ids[i] == "x") next
    
    if ((t + i - 1) %% as.numeric(ids[i]) != 0) {
      return(FALSE)
    }
  }
  TRUE
})
tictoc::tic("Validation example")
#x <- map_lgl(0:1068781, ~ check_t(ids, .))
tictoc::toc()

# 15 years to do the input


# Additional validations
v1 <- c("67","7","59","61")
v2 <- c("67","x","7","59","61")
v3 <- c("67","7","x","59","61")
v4 <- c("1789","37","47","1889")

# The earliest timestamp that matches the list 17,x,13,19 is 3417.
check_t(v1, 754018)
check_t(v2, 779210)
check_t(v3, 1261476)
check_t(v4, 1202161486)

# For v1

# t %% 67 == 0
# (t %% 7) == (7 - 1)
# (t %% 59) == (59 - 2)
# (t %% 61) == (61 - 3)
# 
# # Reduce to linear equations
# 
# t = 68 * n1
# t = 7  * n2 - 1
# t = 59 * n3 - 2
# t = 61 * n4 - 3
# 
# 0 = 68, 0, 0, 0, -1
# 1 = 0, 7, 0, 0,  -1
# 2 = 0, 0, 59, 0, -1
# 3 = 0, 0, 0, 61, -1
# 
# B <- matrix(c(0, 1, 2, 3), nrow = 4)
# A <- matrix(c(
#   68, 0, 0, 0, -1,
#   0, 7, 0, 0, -1,
#   0, 0, 59, 0, -1,
#   0, 0, 0, 61, -1), nrow = 4, byrow = TRUE)

# Won't work, 5 unknowns and four constraints


# 3 at a time, there were 2 left in the basket. 
# When she took them out 5 at a time, there were 3 left and 
# when she took them out 7 at a time, there were 2 left.

# x == 2 mod (3)
# x = 3 mod(5)
# x = 2 mod(7)
# x = a mod(n)

# Two equations , three unknowns
# x = a1 + (a2-a1)*m1*n1
# x = a2 + (a1-a2)*m2*n2






# Iterative bezout --------------------------------------------------------

bez <- function(a, n) {
  stopifnot({
    length(a) == length(n) # Same length mods and rems
    length(a) >= 2  # Need at least two of each
  })
  
  out <- as.bigz(vector(mode = "numeric", length = length(a) - 1))
  
  # First iteration
  a <- as.bigz(a)
  
  # From https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Using_the_existence_construction
  m <- numbers::extGCD(n[1], n[2])[2:3] # Bezout's pair
  out[1] <- a[1] + (a[2]-a[1])*m[1]*n[1]
  out[1] <- out[1] %% (n[1] * n[2])
  
  if (length(a) == 2) { 
    return(out)
  }
  
  # Iterate through the rest of the pairs
  for (i in 2:(length(a)-1)) {
    out[i] <- bez(a = c(out[i-1], a[i+1]),
                  n = c(prod(n[1:i]), n[i+1])
    )
    out[i] <- out[i] %% prod(n[1:(i+1)])
  }
  tail(out, 1)
}

a <- c(0, 3, 4)
n <- c(3, 4, 5)

bez(a, n)  # 39


# Validation Examples -----------------------------------------------------

v <- c("17","x","13","19")
r <- 3417

n <- as.numeric(v)
a <- n[!is.na(n)] - (which(!is.na(n)) - 1)
n <- n[!is.na(n)]
a <- a %% n

bez(a, n) # This works! 


# Turn it into a function -------------------------------------------------

# Function to parse the schedule into remainders and modulos
ps <- function(v) { 
  suppressWarnings( {
    n <- as.numeric(v)
    a <- n[!is.na(n)] - (which(!is.na(n)) - 1)
    n <- n[!is.na(n)]
    a <- a %% n
  })
  list(a = as.numeric(a), n = as.numeric(n))
}

# Additional validations
v1 <- c("67","7","59","61") # 754018
bez(ps(v1)$a, ps(v1)$n) == 754018

v2 <- c("67","x","7","59","61") # 779210
bez(ps(v2)$a, ps(v2)$n) == 779210

v3 <- c("67","7","x","59","61") # 1261476
bez(ps(v3)$a, ps(v3)$n) == 1261476

v4 <- c("1789","37","47","1889") # 1202161486
bez(ps(v4)$a, ps(v4)$n) == 1202161486

# The actual test

tictoc::tic("Part 2")

input <- read_lines("day13-input.txt")
ids <- unlist(str_split(input[2], pattern = ","))
solution <- bez(ps(ids)$a, ps(ids)$n)
solution

tictoc::toc()

# 565311111692288 too high
# 247086664214628 # Correct (needed more precision)
# 179378772246528 too low
# 51233980284928 too low

numbers::chinese(ps(ids)$a, ps(ids)$n) %>% as.bigz()
