library(tidyverse)

input <- read_lines("day11-input.txt") 
# 
# new_mat <- input %>%
#   str_split(pattern = "") %>%
#   unlist() %>% matrix(nrow = length(input), byrow = TRUE)
# 
# values <- c("." = 0, "L" = 0, "#" = 1)
# dims <- dim(new_mat)
# mat <- matrix(data = 0, nrow = dims[1], ncol = dims[2])
# iter <- 1
# 
# while (!identical(new_mat, mat)) {
#   iter <- iter + 1
#   message(iter)
#   mat <- new_mat
#   for (x in 1:dims[1]) {
#     for (y in 1:dims[2]) {
#       #message(x, y)
# 
#       # Make sure we don't go off the side of the matrix
#       x_min <- max(c(x-1, 1))
#       x_max <- min(c(x+1, dims[1]))
# 
#       y_min <- max(c(y-1, 1))
#       y_max <- min(c(y+1, dims[2]))
# 
#       neighbors <- mat[x_min:x_max, y_min:y_max] %>%
#         values[.] %>%
#         sum() - values[mat[x, y]]
# 
#       # If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
#       if (mat[x, y] == "L" & neighbors == 0) {
#         new_mat[x, y] <- "#"
#       }
#       # If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
#       else if (mat[x, y] == "#" & neighbors >= 4) {
#         new_mat[x, y] <- "L"
#         # Otherwise, the seat's state does not change.
#       } else {
#         new_mat[x, y] <- mat[x, y]
#       }
#     }
#   }
# }
# 
# # How many seats end up occupied?
# new_mat %>%
#   values[.] %>%
#   sum()
# 
# # 71 not right


# Part 2 ------------------------------------------------------------------

new_mat <- input %>% 
  str_split(pattern = "") %>% 
  unlist() %>%
  matrix(nrow = length(input), byrow = TRUE)

values <- c("." = 0, "L" = 0, "#" = 1)
dims <- dim(new_mat)
mat <- matrix(data = 0, nrow = dims[1], ncol = dims[2])
iter <- 0

look_direction <- function(mat, from, direction) {
  
  # Starting position
  stopifnot(length(from) == 2)
  x <- from[1]
  y <- from[2]
  
  stopifnot(x <= dim(mat)[1])
  stopifnot(y <= dim(mat)[2])
  
  if (direction == "all") {
    o <- c(
      look_direction(mat, from, "n"),
      look_direction(mat, from, "s"),
      look_direction(mat, from, "e"),
      look_direction(mat, from, "w"),
      look_direction(mat, from, "ne"),
      look_direction(mat, from, "nw"),
      look_direction(mat, from, "se"),
      look_direction(mat, from, "sw")
    )
    return(sum(o))
  } else
  
  if (direction == "w") {
    if ((y-1) < 1) { return(0) }
    los <- mat[x, (y-1):1]  # Line of sight
    los <- los[los != "."]
  } else
  
  if (direction == "e") {
    if ((y+1) > dim(mat)[2]) { return(0) }
    los <- mat[x, (y+1):dim(mat)[2]]  # Line of sight
    los <- los[los != "."]
  } else
  
  if (direction == "s") {
    if ((x+1) > dim(mat)[1]) { return(0) }
    los <- mat[(x+1):dim(mat)[1], y]  # Line of sight
    los <- los[los != "."]
  } else
  
  if (direction == "n") {
    if ((x-1) < 1) { return(0) }
    los <- mat[(x-1):1, y]  # Line of sight
    los <- los[los != "."]
  } else
  
  if (direction == "nw") {
    if (x == 1) return(0)
    if (y == 1) return(0)
    xj <- (x-1):1
    yj <- (y-1):1
    jumps <- min(c(length(xj), length(yj)))
    xj <- xj[1:jumps]
    yj <- yj[1:jumps]
    los <- map2_chr(xj, yj, ~ mat[.x, .y])
  } else
  if (direction == "sw") {
    if (x == dim(mat)[1]) return(0)
    if (y == 1) return(0)
    xj <- (x+1):dim(mat)[1]
    yj <- (y-1):1
    jumps <- min(c(length(xj), length(yj)))
    xj <- xj[1:jumps]
    yj <- yj[1:jumps]
    los <- map2_chr(xj, yj, ~ mat[.x, .y])
  } else
  if (direction == "se") {
    if (x == dim(mat)[1]) return(0)
    if (y == dim(mat)[2]) return(0)
    xj <- (x+1):dim(mat)[1]
    yj <- (y+1):dim(mat)[2]
    jumps <- min(c(length(xj), length(yj)))
    xj <- xj[1:jumps]
    yj <- yj[1:jumps]
    los <- map2_chr(xj, yj, ~ mat[.x, .y])
  } else
  if (direction == "ne") {
    if (x == 1) return(0)
    if (y == dim(mat)[2]) return(0)
    xj <- (x-1):1
    yj <- (y+1):dim(mat)[2]
    jumps <- min(c(length(xj), length(yj)))
    xj <- xj[1:jumps]
    yj <- yj[1:jumps]
    los <- map2_chr(xj, yj, ~ mat[.x, .y])
  }
  
  first_seen <- first(los[los != "."])
  
  if(length(first_seen) == 0 | is.na(first_seen)) { 
    return(0) 
    } 
  else if (first_seen == "#") {
    return(1)
  } else {
    return(0)
  }
}

tictoc::tic("The Whole Thing")
while (!identical(new_mat, mat)) {
  iter <- iter + 1
  tictoc::tic(paste0("Iteration ", iter))
  mat <- new_mat
  for (x in 1:dims[1]) {
    for (y in 1:dims[2]) {

      neighbors <- look_direction(mat, from = c(x, y), direction = "all")
      
      # If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
      if (mat[x, y] == "L" & neighbors == 0) { 
        new_mat[x, y] <- "#"
      } 
      # it now takes five or more visible occupied seats for an occupied seat to become empty
      else if (mat[x, y] == "#" & neighbors >= 5) { 
        new_mat[x, y] <- "L"
        # Otherwise, the seat's state does not change.
      } else {
        new_mat[x, y] <- mat[x, y]
      }
    }
  }
  tictoc::toc()
}
tictoc::toc()
# How many seats end up occupied?
new_mat %>% 
  values[.] %>% 
  sum()


# 2199 too high