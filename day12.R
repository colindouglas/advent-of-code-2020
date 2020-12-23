library(tidyverse)

input <- read_lines("day12-validation.txt")

# dirs <- list(
#   "N" = c(0, 1),
#   "E" = c(1, 0),
#   "S" = c(0, -1),
#   "W" = c(-1, 0)
# )
# 
# # Rotation matrix (from geometry)
# turn_matrix <- function(theta) { 
#   
#   theta <- theta / 360 * pi * 2
#   
#   round(matrix(
#     c(cos(theta), -sin(theta),
#       sin(theta), cos(theta)), 
#     nrow = 2, ncol = 2, byrow = TRUE), 0)
# }
# 
# 
# dir_current <- dirs[["E"]]  # Start facing east
# pos_current <- c(0, 0)
# 
# for (ins in input) {
#   
#   dir <- str_extract(ins, "[NSEWFLR]") # The direction
#   mag <- as.numeric(str_extract(ins, "[0-9]+"))
#   
#   
#   # If we're going a cardinal direction, go that way without turning
#   if (dir %in% c("N", "S", "E", "W")) {
#     pos_current <- pos_current + dirs[[dir]]*mag
#   }
#   
#   # If we're going forward, take that many steps without changing direction
#   if (dir == "F") {
#     pos_current <- pos_current + dir_current*mag
#   }
#   
#   # If we're turning, call the turn function
#   if (dir %in% c("L", "R")) {
#     
#     rotation <- c("R" = -1, "L" = 1)
#     
#     theta <- mag*rotation[dir]
#     
#     dir_current <- as.vector(turn_matrix(theta) %*% dir_current)
#   }
# }
# 
# abs(pos_current[1]) + abs(pos_current[2]) # Should be 25
# 


# Part 2 ------------------------------------------------------------------

input <- read_lines("day12-input.txt")

dirs <- list(
  "N" = c(0, 1),
  "E" = c(1, 0),
  "S" = c(0, -1),
  "W" = c(-1, 0)
)

# Rotation matrix (from geometry)
turn_matrix <- function(theta) { 
  
  theta <- theta / 360 * pi * 2
  
  round(matrix(
    c(cos(theta), -sin(theta),
      sin(theta), cos(theta)), 
    nrow = 2, ncol = 2, byrow = TRUE), 0)
}


pos_ship <- c(0, 0)
pos_wp <- c(10, 1)

for (ins in input) {
  
  dir <- str_extract(ins, "[NSEWFLR]") # The direction
  mag <- as.numeric(str_extract(ins, "[0-9]+"))
  
  
  # If we're going a cardinal direction, go that way without turning
  if (dir %in% c("N", "S", "E", "W")) {
    pos_wp <- pos_wp + dirs[[dir]]*mag
  }
  
  # If we're going forward, take that many steps without changing direction
  if (dir == "F") {
    pos_ship <- pos_ship + mag * pos_wp
  }
  
  # If we're turning, call the turn function
  if (dir %in% c("L", "R")) {
    
    rotation <- c("R" = -1, "L" = 1)
    
    theta <- mag*rotation[dir]
    
    pos_wp <- as.vector(turn_matrix(theta) %*% pos_wp)
  }
}

abs(pos_ship[1]) + abs(pos_ship[2]) # Should be 25



