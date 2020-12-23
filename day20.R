library(tidyverse)


# Tile parsing ------------------------------------------------------------

input <- read_lines("day20-input.txt")

tile_start_rows <- grep("Tile*", input)
tile_ids <- input[tile_start_rows] %>%
  str_extract("[0-9]{4}")

tiles <- list()

for (i in 1:length(tile_start_rows)) {
  
  # The start of each tile is the row with "Tile" plus one
  tile_start <- tile_start_rows[i] + 1
  
  # The end of each tile is the next row start minus 2
  if (i < length(tile_start_rows)) {
    tile_end <- tile_start_rows[i + 1] - 2
  } else {
    # If we're at the end of the input, the end of the tile is the last line of input
    tile_end <- length(input)
  }
  
  tile <- input[tile_start:tile_end] %>%
    str_trim() %>%
    str_split(pattern = "", simplify = TRUE) %>%
    as.matrix()
  
  # Stop if the tile isn't square
  stopifnot(dim(tile)[1] == dim(tile)[2])
  
  tiles[[tile_ids[i]]] <- tile
  
}

# Rotates a matrix clockwise
rotate_cw <- function(m, n = 1) {
  if (n == 0) return(m)
  if (is.list(m)) m <- m[[1]]
  for (nothing in 1:n) {
    m <- t(apply(m, 2, rev))
  }
  m
}

# Take the row when it's at the top
# If it's the same as the reverse of a row, that means they fit together

get_side <- function(m, side) {
  d <- dim(m)[1]

  n <- switch(side,
              "north" = 0,
              "west" = 1,
              "south" = 2,
              "east" = 3)
  
  for (nothing in seq_len(n)) {
    m <- rotate_cw(m)
  }
  
  m[1,]
  
}

# Checks whether two rows match with each other
# Assumes each row is in the "top" position
check_mating <- function(x, y) {
  x <- str_split(x, pattern = "", simplify = TRUE)
  y <- str_split(y, pattern = "", simplify = TRUE)
  all(x == rev(y)) | all(x == y)
}

tictoc::tic("Make Combos")
combos <- crossing(
  top_tile = tile_ids,
  top_side = c("north", "south", "east", "west"),
  bottom_tile = tile_ids,
  bottom_side = c("north", "south", "east", "west")) %>%
  rowwise() %>%
  # Can't match a tile with itself
  filter(top_tile != bottom_tile) 
tictoc::toc()

tictoc::tic("Get sides")
combos <- combos %>%
  mutate(top_pattern = paste(get_side(tiles[[top_tile]], side = top_side), collapse = ""),
         bottom_pattern = paste(get_side(tiles[[bottom_tile]], side = bottom_side), collapse = ""))
tictoc::toc()

tictoc::tic("Check for mating")
combos <- combos %>%
  mutate(match = check_mating(top_pattern, bottom_pattern))
tictoc::toc()

combos %>%
  filter(match) %>%
  group_by(top_tile) %>%
  count()

corners <- combos %>%
  filter(match) %>%
  group_by(top_tile) %>%
  count() %>%
  filter(n == 2) 

stopifnot(nrow(corners) == 4)

corners %>%
  pull(top_tile) %>%
  gmp::as.bigz() %>%
  prod()  # Validation 20899048083289

# Part 2 ------------------------------------------------------------------

# How many rotations do you need to do to get it facing the right direction
how_many_rots <- function(dir, target) {
  if (dir == target) return(0)
  
  if (dir == "north" & target == "east") return(1)
  if (dir == "north" & target == "south") return(2)
  if (dir == "north" & target == "west") return(3)
  
  if (dir == "east" & target == "south") return(1)
  if (dir == "east" & target == "west") return(2)
  if (dir == "east" & target == "north") return(3)
  
  if (dir == "south" & target == "west") return(1)
  if (dir == "south" & target == "north") return(2)
  if (dir == "south" & target == "east") return(3)
  
  if (dir == "west" & target == "north") return(1)
  if (dir == "west" & target == "east") return(2)
  if (dir == "west" & target == "south") return(3)
}

# Opposite direction
opposite <- function(dir) {
  case_when(
    dir == "north" ~ "south",
    dir == "south" ~ "north",
    dir == "east" ~ "west",
    dir == "west" ~ "east"
  )
}



fill_down <- function(starting_tile, ignore = c()) {
  i <- 1
  v <- c(starting_tile)
  
  ignore <- ignore[!is.na(ignore)]
  
  current_tile <- edges %>%
    filter(top_tile == starting_tile,
           !(bottom_tile %in% ignore)) %>%
    head(1)
  
  while (TRUE) {
    i <- i + 1
    v <- c(v, current_tile$bottom_tile)
    
    # Get the next tile in the chain
    current_tile <- edges %>%
      filter(top_tile == current_tile$bottom_tile,
             top_side == opposite(current_tile$bottom_side)) %>%
      head(1)
    
    if (nrow(current_tile) == 0) break
  }
  v
}


edges <- filter(combos, match)

# We have to put the damn thing together

image_dims <- sqrt(length(tiles))
image_id <- matrix(nrow = image_dims, ncol = image_dims)
image_rot <- matrix(nrow = image_dims, ncol = image_dims)



# Pick the a corner, put it at 1,1
starting_corner <- edges %>% 
  filter(top_tile %in% corners$top_tile) %>%
  head(1) %>%
  pull(top_tile)

image_id[, 1] <- fill_down(starting_corner)


for (i in 1:dim(image_id)[1]) {
  image_id[i, ] <- fill_down(image_id[i, 1], ignore = as.vector(image_id))
}



# Mash the whole thing together -------------------------------------------

image <- matrix(nrow = nrow(image_id) * nrow(tiles[[1]]),
                ncol = ncol(image_id) * ncol(tiles[[1]]))

for (i in 1:nrow(image_id)) {
  for (j in 1:ncol(image_id)) {
    
    tile_to_place <- tiles[[image_id[i, j]]]
    d <- nrow(tile_to_place)
    image[((i-1)*d+1):(i*d), ((j-1)*d+1):(j*d)] <- tile_to_place
  }
}


# Figure out the orientation of the first and second tile  ---------------------------
i <- j <- 1

range_x <- ((i-1)*d+1):(i*d)
range_y <- ((j-1)*d+1):(j*d)

for (nothing1 in 1:4) {
  # This loop rotates the bottom tile
  for (nothing2 in 1:4) {
    bottom_11 <- image[d, 1:d]
    top_12 <- image[d+1, 1:d]
    
    if(all(bottom_11 == top_12)) {
      message("matched", nothing2)
      break
    }
    image[range_x+d, range_y] <- rotate_cw(image[range_x+d, range_y])
  }
  bottom_11 <- image[d, 1:d]
  top_12 <- image[d+1, 1:d]
  
  if(all(bottom_11 == top_12)) {
    message("matched ", nothing1)
    break
  }
  image[range_x, range_y] <- rotate_cw(image[range_x, range_y])
}

fill_column_down <- function(col) {
  # Now we do the third tile and so on
  for (j in 2:nrow(image_id)) {
    range_x <- ((col-1)*d+1):(col*d)
    range_y <- ((j-1)*d+1):(j*d)
    
    spinning_tile <- image[range_y, range_x]
    for (nothing2 in 1:2) {
      for (nothing in 1:4) {
        test_row <- spinning_tile[1, 1:d]
        match_against <- image[min(range_y)-1, range_x]
        message(test_row)
        message(match_against)
        
        if (all(test_row == match_against)) {
          message("CHECK")
          image[range_y, range_x] <<- spinning_tile
          break
        }
        
        spinning_tile <- rotate_cw(spinning_tile)
      }
      spinning_tile <- t(spinning_tile)
    }
  }
}

fill_column_down(1)

image <- rotate_cw(image)

walk(1:nrow(image_id), ~ fill_column_down(.))



# OK IT'S STITCHED TOGETHER -----------------------------------------------

# Now time to remove the edges 

# For d = 10 and td = 30, the edges are 1, 10, 11, 20, 21, 30
td <- dim(image)[1]

edge_rows <- seq(0, td, by = d)
edge_rows <- c(edge_rows, edge_rows + 1)
edge_rows <- edge_rows[between(edge_rows, 1, td)]

image <- image[-edge_rows, -edge_rows]

detect_monster <- function(m, x, y) {
  #x <- 3;y <- 4
  if (y-1 < 1) return(FALSE)
  if (y+1 > dim(m)[2]) return(FALSE)
  
  if (x+19 > dim(m)[2]) return(FALSE)
  
  ys <- c(0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, -1, 0, 0)
  xs <- c(0, 1, 4, 5, 6, 7, 10, 11, 12, 13, 16, 17, 18, 18, 19)

  monster <- map2_chr(xs, ys, ~m[y+.y, x+.x])
  
  if (all(monster == "#")){
    
   walk2(xs, ys, function(i, j) { 
     image[y+j, x+i] <<- "O"
     return(NULL)
     })
    return(TRUE)
  }
  FALSE
}

#image <- t(image)
#image <- rotate_cw(image)

monsters <- crossing(
  x = 1:td,
  y = 1:td) %>%
  rowwise() %>%
  mutate(monster = detect_monster(image, x, y))

plot(image)

sum(image == "#")

# Reset it 
image[image == "O"] <- "#"
# 2344 too high
# 2339 too high
# 1309 too low