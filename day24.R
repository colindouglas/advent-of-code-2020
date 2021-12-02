library(tidyverse)

input <- read_lines("day24-input.txt")
#input <- list("nwwswee")

input <- str_split(input, pattern = "")


# Parse the input
parse_directions <- function(x) {
  dirs <- c()
  i <- 1
  while (i <= length(x)) {
    if (x[i] %in% c("n", "s")) {
      dirs <- append(dirs, paste(x[i + 0:1], collapse = ""))
      i <- i + 2
    } else if (x[i] %in% c("e", "w")) {
      dirs <- append(dirs, x[i])
      i <- i + 1
    }
  }
  dirs
}

directions <- map(input, parse_directions)
#sq <- sqrt(0.5)
sq <- 0.5

dir_to_coords <- list(
  "se" = c(sq, -sq),
  "sw" = c(-sq, -sq),
  "ne" = c(sq, sq),
  "nw" = c(-sq, sq),
  "w" = c(-2*sq, 0),
  "e" = c(2*sq, 0)
)

follow_directions <- function(dirs) {
  
  pos <- c(x = 0, y = 0)
  i <- 0
  
  
  
  for (i in seq_along(dirs) ){
    dir <- dirs[[i]]
    pos <- pos + dir_to_coords[[dir]]
  }
  pos
}

map(directions, follow_directions)

visited <- reduce(map(directions, follow_directions), bind_rows) 

out <- visited %>%
  group_by(x, y) %>%
  tally() %>%
  mutate(color = ifelse((n %% 2 == 1), "black", "white"))

out %>% 
  group_by(color) %>%
  tally()


# Part 2 ------------------------------------------------------------------



day0 <- select(out, -n)



get_color <- function(df_colors, x, y) {
  
  col <- df_colors$color[df_colors$x == x & df_colors$y == y]
  
  stopifnot(length(col) <= 1)  # No tile should be duplicated
  
  # If that position doesn't exist yet, add it to the list of tile colors
  # All tiles start as white
  if (length(col) == 0) {
    # tile_colors <<- tile_colors %>%
    #   ungroup() %>%
    #   add_row(x = x,
    #           y = y,
    #           color = "white")
    "white"
  } else {
    col
  }
}

df <- day0
iterate <- function(df) {
  
  # EXPAND the GRID DF so that ALL OF THE NEIGHBORS ARE THERE
  df_grid <- tibble()
  for (i in seq_len(nrow(df))) {
    
    x_pos <- df$x[[i]]
    y_pos <- df$y[[i]]
    
    # Slow
    # neighbors <- map_dfr(c("se", "ne", "sw", "nw", "e", "w"), ~ follow_directions(.) + c(x_pos, y_pos)) %>%
    #   add_row(x = x_pos,
    #           y = y_pos)
    
    # Faster
    # neighbors <- list(
    #   c(x = sq, y = -sq) + c(x_pos, y_pos),
    #   c(x = -sq, y = -sq) + c(x_pos, y_pos),
    #   c(x = sq, y = sq) + c(x_pos, y_pos),
    #   c(x = -sq, y = sq) + c(x_pos, y_pos),
    #   c(x = -2*sq, y = 0) + c(x_pos, y_pos),
    #   c(x = 2*sq, y = 0) + c(x_pos, y_pos)
    # ) %>% 
    #   bind_rows() %>%
    #   add_row(x = x_pos,
    #           y = y_pos)
    
    # Even faster
    neighbors <- tibble(
      x = c(sq, -sq, sq, -sq, -2*sq, 2*sq, 0) + x_pos,
      y = c(-sq, -sq, sq, sq, 0, 0, 0) + y_pos)
    
    df_grid <- bind_rows(df_grid, neighbors)
    
  }
  
  df_grid <- distinct(df_grid)
  
  # FIND THE COLORS OF EACH OF THE GRID POSITIONS
  # NO COLOR MEANS WHITE
  
  df_grid <- df_grid %>%
    left_join(df, by = c("x", "y")) %>%
    mutate(color = replace_na(color, "white"))
  
  df <- df_grid
  df$next_color <- NA
  for (i in seq_len(nrow(df))) {
    
    x_pos <- df$x[[i]]
    y_pos <- df$y[[i]]
    color <- df$color[[i]]
    
    #neighbors <- map(c("se", "ne", "sw", "nw", "e", "w"), ~ follow_directions(.) + c(x_pos, y_pos))
    
    neighbors <- list(
      c(x = sq, y = -sq) + c(x_pos, y_pos),
      c(x = -sq, y = -sq) + c(x_pos, y_pos),
      c(x = sq, y = sq) + c(x_pos, y_pos),
      c(x = -sq, y = sq) + c(x_pos, y_pos),
      c(x = -2*sq, y = 0) + c(x_pos, y_pos),
      c(x = 2*sq, y = 0) + c(x_pos, y_pos)
    )
    
    
    neighbor_colors <- map_chr(neighbors, ~ get_color(df, .[["x"]], .[["y"]]))
    adj_black <- sum(neighbor_colors == "black")
    df$next_color[[i]] <- case_when(
      color == "black" & adj_black == 0 ~ "white",
      color == "black" & adj_black > 2 ~ "white",
      color == "white" & adj_black  == 2 ~ "black",
      TRUE ~ color
    )
  }
  select(df, x, y, color = next_color)
}


tictoc::tic("Part 2")
current <- day0

for (n in seq_len(100)) {
  tictoc::tic(paste0(c("Iteration ", n)))
  current <- iterate(current)
  tictoc::toc()
  message(paste0(c(n, ": ", sum(current$color == "black"))))
}
tictoc::toc()

# 5359.539 seconds

5359.539 / 60 / 60




