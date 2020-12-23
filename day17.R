library(tidyverse)

# THIS IS JUST CONWAY'S GAME OF LIFE IN 3D
# COME ON

input <- read_lines("day17-input.txt")


parsed_input <- str_split(input, pattern = "", simplify = TRUE)
parsed_input <- as.vector(ifelse(parsed_input == "#", 1L, 0L))

df_states <- crossing(
  x = 0:(length(input)-1),
  y = 0:(length(input)-1),
  z = 0) %>%
  mutate(state = parsed_input)

count_neighbors <- function(df_states, x, y, z) {

  df_neighbors <- crossing(
    x = x + (-1):1,
    y = y + (-1):1,
    z = z + (-1):1)

  df_neighbors <- left_join(df_neighbors, select(df_states, x, y, z, state), by = c("x", "y", "z"))

  df_neighbors$state <- replace_na(df_neighbors$state, 0)

  # Don't count the cell itself
  sum(df_neighbors$state) - df_neighbors$state[df_neighbors$x == x &
                        df_neighbors$y == y &
                        df_neighbors$z == z]

  }

next_state <- function(states, neighbors) {
  case_when(
    # If a cube is active and exactly 2 or 3 of its neighbors are also active,
    # the cube remains active. Otherwise, the cube becomes inactive.
    states == 1 & neighbors %in% 2:3 ~ 1L,
    states == 1 ~ 0L,

    # If a cube is inactive but exactly 3 of its neighbors are active,
    # the cube becomes active. Otherwise, the cube remains inactive.
    states == 0 & neighbors == 3 ~ 1L,
    states == 0 ~ 0L,
    TRUE ~ as.integer(NA)
  )
}

expand_coords <- function(df_states) {

  xr <- c(min(df_states$x) - 1, max(df_states$x) + 1)
  yr <- c(min(df_states$y) - 1, max(df_states$y) + 1)
  zr <- c(min(df_states$z) - 1, max(df_states$z) + 1)

  out <- crossing(
    x = xr[1]:xr[2],
    y = yr[1]:yr[2],
    z = zr[1]:zr[2]
  )

  out <- left_join(out, df_states, by = c("x", "y", "z"))
  out$state <- replace_na(out$state, 0)
  out
}

iterate_state <- function(df_states, plot = FALSE) {
  tictoc::tic("Iteration complete")
  out <- df_states %>%
    expand_coords() %>%
    rowwise() %>%
    mutate(neighbors = count_neighbors(df_states, x, y, z),
           state = next_state(state, neighbors))
  tictoc::toc()
  out
}

plot_states <- function(df_states) {
  print(df_states %>%
          ggplot(aes(x = x, y = -y)) +
          geom_raster(aes(fill = as.logical(state))) +
          facet_wrap(~ z) +
          scale_fill_discrete(guide = FALSE) +
          theme(aspect.ratio=1)
  )
  df_states
}


six_iters <- df_states %>%
  iterate_state() %>%
  iterate_state() %>%
  iterate_state() %>%
  iterate_state() %>%
  iterate_state() %>%
  iterate_state()

six_iters$state %>% sum()

plot_states(six_iters)


# Part 2 ------------------------------------------------------------------

# THIS IS JUST 4D CONWAY CMON

input <- read_lines("day17-input.txt")


parsed_input <- str_split(input, pattern = "", simplify = TRUE)
parsed_input <- as.vector(ifelse(parsed_input == "#", 1L, 0L))

df_states <- crossing(
  x = 0:(length(input)-1),
  y = 0:(length(input)-1),
  z = 0,
  w = 0) %>%
  mutate(state = parsed_input)

count_neighbors <- function(df_states, x, y, z, w) {

  neighbors <- df_states$state[
    df_states$x %in% (x + (-1):1) &
      df_states$y %in% (y + (-1):1) &
      df_states$z %in% (z + (-1):1) &
      df_states$w %in% (w + (-1):1)]
  
  center_cell <- df_states$state[
    df_states$x == x &
      df_states$y == y &
      df_states$z == z &
      df_states$w == w]
  
  sum(neighbors, na.rm = TRUE) - center_cell
  
}

next_state <- function(states, neighbors) {
  case_when(
    # If a cube is active and exactly 2 or 3 of its neighbors are also active, 
    # the cube remains active. Otherwise, the cube becomes inactive.
    states == 1 & (neighbors == 2 | neighbors == 3) ~ 1L,
    states == 1 ~ 0L,
    
    # If a cube is inactive but exactly 3 of its neighbors are active, 
    # the cube becomes active. Otherwise, the cube remains inactive.
    states == 0 & neighbors == 3 ~ 1L,
    states == 0 ~ 0L,
    TRUE ~ as.integer(NA)
  )
}

expand_coords <- function(df_states) {
  
  xr <- c(min(df_states$x) - 1, max(df_states$x) + 1)
  yr <- c(min(df_states$y) - 1, max(df_states$y) + 1)
  zr <- c(min(df_states$z) - 1, max(df_states$z) + 1)
  wr <- c(min(df_states$w) - 1, max(df_states$w) + 1)
  
  out <- crossing(
    x = xr[1]:xr[2],
    y = yr[1]:yr[2],
    z = zr[1]:zr[2],
    w = wr[1]:wr[2],
  )
  
  out <- left_join(out, df_states, by = c("x", "y", "z", "w"))
  out$state <- replace_na(out$state, 0)
  out
}

iterate_state <- function(df_states) {
  tictoc::tic("Iteration complete")
  
  out <- expand_coords(df_states)
  
  out <- out %>%
    rowwise() %>%
    mutate(neighbors = count_neighbors(out, x, y, z, w),
           state = next_state(state, neighbors)) 
  
  tictoc::toc()
  
  out
}


six_iters <- df_states %>%
   iterate_state() %>%
   iterate_state() %>%
   iterate_state()
   iterate_state() %>%
   iterate_state() %>%
   iterate_state()

sum(six_iters$state)  # Validation 848

