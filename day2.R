library(tidyverse)

policy <- tibble(
  line = readLines("day2-input.txt")
) %>%
  separate(line, into = c("low", "high", "char", "pass"), remove = FALSE)


policy %>%
  mutate(count = str_count(pass, pattern = char),
         ok = count >= as.numeric(low) & count <= as.numeric(high)) %>%
  pull(ok) %>% sum()



# Part 2 ------------------------------------------------------------------


policy %>%
  mutate(first_ok = str_sub(pass, as.numeric(low), as.numeric(low)) == char,
         second_ok = str_sub(pass, as.numeric(high), as.numeric(high)) == char,
         ok = xor(first_ok, second_ok)) %>%
  pull(ok) %>% sum()
