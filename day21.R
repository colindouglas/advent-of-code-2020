library(tidyverse)

input <- read_lines("day21-input.txt")

check_allergen <- function(a) {
  
  these_foods <- foods %>%
    filter(map_lgl(allergens, ~ a %in% .))
  
  these_ingredients <- these_foods$ingredients
  list(reduce(these_ingredients, intersect))
  
}

to_trans_vector <- function(df) {
  
  df <- df %>%
    filter(length(translation) == 1)
  
  v <- unlist(df$translation)
  names(v) <- df$allergen
  
  v  
}

translation_key <- c()


# Parse the input
foods <- tibble(input = input) %>%
  separate(input, into = c("ingredients", "allergens"), sep = "\\(", remove = FALSE) %>%
  mutate(allergens = str_remove_all(allergens, pattern = "(\\))|(contains )"),
         ingredients = str_split(ingredients, pattern = " "),
         allergens = str_split(allergens, pattern = ", ")) %>%
  mutate(ingredients = map(ingredients, ~ .[nchar(.) > 0]))

# Make a list of the allergens and potential translations
allergens <- tibble(
  allergen = unique(reduce(foods$allergens, c))
) %>%
  rowwise() %>%
  mutate(translation = check_allergen(allergen))


while (TRUE) {
  translation_key  <- c(translation_key, to_trans_vector(allergens))
  
  # Remove allergens we know
  allergens <- allergens %>%
    filter(!(allergen %in% names(translation_key)))
  
  if (nrow(allergens) < 1) { break }
  
  allergens <- mutate(allergens, translation =  list(translation[!(translation %in% translation_key)]))
}

all_ingredients <- reduce(foods$ingredients, c)

no_allergen <- all_ingredients[!(all_ingredients %in% translation_key)]


# Part 2 ------------------------------------------------------------------

translation_key[sort(names(translation_key))] %>% paste0(collapse = ",")

