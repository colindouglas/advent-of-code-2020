library(tidyverse)

passports <- read_file("day4-input.txt") %>% 
  str_split(., pattern = "\n\n", simplify = TRUE) %>%
  str_replace_all("\n", " ")

to_kvpair <- function(x) {

  kv_pairs <- str_split(x, pattern = "(:)| ", simplify = TRUE) # Split at colon
  kv_pairs <- kv_pairs[kv_pairs != ""] # Drop the empty values
  
  out <- kv_pairs[seq(2, length(kv_pairs), by = 2)] # Values are even elements
  names(out) <- kv_pairs[seq(1, length(kv_pairs), by = 2)] # Keys are odd elements
  
  out
}

passports <- map(passports, to_kvpair)

check_valid <- function(v) {
  required <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid") 
  v <- unlist(v)
  all(required %in% names(v))
}

valid_pp <- passports[map_lgl(passports, check_valid)]

length(valid_pp)

# Part 2 ------------------------------------------------------------------

validate_fields <- function(v) {
  
  v <- unlist(v)
  
  #   byr (Birth Year) - four digits; at least 1920 and at most 2002.
  byr <- as.numeric(v["byr"])
  if (is.na(byr) | !between(byr, 1920, 2002)) { 
    return(FALSE) 
    }

  # iyr (Issue Year) - four digits; at least 2010 and at most 2020.
  iyr <- as.numeric(v["iyr"])
  if (is.na(iyr) | !between(iyr, 2010, 2020)) { 
    return(FALSE) 
  }

  # eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
  eyr <- as.numeric(v["eyr"])
  if (is.na(eyr) | !between(eyr, 2020, 2030)) { 
    return(FALSE) 
  }

  # hgt (Height) - a number followed by either cm or in:
  hgt_unit <- str_extract(v["hgt"], "(cm)|(in)")
  hgt_mag <- str_extract(v["hgt"], "[0-9]+")
  if (is.na(hgt_unit) | is.na(hgt_mag)) {     
    return(FALSE)  
    }

  #  If cm, the number must be at least 150 and at most 193.
  if (hgt_unit == "cm" & !between(hgt_mag, 150, 193)) { 
    return(FALSE) 
    }

  #  If in, the number must be at least 59 and at most 76.
  if (hgt_unit == "in" & !between(hgt_mag, 59, 76)) { 
    return(FALSE) 
  }

  # hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
  hcl <- str_extract(v["hcl"], "#[0-9a-f]{6}")
  if (is.na(hcl)) { return(FALSE) }

  # ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
  ecl <- v["ecl"]
  if (!(ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"))) { 
    return(FALSE)
  }

  # pid (Passport ID) - a nine-digit number, including leading zeroes.
  pid <- v["pid"]
  if (nchar(pid) != 9 | is.na(as.numeric(pid))) { 
    return(FALSE) 
    }

  return(TRUE)
}

map_lgl(valid_pp, validate_fields) %>% sum()
