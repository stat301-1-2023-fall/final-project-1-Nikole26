# Loading libraries-----------
library(tidyverse)
library(skimr)
library(DT)
library(stringr)

# Loading data -----------------
european_restaurants <- read_csv("data/tripadvisor_european_restaurants.csv")


# Data management--------------
skim_without_charts(european_restaurants)
head(european_restaurants)

# Adding a country code
countries_dict <- c('Austria' = 'AUT', 'Belgium' = 'BEL', 'Bulgaria' = 'BGR', 'Croatia' = 'HRV', 'Czech Republic' = 'CZE',
                    'Denmark' = 'DNK', 'England' = 'GBR', 'Finland' = 'FIN', 'France' = 'FRA', 'Germany' = 'DEU',
                    'Greece' = 'GRC', 'Hungary' = 'HUN', 'Ireland' = 'IRL', 'Italy' = 'ITA', 'Northern Ireland' = 'GBR',
                    'Poland' = 'POL', 'Portugal' = 'PRT', 'Romania' = 'ROU', 'Scotland' = 'GBR', 'Slovakia' = 'SVK',
                    'Spain' = 'ESP', 'Sweden' = 'SWE', 'The Netherlands' = 'NLD', 'Wales' = 'GBR')

# Displaying the code for each restaurant 
european_restaurants$country_code <- 
  ifelse(european_restaurants$country %in% names(countries_dict),                   countries_dict[european_restaurants$country],
         european_restaurants$country)   

# Creating a new variable: average price
#european_restaurants %>%
 # mutate(
  #  minimum_range = as.numeric(str_extract(price_range, "\\d+(\\.\\d+)?")),
   # maximum_range = as.numeric(str_extract_all(price_range, "\\d+(\\.\\d+)?") %>% sapply(function(x) x[2])),
  #  avg_price = (minimum_range + maximum_range) / 2
  #) %>%
  #select(-c(minimum_range, maximum_range))

#european_restaurants %>%
#summarise_all(~sum(is.na(.))) %>%
#  filter(avg_price > 0) %>%
#  drop_na() %>%
#  mutate_all(as.integer)

# Dealing with NA value for award--------------+
restaurants_tidy <- european_restaurants %>%
  rename(
    vegetarian = vegetarian_friendly,
    vegan = vegan_options,
    gluten_free = gluten_free
    ) %>%
  mutate(
    awards = ifelse(is.na(awards), "None", awards),
    special_diets = case_when(
      vegetarian == "Y" ~ "vegetarian",
      vegan == "Y" ~ "vegan",
      gluten_free == "Y" ~ "gluten_free",
      TRUE ~ "None"
    )) %>%
  select(-vegetarian, -vegan, -gluten_free)
    