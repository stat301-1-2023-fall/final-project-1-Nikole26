# Loading libraries-----------
library(tidyverse)
library(skimr)
library(stringr)
library(ggplot2)
library(plotly)
library(tidyr)
library(knitr)
library(ggrepel)
library(kableEXTRA)
# Loading data -----------------
data <- read_csv("data/tripadvisor_european_restaurants.csv")

# Data cleanead-----------
data_tidy_1 <- data %>%
  rename(
    vegetarian = vegetarian_friendly,
    vegan = vegan_options,
    gluten_free = gluten_free
  ) %>%
  mutate(
    awards = ifelse(is.na(awards), "none", awards),
    special_diets = case_when(
      vegetarian == "Y" ~ "vegetarian",
      vegan == "Y" ~ "vegan",
      gluten_free == "Y" ~ "gluten_free",
      TRUE ~ "None"
    )) %>%
  select(-vegetarian, -vegan, -gluten_free) 

data_tidy_1 <- data_tidy_1 |>
  mutate(price_level = case_when(
    price_level == "€€€€" ~ "expensive",
    price_level == "€€-€€€" ~ "mid-range",
    price_level == "€" ~ "cheap",
    TRUE ~ price_level  # Keep other values unchanged
  ))

data_tidy_1$top_tags <- str_split(data_tidy_1$top_tags, ", ", simplify = FALSE)

data_tidy_1$top_tags <- lapply(data_tidy_1$top_tags, function(tags) {
  tags <- tags[!(tags %in% c("Vegetarian Friendly", "Vegan Options", "Gluten Free Options",
                             "Cheap Eats", "Mid-range", "Fine Dining"))]
  if (length(tags) == 0) {
    tags <- NA
  }
  tags
})

data_tidy_2 <-  data_tidy_1 |>
  mutate(
    minimum_range = as.numeric(str_extract(price_range, "\\d+(\\.\\d+)?")),
    maximum_range = as.numeric(str_extract_all(price_range, "\\d+(\\.\\d+)?") %>% sapply(function(x) x[2])),
    avg_price = (minimum_range + maximum_range) / 2
  ) |>
  select(-c(minimum_range, maximum_range))

data_tidy_2$top_tags <- lapply(data_tidy_2$top_tags, function(tags) {
  tags[tags %in% c("Brew Pub", "Wine Bar", "Pub")] <- "Bar"
  
  tags[tags %in% c("Pizza", "Northern Italy", "Southern Italy",
                   "Nepalese", "Vietnamese", "Hong Kong", "Thai", "Bangladeshi",
                   "Cantonese", "Zhejiang", "Japanese", "Sushi", "Asian",
                   "Mongolian", "Egypt", "Egyptian", "Balti", "Philippine",
                   "Tuscan", "Indian", "Tibetan", "Fujian", "Indonesian",
                   "Korean", "Hubei", "Turkish", "Russian", "Taiwanese", 
                   "Persian", "Middle Eastern", "Lebanese", "Cambodian",
                   "Japenese", "Pakistani", "Szechuan", "Chinese")] <- "Asia"
  
  tags[tags %in% c("Welsh", "Scottish", "Irish", "European", "French",
                   "Italian", "Mediterranean", "British", "Central-Italian",
                   "Dutch", "Greek", "Portuguese", "Czech", "Scandinavian", 
                   "Central European", "Danish", "Romanian", "Lombard",
                   "Ligurian", "Sicilian", "Polish", "Sardinian", "Campania",
                   "Neapolitan", "Austrian", "German", "Belgian", "Southern-Italian",
                   "Basque", "Turkish", "Apulian", "Russian", "Romana",
                   "Northern-Italian", "Southwestern", "Norwegian", "Scandinavian",
                   "Eastern European", "Moroccan", "Armenian", "Swiss", "Spanish",
                   "Catalan")] <- "Europe"
  
  tags[tags %in% c("Egyptian", "Egypt", "African", "Algerian", "Contemporary",
                   "Arabic", "Tunisian")] <- "Africa"
  
  tags[tags %in% c("Chilean", "Venezuelan", "Peruvian", "South American",
                   "Argentinian", "Latin", "Brazilian")] <- "South America"
  
  tags[tags %in% c("Mexican", "Central American", "Native American", "Cuban", "Latin",
                   "Caribbean", "Canadian", "American")] <- "North America"
  
  tags[tags %in% c("New Zealand", "Polynesian", "Australian")] <- "Oceania"
  
  tags <- tags[!(tags %in% c("Delivery Only", "Dinner", "Diner", "Healthy", "Barbecue", "Caucasian",
                             "Quick Bites", "Cajun & Creole", "International", "Deli", "Dessert",
                             "Bakeries", "Fast food", "Cafe", "Street Food", "Seafood", "Gastropub", 
                             "Dining bars", "Grill", "Steakhouse", "Beer restaurants",
                             "Halal", "Soups", "Speciality Food Market"))]
  
  if (length(tags) == 0) {
    tags <- NA
  } else {
    tags <- paste0(unique(tags), collapse = ", ")
  }
  
  tags
})

rows_NA <- data_tidy_2 |>
  summarise(across(everything(), ~sum(is.na(.))))

data_tidy_3 <- select(data_tidy_2, -address, -original_location, 
                      -keywords, -features, -cuisines, -popularity_detailed, 
                      -popularity_generic, -price_range, -original_open_hours)

data_final <- data_tidy_3[rowSums(!is.na(data_tidy_3)) > 24, ]

data_final <- na.omit(data_final, cols = "average_price")

data_final <- data_final |>
  rename(cuisines = top_tags,
         food_rating = food,
         value_rating = value,
         service_rating = service)

# Digging deeper France--------------------
french_restaurants <- data_final |>
  filter(country == "France")

ggplot(french_restaurants, aes(x = price_level, fill = price_level)) +
  geom_bar() +
  labs(title = "Distribution of French Restaurants by Price Level",
       x = "Price Level",
       y = "Number of Restaurants") +
  scale_fill_manual(values = c("cheap" = "darkgreen", "mid-range" = "orange", "expensive" = "red"))

top_rated_restaurants <- french_restaurants[french_restaurants$avg_rating == max(french_restaurants$avg_rating), ]
