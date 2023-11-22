# Loading libraries-----------
library(tidyverse)
library(skimr)
library(DT)
library(stringr)
library(gt)
library(webshot2)

# Loading data -----------------
european_restaurants <- read_csv("data/tripadvisor_european_restaurants.csv")


# Data management--------------
skim_without_charts(european_restaurants)
head(european_restaurants)

# Adding a country code
#countries_dict <- c('Austria' = 'AUT', 'Belgium' = 'BEL', 'Bulgaria' = 'BGR', 'Croatia' = 'HRV', 'Czech Republic' = 'CZE',
#                    'Denmark' = 'DNK', 'England' = 'GBR', 'Finland' = 'FIN', 'France' = 'FRA', 'Germany' = 'DEU',
#                    'Greece' = 'GRC', 'Hungary' = 'HUN', 'Ireland' = 'IRL', 'Italy' = 'ITA', 'Northern Ireland' = 'GBR',
#                    'Poland' = 'POL', 'Portugal' = 'PRT', 'Romania' = 'ROU', 'Scotland' = 'GBR', 'Slovakia' = 'SVK',
#                    'Spain' = 'ESP', 'Sweden' = 'SWE', 'The Netherlands' = 'NLD', 'Wales' = 'GBR')

# Displaying the code for each restaurant 
#european_restaurants$country_code <- 
#  ifelse(european_restaurants$country %in% names(countries_dict),                   countries_dict[european_restaurants$country],
#         european_restaurants$country)   

#european_restaurants %>%
#summarise_all(~sum(is.na(.))) %>%
#  filter(avg_price > 0) %>%
#  drop_na() %>%
#  mutate_all(as.integer)

# Dealing with NA value for award and special diets--------------
restaurants_tidy <- european_restaurants %>%
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

# Renaming Price level
## First identifying the types of values given
unique(restaurants_tidy$price_level)
## Renaming as Expensive  
restaurants_tidy <- restaurants_tidy |>
  mutate(price_level = case_when(
    price_level == "€€€€" ~ "expensive",
    price_level == "€€-€€€" ~ "mid-range",
    price_level == "€" ~ "cheap",
    TRUE ~ price_level  # Keep other values unchanged
  ))

# Wrangling top tags and dealing with doubles ------------
## Separating the values in the strings
restaurants_tidy$top_tags <- str_split(restaurants_tidy$top_tags, ", ", simplify = FALSE)

## Replace empty strings with NA in each list
#restaurants_tidy$top_tags <-  ifelse(split_tags == "", NA, split_tags)
#restaurants_tidy$top_tags <- lapply(restaurants_tidy$top_tags, function(tags) {
#  tags[tags == ""] <- NA
#  tags
#})

## If you want to collapse the list into a single character vector
#restaurants_tidy$top_tags <- sapply(restaurants_tidy$top_tags, function(tags) {
#  if (!is.na(tags)) {
#    paste(tags, collapse = ", ")
#  } else {
#    NA
#  }
#})

#unique(unlist(restaurants_tidy$top_tags))
## Knowing that Vegetarian, vegan and gluten free has already been dealt
##Getting rid of special diet values
#restaurants_tidy_2 <- restaurants_tidy %>%
#  filter_at(vars(starts_with("top_tags")), any_vars(!(. %in% c("Vegetarian Friendly",
#                                                               "Vegan Options", "Gluten Free Options",
#                                                               "Cheap Eats", "Mid-range", "Fine Dining"))))

restaurants_tidy$top_tags <- lapply(restaurants_tidy$top_tags, function(tags) {
  tags <- tags[!(tags %in% c("Vegetarian Friendly", "Vegan Options", "Gluten Free Options",
                             "Cheap Eats", "Mid-range", "Fine Dining"))]
  if (length(tags) == 0) {
    tags <- NA
  }
  tags
})

# Reorganizing with price range-------------
##Creating a new variable: average price
restaurants_tidy <-  restaurants_tidy |>
 mutate(
  minimum_range = as.numeric(str_extract(price_range, "\\d+(\\.\\d+)?")),
 maximum_range = as.numeric(str_extract_all(price_range, "\\d+(\\.\\d+)?") %>% sapply(function(x) x[2])),
  avg_price = (minimum_range + maximum_range) / 2
) |>
select(-c(minimum_range, maximum_range))

#Seeing unique values---------------
##Meals
unique(restaurants_tidy$meals)

##Cuisines
unique(restaurants_tidy$cuisines)

##Unique values of top_tags
unique(restaurants_tidy$top_tags)

#Showing why I work with cuisines and not top_tags---------------
# Assuming 'restaurants_tidy' is your dataset
table_data <- head(select(restaurants_tidy, top_tags, cuisines), 10)

## Create a gt table
gt_table <- gt(table_data) %>%
  tab_spanner(
    label = "Comparison between cuisines and top_tags",
    columns = c(everything())
  )

## Save the gt table as an image
gtsave(gt_table, file = "figures/table_image.png")

# Wrangling the top_tags---------------------
restaurants_tidy_3 <- mutate(restaurants_tidy_2, top_tags = lapply(top_tags, function(tags) {
  if (!is.null(tags)) {
    tags[tags %in% c("Pizza", "Northern Italy", "Southern Italy")] <- "Italian"
    tags[tags == "Sushi"] <- "Japanese"
    tags[tags %in% c("Brew Pub", "Wine Bar", "Pub")] <- "Bar"
    tags <- gsub("Delivery only", "", tags)
    tags <- unique(tags)
  }
  return(tags)
}))

#restaurants_tidy_3 <- restaurants_tidy_3 |>
#  rename(top_tags = "cuisines")
  #  mutate(cuisines = trimws(cuisines)) |>
#  separate_rows(meals, sep = ", ") |>
#  mutate(cuisines_present = 1) |>
#  pivot_wider(names_from = cuisines, values_from = cuisines_present, values_fill = NA)

#restaurants_tidy_3 <- restaurants_tidy %>%
#  unnest(top_tags) %>%
#  mutate(tag_present = 1) %>%
#  pivot_wider(names_from = top_tags, values_from = tag_present, values_fill = 0)

# Dropping variables that won't be needed for the analysis----------
## After all the wrangling data find the NA for each variable
missingness <- restaurants_tidy_3 |>
  summarise(across(-avg_price, ~sum(is.na(.))))

#address,original location, keywords, feautures, cuisine, popularity