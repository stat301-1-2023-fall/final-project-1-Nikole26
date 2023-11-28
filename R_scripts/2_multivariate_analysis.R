# Loading libraries-----------
library(tidyverse)
library(skimr)
library(DT)
library(stringr)
library(gt)
library(webshot2)
library(ggplot2)
library(plotly)
library(tidyr)
library(knitr)
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
  rename(cuisines = top_tags)

# Multivariate Analysis--------------------
## Latitude vs longitude
visualization_data <- na.omit(data_final)
plot_ly(visualization_data, x = ~longitude, y = ~latitude, text = ~city, type = 'scatter', mode = 'markers') %>%
  layout(title = "Cities Scatter Plot", xaxis = list(title = "Longitude"), yaxis = list(title = "Latitude"))

## Food Rating Highest and lowest values

###Since most of the restaurants offer a cuisine in Europe, we will narrow the 
###ratings within restaurant that offer European cuisine, bc that's what most inrested about

european_restaurants <- filter(data_final, cuisines == "Europe")

### Highest ratings
top_food_ratings <- european_restaurants |>
  arrange(desc(food_rating)) |>
  head(10)  

ggplot(top_food_ratings, aes(x = restaurant_name, y = food_rating, fill = restaurant_name)) +
  geom_bar(stat = "identity") +
  ggtitle("Top 10 European Restaurants based on Food Rating") +
  xlab("Restaurant") +
  ylab("Food Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Lowest ratings
bottom_food_ratings <- european_restaurants |>
  arrange(food_rating) |>
  head(10)

ggplot(bottom_food_ratings, aes(x = reorder(restaurant_name, -food_rating), y = food_rating, fill = paste(restaurant_name, city))) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Bottom 10 European Restaurants based on Food Rating") +
  xlab("Restaurant") +
  ylab("Food Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Service Rating Highest and lowest values

### Highest ratings
top_service_rating <- european_restaurants |>
  arrange(desc(service_rating)) |>
  head(10)  

ggplot(top_service_rating, aes(x = restaurant_name, y = service_rating)) +
  geom_bar(stat = "identity") +
  ggtitle("Top 10 European Restaurants based on Service Rating") +
  xlab("Restaurant") +
  ylab("Service Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Lowest ratings
bottom_service_rating <- european_restaurants |>
  arrange(service_rating) |>
  head(10)

ggplot(bottom_service_rating, aes(x = reorder(restaurant_name, -service_rating), y = service_rating)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Bottom 10 European Restaurants based on Service Rating") +
  xlab("Restaurant") +
  ylab("Service Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Value Rating Highest and lowest values
### Highest ratings
top_value_rating <- european_restaurants |>
  arrange(desc(value_rating)) |>
  head(10)  

ggplot(top_value_rating, aes(x = restaurant_name, y = value_rating)) +
  geom_bar(stat = "identity") +
  ggtitle("Top 10 European Restaurants based on Value Rating") +
  xlab("Restaurant") +
  ylab("Value Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
top_value_ratings <- european_restaurants %>% top_n(10, wt = value_rating)

### Lowest ratings
bottom_value_rating <- european_restaurants |>
  arrange(value_rating) |>
  head(10)

ggplot(bottom_value_rating, aes(x = reorder(restaurant_name, -value_rating), y = value_rating)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Bottom 10 European Restaurants based on Service Rating") +
  xlab("Restaurant") +
  ylab("Value Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Looking at the Flush restaurants
european_restaurants |>
  filter(restaurant_name == "Flunch") |>
  select(city, food_rating, service_rating, value_rating) |>
  kable()
  

# Combine the top 10 restaurants for each category into a single data frame
#top_restaurants <- bind_rows(
#  mutate(top_food_ratings, category = "Food Rating"),
#  mutate(top_service_ratings, category = "Service Rating"),
#  mutate(top_value_ratings, category = "Value Rating")
#)

# Create the plot
#ggplot(top_restaurants, aes(x = reorder(restaurant_name, -rating), y = rating, fill = category)) +
#  geom_bar(stat = "identity", position = "dodge") +
#  labs(title = "Top 10 Restaurants Comparison",
#       y = "Rating",
#      x = "Restaurant Name",
#       fill = "Rating Category") +
#  theme_minimal() +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))
