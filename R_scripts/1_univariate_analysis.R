# Loading libraries-----------
library(tidyverse)
library(skimr)
library(DT)
library(stringr)
library(gt)
library(webshot2)
library(ggplot2)

# Loading data -----------------
data <- read_csv("data/tripadvisor_european_restaurants.csv")

# Cleaning and Wrangling------------
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
  
  tags[tags %in% c("Egyptian", "Egypt", "African", "Algerian", "Contemporary")] <- "Africa"
  
  tags[tags %in% c("Chilean", "Venezuelan", "Peruvian", "South American",
                   "Argentinian", "Latin", "Brazilian")] <- "South America"
  
  tags[tags %in% c("Mexican", "Central American", "Native American", "Cuban", "Latin",
                   "Caribbean", "Canadian", "American")] <- "North America"
  
  tags[tags %in% c("New Zealand", "Polynesian")] <- "Oceania"
  
  tags <- tags[!(tags %in% c("Delivery Only", "Dinner", "Diner", "Healthy", "Barbecue", "Caucasian",
                             "Quick Bites", "Cajun & Creole", "International", "Deli", "Dessert",
                             "Bakeries", "Fast food", "Cafe", "Street Food", "Seafood", "Gastropub", 
                             "Dining bars", "Grill", "Steakhouse"))]
  
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

#Univariate analysis---------------------
## For 'resturant_name'
## As 'resturant_name' is a categorical variable, a bar plot might be useful
top_restaurants <- data_final %>%
  count(restaurant_name) %>%
  top_n(10, wt = n)  # Adjust the number as needed

# Filter the data for the top restaurants
data_top <- data_final %>%
  filter(restaurant_name %in% top_restaurants$restaurant_name)

# Bar plot for the top restaurants
top_restaurants_plot <- ggplot(data_top, aes(x = restaurant_name)) +
  geom_bar(fill = "skyblue", color = "black", alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top Restaurants by Frequency",
       x = "Restaurant Names",
       y = "Count") 
ggsave("figures/top_restaurants_plot.png", top_restaurants_plot, width = 10, height = 6)


## For 'avg_rating'
avg_rating_plot <- ggplot(data_final, aes(x = avg_rating)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Average Ratings",
       x = "Average Rating",
       y = "Frequency") 
ggsave("figures/avg_rating_plot.png", avg_rating_plot, width = 10, height = 6)


# Univariate analysis for 'open_days_per_week'
data_final$open_days_per_week <- factor(data_final$open_days_per_week,
                                          levels = c(1, 2, 3, 4, 5, 6, 7),
                                          labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
open_days_per_week_plot <- ggplot(data_final, aes(x = open_days_per_week)) +
  geom_bar(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Open Days per Week",
       x = "Open Days per Week",
       y = "Count") +
  scale_x_discrete(labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) 

ggsave("figures/open_days_per_week_plot.png", open_days_per_week_plot, width = 10, height = 6)


# Univariate analysis for 'country'
country_plot <- ggplot(data_final, aes(x = country)) +
  geom_bar(fill = "skyblue", color = "black", alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Countries",
       x = "Country",
       y = "Count") 
ggsave("figures/country_plot.png", country_plot, width = 10, height = 6)


# Univariate analysis for 'price_level'
price_level_plot <- ggplot(data_final, aes(x = price_level)) +
  geom_bar(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Price Levels",
       x = "Price Level",
       y = "Count")
ggsave("figures/price_level_plot.png", price_level_plot, width = 10, height = 6)

#For 'special_diets'
special_diets_plot <- ggplot(data_final, aes(x = special_diets)) +
  geom_bar(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Special Diets",
       x = "Special Diets",
       y = "Count")
ggsave("figures/special_diets_plot.png", special_diets_plot, width = 10, height = 6)

## For 'avg_price'
avg_price <- ggplot(data_final, aes(x = avg_price)) +
  geom_histogram(bins = 50, binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Average Price",
       x = "Average Price",
       y = "Frequency") 
ggsave("figures/avg_price.png", avg_price, width = 10, height = 6) 