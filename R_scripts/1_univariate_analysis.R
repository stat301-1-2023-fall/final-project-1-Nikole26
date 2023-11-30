# Loading libraries-----------
library(tidyverse)
library(skimr)
library(DT)
library(stringr)
library(gt)
library(webshot2)
library(ggplot2)

# Loading data -----------------
data <- read_rds("data/data_cleaned.rds")

#Univariate analysis---------------------
## For 'resturant_name'
## As 'resturant_name' is a categorical variable, a bar plot might be useful
top_restaurants <- data %>%
  count(restaurant_name) %>%
  top_n(10, wt = n)  # Adjust the number as needed

# Filter the data for the top restaurants
data_top <- data %>%
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
avg_rating_plot <- ggplot(data, aes(x = avg_rating)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Average Ratings",
       x = "Average Rating",
       y = "Frequency") 
ggsave("figures/avg_rating_plot.png", avg_rating_plot, width = 10, height = 6)


# Univariate analysis for 'open_days_per_week'
data$open_days_per_week <- factor(data$open_days_per_week,
                                          levels = c(1, 2, 3, 4, 5, 6, 7),
                                          labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
open_days_per_week_plot <- ggplot(data, aes(x = open_days_per_week)) +
  geom_bar(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Open Days per Week",
       x = "Open Days per Week",
       y = "Count") +
  scale_x_discrete(labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) 

ggsave("figures/open_days_per_week_plot.png", open_days_per_week_plot, width = 10, height = 6)


# Univariate analysis for 'country'
country_plot <- ggplot(data, aes(x = country)) +
  geom_bar(fill = "skyblue", color = "black", alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Countries",
       x = "Country",
       y = "Count") 
ggsave("figures/country_plot.png", country_plot, width = 10, height = 6)


# Univariate analysis for 'price_level'
price_level_plot <- ggplot(data, aes(x = price_level)) +
  geom_bar(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Price Levels",
       x = "Price Level",
       y = "Count")
ggsave("figures/price_level_plot.png", price_level_plot, width = 10, height = 6)

#For 'special_diets'
special_diets_plot <- ggplot(data, aes(x = special_diets)) +
  geom_bar(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Special Diets",
       x = "Special Diets",
       y = "Count")
ggsave("figures/special_diets_plot.png", special_diets_plot, width = 10, height = 6)

## For 'avg_price'
avg_price <- ggplot(data, aes(x = avg_price)) +
  geom_histogram(bins = 50, binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Average Price",
       x = "Average Price",
       y = "Frequency") 
ggsave("figures/avg_price.png", avg_price, width = 10, height = 6) 

## For 'total number of reviews'
ggplot(data, aes(x = total_reviews_count)) +
  geom_histogram(bins = 100, binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of a Total Number of Reviews",
       x = "Total Number of Reviews",
       y = "Count") 

## For Cuisines
data$cuisines <- ifelse(is.na(data$cuisines) | data$cuisines == "", "Unknown", data$cuisines)

### Split the cuisines variable into multiple rows
split_cuisines <- data |>
  separate_rows(cuisines, sep = ", ") |>
  mutate(cuisines = trimws(cuisines))  # Remove leading/trailing whitespaces

### Plotting (assuming a simple bar plot)
ggplot(split_cuisines, aes(x = cuisines)) +
  geom_bar() +
  labs(title = "Cuisines Plot", x = "Cuisine", y = "Count")