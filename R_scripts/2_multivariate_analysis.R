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
data <- read_rds("data/data_cleaned.rds")

# Multivariate Analysis--------------------
## Latitude vs longitude
visualization_data <- na.omit(data)
plot_ly(visualization_data, x = ~longitude, y = ~latitude, text = ~city, type = 'scatter', mode = 'markers') %>%
  layout(title = "Cities Scatter Plot", xaxis = list(title = "Longitude"), yaxis = list(title = "Latitude"))

## Food Rating Highest and lowest values

###Since most of the restaurants offer a cuisine in Europe, we will narrow the 
###ratings within restaurant that offer European cuisine, bc that's what most inrested about

european_cuisine <- filter(data, cuisines == "Europe")

### Highest ratings
top_food_ratings <- european_cuisine |>
  arrange(desc(food_rating)) |>
  head(10)  

ggplot(top_food_ratings, aes(x = restaurant_name, y = food_rating)) +
  geom_bar(stat = "identity") +
  ggtitle("Top 10 European Restaurants based on Food Rating") +
  xlab("Restaurant") +
  ylab("Food Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Lowest ratings
bottom_food_ratings <- european_cuisine |>
  arrange(food_rating) |>
  head(10)

ggplot(bottom_food_ratings, aes(x = restaurant_name, y = food_rating, fill = paste(restaurant_name, city))) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Bottom 10 European Restaurants based on Food Rating") +
  xlab("Restaurant") +
  ylab("Food Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Service Rating Highest and lowest values

### Highest ratings
top_service_rating <- european_cuisine |>
  arrange(desc(service_rating)) |>
  head(10)  

ggplot(top_service_rating, aes(x = restaurant_name, y = service_rating)) +
  geom_bar(stat = "identity") +
  ggtitle("Top 10 European Restaurants based on Service Rating") +
  xlab("Restaurant") +
  ylab("Service Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Lowest ratings
bottom_service_rating <- european_cuisine |>
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
top_value_rating <- european_cuisine |>
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
bottom_value_rating <- european_cuisine |>
  arrange(value_rating) |>
  head(10)

ggplot(bottom_value_rating, aes(x = reorder(restaurant_name, -value_rating), y = value_rating)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Bottom 10 European Restaurants based on Value Rating") +
  xlab("Restaurant") +
  ylab("Value Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Looking at the Flush restaurants
european_cuisine |>
  filter(restaurant_name == "Flunch") |>
  select(restaurant_name, city, food_rating, service_rating, value_rating, avg_rating) |>
  arrange(desc(food_rating), desc(service_rating), desc(value_rating)) |>
  kable()
  
# Price vs Average Rating
ggplot(european_cuisine, aes(x = avg_price, y = avg_rating)) +
  geom_point() +
  labs(title = "Price vs Average Rating",
       x = "Price",
       y = "Average Rating") +
  theme_minimal()

####Maybe lot's of trip advisor reviews are mostly withing resasonable budget,
####or budget-frfiendly food

# Expensive and avr rating
most_expensive <- european_cuisine %>%
  arrange(desc(avg_price)) %>%
  head(5)
ggplot(most_expensive, aes(x = reorder(restaurant_name, -avg_price), y = avg_price, fill = avg_rating)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Most Expensive Restaurants: Price and Average Rating",
       x = "Restaurant",
       y = "Price",
       fill = "Average Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Price level vs top rating



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
