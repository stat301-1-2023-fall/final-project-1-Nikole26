# Loading libraries-----------
library(tidyverse)
library(skimr)
library(stringr)
library(ggplot2)
library(plotly)
library(tidyr)
library(knitr)
library(ggrepel)
library(kableExtra)
# Loading data -----------------
data <- read_rds("data/data_cleaned.rds")

# Digging deeper France--------------------
## Price level Frequency for Top French Restaurants
french_restaurants <- data |>
  filter(country == "France")

ggplot(french_restaurants, aes(x = price_level, fill = price_level)) +
  geom_bar() +
  labs(title = "Distribution of French Restaurants by Price Level",
       x = "Price Level",
       y = "Number of Restaurants") +
  scale_fill_manual(values = c("cheap" = "darkgreen", "mid-range" = "orange", "expensive" = "red"))

## Top Rated Resturants (avg_rating and price level)
top_rated_restaurants <- french_restaurants[french_restaurants$avg_rating == max(french_restaurants$avg_rating), ]
kable(top_rated_restaurants[, c("restaurant_name", "avg_rating", "price_level")], format = "html") %>%
  kable_styling()

### Number of restaurants per pricel level category
price_level_counts <- top_rated_restaurants |>
  group_by(price_level) |>
  summarise(count = n())

tableHTML(price_level_counts)

# How many french restaurants have an award?
french_restaurants <- french_restaurants %>%
  mutate(awards_indicator = ifelse(!is.na(awards) & awards != "none", "yes", "no"))

awards_count <- table(french_restaurants$awards_indicator)

# Calculate percentages
awards_percentage <- prop.table(awards_count) * 100

# Create a data frame for ggplot
awards_data <- data.frame(category = names(awards_percentage), percentage = as.numeric(awards_percentage))

# Create a simple pie chart with percentages
ggplot(awards_data, aes(x = "", y = percentage, fill = category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Distribution of Restaurants with Awards") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("yes" = "darkblue", "no" = "lightblue")) +  # Customize fill colors as needed
  geom_text(aes(label = sprintf("%.1f%%", percentage)), position = position_stack(vjust = 0.5), size = 4)

