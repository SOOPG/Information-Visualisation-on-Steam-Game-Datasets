#Load libraries
pacman::p_load(pacman, 
               dplyr, 
               GGally, 
               ggplot2, 
               ggthemes, 
               ggvis, 
               httr, 
               lubridate, 
               plotly, 
               rio, 
               rmarkdown, 
               scales, 
               shiny, 
               stringr, 
               tidyr, 
               readxl)

# Relative path to the dataset from the R script
file_path <- "../Steam Games Dataset.xlsx"
Steam_Games_Dataset <- readxl::read_excel(file_path)

# Filter rows based on categorical reviews for 'Recent Reviews Summary'
allowed_reviews <- c("Overwhelmingly Positive", 
                     "Very Positive", 
                     "Mostly Positive", 
                     "Positive",
                     "Overwhelmingly Negative", 
                     "Very Negative", 
                     "Mostly Negative", 
                     "Negative")
Steam_Games_Dataset <- Steam_Games_Dataset[Steam_Games_Dataset$`Recent Reviews Summary` %in% allowed_reviews, ]

# If 'All Reviews Summary' is empty (or NA), use 'Recent Reviews Summary'
Steam_Games_Dataset$`All Reviews Summary`[is.na(Steam_Games_Dataset$`All Reviews Summary`)] <- Steam_Games_Dataset$`Recent Reviews Summary`[is.na(Steam_Games_Dataset$`All Reviews Summary`)]

# Replace 'FREE' with 0 in 'Original Price' and 'Discounted Price'
Steam_Games_Dataset$`Original Price`[Steam_Games_Dataset$`Original Price` == "FREE"] <- 0
Steam_Games_Dataset$`Discounted Price`[Steam_Games_Dataset$`Discounted Price` == "FREE"] <- 0

# Remove the $ sign and convert prices to numeric values
Steam_Games_Dataset$`Original Price` <- as.numeric(gsub("\\$", "", Steam_Games_Dataset$`Original Price`))
Steam_Games_Dataset$`Discounted Price` <- as.numeric(gsub("\\$", "", Steam_Games_Dataset$`Discounted Price`))

# Ensure prices are numeric
Steam_Games_Dataset$`Original Price` <- as.numeric(as.character(Steam_Games_Dataset$`Original Price`))
Steam_Games_Dataset$`Discounted Price` <- as.numeric(as.character(Steam_Games_Dataset$`Discounted Price`))

# Remove rows with NA values in both price columns
Steam_Games_Dataset <- Steam_Games_Dataset[!is.na(Steam_Games_Dataset$`Original Price`) & !is.na(Steam_Games_Dataset$`Discounted Price`), ]

# Factorize 'All Reviews Summary' based on the review levels
review_levels <- c("Overwhelmingly Negative", 
                   "Very Negative", 
                   "Mostly Negative", 
                   "Negative", 
                   "Positive", 
                   "Mostly Positive", 
                   "Very Positive", 
                   "Overwhelmingly Positive")
Steam_Games_Dataset$`All Reviews Summary` <- factor(Steam_Games_Dataset$`All Reviews Summary`, levels = review_levels)

# Filter out games with prices greater than $70
Steam_Games_Dataset <- Steam_Games_Dataset[Steam_Games_Dataset$`Original Price` <= 70 & Steam_Games_Dataset$`Discounted Price` <= 70, ]

# Add a 'Price Difference' column
Steam_Games_Dataset$`Price Difference` <- Steam_Games_Dataset$`Original Price` - Steam_Games_Dataset$`Discounted Price`

# Filter out games with no discount
Steam_Games_Dataset <- Steam_Games_Dataset[Steam_Games_Dataset$`Price Difference` > 0, ]

# Prepare the data for visualization
prices_long_format <- Steam_Games_Dataset %>%
  select(`All Reviews Summary`, `Original Price`, `Discounted Price`) %>%
  gather(key = "Type", value = "Price", -`All Reviews Summary`)

# Generate and display the density plot faceted by review category
density_plot <- ggplot(prices_long_format, aes(x = Price, fill = Type)) +
  geom_density(alpha = 0.7) +
  facet_wrap(~ `All Reviews Summary`, scales = "free") +
  scale_fill_manual(values = c("Original Price" = "blue", "Discounted Price" = "red")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Density Plot of Game Prices until 70$ by User Review Category",
       x = "Price",
       y = "Density")

print(density_plot)
