pacman::p_load(pacman,dplyr,GGally,ggplot2,ggthemes,ggvis,httr,lubridate,plotly,rio,rmarkdown,shiny,stringr,tidyr)

library(readxl)
Steam_Games_Dataset <- read_excel("C:/Users/wilem/OneDrive/Desktop/Desktop Apps/UNM/Year 3/Fundamentals of InfoViz/CW1/Information-Visualisation-on-Steam-Game-Datasets/Steam Games Dataset.xlsx")

# Create a working dataframe
df <- Steam_Games_Dataset

#Filter rows based on steam categorical reviews for 'Recent Reviews Summary':
allowed_reviews <- c("Overwhelmingly Positive", 
                     "Very Positive", 
                     "Mostly Positive", 
                     "Positive",
                     "Overwhelmingly Negative", 
                     "Very Negative", 
                     "Mostly Negative", 
                     "Negative")
df <- df[df$`Recent Reviews Summary` %in% allowed_reviews, ]

#If 'All Reviews Summary' is empty (or NA), assume its 'Recent Reviews Summary':
df$`All Reviews Summary`[is.na(df$`All Reviews Summary`)] <- df$`Recent Reviews Summary`[is.na(df$`All Reviews Summary`)]

#Replace FREE with 0 in 'Original Price' and 'Discounted Price'
df$`Original Price`[df$`Original Price` == "FREE"] <- 0
df$`Discounted Price`[df$`Discounted Price` == "FREE"] <- 0

#Remove the $ sign on both dataset columns
df$`Original Price` <- as.numeric(gsub("\\$", "", df$`Original Price`))
df$`Discounted Price` <- as.numeric(gsub("\\$", "", df$`Discounted Price`))

# Convert prices to numeric again to ensure they are numeric
df$`Original Price` <- as.numeric(as.character(df$`Original Price`))
df$`Discounted Price` <- as.numeric(as.character(df$`Discounted Price`))

#Remove 'Non-Available (NA)' values on both column
df <- df[!is.na(df$`Original Price`) & !is.na(df$`Discounted Price`), ]

# Convert Review Category into x-axis
review_levels <- c("Overwhelmingly Negative", 
                   "Very Negative", 
                   "Mostly Negative", 
                   "Negative", 
                   "Positive", 
                   "Mostly Positive", 
                   "Very Positive", 
                   "Overwhelmingly Positive")
df$`All Reviews Summary` <- factor(df$`All Reviews Summary`, levels = review_levels)

# Filter out games with 'Original Price' and 'Discounted Price' greater than $70
df <- df[df$`Original Price` <= 70 & df$`Discounted Price` <= 70, ]

# Add a new column 'Price Difference'
df$`Price Difference` <- df$`Original Price` - df$`Discounted Price`

# Filter out games where it has no discount
df <- df[df$`Price Difference` > 0, ]

View(df)

# Create a combined 'Price' column and a 'Type' column to distinguish between original and discounted prices
df_long <- df %>%
  select(`All Reviews Summary`, `Original Price`, `Discounted Price`) %>%
  gather(key = "Type", value = "Price", -`All Reviews Summary`)

# Generate the density plot faceted by review category
density_plot <- ggplot(df_long, aes(x = Price, fill = Type)) +
  geom_density(alpha = 0.7) +
  facet_wrap(~ `All Reviews Summary`, scales = "free") +
  scale_fill_manual(values = c("Original Price" = "blue", "Discounted Price" = "red")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Density Plot of Game Prices by User Review Category",
       x = "Price",
       y = "Density")

# Display the plot
print(density_plot)