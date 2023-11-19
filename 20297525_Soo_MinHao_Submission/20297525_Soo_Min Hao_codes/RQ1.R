#Load libraries
pacman::p_load(pacman, 
               dplyr, 
               GGally, 
               ggmosaic, 
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

# Data cleaning and transformation
# Remove "UTC" from Release Date and convert to Date format
Steam_Games_Dataset$`Release Date` <- as.Date(gsub(" UTC", "", Steam_Games_Dataset$`Release Date`), format="%Y-%m-%d")

# Filter out games released before 2003
Steam_Games_Dataset <- Steam_Games_Dataset %>%
  filter(year(`Release Date`) >= 2003)

# Preparing data for visualization
all_years <- data.frame(Year = 2003:2023)
game_release_data <- Steam_Games_Dataset %>%
  group_by(Year = year(`Release Date`)) %>%
  summarise(Count = n()) %>%
  right_join(all_years, by = "Year") %>%
  replace_na(list(Count = 0))

# Visualization using ggplot
p <- game_release_data %>%
  ggplot(aes(x = Year, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_continuous(breaks = 2003:2023, minor_breaks = NULL) +
  labs(title = "Number of Games Released Each Year on Steam Platform",
       x = "Year", y = "Number of Games Released on That Year") +
  theme_minimal()

# Convert ggplot to plotly for interactivity
interactive_plot <- ggplotly(p)
interactive_plot

