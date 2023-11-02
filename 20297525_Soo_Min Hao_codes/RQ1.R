#Load libraries
pacman::p_load(pacman,dplyr,GGally,ggplot2,ggthemes,ggvis,httr,lubridate,plotly,rio,rmarkdown,shiny,stringr,tidyr)
library(readxl)

#Read Dataset
Steam_Games_Dataset <- read_excel("C:/Users/wilem/OneDrive/Desktop/Desktop Apps/UNM/Year 3/Fundamentals of InfoViz/CW1/Information-Visualisation-on-Steam-Game-Datasets/Steam Games Dataset.xlsx")

##For Number of Games Released On Steam Platform Each Year
# Adjust Release Date by removing "UTC" and convert to Date format
Steam_Games_Dataset$`Release Date` <- as.Date(gsub(" UTC", "", Steam_Games_Dataset$`Release Date`), format="%Y-%m-%d")
all_years <- data.frame(Year = 2000:2023)

# Plotting with ggplot
p <- Steam_Games_Dataset %>% 
  group_by(Year = year(`Release Date`)) %>%
  summarise(Count = n()) %>%
  right_join(all_years, by = "Year") %>%
  replace_na(list(Count = 0)) %>%
  ggplot(aes(x = Year, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_continuous(breaks = 2000:2023, minor_breaks = NULL) +
  labs(title = "Number of Games Released Each Year on Steam Platform",
       x = "Year", y = "Number of Games") +
  theme_minimal()

#ggplot to plotly
interactive_plot <- ggplotly(p)
interactive_plot

##For Average Price Trend of Games Over The Years
