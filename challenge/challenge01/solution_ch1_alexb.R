# ------------------------------------------------------------------------- #
#                              Dubois Challange                             #    
# ------------------------------------------------------------------------- #

# Challenge 1. Comparative increase of white and colored population of Georgia


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(scales)
library(extrafont)


# Inputs: -----------------------------------------------------------------

# Color palette (dubois style):

palette <- c(
  black = "#000000",
  brown = "#654321",
  tan = "#d2b48c",
  gold = "#ffd700",
  pink = "#ffc0cb",
  red = "#dc143c",
  green = "#00aa00",
  blue = "#4682b4",
  paper = "#f7e6c6"
)

# Data, after making a clone of the forked repository:

data_db <- read_csv("challenge/challenge01/data.csv")

# Fonts

# font_import()
loadfonts(device = "win")


# Graphic: ----------------------------------------------------------------



plot_challenge_1 <- data_db %>% 
  pivot_longer(cols = 2:3,
               names_to = "population",
               values_to = "number") %>%
  mutate(population = str_to_upper(population)) %>% 
  ggplot(aes(x = Year, y = number,group = population)) + 
  geom_line(aes(linetype = population),show.legend = T) + 
  scale_y_reverse(breaks = seq(0,100,by = 5)) +
  scale_x_continuous(breaks = seq(1790,1890,by = 10)) + 
  scale_linetype_manual(values = c("dashed","solid"))+
  labs(title = "COMPARATIVE INCREASE OF WHITE AND COLORED \nPOPULATION OF GEORGIA",
       linetype = "",
       x = "",
       y = "PERCENT")+
  coord_flip() + 
  theme_linedraw()+
  theme(legend.position = "bottom",
        axis.ticks = element_blank(),
        panel.grid.major = element_line(color = palette["red"]),
        panel.grid.minor =  element_blank(),
        panel.background = element_rect(fill = palette["paper"]),
        plot.background = element_rect(fill = palette["paper"]),
        title = element_text(family = "Copperplate Gothic Bold"),
        text = element_text(family = "Copperplate Gothic Light"),
        plot.title = element_text(hjust = 0.5, size = 18, margin=margin(0,0,30,0)),
        legend.background = element_rect(fill = palette["paper"]),
        legend.key  = element_rect(fill = palette["paper"]))

  
  
  
