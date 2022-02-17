# ------------------------------------------------------------------------- #
#                              Dubois Challange                             #    
# ------------------------------------------------------------------------- #

# Challenge 1. Comparative increase of white and colored population of Georgia


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(scales)
library(extrafont)
library(showtext)
library(cowplot)
library(ggbrace)


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
font_add_google(name = "Space Mono", family = "space")
font_add_google(name = "Cormorant Garamond", family = "corgar")
showtext_auto()



# Preprocessing -----------------------------------------------------------

data_db <-
  data_db %>% 
  pivot_longer(cols = 2:3,
               names_to = "population",
               values_to = "number") %>%
  mutate(population = str_to_upper(population)) 

# Graphic: ----------------------------------------------------------------

plot_challenge_1 <- data_db %>% 
  ggplot(aes(x = Year, y = number,group = population)) + 
  geom_line(aes(linetype = population),show.legend = T) + 
  geom_brace(aes(y=c(0,100), x=c(1788,1784)), inherit.data=F, rotate=270) +
  scale_y_reverse(breaks = seq(0,100,by = 5),expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1790,1890,by = 10),expand = c(0, 0)) + 
  scale_linetype_manual(values = c("solid","dashed"))+
  labs(title = "COMPARATIVE INCREASE OF WHITE AND COLORED \nPOPULATION OF GEORGIA.",
       linetype = "",
       x = "",
       y = "PERCENTS")+
  coord_flip(x=c(1790,1890), clip = "off") +
  # coord_cartesian(y=c(1790,1890), clip = "off")+
  theme_linedraw()+
  theme(
        text = element_text(family = "sans"),
        axis.ticks = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 6),
        axis.title.x = element_text(margin = margin(t = 40,r = 0,b = 0,0)),
        panel.grid.major = element_line(color = palette["red"],size = 0.5),
        panel.grid.minor =  element_blank(),
        panel.background = element_rect(fill = palette["paper"]),
        plot.background = element_rect(fill = palette["paper"]),
        plot.title = element_text(family = "space",face = "bold",
                                  hjust = 0.4, size = 20, 
                                  margin=margin(5,0,50,0)),
        plot.margin = margin(t = 1, r = 4, b = 3, l = 4, "cm"),
        legend.background = element_blank(),
        legend.key  = element_rect(fill = palette["paper"]),
        legend.position = c(0.7,-0.1),
        legend.direction = "horizontal",
        legend.text = element_text(margin = margin(r = 250, unit = "pt"),hjust = 1,size = 6),
        legend.key.size = unit(2, 'cm')
        )




# Printing the plot: ------------------------------------------------------

  
  png("challenge/challenge01/challenge_1_solution.png",width = 800,height = 1025,units = "px")
  
  print(plot_challenge_1)
  
  dev.off()
  
