# ------------------------------------------------------------------------- #
#                              Dubois Challange                             #    
# ------------------------------------------------------------------------- #

# Challenge 3. Occupation of negroes and whites in Georgia


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(scales)
library(showtext)
library(cowplot)
library(magick)
library(grid)
library(gridExtra) 

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


colores_pie <- c('Invisible' = unname(palette["paper"]),
                 'Agriculture, Fisheries and Mining' = unname(palette["red"]),
                 'Manufacturing and Mechanical Industries' = unname(palette["blue"]),
                 'Domestic and Personal Service' = unname(palette["gold"]),
                 'Professions' = unname(palette["brown"]),
                 'Trade and Transportation' = unname(palette["paper"]))

# Data, after making a clone of the forked repository:

data_db <- read_csv("challenge/challenge04/data.csv")

# Fonts

# font_import()
# loadfonts(device = "win")
font_add_google(name = "Space Mono", family = "space")
font_add_google(name = "Almendra Display", family = "almendra")

showtext_auto()


# Reescalando la base para generar los "abanicos" -------------------------


data_db <- data_db %>% 
  pivot_longer(cols = 2:3,names_to = "category",values_to = "percent") %>% 
  group_by(Year) %>%  
  mutate(percent = percent/sum(percent))

data_label <- data_db %>% 
  filter(category == "Free") %>% 
  mutate(
         percent_l = if_else(Year == 1800,0.11,percent),
         percent_l = percent_l*100, 
         percent = if_else(percent_l == 100,0.11,percent),
         percent = 1 - percent,
         label = str_c(percent_l,"%") )

plot <- data_db %>% 
  ggplot() + 
  geom_area(mapping = aes(x = Year,y  = percent,fill = category)) +
  geom_text(data = data_label,mapping = aes(x = Year,y = percent,label = label), vjust = -1,size = 5 ) +
  scale_x_continuous(breaks = seq(1790,1890,10),position = "top") +
  scale_fill_manual(values = c(unname(palette["green"]),unname(palette["black"])))  +
  theme(
    panel.background=element_blank(), 
    panel.grid.major=element_blank(), 
    panel.grid.minor=element_blank(), 
    panel.spacing = unit(c(0, 5, 0,5), "cm"),       
    axis.ticks=element_blank(), 
    text = element_text(family = "space"),
    axis.text.y=element_blank(), 
    axis.title.x=element_blank(), 
    axis.title.y=element_blank(),
    axis.text.x.top = element_text(size = 17,vjust = -15,face = "bold"),
    plot.background = element_rect(fill = palette[["paper"]],colour = NA),
    plot.margin = unit(c(6.8,-0.2,-0.5,-0.2), "cm"),  # Edited code
    legend.position = 'none')


plot <- ggdraw(plot) +
  draw_text(text = "PROPORTION OF FREEMEN AND SLAVES AMONG AMERICAN NEGROES .",
            y = 0.98,family = "space",size = 16,fontface = "bold") +
  draw_text(text = "PROPORTION DES NÈGRES ET ESCLAVES EN AMÈRIQUE .",
            y = 0.92,family = "space",size = 16,fontface = "bold") +
  draw_text(text = "DONE BY ATLANTA UNIVERSITY",
            y = 0.87,family = "space",size = 10) +
  draw_text(text = "FREE - LIBRE",
            y = 0.73,family = "space",size = 18) +
  draw_text(text = "SLAVES\nESCLAVES",
            y = 0.4,family = "space",size = 22,
            color = "white",fontface = "bold")

png("challenge/challenge04/solution_ch4_alexb.png",width = 800,height = 1025,units = "px")

plot

dev.off()

