# ------------------------------------------------------------------------- #
#                              Dubois Challange                             #    
# ------------------------------------------------------------------------- #

# Challenge 3. Occupation of negroes and whites in Georgia


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(scales)
library(extrafont)
library(showtext)
library(cowplot)

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

data_db <- read_csv("challenge/challenge03/data.csv")

# Fonts

# font_import()
# loadfonts(device = "win")
font_add_google(name = "Space Mono", family = "space")
font_add_google(name = "Almendra Display", family = "almendra")

showtext_auto()


# Reescalando la base para generar los "abanicos" -------------------------


auxiliar <- distinct(data_db,Group) %>% 
  mutate(Percentage = 70,
         Occupation = "Invisible")

fct_level <- data_db %>% pull(Occupation) %>% unique

data_db <- data_db %>% 
  mutate(
    value = Percentage,
    Percentage = 0.3*Percentage,
    label = percent(value/100)
  ) %>% 
  bind_rows(auxiliar) %>% 
  mutate(Occupation = factor(Occupation,
                             levels = c("Invisible",fct_level)),
         transparency = if_else(Occupation == "Invisible",1,0),
         size_text = if_else(value < 20,1,0))

data_db <- data_db %>%
  group_by(Group) %>% 
  arrange(Group,desc(Occupation)) %>% 
  mutate(ypos = cumsum(Percentage)-0.5*Percentage)


plot_list <- map2(.x = data_db %>% 
       split(.$Group),
     .y = list(
       c(0,0,-325,0),
       c(-325,0,0,0)
     ),
     .f = ~{
       
       ggplot(data = .x,mapping = aes(x = "",y = Percentage,fill = Occupation)) +
         geom_bar(stat = "identity") + 
         geom_text(aes(y = ypos,label = label))
     } 
  ) 


cowplot::plot_grid(plotlist = plot_list)
