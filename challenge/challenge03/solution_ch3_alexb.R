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

data_db <- read_csv("challenge/challenge02/data.csv")

# Fonts

# font_import()
# loadfonts(device = "win")
font_add_google(name = "Space Mono", family = "space")
font_add_google(name = "Almendra Display", family = "almendra")

showtext_auto()


