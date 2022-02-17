# ------------------------------------------------------------------------- #
#                              Dubois Challange                             #    
# ------------------------------------------------------------------------- #

# Challenge 2. Comparative increase of white and colored population of Georgia


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

data_db <- read_csv("challenge/challenge02/data.csv")

# Fonts

# font_import()
# loadfonts(device = "win")
font_add_google(name = "Space Mono", family = "space")
font_add_google(name = "Almendra Display", family = "almendra")

showtext_auto()


# Graphic: ----------------------------------------------------------------

data_db <-
  data_db %>% 
  pivot_longer(cols = 3:5,
               names_to = "variable",
               values_to = "value") %>%
  mutate(across(.cols = c(variable,Population,Age),str_to_upper),
         Age = str_replace_all(Age," ","\n"),
         Population = factor(Population,levels = c("NEGROES","GERMANY")),
         Age = factor(Age,levels = c("15-40","40-60","60\nAND\nOVER")),
         variable = factor(variable,levels = c("SINGLE","MARRIED","DIVORCED AND WIDOWED")) )

data_aux <- data_db %>% 
  select(variable,Age) %>% 
  unique %>% 
  mutate(value = NA)

data_text <- data_db %>% 
  filter(value > 10) %>% 
  group_by(Population,Age) %>% 
  arrange(Age,Population) %>% 
  mutate(cumsum = cumsum(value),
         cumsum_l = replace_na(lag(cumsum),0),
         position = (cumsum + cumsum_l)/2,
         value = percent(value/100))

plot_challenge_2 <-
 
  ggplot() + 
  geom_point(data = data_aux,aes(x = value,y = value,color = variable,fill = variable),size = 10) +
  geom_col(data = data_db ,aes(x = Population,
               y = value,
               fill = variable,
               group = Age),
           width = 0.5,
           position = "stack",show.legend = F) + 
  geom_text(data = data_text,aes(x = Population,y = position,label = value))+
  # geom_brace(aes(y=c(0,1), x=c(0,1),group = c), inherit.data=F, rotate=90) +
  facet_grid(vars(Age),switch = "y",scales = "free", space = "free")+
  scale_fill_manual(values = c(SINGLE = unname(palette["red"]), 
                               MARRIED = unname(palette["gold"]) , 
                               `DIVORCED AND WIDOWED` = unname(palette["green"]))) +
  scale_color_manual(values = c(SINGLE = unname(palette["red"]), 
                                MARRIED = unname(palette["gold"]) , 
                                `DIVORCED AND WIDOWED` = unname(palette["green"]))) +
  labs(title = "CONJUGAL CONDITION",
       color = "",
       fill = "",
       x = "",
       y = "") +
  coord_flip()   +
  guides(fill = guide_legend(override.aes=list(shape = 21,size = 12),
                             ncol=2,
                             title.hjust = 0.5)) +
  theme_minimal()  +
  
  theme(
    # panel.border  = element_rect(colour = NA), 
    text = element_text(family = "space"),
    axis.text.y = element_text(margin = margin(r = -280),size = 12),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor =  element_blank(),
    panel.spacing = unit(60, "pt"),
    # panel.background = element_rect(fill = palette["paper"]),
    plot.background = element_rect(fill = palette["paper"],color = palette["paper"]),
    plot.title = element_text(family = "space",face = "bold",
                              hjust = 0.25, size = 20, 
                              margin=margin(5,0,0,0)),
    plot.margin = margin(t = 1, r = 4, b = 3, l = 8, "cm"),
    legend.background = element_rect(fill = palette["paper"],color = palette["paper"]),
    legend.key  = element_rect(fill = palette["paper"],color = palette["paper"]),
    legend.position = "top",
    legend.direction = "vertical",
    legend.text = element_text(margin = margin(r = 15, unit = "pt"),size = 12),
    legend.spacing.x = unit(1.0, "cm"),
    strip.background = element_rect(fill = palette["paper"],colour = palette["paper"]),
    strip.text.y.left = element_text(angle = 0,margin = margin(r = 150,l = 100),size = 14)
  )



# Anotations --------------------------------------------------------------

plot_challenge_2 <- ggdraw(plot = plot_challenge_2) +
  draw_text(text = "{",x = 0.25,y = 0.23,size = 130,family = "almendra") +
  draw_text(text = "{",x = 0.25,y = 0.50,size = 130,family = "almendra") +
  draw_text(text = "{",x = 0.25,y = 0.77,size = 130,family = "almendra")


  # Printing the plot: ------------------------------------------------------


png("challenge/challenge02/challenge_2_solution.png",width = 800,height = 1025,units = "px")

print(plot_challenge_2)

dev.off()

