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
         transparency = if_else(Occupation == "Invisible",0,1),
         size_text = if_else(value < 12,0,1),
         color_line = if_else(Occupation == "Invisible",0,1),
         color_line = factor(color_line))

data_db <- data_db %>%
  group_by(Group) %>% 
  arrange(Group,desc(Occupation)) %>% 
  mutate(ypos = cumsum(Percentage)-0.5*Percentage)


plot_list <- map2(.x = data_db %>% 
                    split(.$Group),
                  .y = c(2*pi*0.85,2*pi*0.35),
     .f = ~{
       
       ggplot(data = .x,mapping = aes(x = "",
                                      y = Percentage,
                                      fill = Occupation,
                                      alpha = transparency,
                                      color = color_line)) +
         geom_bar(stat = "identity") + 
         geom_text(aes(y = ypos,label = label,size = size_text),nudge_x = 0.3) +
         coord_polar(theta = "y",start = .y,direction = -1)+
         scale_fill_manual(values = colores_pie)+
         scale_size(range = c(5,10)) + 
         scale_color_manual(values = c(palette["paper"],"black"))
     } 
  ) 


plot_list <- map(.x = plot_list,
     .f = ~{
       
       .x + 
         theme(
           panel.background=element_blank(), 
           panel.grid.major=element_blank(), 
           panel.grid.minor=element_blank(), 
           panel.spacing = unit(c(0, 0, 0, 0), "cm"),       
           axis.ticks=element_blank(), 
           axis.text.x=element_blank(), 
           axis.text.y=element_blank(), 
           axis.title.x=element_blank(), 
           axis.title.y=element_blank(),
           plot.background = element_rect(fill = palette[["paper"]],colour = NA),
           plot.margin = unit(c(0,0,0,0), "cm"),  # Edited code
           legend.position = 'none')
     } 
) 

walk2(.x = plot_list,
      .y = c("challenge/challenge03/parte_1.png",
             "challenge/challenge03/parte_2.png"),
     .f = ~{
       
       name <- .y
       
       plot <- .x
       
     ggsave(filename = name,plot = plot,dpi = 250)
       
     })


walk2(.x = c("parte_1","parte_2"),
     .y = c("905x554+560+139","905x554+560+698"),
     ~{

       imagen <- image_read(path = str_c("challenge/challenge03/",.x,".png"))
       
       # browser()
       
       imagen <-
         image_crop(image = imagen,geometry = .y)
       
       image_write(image = imagen,path = str_c("challenge/challenge03/",.x,"_crop.png"))
       
              
     })



plots <- c("parte_1","parte_2") %>% 
  map(~str_c("challenge/challenge03/",.x,"_crop.png")) %>% 
  map(image_read)


legend_1 <- tibble(leg = c(
  'Agriculture, Fisheries \nand Mining' ,
  'Manufacturing and \nMechanical Industries' ))
  
legend_2 <- tibble(leg = c(
  'Domestic and \nPersonal Service' ,
  'Professions' ,
  'Trade and \nTransportation' ))

sides <- c("left","right")

legends <- pmap(
  .l = list(leyenda = list(legend_1,legend_2),
            colores = list(c('Agriculture, Fisheries \nand Mining' = unname(palette["red"]),
                             'Manufacturing and \nMechanical Industries' = unname(palette["blue"])),
                           c('Domestic and \nPersonal Service' = unname(palette["gold"]) %>% str_to_upper(),
                             'Professions' = unname(palette["brown"]),
                             'Trade and \nTransportation' = unname(palette["paper"]) %>% str_to_upper())),
            posicion = c(2,1)),
  .f = function(leyenda,colores,posicion)  { 
    
    # colores <- .y
    
    names(colores) <- str_to_upper(names(colores))

    # 
    # browser()   
    # 
    plot <- leyenda %>% 
      mutate(value = 1,
             leg = str_to_upper(leg)) %>% 
      ggplot(aes(x = 1,y = value,fill = leg)) + 
      geom_point(color = "black") +
      scale_fill_manual(values = colores) +
      # scale_color_manual(values = colores) +
      theme(text = element_text(family = "space"),
            legend.title = element_blank(),
            legend.text.align = 0.5,
            legend.spacing.x = unit(1, 'cm'),
            # legend.position = "right",
            legend.background = element_rect(fill = palette["paper"],color = palette["paper"]),
            legend.key  = element_rect(fill = palette["paper"],color = palette["paper"]),
            legend.text = element_text(margin = margin(t = 20),vjust = 2)) 
    
    plot <- plot+
      guides(fill = guide_legend(label.position = sides[posicion],
                                  override.aes=list(shape = 21,size = 12),
                                  ncol=1)) 
    
    
    legend <- cowplot::get_legend(plot)
    
    return(legend)
  })



plot(legends[[2]])
plot_grid(legends[[1]],legends[[2]])

plot <- ggdraw() +
  theme(plot.background = element_rect(fill=palette["paper"], color = NA)) +
  draw_image(image = "challenge/challenge03/parte_1_crop.png",x = 0,y = 0.2,scale = 0.815)+
  draw_image(image = "challenge/challenge03/parte_2_crop.png",x = 0,y = -0.2,scale = 0.815)+ 
  draw_plot(legends[[1]],x = -0.35,scale = 2)+
  draw_plot(legends[[2]],x = 0.35,scale = 2) +
  draw_text(text = "WHITES",x = 0.5,y = 0.08,family = "space")+
  draw_text(text = "NEGROES",x = 0.5,y = 0.91,family = "space")+
  draw_text(text = str_to_upper("Occupation of negroes and whites in Georgia"),
            x = 0.5,y = 0.95,family = "space",fontface = "bold",size = 20) +
  draw_line(x = c(0.093,0.905),
            y = c(0.264,0.735))  

plot <- add_sub(plot = plot,label = "Elaborado por: Alex Bajaña | twitter: @AlexBajaa5 | Github: https://github.com/AlexB4891",
                size = 8)

plot <- ggdraw(plot)

png("challenge/challenge03/solution_ch3_alexb.png",width = 800,height = 1025,units = "px")

plot

dev.off()

