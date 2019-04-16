# devtools::install_github("schochastics/Rokemon")
# Rokemon::import_pokefont()
library(Rokemon)
library(tidyverse)
library(ggplot2)
library(png)
library(grid)

data(pokemon)
head(pokemon)

# ggplot(pokemon,aes(pokedex_number,defense))+
#   geom_point(shape = 15,col = "#006400",size=2)+
#   theme_gameboy()+
#   labs(title = "Classic Gameboy Theme")



# lf <- list.files("pokemon/main-sprites/yellow/",full.names = T,pattern = ".png")
# pokedex_number <- strsplit(basename(lf),'\\.') %>% sapply(., "[[", 1)
# img_list <- lapply(lf, readPNG) 
# names(img_list) <- pokedex_number


lf <- list.files("pokemon/1st_gen/",full.names = T,pattern = ".png")
pokedex_number <- substr(basename(lf),1,3) %>% as.numeric()
img_list <- lapply(lf, readPNG) 
names(img_list) <- pokedex_number

# 
# geom_point(shape = 15,col = "#006400",size=2)+
#   theme_gameboy()+
#   labs(title = "Classic Gameboy Theme")

pp <- pokemon[pokemon$pokedex_number<152,]
# pp$pokedex_number <- pp$pokedex_number

p <- ggplot(pp,aes(pokedex_number,defense)) +
  geom_point(size=0) +
  theme_minimal() 

for(i in 1:nrow(pp)){
  p = p + annotation_custom(
    rasterGrob(img_list[[as.character(pp$pokedex_number[23])]]),
    xmin = pp$pokedex_number[i]-10, xmax = pp$pokedex_number[i]+10, 
    ymin = pp$defense[i]-10, ymax =  pp$defense[i]+10
  ) 
}
p + theme_gameboy()

