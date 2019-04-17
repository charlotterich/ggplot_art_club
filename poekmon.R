# devtools::install_github("schochastics/Rokemon")
# Rokemon::import_pokefont()
library(Rokemon)
library(tidyverse)
library(ggplot2)
library(png)
library(grid)
library(cowplot)

load_images <- function(){
  
  lf <- list.files("pokemon/1st_gen/",full.names = T,pattern = ".png")
  pokedex_number <- substr(basename(lf),1,3) %>% as.numeric()
  img_list <- lapply(lf, readPNG) 
  names(img_list) <- pokedex_number
}

poke_scatter <- function(XX,YY,pokemon,size=7){
  pp <- pokemon[pokemon$pokedex_number<150,]
  # pp$pokedex_number <- pp$pokedex_number
  pp <- pp %>% select("pokedex_number",XX,YY)  %>% drop_na()
  
  plt <- ggplot(pp,aes_string(XX,YY)) +
    geom_point(size=0) +
    theme_minimal() 
  
  for(i in 1:nrow(pp)){
    # i=pd_num
    plt = plt + annotation_custom(
      rasterGrob(img_list[[as.character(pp$pokedex_number[i])]]),
      xmin = pp[[XX]][i]-size, xmax = pp[[XX]][i]+size ,
      ymin = pp[[YY]][i]-size, ymax =  pp[[YY]][i]+size
    ) 
  }
  plt <- plt + theme_gameboy() 
  return(plt)
}


data(pokemon)
head(pokemon)

# load_images()


XX="height_m"
YY="weight_kg"
plt <- poke_scatter(XX,YY,pokemon,size=10)

bmi=as.data.frame(cbind(xx= seq(min(pokemon$height_m,na.rm = T), max(pokemon$height_m,na.rm = T),by=0.1 ),
yy=18.5/(xx^2)))
plt <- plt + geom_line(data = bmi,aes(x=xx,y=yy)) + xlim(0,5) + ylim(0,250) 
save_plot(filename = "~/Desktop/pokeplot.png",plot = plt)


# bmi plot
plt <- ggplot(pp,aes_string(XX,YY)) +
  geom_point(size=0) +
  theme_minimal() 
plt

weights <- seq(from = 40, to = 135, by = 5)
heights <- seq(from = 140, to = 210)

bmi_tab <- expand.grid(list(weights=weights,heights=heights))

y=
