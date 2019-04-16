library(png)

# x <- readPNG("~/Desktop/512px-Mona_Lisa.PNG")
# d <- dim(x)
# xcoord <- t(expand.grid(1:d[1], 1:d[2]))
# xcoord <- t(xcoord/d)

# BiocManager::install("imager")
library(imager)


x <- readPNG(system.file('extdata/parrots.png',package='imager'))

x_long<- reshape2::melt(x)
colnames(x_long) <- c("X","Y","col","value")

library(ggplot2)
g1 <- ggplot(x_long,aes(x=X,y=Y,colour=value)) + geom_tile() + 
  coord_flip() + scale_x_reverse() + 
  theme_classic() +
  scale_colour_distiller(palette = "Spectral")
ggsave("~/Desktop/test.png")

df <- as.data.frame(x)
df2 <- reshape2::melt(df)
