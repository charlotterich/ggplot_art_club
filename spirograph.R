# spirograph.R

library(ggplot2)
library(wesanderson)
library(cowplot)
library(data.table)

get_spiro <-  function(r1,r2,pos,res=270,t=1:10000){
  x=(r1-r2)*cos(t) + pos*cos((r1-r2)*t/r2)
  y=(r1-r2)*sin(t) - pos*sin((r1-r2)*t/r2)
  
  df <- data.frame(X=x,Y=y)
  return(df)
}

plot_spiro <- function(df,id=NULL){
  
  if(is.null(id)){
    ncolours <- 1
    g1 <- ggplot(df, mapping=aes(x=X,y=Y)) + 
      geom_point(colour="#FF0000FF",size=0.2) 
    
  }else{
    df[[id]] <- factor(df[[id]])
    ncolours <- length(levels(df[[id]]))
    
    g1 <- ggplot(df, mapping=aes(x=X,y=Y)) + 
      geom_point(aes_string(colour=id),size=0.2)  +
      scale_color_manual(values = rainbow(ncolours)) +
      theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
  }
  
  g1 <- g1 +    
    lims(x=c(-1000,1000),y=c(-1000,1000)) +
    theme_void() +
    theme(legend.position = "none",
      plot.background = element_rect(fill = "black"),
     panel.background = element_rect(fill = "black")
    )
  
  
  return(g1)
}



# Attempt 1 -----------------------------------------------------------------------------------

# 
# df1 <- get_spiro(r1=125,r2=(-75),pos=125,res=10,t=1:10000)
# df1$id <- rep("s1",nrow(df1))
# 
# df2 <- get_spiro(r1=125,r2=(-75),pos=125,res=10,t=1:10000)
# df2 <- df2/4
# df2$id <- rep("s2",nrow(df2))
# 
# df3 <- get_spiro(r1=125,r2=(-75),pos=125,res=10,t=1:50000)
# df3 <- df3*4
# df3$id <- rep("s3",nrow(df3))
# 
# df_all <- rbind(df1,df2,df3)
# df_all$is <- factor(df_all$id)
# 
# 
# 
# plot_spiro(df_all,"id")
# 
# ggsave("spiro1.png")


# Attempt 2 -----------------------------------------------------------------------------------

# aim to get spiro graphs side by side

# df1 <- get_spiro(r1=60,r2=(-15),pos=125)
# 
# df2 <- get_spiro(r1=40,r2=(-30),pos=125)
# df2$X <- df2$X+750
# 
# df3 <- get_spiro(r1=5,r2=(60),pos=60)
# df3$X <- df3$X-750
# 
# 
# 
# df_all <- rbindlist(list(df1,df2,df3),idcol = "id")
# 
# plt <- plot_spiro(df_all,id="id")
# plt
# save_plot(plt,filename = "spiro2.png",base_aspect_ratio = 1)
# 
# 
# # attempt3 ------------------------------------------------------------------------------------

# aim to get randomly positioned spirographs
df_all <- list()
df_all[[1]] <- get_spiro(r1=60,r2=(-15),pos=125)
max_x <- max(df_all[[1]]$X)
max_y <- max(df_all[[1]]$Y)
i=2

while(i<10){
  df <- data.frame(X=df_all[[1]]$X + sample(x = -800:800,1),
                   Y=df_all[[1]]$Y + sample(x = -800:800,1))
  
  # check if overlap is too big ## needs fixing
  if(any(abs(max_x - max(df$X)) <1200) | 
     any(abs(max_y - max(df$Y)) <1200 )){
  
    df_all[[i]] <- df
    
    max_x <- c(max_x,max(df$X))
    max_y <- c(max_y,max(df$Y))
    i=i+1
  }
}

df_long <- rbindlist(df_all,idcol = "id")

plt <- plot_spiro(df_long,id="id")
plt


save_plot(plt,filename = "spiro3.png",base_aspect_ratio = 1)

