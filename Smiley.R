# R Style Ninja ---- www.rstyle.ninja #

# HappyR
library(ggplot2)

# Head
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

# Smile!
circleFun2 <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*(pi/-2),length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

# Smile! corner 1
circleFun3 <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*(pi/-2.5),length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

# Smile! corner 2
circleFun4 <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(-.5,2.5*(pi/-2.5),length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

data <- circleFun(c(1,-1),2.3,npoints = 100) #head
data2 <- data.frame(x=c(.625,.625,.675,.575), y= c(-.59,-.41,-.5,-.5)) #eye
data3 <- data.frame(x=c(1.375,1.375,1.325,1.425), y= c(-.59,-.41,-.5,-.5)) #eye
data4 <- circleFun2(c(1,-1),1.7,npoints = 100) #smile
data5 <- circleFun3(c(.13,-.90),.15,npoints = 100) #smile corner
data6 <- circleFun4(c(1.87,-.90),.15,npoints = 100) #smile corner

# plot all data with ggplot
ggplot(data,aes(x,y)) + 
  geom_polygon(color="black", fill="yellow", size=3) +
  stat_ellipse(data=data2, geom="polygon") +
  stat_ellipse(data=data3, geom="polygon") +
  geom_path(data=data4, size = 8, lineend="round") +
  geom_path(data=data5, size = 3, lineend="round") +
  geom_path(data=data6, size = 3, lineend="round") +
  theme(legend.position="none",
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

# END
