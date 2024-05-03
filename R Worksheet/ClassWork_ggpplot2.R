if(!require("tidyverse")){
  install.packages("tidyverse")
  library(tidyverse)
}
# install.packages('ggrepel')
library(ggrepel)
# install.packages('ggpubr')
library(ggpubr)

data("mpg")
?mpg

unique(mpg$fl)
summary(mpg)

ggplot(data=mpg) + #Loads data into ggplot
  geom_point(mapping = aes(x=displ, y=hwy)) #Loads the aesthetic of the data (geom = point)

ggplot(data=mpg, aes(x=displ, y=hwy)) + geom_point() #Does same as above command (but shorter and more common syntax)

ggplot(data=mpg, aes(x=displ, y=hwy, color=class)) + geom_point() #Adds color aesthetic for data points

ggplot(data=mpg, aes(x=displ, y=hwy, size=class)) + geom_point() #Adds size aesthetic for data points

ggplot(data=mpg, aes(x=displ, y=hwy, alpha=class)) + geom_point() #Adds transparency aesthetic for data points

ggplot(data=mpg, aes(x=displ, y=hwy, shape=class)) + geom_point() #Adds shape aesthetic for data points (only 6 possible shapes can be assigned to the data)

p <- ggplot(data=mpg) + 
  geom_point(mapping = aes(x=displ, y=hwy, color = class))
p #Plot object named "p"

p + theme(legend.position = "top") #Adds a theme to the data (assigns the legend to the top of the plot)

data("mtcars")
df <- mtcars
df$cyl <- as.factor(df$cyl) #Reassigns the data in df's "cyl" column into bins (as a categorical variable)
summary(df)

s <- ggplot(df, aes(wt, mpg)) #Creates a scatterplot (and line 42 assigns each data point a way to be represented as points)
s
s + geom_point() 

ggpubr::show_point_shapes() #Shows you what all the available shape points are
s + geom_point(color = "blue", size=2, shape=23)
s + geom_point(aes(shape=cyl))

#"r-graph-gallery.com" <----- Gives R commands for specific types of graphs, colors, and shapes that can be used to better display the data

newMpg <- read.csv("newMpg.csv")
summary(newMpg)
ggplot(data=newMpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color=fl))

#Creates a linear model with ggplot
s <- ggplot(df, aes(wt, mpg))
s
s+ geom_point()
s + geom_point() + geom_smooth(method=lm, se=FALSE) #Linear Model Generation Here

s + geom_point() + geom_rug()


.labs <- rownames(df) #Gets labeling data
.labs

s + geom_point(aes(color = cyl)) + 
  geom_text(aes(label=.labs, color = cyl), size = 3) #Assigns labels to actual data points

s + geom_point(aes(color = cyl)) + 
  geom_text_repel(aes(label=.labs, color = cyl), size = 3)

data("ToothGrowth")
?ToothGrowth
summary(ToothGrowth)
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

# Creates a Dotplot
d <- ggplot(data=ToothGrowth, aes(x=dose, y=len))
d
d + geom_dotplot(binaxis = "y", stackdir="center", fill="lightgray") +
  stat_summary(fun.data = "mean_sdl", fun.args = list(mult=1))


st <- ggplot(ToothGrowth, aes(x=dose, y=len))
st + geom_jitter(aes(shape = dose, color =  dose), 
                 position = position_jitter(0.2), size = 1.2) +
  stat_summary(aes(color=dose), size=0.4, fun.data="mean_sdl", fun.args=(multi=1))

###Line Plots
data("economics")
?economics
summary(economics)
ggplot(data=economics, aes(x=date, y =pop)) + 
  geom_line(aes(size=unemploy/pop))

###Bar Plots
df<-data.frame(dose=c("D0.5", "D1", "D2"), len=c(4.2, 10, 29.5))
df
bar <- ggplot(data=df, aes(x=dose, y = len)) +
  geom_col(fill = "blue")
bar
bar + geom_text(aes(label = len), vjust = -0.3)

##Histograms
ggplot(data=ToothGrowth, aes(x=len)) +
  geom_histogram(bins=10)