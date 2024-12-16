# Day 3 - get and set working directory
setwd("C:/Users/lisad/Downloads/advent_calendaR/")
getwd()

# Day 10 - install and load dplyr
library(dplyr)
?dplyr # help function

# Day 8
sl.dat<-read.csv("sleigh.data.csv") # read in csv into dataset
sl.dat
str(sl.dat)
# ?str

# Day 9
colnames(sl.dat) # list column names
rownames(sl.dat) # list row names

sl.dat[,4] # show 4th column
sl.dat[5,] # show 5th row
sl.dat[5,3] # 5th row 3rd column

# Day 12
red <-subset(sl.dat, colour==0) # create subset of data - red sleighs
green <-subset(sl.dat, colour==1) # create subset of data - green sleighs

heavy.red <-subset(red, weight>2) # create subset of red data - heavy red

heavy.red

# dplyer filter() function does the same thing as subset
?filter
filter(green, weight>2)

heavy.green <-filter(green, weight>2)

heavy.green

# Day 13
# select only columns between km_per_carrot and bells (inclusive)
sl_var <-select(sl.dat,km_per_carrot:bells)

# select all columns except deerpower
sl_var2 <-select(sl_var, -deerpower)

sl_var

sl_var2

# Day 14
?mutate()

tree.dat<-read.csv("xmas.trees.csv")
tree.dat

# create new derived column
tree.dat%>%
  dplyr::mutate(needles.by.height=tree.dat$needle.drop/tree.dat$height)

colnames(tree.dat)

tree.dat

# new dataset with derived column
tree2.dat <-tree.dat%>%
  dplyr::mutate(needles.by.height=tree.dat$needle.drop/tree.dat$height)

tree2.dat

colnames(tree2.dat)

new.tree.dat<-tree.dat%>%
  dplyr::mutate(needles.by.height=tree.dat$needle.drop/tree.dat$height)

# write new dataset to csv
write.csv(new.tree.dat, file="tree.data2.csv")

# Day 15
hist(sl.dat$deerpower) # create histogram of deerpower ($ selects subset)

hist(red$deerpower)

# same using ggplot
library(ggplot2)
?ggplot

ggplot(sl.dat, aes(x=deerpower)) + geom_histogram(binwidth = 50, # set to 50
                                                  center = 25, # centre around middle
                                                  colour="black", # black outline
                                                  fill="pink") # pink fill

# Day 16
boxplot(xmas.magic~type, data=tree.dat) # box plot

# subset tree types
pines <-dplyr::filter(tree.dat, type=="pine")
firs <-dplyr::filter(tree.dat, type=="fir")
spruces <-dplyr::filter(tree.dat, type=="spruce")

boxplot(pines$height~spruces$height) # this is pointless! Just matching rows in each dataset

boxplot(pines$height, spruces$height, col=c("red", "forestgreen"), names=c("Pines", "Spruces"), ylab="Tree Height")
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf 

colnames(tree.dat)

boxplot(pines$height~pines$needle.drop) # better illustration!

pines

spruces

sl.dat

boxplot(sl.dat$weight~sl.dat$colour)

ggplot(sl.dat, aes(group=colour, x=colour, y=weight)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE) + 
  scale_x_continuous(breaks=seq(0, 1, 1))

# Day 17
# Summary statistics
xmas <- c("naughty","nice","nice","naughty","nice","naughty","nice","naughty") 
kids_age <-c(2,6,7,10,3,4,5,9)

max(kids_age)
min(kids_age)
mean(kids_age)

?tapply() # apply a function over a ranged array
tapply(kids_age, xmas, mean)

summary(tree.dat$height) # summary stats for height

summary(tree.dat) # summary for dataset

mean(pines$xmas.magic)
mean(spruces$xmas.magic)

t.test(pines$xmas.magic, spruces$xmas.magic) # t-test to see if statistically different

# Day 18
tree.dat3 <-data.frame(type =c(rep("pine",5), rep("spruce",5), rep("maple",5)), xmas.magic=c(rep("8",5),rep("6",5), rep(NA,5)))

tree.dat3

# Day 19
# Dealing with missing data using na.omit()
# https://www.statmethods.net/input/missingdata.html - resource for missing data
tree.dat4 <-na.omit(tree.dat3)

tree.dat4

# Day 20
# Linear regression
# https://www.r-bloggers.com/checking-glm-model-assumptions-in-r/.
tree.lm <-lm(fragrance~xmas.magic, data=tree.dat)

summary(tree.lm)

# Day 21
plot(fragrance~xmas.magic, data=tree.dat)
abline(tree.lm) # add trendline

# Using ggsplot to create a scatter plot with trendline
ggplot(tree.dat, aes(x=xmas.magic, y=fragrance)) + 
  geom_point(size=2, shape=23) + 
  geom_smooth(method=lm, se=FALSE) #add linear trend line

# Day 22
# Customising plots
# col = colour
# cex = size of points
# bg = background colour
# pch = point style
# xlab = X label
# ylab = Y label
# lty = line type (dashed, dotted, etc.)
# col.lab = label colours

# https://www.tutorialspoint.com/r/r_scatterplots.htm
plot(fragrance~xmas.magic, data=tree.dat, col=11, pch=36, cex=4)

plot(fragrance~xmas.magic, data=tree.dat, cex=1.5, bg=10, pch=23, xlab="Tree Fragrance", ylab="Christmas Magic")
abline(tree.lm)

# Day 24
# Resources
# https://stackoverflow.com
# https://www.r-bloggers.com
# https://www.r-graph-gallery.com/ggplot2-package.html
# https://www.statmethods.net/index.html

# Day 25
plot(1:10,1:10,xlim=c(-5,5),ylim=c(0,10),type="n",xlab="",ylab="",xaxt="n",yaxt="n")

rect(-1,0,1,2,col="tan3",border="tan4",lwd=3)
polygon(c(-5,0,5),c(2,4,2),col="palegreen3",border="palegreen4",lwd=3)
polygon(c(-4,0,4),c(3.5,5.5,3.5),col="palegreen4",border="palegreen3",lwd=3)
polygon(c(-3,0,3),c(5,6.5,5),col="palegreen3",border="palegreen4",lwd=3)
polygon(c(-2,0,2),c(6.25,7.5,6.25),col="palegreen4",border="palegreen3",lwd=3)

points(x=runif(4,-5,5),y=rep(2,4),col=sample(c("blue","red"),size=4,replace=T),cex=3,pch=19)
points(x=runif(4,-4,4),y=rep(3.5,4),col=sample(c("blue","red"),size=4,replace=T),cex=3,pch=19)
points(x=runif(4,-3,3),y=rep(5,4),col=sample(c("blue","red"),size=4,replace=T),cex=3,pch=19)
points(x=runif(4,-2,2),y=rep(6.25,4),col=sample(c("blue","red"),size=4,replace=T),cex=3,pch=19)
points(0,7.5,pch=8,cex=5,col="gold",lwd=3)

xPres = runif(10,-4.5,4.5)
xWidth = runif(10,0.1,0.5)
xHeight=runif(10,0,1)
for(i in 1:10){
  rect(xPres[i]-xWidth[i],0,xPres[i]+xWidth[i],xHeight[i],col=sample(c("blue","red"),size=1))
  rect(xPres[i]-0.2*xWidth[i],0,xPres[i]+0.2*xWidth[i],xHeight[i],col=sample(c("gold","grey87"),size=1))
}

