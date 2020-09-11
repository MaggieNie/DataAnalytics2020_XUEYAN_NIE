help("read.csv")
EPI_data <-read.csv(file.choose("2010EPI_data.csv"),header = T)
EPI_data

attach(EPI_data) 
EPI_data$EPI 

tf <- is.na(EPI_data$EPI) # records True values if the value is NA
E <- EPI_data$EPI[!tf] # filters out NA values, new array
E

summary(EPI_data$EPI) 
fivenum(EPI_data$EPI, na.rm = TRUE)
help(stem)
stem(EPI_data$EPI)  
hist(EPI_data$EPI)
hist(EPI_data$EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI_data$EPI,na.rm=TRUE,bw=1)) 

help(rug) 
rug(EPI_data$EPI) 

boxplot(EPI_data$EPI,EPI_data$ENVHEALTH, EPI_data$ECOSYSTEM, EPI_data$DALY, EPI_data$AIR_H, EPI_data$WATER_H, EPI_data$AIR_EWATER_E,EPI_data$BIODIVERSITY )

help(distributions) 

#part2

plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)

help("qqnorm") 
par(pty="s") 
qqnorm(EPI) #qqline(EPI)
qqline(EPI)

x<-seq(30,95,1)#????
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

plot(ecdf(EPI_data$EPI),do.points=FALSE,verticals = TRUE) 
plot(ecdf(EPI_data$EPI),do.points=TRUE,verticals = TRUE) # points are visible on the plot.
par(pty="s")
help("qqnorm") # read the RStudio documentation for qqnorm
help("qqplot") # read the RStudio documentation for qqplot
qqnorm(EPI_data$EPI)
qqline(EPI_data$EPI) # adding the line on the Q-Q plot
x <- seq(30,95,1)
x
x2 <-seq(30,95,2)
x2
x2 <-seq(30,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)


boxplot(EPI_data$EPI,EPI_data$DALY)

#-------------------------------

multivariate <-read.csv(file.choose( "/Desktop/DA/multivariate.csv"),header = T)
head(multivariate)
attach(multivariate)
help(lm)
mm <-lm(Homeowners~Immigrant)
mm # mm here is a R object. 
summary(mm)$coef # The output above shows the estimate of the regression beta coefficients (column Estimate) and 

plot(Homeowners~Immigrant)
help(abline)
abline(mm)
abline(mm,col=2,lwd=3)

newImmigrantdata <- data.frame(Immigrant = c(0,  20))
mm %>% predict(newImmigrantdata)

abline(mm)
abline(mm,col=3,lwd=3) # line color = green, line width = 3
attributes(mm)
mm$coefficients

#-----------------------------------------
# Creating Plots 
# Chapter 2 -- R Graphics Cookbook.

plot(mtcars$wt,mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data = mtcars)
ggplot(mtcars,aes(x=wt,y=mpg))+ geom_point()
plot(pressure$temperature,pressure$pressure, type = "l")
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure/2, col="red")
points(pressure$temperature,pressure$pressure/2, col="blue")
library(ggplot2)
qplot(pressure$temperature,pressure$pressure, geom="line")
qplot(temperature,pressure, data = pressure, geom = "line")
ggplot(pressure, aes(x=temperature,y=pressure)) + geom_line() + geom_point() 
ggplot(pressure, aes(x=temperature, y=pressure))+ geom_line() + geom_point()

# Creating Bar graphs
barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl)) # generate a table of counts.
qplot(mtcars$cyl) # cyl is continous here
qplot(factor(mtcars$cyl)) # treat cyl as discrete
# Bar graph of counts
qplot(factor(cyl),data = mtcars)
ggplot(mtcars,aes(x=factor(cyl))) + geom_bar()

# Creating Histogram
# View the distribution of one-dimentional data with a histogram.
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10) # specify apprioximate number of bins with breaks.
hist(mtcars$mpg, breaks = 5)
hist(mtcars$mpg, breaks = 12)
qplot(mpg, data = mtcars, binwidth=4)
ggplot(mtcars,aes(x=mpg)) + geom_histogram(binwidth = 4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth = 5)

# Creating Box-plot
plot(ToothGrowth$supp, ToothGrowth$len) # using plot() function and pass it a factor of x-values and a vecctor of y-values.
#Formula Syntax
boxplot(len ~ supp, data = ToothGrowth) # if the tow vectors are in the same dataframe, you can use the formula syntax. With
# this syntax you can combine two variables on the x-axis. 
# put interaction  of two variables on x-axis
boxplot(len ~ supp + dose, data = ToothGrowth)
# with ggplot2 you can get the same results above. 
library(ggplot2)
qplot(ToothGrowth$supp, ToothGrowth$len, geom = "boxplot")
# if the two vectors are in the same dataframe, you can use the following syntax 
qplot(supp, len, data = ToothGrowth, geom = "boxplot")
# in ggplot2, the above is equvalent to:
ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()
# Using three seperate vectors
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom = "boxplot")
# You can write the samething above, get the columns from the dataframe
qplot(interaction(supp, dose), len, data = ToothGrowth, geom = "boxplot")
# Using ggplot() you can do the samething and it is equivalent to:
ggplot(ToothGrowth, aes(x=interaction(supp, dose), y=len)) + geom_boxplot()
#Plotting a function curve

