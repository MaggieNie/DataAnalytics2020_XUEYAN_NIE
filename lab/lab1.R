help("read.csv")
EPI_data <-read.csv(file.choose("2010EPI_data.csv"),header = T,skip = 1)

attach(EPI_data)
fix(EPI_data)
View(EPI_data)
# summary(EPI_data)
EPI_data$EPI

##### SUMMARY ####
tf <- is.na(EPI_data$EPI) # records True values if the value is NA
E <- EPI_data$EPI[!tf] # filters out NA values, new array
E
EPI
summary(EPI_data$EPI) 
fivenum(EPI_data$EPI, na.rm = TRUE)
help(stem)
stem(EPI_data$EPI) 
# deal with margin problem
graphics.off() 
par("mar") 
par(mar=c(1,1,1,1))
hist(EPI_data$EPI)
hist(EPI_data$EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI_data$EPI,na.rm=TRUE,bw=1)) 

help(rug) 
rug(EPI_data$EPI) 

boxplot(EPI_data$EPI,EPI_data$ENVHEALTH, EPI_data$ECOSYSTEM, EPI_data$DALY, EPI_data$AIR_H, EPI_data$WATER_H, EPI_data$AIR_EWATER_E,EPI_data$BIODIVERSITY )

help(distributions) 

#Filter
#Landlock
EPILand <- EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]

hist(Eland)
hist(Eland, seq(30.,95.,1.0),prob=TRUE)

#part2

plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)

help("qqnorm") 
par(pty="s") 
qqnorm(EPI) #qqline(EPI)
qqline(EPI)

x<-seq(30.,95.,1.0)
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