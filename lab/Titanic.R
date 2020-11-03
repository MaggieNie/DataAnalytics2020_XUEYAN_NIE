#====Titanic=====
library(rpart)
library(rpart.plot)
data("Titanic")
Titanic_new <- as.data.frame(Titanic)
help(Titanic)

Titanic1<-rpart(Survived ~.,data = Titanic,method = "anova",mode=TRUE)

Titanic1 
rpart.plot(Titanic1,type = 3,fallen.leaves = TRUE)

# Ctree, hclust, DecisionTree
require(C50)  

set.seed(25)
Titanic
Titanic2= C5.0(Survived ~., data = Titanic_new[0:25,])
Titanic2
plot(Titanic2, main="Applying cTree on Titanic Dataset", tp_args = list(fill = c("green","red")))


#tab <- t(as.data.frame(apply(Titanic_new, 4:1, FUN=sum)))
#tab <- tab[apply(tab, 1, sum) > 0, ]  
#Titanic3 <- hclust(tab)
#help("hclust")


set.seed(678910)
for(i in 1:40){
  # flipping a coin and getting the data
  Titanic_h <- rbinom(1, size = 1, prob = 0.5)
  # if the coin is "Heads", add a common pattern to that row,
  if(Titanic_h){
    data_Matrix[i, ] <- data_Matrix[i, ] + rep(c(0,3), each =5)
  }
}


library(randomForest)
Titanic4 <- randomForest(Survived ~.,   data=Titanic)
Titanic4
