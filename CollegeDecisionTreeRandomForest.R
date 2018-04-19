# Loading the necessary packages 
library(ggplot2)
library(ISLR)
library(dplyr)
library(caTools)
library(rpart)

# Loading College df from ISLR library
df <- College

# Obtain the first several rows of df
head(College)

# Show descriptive statistics
summary(College)

# Scatterplot of graduation rate vs room and boarding costs
ggplot(df,aes(Room.Board,Grad.Rate)) + geom_point(aes(color=Private))

# Scatterplot of graduation rate vs student faculty ratio
ggplot(df,aes(Grad.Rate,S.F.Ratio)) + geom_point(aes(color=Private))

# Histogram of full time undergrad students, colored by private
ggplot(df,aes(F.Undergrad)) + geom_histogram(aes(fill=Private),color="black",bins=50)

# Histogram of graduation rate, colored by private
ggplot(df,aes(Grad.Rate)) + geom_histogram(aes(fill=Private),color="black",bins=50)

# Find college with a graduation rate above 100%
subset(df,Grad.Rate>100)

# Fix value of graduation rate above 100%
df['Cazenovia College','Grad.Rate'] <- 100

# Histogram after correction of graduation rate, colored by private
ggplot(df,aes(Grad.Rate)) + geom_histogram(aes(fill=Private),color="black",bins=50)

# Train test split
set.seed(101)
sample = sample.split(df$Private,SplitRatio=0.7)
train = subset(df,sample==T)
test = subset(df,sample==F)

# Building the model and making predictions using a decision tree
tree <- rpart(Private ~ . ,method="class",data=train)
tree.preds <- predict(tree,test)
head(tree.preds)

# Compare results to check if college is private or not
tree.preds <- as.data.frame(tree.preds)
tree.preds$Private <- ifelse(tree.preds$Yes > 0.5,"Yes","No")

# Checking out our confusion matrix
table(tree.preds$Private,test$Private)

# Install rpart.plot package
install.packages('rpart.plot')

# Visualize the tree
library(rpart.plot)
prp(tree)

# Install Random Forest package
install.packages('randomForest')
library(randomForest)

# Build the Random Forest model
rf.model <- randomForest(Private ~ ., data=train,importance=TRUE)

# Making predictions
p <- predict(rf.model,test)
table(p,test$Private)




