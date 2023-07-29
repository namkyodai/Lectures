library(rpart,quietly = TRUE)
library(caret,quietly = TRUE)
library(rpart.plot,quietly = TRUE)
library(rattle)


mushrooms <- read.csv ("mushrooms.csv")
str(mushrooms)


mushrooms <- mushrooms%>%mutate_if(is.character, as.factor)
str(mushrooms)


nrow(mushrooms) - sum(complete.cases(mushrooms))

mushrooms$veil.type <- NULL
table(mushrooms$class,mushrooms$odor)


number.perfect.splits <- apply(X=mushrooms[-1], MARGIN = 2, FUN = function(col){
  t <- table(mushrooms$class,col)
  sum(t == 0)
})

set.seed(12345)
train <- sample(1:nrow(mushrooms),size = ceiling(0.80*nrow(mushrooms)),replace = FALSE)
# training set
mushrooms_train <- mushrooms[train,]
# test set
mushrooms_test <- mushrooms[-train,]

# penalty matrix
penalty.matrix <- matrix(c(0,1,10,0), byrow=TRUE, nrow=2)

# building the classification tree with rpart
tree <- rpart(class~.,
              data=mushrooms_train,
              parms = list(loss = penalty.matrix),
              method = "class")

# Visualize the decision tree with rpart.plot
rpart.plot(tree, nn=TRUE)


#https://www.guru99.com/r-decision-trees.html
accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, data_test, type = 'class')
  table_mat <- table(data_test$survived, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}


