library(ggplot2)
library(ranger)
library(vip)
library(MASS)
ltr <- read.csv('letter-recognition.csv', stringsAsFactors = TRUE)

# Basic box plots of 'outcome' by 'group1' and 'group2'
ggplot(ltr, aes(x = letter, y = ybar)) +
  geom_boxplot() +
  labs(title = "Box Plots of Outcome by Group1 and Group2",
       x = "Group 1",
       y = "Outcome Variable") +
  theme_minimal()

# Most useful variables appear to be xbar, ybar, xege, yege, width
# Most identifiable letters appear to be A, H, I, L, M, T, W


# Random Forest
f <- ranger(letter ~ ., data  = ltr, num.trees = 200
  #, mtry = 3
  , classification = TRUE, importance = "impurity")
f$prediction.error
vip(f)



### ### ### ### ### ### 
###   NEURAL NET    ### 
### ### ### ### ### ### 
library(torch)
tst <- sample(1:nrow(ltr),2000)

#convert vector response variable to factor
Ytrain <- ltr$letter[-tst]
Ytest <- ltr$letter[tst]
Xtrain = ltr[-tst,2:ncol(ltr)]
Xtest = ltr[tst,2:ncol(ltr)]


#Now, standardize our data and convert into tensors
XtrainMean <- apply(Xtrain,2,mean)
XtrainSd <- apply(Xtrain,2,sd)
Xtrain_stand <- sweep(sweep(Xtrain, 2, XtrainMean,"-"), 2, XtrainSd, "/")
Xtest_stand <- sweep(sweep(Xtest, 2, XtrainMean,"-"), 2, XtrainSd, "/")

x_train <- torch_tensor(as.matrix(Xtrain_stand))
y_train <- torch_tensor(Ytrain)

x_test <- torch_tensor(as.matrix(Xtest_stand))
y_test <- torch_tensor(Ytest)


net = nn_module(
  "class_net",
  initialize = function(){
    self$linear1 = nn_linear(16,26) # 16 columns in the X matrix
    self$linear2 = nn_linear(26,26)
    self$linear4 = nn_linear(26,26) # 26 categories (letters in the alphabet)
    
  },
  forward = function(x) {
    x %>%
      self$linear1() %>%
      nnf_relu() %>%
      self$linear2() %>%
      nnf_relu() %>%
      #self$linear3() %>%
      #nnf_relu() %>%
      self$linear4() %>%
      nnf_softmax(2)
  }
)

model2 = net()
#this will let us know how many parameters we have;
#note that there are a lot, but for a DNN, not that many
model2 

# Now, we define our loss function (cross-entropy), optimizer (adam), learning rate, and the number of epochs to consider for the SGD.

#define the cost and optimizer
criterion <- nn_cross_entropy_loss()
optimizer <- optim_adam(model2$parameters, lr = 0.005)

epochs = 1500
loss_values <- numeric()
accuracy_values <- numeric()
test_loss_values <- numeric()

### Training the model
# Now, let's train the network (some of this is just the book-keeping of our loss and accuracy values).

for(i in 1: epochs){
  optimizer$zero_grad()
  y_pred = model2$forward(x_train)
  loss = criterion(y_pred, y_train)
  loss$backward()
  optimizer$step()
  
  ### administration:
  # Append loss and accuracy values to vectors
  loss_values <- c(loss_values, as.numeric(loss))
  
  
  # Calculate validation loss
  test_outputs = model2(x_test)
  test_loss = criterion(test_outputs, y_test)
  
  # Append the current validation loss to the vector
  test_loss_values <- c(test_loss_values, test_loss$item())
  
  #check training
  if(i %% 100 == 0){
    winners = y_pred$argmax(dim=2)
    corrects = (winners == y_train)
    accuracy = corrects$sum()$item() / y_train$size()
    
    cat("Epoch:", i, "Loss:", loss$item(),"Accuracy:", accuracy, "\n")
    accuracy_values <- c(accuracy_values, accuracy)
     }
}

 

# Plot the loss values
plot(1:epochs, loss_values, type="l", col="blue", xlab="Epochs", 
     ylab="Loss", main="Loss vs Epochs")
lines(1:epochs, test_loss_values, col="red")
legend("topright", legend=c("Training Loss", "Validation Loss"),
       col=c("blue", "red"), lty=1)


 

### Test Data Evaluation
# Let's see how well we do on the test data.  

yTST_pred = model2$forward(x_test)     #classify the new data
lossTST = criterion(yTST_pred, y_test) #get the loss for this classification
winnersTST = yTST_pred$argmax(dim=2)   #get the category with highest probability
#get accuracyc
correctsTST = (winnersTST == y_test)
accuracyTST = correctsTST$sum()$item() / y_test$size()
cat("Test", "Loss:", lossTST$item(),"Accuracy:", accuracyTST, "\n")



### ### ### ### ### ### 
###       LDA       ### 
### ### ### ### ### ### 
set.seed(1337)
ind <- sample(1:nrow(ltr), .1 * nrow(ltr),replace = FALSE)
train <- ltr[-ind, ]
test <- ltr[ind, ]
model_lda = lda(letter ~ ., data = train)
p_in <- predict(model_lda, newdata = train)
p_out <- predict(model_lda, newdata = test)
check_in <- p_in$class == train$letter
check_out <- p_out$class == test$letter
mean(check_in)
mean(check_out)


### ### ### ### ### ### 
###       QDA       ### 
### ### ### ### ### ### 
model_qda = qda(letter ~ ., data = train)
p_in <- predict(model_qda, newdata = train)
p_out <- predict(model_qda, newdata = test)
check_in <- p_in$class == train$letter
check_out <- p_out$class == test$letter
mean(check_in)
mean(check_out)
pairs <- cbind(as.character(p_out$class), as.character(test$letter))
pairs <- pairs[which(pairs[,1] != pairs[,2]), ]

library(caret)
confusionMatrix(factor(pairs[ ,2]), factor(pairs[,1]))

