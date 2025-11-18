rm(list=ls())

# Credit for much of this script goes to Chris Wikle and his 2024 Summer Institute of Applied Statistics at BYU
# ... with a bit of advice from Chat GPT. But Chris's stuff worked better ;)

# Challenging single variable function
f = function(x){
  ifelse(x>.5,
         x^-.2,
         log(x))
}


# Generate data
nn = 250
xx = sort(runif(nn))
yy = f(xx) + rnorm(nn,sd=.01)
data = data.frame(y=yy,x=xx)

par(mfrow=c(1,1)) # single plot

plot(xx,yy,pch=19)
curve(f,add=T,n = 9999)


# This will ease plotting later:
oos.seq = seq(from=0.000001,to=1,length.out = 900)
oos.df = data.frame(x = oos.seq)

### Let's look at Rtorch, like PyTorch. 
### It's a bit more involved than Keras, but it's given me far fewer errors. 
### Here's a basic Rtorch script, but it can be much more customized. 
library(torch)


#### STANDARDIZE OUR VARIABLES

tst = sample(1:250,50)

XtrainMean <- mean(xx[-tst])
XtrainSd <- sd(xx[-tst])

# now, standardize the validation and predict data using the training means/sd; as
# expected, these don't have exactly mean 0 and sd 1
Xtrain_stand <- (xx[-tst] - XtrainMean)/XtrainSd
Xtest_stand <- (xx[tst] - XtrainMean)/XtrainSd
Xpredict_stand <- (oos.seq - XtrainMean)/XtrainSd

# standardize the Y's the same way
YtrainMean = mean(yy[-tst])
YtrainSd = sd(yy[-tst])

Ytrain_stand <- (yy[-tst] - YtrainMean)/YtrainSd
Ytest_stand <- (yy[tst] - YtrainMean)/YtrainSd
# Ypredict doesn't exist yet

### Tensor Implementation

# Now, convert our input data and labels into tensors.

Xtrain_stand <- as.matrix(Xtrain_stand)
Xtrain_stand <- torch_tensor(Xtrain_stand)

Xtest_stand <- as.matrix(Xtest_stand)
Xtest_stand <- torch_tensor(Xtest_stand)

Xpredict_stand <- as.matrix(Xpredict_stand)
Xpredict_stand <- torch_tensor(Xpredict_stand)

Ytrain_stand <- torch_tensor(Ytrain_stand)
Ytest_stand <- torch_tensor(Ytest_stand)




regression_net = nn_module(
  "RegressionNN",
  initialize = function(){
    self$linear1 = nn_linear(1,60)
    self$linear2 = nn_linear(60,1)
  },

  forward = function(x) {
    x <- torch_relu(self$linear1(x))
    x <- self$linear2(x)
    return(x)
  }
)


# Here is a version of this model that has some more layers (a deep model);

regression_net = nn_module(
 "RegressionNN",
 initialize = function(){
   self$linear1 = nn_linear(1,64)
   self$linear2 = nn_linear(64,32)
   self$linear4 = nn_linear(32,1)
 },

 forward = function(x) {
   x <- torch_relu(self$linear1(x))
   x <- torch_relu(self$linear2(x))
   x <- self$linear4(x)
   return(x)
 }
)


#### INITIALIZE THE MODEL


torch_manual_seed(2)

model = regression_net()

# Create the cost function and the optimizer
criterion = nn_mse_loss() 
optimizer = optim_adam(model$parameters, lr = 0.001)


### TRAIN THE MODEL
#specify the number of epochs
epochs = 1000
train_loss_values <- numeric(0)
test_loss_values <- numeric(0)

for(i in 1: epochs){
  
  # Start at zero 
  optimizer$zero_grad()
  
  # Forward pass
  outputs = model(Xtrain_stand)
  outputs = torch_reshape(outputs,list(-1))
  
  # Calculate loss
  loss = criterion(outputs, Ytrain_stand)
  
  # Back propogation (calculate gradient of loss w.r.t. all tensors)
  loss$backward()
  
  # Update weights (paramenters)
  optimizer$step()
  
  #### keep track of in/out-of-sample loss metric: 
  
  # Append the current training loss to the vector
  train_loss_values <- c(train_loss_values, loss$item())
  
  # Calculate validation loss
  test_outputs = model(Xtest_stand)
  test_outputs = torch_reshape(test_outputs, list(-1))
  test_loss = criterion(test_outputs, Ytest_stand)
  
  # Append the current validation loss to the vector
  test_loss_values <- c(test_loss_values, test_loss$item())
  
  #check training
  if(i %% 10 == 0){
    cat("Epoch:", i, "Loss:", loss$item(), "\n")
  }
}

# Plot the loss values
plot(1:epochs, train_loss_values, type="l", col="blue", xlab="Epochs", 
     ylab="Loss", main="Loss vs Epochs")
lines(1:epochs, test_loss_values, col="red")
legend("topright", legend=c("Training Loss", "Validation Loss"),
       col=c("blue", "red"), lty=1)


 
### Test Set Evaluation
# Let's see how it does on the test set.

# Test the model
predicted <- torch_reshape(model(Xtest_stand),list(-1))
mse <- criterion(predicted, Ytest_stand)
cat(sprintf('Mean Squared Error on Test Data: %.4f\n', as.array(mse)))

#### Plot how we did: 

plot(xx,yy,pch=19)
curve(f,add=T,n = 9999)

y.hat = torch_reshape(model(Xpredict_stand),list(-1))
lines(x = oos.seq, y = y.hat, col = 2,lty=3)
# What happened???
# we have to undo the center/scaling: 
lines(x = oos.seq, y = y.hat*YtrainSd+YtrainMean, col = 2,lwd=3)












### ### ### ### ### ### 
### CLASSIFICATION  ### 
### ### ### ### ### ### 

# Now let's do classification on our letters data

txt = read.csv('~/Dropbox/Teaching/Stat 536/7 - Multinomial/letter-recognition.txt')

tst = sample(1:nrow(txt),2000)

#convert vector response variable to factor
txt$letter = as.factor(txt$letter)
Ytrain <- txt$letter[-tst]
Ytest <- txt$letter[tst]
Xtrain = txt[-tst,2:ncol(txt)]
Xtest = txt[tst,2:ncol(txt)]



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
    self$linear1 = nn_linear(16, 64) # 16 columns in the X matrix
    self$linear2 = nn_linear(64,32)
    self$linear3 = nn_linear(32,32)
    self$linear4 = nn_linear(32,26) # 26 categories (letter in the alphabet)
    
  },
  forward = function(x) {
    x %>%
      self$linear1() %>%
      nnf_relu() %>%
      self$linear2() %>%
      nnf_relu() %>%
      self$linear3() %>%
      nnf_relu() %>%
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
optimizer <- optim_adam(model2$parameters, lr = 0.01)

epochs = 150
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
  if(i %% 10 == 0){
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
#get accuracy
correctsTST = (winnersTST == y_test)
accuracyTST = correctsTST$sum()$item() / y_test$size()
cat("Test", "Loss:", lossTST$item(),"Accuracy:", accuracyTST, "\n")


 