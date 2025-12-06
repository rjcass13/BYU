library(torch)
library(tokenizers)

# Load data
comp <- read.csv("CompanyComplaints.csv")
tokens <- sapply(comp$Complaint, tokenize_words)
departments <- unique(comp$Department)

# Function to count occurences of a certain word in a complain
count_occurences <- function(X, word) {
    sum(stringr::str_count(X, pattern = word))
}

# Function to count occurences of a certain word in a complain
avg_word_length <- function(X) {
    mean(sapply(X, nchar))
}

# Tokens
department <- comp$Department
length <- as.vector(sapply(X = tokens, FUN = length))
word_length <- as.vector(sapply(X = tokens, FUN = avg_word_length))
debt_count <- as.vector(sapply(X = tokens, FUN = count_occurences, word = 'debt'))
collect_count <- as.vector(sapply(X = tokens, FUN = count_occurences, word = 'collect'))
credit_count <- as.vector(sapply(X = tokens, FUN = count_occurences, word = 'credit'))
loan_count <- as.vector(sapply(X = tokens, FUN = count_occurences, word = 'loan'))
navient_count <- as.vector(sapply(X = tokens, FUN = count_occurences, word = 'navient'))
paypal_count <- as.vector(sapply(X = tokens, FUN = count_occurences, word = 'paypal'))
back_count <- as.vector(sapply(X = tokens, FUN = count_occurences, word = 'back'))
student_count <- as.vector(sapply(X = tokens, FUN = count_occurences, word = 'student'))
checking_count <- as.vector(sapply(X = tokens, FUN = count_occurences, word = 'checking'))

dt <- as.data.frame(cbind(department, length, word_length, debt_count, collect_count, 
    credit_count, loan_count, navient_count, paypal_count, back_count, 
    student_count, checking_count))

as.numeric(dt$department, levels = departments)

###### NN
set.seed(1337)
ind <- sample(1:nrow(dt), .1 * nrow(dt), replace = FALSE)
train <- dt[-ind, ]
test <- dt[ind, ]
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
  outputs = model(train)
  outputs = torch_reshape(outputs,list(-1))
  
  # Calculate loss
  loss = criterion(outputs, train$department)
  
  # Back propogation (calculate gradient of loss w.r.t. all tensors)
  loss$backward()
  
  # Update weights (paramenters)
  optimizer$step()
  
  #### keep track of in/out-of-sample loss metric: 
  
  # Append the current training loss to the vector
  train_loss_values <- c(train_loss_values, loss$item())
  
  # Calculate validation loss
  test_outputs = model(test)
  test_outputs = torch_reshape(test_outputs, list(-1))
  test_loss = criterion(test_outputs, test$department)
  
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







###############################
### Top Words by Department ###
###############################
# Find top words for each department

dep_names <- c('debt', 'cred_rep', 'mon', 'mort', 'st_loan', 'acct', 'card')
words_to_exclude <- c('xxxx', 'that', 'this', 'have', 'they', 'with', 'from')
set.seed(1337)
for (i in 1:7) {
    dep_tokens <- tokens[which(comp$Department == departments[i])]
    ind <- sample(1:length(dep_tokens), .2 * length(dep_tokens))
    dep_tokens <- dep_tokens[ind]
    words <- unlist(dep_tokens)

    word_counts <- as.data.frame(table(words))
    colnames(word_counts) <- c("word", "frequency")
    word_counts$word <- as.character(word_counts$word)
    word_counts <- word_counts[order(word_counts$frequency, decreasing = TRUE), ]
    word_counts <- word_counts[which(nchar(word_counts$word) >= 4), ]
    word_counts <- word_counts[-which(word_counts$word %in% words_to_exclude), ]
  
    var_name <- paste0(dep_names[i], '_word_counts')
    assign(var_name, word_counts)
    cat('Done', dep_names[i], '\n')
}

dep_tokens <- tokens[which(comp$Department == departments[2])]
nchar(as.character(acct_word_counts$word))

# Top words per department:
# Debt Collection: debt, credit, account, collection, company, report
# Credit Report: credit, report, account, information, reporting, consumer, accounts
# Money: account, money, bank, paypal, sent, back
# Mortgage: loan, mortgage, payment, account, payments, home
# Student Loans: loan, loans, payments, payment, navient, student, account, credit
# Account: account, bank, check, money, funds, checking
# Credit Card: card, credit, account, bank, payment, received

