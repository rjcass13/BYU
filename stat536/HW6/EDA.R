library(torch)
library(tokenizers)

# Load data
comp <- read.csv("CompanyComplaints.csv")
tokens = sapply(comp$Complaint, tokenize_words)

# Function to count occurences of a certain word in a complain
count_occurences = function(X, word) {
    sum(stringr::str_count(X, pattern = word))
}

# Tokens
debt_count = as.vector(sapply(X = tokens, FUN = count_occurences, word = 'debt'))
collect_count = as.vector(sapply(X = tokens, FUN = count_occurences, word = 'collect'))
credit_count = as.vector(sapply(X = tokens, FUN = count_occurences, word = 'credit'))
loan_count = as.vector(sapply(X = tokens, FUN = count_occurences, word = 'loan'))
length = as.vector(sapply(X = tokens, FUN = length))
