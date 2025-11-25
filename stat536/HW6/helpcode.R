library(tokenizers)

dat = read.csv('~/Dropbox/Teaching/Stat 536/7 - Multinomial/HW7/CompanyComplaints.csv')
ss = dat[1:100,]
tokens = sapply(ss$Complaint,tokenize_words)
tally.word = function(X,target.word){
    sum(stringr::str_count(X,pattern = target.word))
}

x.debt = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'debt'))
x.collect = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'collect'))
x.credit = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'credit'))
x.loan = as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'loan'))
x.length = as.vector(sapply(X = tokens, FUN = length))

library(rpart)
library(rpart.plot)
mod = rpart(ss$Department ~ x.length + x.collect + x.debt  + x.credit + x.loan,control = rpart.control(cp = 0.0))
rpart.plot(mod)
