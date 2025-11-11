library(MASS)
adm <- read.csv('Admissions.csv')
adm$Status <- ifelse(adm$Status == 'Admitted', 1, 0)
cols_to_factor <- c("University.Rating", "SOP", "LOR", "Research")
adm[cols_to_factor] <- lapply(adm[cols_to_factor], as.factor)

mod <- glm(Status ~ ., data = adm, family = binomial)
summary(mod)
fin_mod <- stepAIC(mod, direction = "both")
coef(fin_mod)

plot(adm$CGPA,jitter(adm$Status))
not_accept <- adm[which(adm$Status == 0),]
gpa_cutoff <- max(not_accept$CGPA)
low_accept <- adm[which((adm$Status == 1) & (adm$CGPA <= gpa_cutoff)),]
plot(low_accept$CGPA, low_accept$Research)


## JDF hacks for plotting cool stuff
dim = 101
gre_seq = seq(290, 340, length.out = dim)
gpa_seq = seq(min(adm$CGPA), 4, length.out = dim)
uni_seq = seq(1, 5, length.out = dim)
res_seq = seq(0, 1, length.out = dim)
sop_seq = seq(1, 5, length.out = dim)
lor_seq = seq(1, 5, length.out = dim)
gre_test = rep(gre_seq, dim)
gpa_test = rep(gpa_seq, dim)
uni_test = rep(uni_seq, dim)
res_test = rep(res_seq, dim)
sop_test = rep(sop_seq, dim)
lor_test = rep(lor_seq, dim)
X_test = cbind(gre_test, gpa_test, res_test, uni_test, sop_test, lor_test)
#points(X_test,col=6,cex=.2)
X_test_df = data.frame(CGPA = gpa_test, GRE.Score = gre_test, Research = res_test, University.Rating = uni_test, SOP = sop_test, LOR = lor_test)


#################################
########### KNN
#################################
# X <- fastDummies::dummy_cols(ad[,1:6])  # faster than model.matrix, but doesn't like this dataset because there are no categorical variables
X_train = model.matrix(Status ~ GRE.Score + CGPA + Research + University.Rating, data = adm)
X_combined = rbind(X_train[,-1], X_test)
# indices of train, test
train = 1:nrow(X_train)
test = (nrow(X_train)+1):nrow(X_combined)

# Center and Scale (standardize)
X_scaled <- scale(X.combined)

library(class)
model1 = knn(train = X_scaled[train,], test=X_scaled[test,], cl = adm$Status, k = 3)
plot(gre_test, gpa_test, col=as.numeric(model1),pch=19)


# Decorrelate
XX <- X_scaled%*%solve(chol(cov(X_scaled)))

model2 = knn(train = XX[train,], test=XX[test,], cl = adm$Status, k = 3)
plot(gre_test, gpa_test, col=as.numeric(model2), pch=19)



#################################
########### Bagging
#################################
library(ranger)
library(vip)
rf <- ranger(Status ~ ., data=adm, num.trees = 200
  # , mtry = 4
  , importance = "impurity", probability = T)  
p = predict(rf, data = X_test_df)
plot_these = which(p$predictions[,1] < .6 & p$predictions[,1] > .4)
plot(adm$GRE.Score, adm$CGPA, col = as.numeric(adm$Status) + 1, pch=16)
points(X_test_df$GRE.Score[plot_these], X_test_df$CGPA[plot_these], col=5, pch=20)
# Different Importance outputs
importance(rf)
vip(rf, num_features = 20)

#################################
########### Boosting
#################################
library(gbm)
boost <- gbm(Status ~ CGPA + GRE.Score, data=adm
             # , distribution = "bernoulli"
             # , n.trees = 100
             )
# yhat.boost = predict(boost,newdata = ??)
summary(boost)
p = predict(boost, newdata = X_test_df,type='response')
plot_these = which(p<.6 & p>.4)
plot(adm$GRE.Score, adm$CGPA, col = as.numeric(adm$Status) + 1, pch=16)
points(gre_test[plot_these], gpa_test[plot_these], col=5, pch=20)

