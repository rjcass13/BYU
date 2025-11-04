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
gre_seq = seq(290,340,length.out = dim)
gpa_seq = seq(min(adm$CGPA),4,length.out = dim)
uni_seq = seq(1,5,length.out = dim)
res_seq = seq(0,1,length.out = dim)
gre_test = rep(gre_seq,dim)
gpa_test = rep(gpa_seq,each=dim)
uni_test = rep(uni_seq,dim)
res_test = rep(res_seq,dim)
X.test = cbind(gre_test,gpa_test, res_test, uni_test)
points(X.test,col=4,cex=.2)
X.test.df = data.frame(CGPA = gpa_test, GRE.Score = gre_test, Research = res_test, University.Rating = uni_test)


#################################
########### KNN
#################################
# X <- fastDummies::dummy_cols(ad[,1:6])  # faster than model.matrix, but doesn't like this dataset because there are no categorical variables
X.train = model.matrix(Status ~ GRE.Score + CGPA + Research + University.Rating, data = adm)
X.combined = rbind(X.train[,-1],X.test)
# indices of train, test
train = 1:nrow(X.train)
test = (nrow(X.train)+1):nrow(X.combined)

# Center and Scale (standardize)
X.scaled <- scale(X.combined)

library(class)
model1 = knn(train = X.scaled[train,], test=X.scaled[test,], cl = adm$Status, k = 3)
plot(gre_test, gpa_test, col=as.numeric(model1),pch=19)


# Decorrelate
XX <- X.scaled%*%solve(chol(cov(X.scaled)))

model2 = knn(train = XX[train,], test=XX[test,], cl = adm$Status, k = 3)
plot(gre_test, gpa_test, col=as.numeric(model2), pch=19)
