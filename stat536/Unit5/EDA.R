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


