library('dplyr')

dat0 <- read.csv("Spotify_Kaggle/Spotify_Kaggle_subset.csv", head = TRUE)
head(dat0)

table(dat0$genre)
dat <- dat0
dat <- dat0 %>% filter(genre == "techno")

head(dat)

y <- dat$popularity
indx_signature <- 4:6
(cat_keep <- which(colSums(dat[,indx_signature]) > 0))
Xcat <- as.matrix(dat[,indx_signature[cat_keep]])
X <- as.matrix(dat[,-c(1:6)]) |> scale()

## STAN
library("rstan")

data_stan <- list(y = y, N = length(y), 
                  Xcat = Xcat, X = X,
                  Jcat = ncol(Xcat), J = ncol(X))

params <- c("alpha", "beta_cat", "beta")

# n_cores <- parallel::detectCores()   # Count how many cores are available
n_cores <- 5
options(mc.cores = n_cores)          

fit_stan <- stan(model_code = readLines("slides15_shrinkage_Spotify.stan"),
                 data = data_stan, pars = params,
                 iter = 5000, warmup = 500, thin = 5, chains = n_cores)

sim_stan <- extract(fit_stan)

# summary(fit_stan)
plot(fit_stan, pars = "beta")
colnames(X)

plot(fit_stan, pars = "beta_cat")
colnames(Xcat)


rstan::traceplot(fit_stan)
