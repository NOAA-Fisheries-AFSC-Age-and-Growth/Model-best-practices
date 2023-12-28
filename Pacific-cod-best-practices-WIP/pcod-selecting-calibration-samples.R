# A script to explore
## using "best" specimens via agree-only ages
## using random selection
## compare to kennard stone

# New idea - for test set - bootstrap from agree, bootstrap from all, and use ks? Then test against what... 

# Packages
library(pls)
library(mdatools)
library(tidyverse)
library(prospectr)

# # Load in data
# load("~/AFSC A&G Contract/Simulation Project/Model-best-practices/Pacific-cod-best-practices/dependency_files-pcod/dat_sg.rda")

# 
# # load("~/AFSC A&G Contract/Simulation Project/Model-best-practices/Pacific-cod-best-practices/dependency_files-pcod/sg_dat_L.rda")
# 
# # Filter all by spectra collected between 6/2021 and 5/2022
# ## Filter out all of 2022, and 2021 if month >= 6
# dat_sg_rm <- dat_sg %>%
#   filter(scan_year == "2021" & scan_month %in% c(6,7,8,9,10,11,12))
# 
# dat_sg_filt <- anti_join(dat_sg, dat_sg_rm)
# 
# dat_sg_filt <- dat_sg_filt%>%
#   filter(scan_year != "2022")
# 
# summary(dat_sg_filt$scan_year)
# 
# # PCA for outliers
# # PCA
# m1 = mdatools::pca(dat_sg_filt[,c(38:942)], 7, scale = TRUE, info = "Pcod age PCA")
# 
# # Scree Plot
# plotVariance(m1$res$cal, show.labels = TRUE)
# 
# # Plot Residuals
# plotResiduals(m1, show.labels = TRUE)
# 
# # Remove outlier
# c <- categorize(m1)
# index_outliers <- as.numeric(which(c == "outlier"))
# 
# dat_sg_filt <- dat_sg_filt[-c(index_outliers),]
# 
# # PCA
# m1 = mdatools::pca(dat_sg_filt[,c(38:942)], 7, scale = TRUE, info = "cod age PCA")
# 
# # Scree Plot
# plotVariance(m1$res$cal, show.labels = TRUE)
# 
# # Plot Residuals
# plotResiduals(m1, show.labels = TRUE)
# 
# # Remove outlier
# c <- categorize(m1)
# index_outliers <- as.numeric(which(c == "outlier"))
# 
# dat_sg_filt <- dat_sg_filt[-c(index_outliers),]
# 
# # PCA
# m1 = mdatools::pca(dat_sg_filt[,c(38:942)], 7, scale = TRUE, info = "cod age PCA")
# 
# # Scree Plot
# plotVariance(m1$res$cal, show.labels = TRUE)
# 
# # Plot Residuals
# plotResiduals(m1, show.labels = TRUE)
# 
# # Remove outlier
# c <- categorize(m1)
# index_outliers <- as.numeric(which(c == "outlier"))
# 
# dat_sg_filt <- dat_sg_filt[-c(index_outliers),]
# 
# # PCA
# m1 = mdatools::pca(dat_sg_filt[,c(38:942)], 7, scale = TRUE, info = "cod age PCA")
# 
# # Scree Plot
# plotVariance(m1$res$cal, show.labels = TRUE)
# 
# # Plot Residuals
# plotResiduals(m1, show.labels = TRUE)
# 
# # Remove outlier
# c <- categorize(m1)
# index_outliers <- as.numeric(which(c == "outlier"))
# 
# dat_sg_filt <- dat_sg_filt[-c(index_outliers),]
# 
# # PCA
# m1 = mdatools::pca(dat_sg_filt[,c(38:942)], 7, scale = TRUE, info = "cod age PCA")
# 
# # Scree Plot
# plotVariance(m1$res$cal, show.labels = TRUE)
# 
# # Plot Residuals
# plotResiduals(m1, show.labels = TRUE)
# 
# # Remove outlier
# c <- categorize(m1)
# index_outliers <- as.numeric(which(c == "outlier"))
# 
# dat_sg_filt <- dat_sg_filt[-c(index_outliers),]
# 
# # PCA
# m1 = mdatools::pca(dat_sg_filt[,c(38:942)], 7, scale = TRUE, info = "cod age PCA")
# 
# # Scree Plot
# plotVariance(m1$res$cal, show.labels = TRUE)
# 
# # Plot Residuals
# plotResiduals(m1, show.labels = TRUE)
# 
# #Clean up
# names(dat_sg_filt) <- sub('X','', names(dat_sg_filt))
# 
# rm(dat_sg)
# rm(dat_sg_rm)
# 
# # Filter for which were double read
# dat_sg_dr <- dat_sg_filt %>%
#   filter(test_age > 0)
# 
# # Make a column with read - test = x
# 
# dat_sg_dr <- dat_sg_dr%>% #make new column for filtering
#   mutate(test_diff = read_age - test_age)
# 
# dat_sg_agree <- dat_sg_dr%>%
#   filter(test_diff == "0")
# 
# save(dat_sg_dr, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/dat_sg_dr.rda")
# 
# save(dat_sg_agree, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/dat_sg_agree.rda")
# 
# save(dat_sg_filt, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod-pcod/dat_sg_filt.rda")

load(file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/dat_sg_dr.rda")
load(file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/dat_sg_agree.rda")
load(file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/dat_sg_filt.rda")

dat_sg_ho <- anti_join(dat_sg_filt, dat_sg_dr)

################################### Functions

# Function to input training set and select all remaining for validation set
select_val <- function(df){
  anti_join(dat_sg_dr, df)
}

# Function to fit model to each train/test set pair and output predictions
extract_preds <- function(train, test) {
  x <- mdatools::pls(train[, c(38:942)], train$final_age, x.test = test[, c(38:942)], y.test = test$final_age, scale = TRUE, ncomp = 10, cv = 20) #when I have more time to run this, change to ncomp = 5 and cv = 1
  comp <- x$testres$ncomp.selected #allow each to use optimal ncomp
  as.data.frame(cbind(x$testres$y.ref, x$testres$y.pred[,comp,1], abs(x$testres$y.pred[,comp,1] - x$testres$y.ref)))   #use x to select preds from matrix
}

# Function to output Mean Absolute Error from each model based on residuals
MAE_func <- function(resids) {
  mean(resids$V3)
}

# Function to extract predictions from train sets applied to hold out dataset
extract_preds_holdout <- function(train) {
  x <- mdatools::pls(train[, c(38:942)], train$final_age, x.test = dat_sg_ho[, c(38:942)], y.test = dat_sg_ho$final_age, scale = TRUE, ncomp = 10, cv = 20) #when I have more time to run this, change to ncomp = 5 and cv = 1
  comp <- x$testres$ncomp.selected #allow each to use optimal ncomp
  as.data.frame(cbind(x$testres$y.ref, x$testres$y.pred[,comp,1], abs(x$testres$y.pred[,comp,1] - x$testres$y.ref)))   #use x to select preds from matrix
}

# Function to use Kennard-Stone to select train sets
ks_select <- function(df) {
  ks_inx <- kenStone(df[,c(38:942)], metric = 'euclid', k = N)
  df[ks_inx$model,]
}

###############################
# bootstrap select different combinations (by age?) of these and use to estimate all remaining samples

## Prepare parameters
train_sets_agree <- list() #empty list
P <- 1 # proportion of data to resample (by age in this case but also could be by length bin or with no grouping) 
Iter <- 100 #number of iterations for resample loop, 10 is a stand in but it looks like 2/3 of n is standard for prediction intervals?

## Random sample data, proportional to final_age, and store these as list ###
for (k in 1:Iter){
  train_sets_agree[[k]] <- dat_sg_agree %>%
    #group_by(final_age) %>% # or nothing
    sample_frac(P, replace = TRUE) #used sample_frac because it allows user to sample by a proportion/percentage instead of a set n. Could also use sample_n() if you prefer setting an n. 
}

# Save this for reference
save(train_sets_agree, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/train_sets_agree_batch2.rda")

## For each of these, make a list of validation data sets that have only any data not in each of the training sets. Can use map() with something like this:

val_sets_agree <- map(train_sets_agree, select_val)

## write function for passing PLS model to each training dataset

preds_list_agree <- purrr::map2(train_sets_agree, val_sets_agree, extract_preds)

## estimate mean absolute error OR mean weighted CV (multinomial) for each model 

MAE_agree <- map(preds_list_agree, MAE_func)

## unlist
MAE_agree_df_100 <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_agree))

save(MAE_agree_df_100, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_agree_df_100_batch2.rda")

## Use train models to estimate hold out dataset

preds_list_agree_ho <- purrr::map(train_sets_agree, extract_preds_holdout)

## estimate mean absolute error OR mean weighted CV (multinomial) for each model 

MAE_agree_ho <- map(preds_list_agree_ho, MAE_func) #use existing MAE_func

## unlist
MAE_agree_df_100_ho <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_agree_ho))

save(MAE_agree_df_100_ho, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_agree_df_100_ho_batch2.rda")

rm(train_sets_agree)
rm(val_sets_agree)
rm(MAE_agree)
rm(MAE_agree_ho)
rm(preds_list_agree_ho)

###################################
# Randomly select (by age?) equivalent sample size, use to predict all remaining specimens
## Prepare parameters
train_sets_rand <- list() #empty list
N <- length(dat_sg_agree$final_age) # proportion of data to resample (by age in this case but also could be by length bin or with no grouping) 
Iter <- 100 #number of iterations for resample loop, 10 is a stand in but it looks like 2/3 of n is standard for prediction intervals?

## Random sample data, proportional to final_age, and store these as list ###
for (k in 1:Iter){
  train_sets_rand[[k]] <- dat_sg_dr %>%
    #group_by(final_age) %>% # or nothing, this doesn't make sense with sample_n
    sample_n(N, replace = TRUE) 
}

# Save this for reference
save(train_sets_rand, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/train_sets_rand_batch2.rda")

## For each of these, make a list of validation data sets that have only any data not in each of the training sets. Can use map() with something like this:
val_sets_rand <- map(train_sets_rand, select_val)

## use extract_preds function for passing PLS model to each training dataset
preds_list_rand <- purrr::map2(train_sets_rand, val_sets_rand, extract_preds)

## Use MAE_func to find mean absolute error  
MAE_rand <- map(preds_list_rand, MAE_func)

## unlist
MAE_rand_df_100 <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_rand))

save(MAE_rand_df_100, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_rand_df_100_batch2.rda")

## Use train models to estimate hold out dataset
### Use function

preds_list_rand_ho <- purrr::map(train_sets_rand, extract_preds_holdout)

## estimate mean absolute error OR mean weighted CV (multinomial) for each model 

MAE_rand_ho <- map(preds_list_rand_ho, MAE_func) #use existing MAE_func

## unlist
MAE_rand_df_100_ho <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_rand_ho))

save(MAE_rand_df_100_ho, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_rand_df_100_ho_batch2.rda")

rm(preds_list_rand)
rm(preds_list_rand_ho)
rm(train_sets_rand)
rm(val_sets_rand)
rm(MAE_rand)
rm(MAE_rand_ho)

##################################
# Use kennard-stone to select and equivalent sample size, use to predict all specimens

## First, select random sample with replacement from full double read data set
## Prepare parameters
ks_sets_rand <- list() #empty list
N <- length(dat_sg_agree$final_age) # proportion of data to resample (by age in this case but also could be by length bin or with no grouping) 
Iter <- 100 #number of iterations for resample loop, 10 is a stand in but it looks like 2/3 of n is standard for prediction intervals?

## Random sample data, proportional to final_age, and store these as list ###
for (k in 1:Iter){
  ks_sets_rand[[k]] <- dat_sg_dr %>%
    #group_by(final_age) %>% # or nothing, this doesn't make sense with sample_n
    sample_n(length(dat_sg_dr$final_age), replace = TRUE) 
}

# Get train sets
train_sets_ks <- purrr::map(ks_sets_rand, ks_select)

# Save this for reference
save(train_sets_ks, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/train_sets_ks_batch2.rda")

## Make validation sets
test_sets_ks <- map(train_sets_ks, select_val)

## use extract_preds function for passing PLS model to each training dataset
preds_list_ks <- purrr::map2(train_sets_ks, test_sets_ks, extract_preds)

## Use MAE_func to find mean absolute error  

MAE_ks <- map(preds_list_ks, MAE_func)

## unlist
MAE_ks_df_100 <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_ks))

save(MAE_ks_df_100, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_ks_df_doublereaddat_batch2.rda")

## Use train models to estimate hold out dataset
### Use function

preds_list_ks_ho <- purrr::map(train_sets_ks, extract_preds_holdout)

## estimate mean absolute error OR mean weighted CV (multinomial) for each model 

MAE_ks_ho <- map(preds_list_ks_ho, MAE_func) #use existing MAE_func

## unlist
MAE_ks_df_100_ho <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_ks_ho))

save(MAE_ks_df_100_ho, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_ks_df_holdout_batch2.rda")

###############################
###############################
# Add exploration by SAMPE SIZE

## N = 100
# For this, use same approach as above but instead of calibration sets with n = 2298, have them from 100, 500, 1000, 1500, 2000
# bootstrap select different combinations (by age?) of these and use to estimate all remaining samples

## Prepare parameters
train_sets_agree <- list() #empty list
N <- 100 # proportion of data to resample (by age in this case but also could be by length bin or with no grouping) 
Iter <- 100 #number of iterations for resample loop, 10 is a stand in but it looks like 2/3 of n is standard for prediction intervals?

## Random sample data, proportional to final_age, and store these as list ###
for (k in 1:Iter){
  train_sets_agree[[k]] <- dat_sg_agree %>%
    #group_by(final_age) %>% # or nothing
    sample_n(N, replace = TRUE) #used sample_frac because it allows user to sample by a proportion/percentage instead of a set n. Could also use sample_n() if you prefer setting an n. 
}

save(train_sets_agree, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/train_sets_agree_n100_batch2.rda")

## For each of these, make a list of validation data sets that have only any data not in each of the training sets. 
val_sets_agree <- map(train_sets_agree, select_val)

## write function for passing PLS model to each training dataset
preds_list_agree <- purrr::map2(train_sets_agree, val_sets_agree, extract_preds)

## estimate mean absolute error OR mean weighted CV (multinomial) for each model 
MAE_agree <- map(preds_list_agree, MAE_func)

## unlist
MAE_agree_df_100_n100 <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_agree))

save(MAE_agree_df_100_n100, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_agree_df_100_n100_batch2.rda")

## Use train models to estimate hold out dataset
### write function
preds_list_agree_ho <- purrr::map(train_sets_agree, extract_preds_holdout)

## estimate mean absolute error OR mean weighted CV (multinomial) for each model 
MAE_agree_ho <- map(preds_list_agree_ho, MAE_func) #use existing MAE_func

## unlist
MAE_agree_df_100_n100_ho <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_agree_ho))

save(MAE_agree_df_100_n100_ho, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_agree_df_100_n100_ho_batch2.rda")

rm(train_sets_agree)
rm(val_sets_agree)
rm(MAE_agree)
rm(MAE_agree_ho)

###################################
# Randomly select (by age?) equivalent sample size, use to predict all remaining specimens
## Prepare parameters
train_sets_rand <- list() #empty list
N <- 100 # sample sizeof data to resample (by age in this case but also could be by length bin or with no grouping) 
Iter <- 100 #number of iterations for resample loop, 10 is a stand in but it looks like 2/3 of n is standard for prediction intervals?

## Random sample data, proportional to final_age, and store these as list ###
for (k in 1:Iter){
  train_sets_rand[[k]] <- dat_sg_dr %>%
    #group_by(final_age) %>% # or nothing, this doesn't make sense with sample_n
    sample_n(N, replace = TRUE) 
}

save(train_sets_rand, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/train_sets_rand_n100_batch2.rda")

## For each of these, make a list of validation data sets that have only any data not in each of the training sets. Can use map() with something like this:
val_sets_rand <- map(train_sets_rand, select_val)

## use extract_preds function for passing PLS model to each training dataset
preds_list_rand <- purrr::map2(train_sets_rand, val_sets_rand, extract_preds)

## Use MAE_func to find mean absolute error  
MAE_rand <- map(preds_list_rand, MAE_func)

## unlist
MAE_rand_df_100_n100 <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_rand))

save(MAE_rand_df_100_n100, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_rand_df_100_n100_batch2.rda")

## Use train models to estimate hold out dataset
### Use function
preds_list_rand_ho <- purrr::map(train_sets_rand, extract_preds_holdout)

## estimate mean absolute error OR mean weighted CV (multinomial) for each model 
MAE_rand_ho <- map(preds_list_rand_ho, MAE_func) #use existing MAE_func

## unlist
MAE_rand_df_100_n100_ho <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_rand_ho))

save(MAE_rand_df_100_n100_ho, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_rand_df_100_n100_ho_batch2.rda")


rm(train_sets_rand)
rm(val_sets_rand)
rm(MAE_rand)
rm(MAE_rand_ho)

##################################
# Use kennard-stone to select and equivalent sample size, use to predict all specimens

## First, select random sample with replacement from full double read data set
## Prepare parameters
ks_sets_rand <- list() #empty list
N <- 100  # proportion of data to resample (by age in this case but also could be by length bin or with no grouping) 
Iter <- 100 #number of iterations for resample loop, 10 is a stand in but it looks like 2/3 of n is standard for prediction intervals?

## Random sample data, proportional to final_age, and store these as list ###
for (k in 1:Iter){
  ks_sets_rand[[k]] <- dat_sg_dr %>%
    #group_by(final_age) %>% # or nothing, this doesn't make sense with sample_n
    sample_n(length(dat_sg_dr$final_age), replace = TRUE) 
}

## Then need to write a function to use kenStone to select calibration sets
ks_select <- function(df) {
  ks_inx <- kenStone(df[,c(38:942)], metric = 'euclid', k = N)
  df[ks_inx$model,]
}

train_sets_ks <- purrr::map(ks_sets_rand, ks_select)

save(train_sets_ks, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/train_sets_ks_n100_batch2.rda")

## Make validation sets
test_sets_ks <- map(train_sets_ks, select_val)

## use extract_preds function for passing PLS model to each training dataset
preds_list_ks <- purrr::map2(train_sets_ks, test_sets_ks, extract_preds)

## Use MAE_func to find mean absolute error  

MAE_ks <- map(preds_list_ks, MAE_func)

## unlist
MAE_ks_df_100_n100 <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_ks))

save(MAE_ks_df_100_n100, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_ks_df_doublereaddat_n100_batch2.rda")

## Use train models to estimate hold out dataset
### Use function
preds_list_ks_ho <- purrr::map(train_sets_ks, extract_preds_holdout)

## estimate mean absolute error OR mean weighted CV (multinomial) for each model 
MAE_ks_ho <- map(preds_list_ks_ho, MAE_func) #use existing MAE_func

## unlist
MAE_ks_df_100_n100_ho <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_ks_ho))

save(MAE_ks_df_100_n100_ho, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_ks_df_n100_holdout_batch2.rda")

rm(train_sets_ks)
rm(val_sets_ks)
rm(MAE_ks_ho)
rm(MAE_rand_ho)

###################################
###################################

# N = 500
# For this, use same approach as above but instead of calibration sets with n = 2298, have them from 100, 500, 1000, 1500, 2000
# bootstrap select different combinations (by age?) of these and use to estimate all remaining samples

## Prepare parameters
train_sets_agree <- list() #empty list
N <- 500 # proportion of data to resample (by age in this case but also could be by length bin or with no grouping) 
Iter <- 100 #number of iterations for resample loop, 10 is a stand in but it looks like 2/3 of n is standard for prediction intervals?

## Random sample data, proportional to final_age, and store these as list ###
for (k in 1:Iter){
  train_sets_agree[[k]] <- dat_sg_agree %>%
    #group_by(final_age) %>% # or nothing
    sample_n(N, replace = TRUE) #used sample_frac because it allows user to sample by a proportion/percentage instead of a set n. Could also use sample_n() if you prefer setting an n. 
}

save(train_sets_agree, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/train_sets_agree_n500_batch2.rda")

## For each of these, make a list of validation data sets that have only any data not in each of the training sets. 
val_sets_agree <- map(train_sets_agree, select_val)

## write function for passing PLS model to each training dataset
preds_list_agree <- purrr::map2(train_sets_agree, val_sets_agree, extract_preds)

## estimate mean absolute error OR mean weighted CV (multinomial) for each model 
MAE_agree <- map(preds_list_agree, MAE_func)

## unlist
MAE_agree_df_100_n500 <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_agree))

save(MAE_agree_df_100_n500, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_agree_df_100_n500_batch2.rda")

## Use train models to estimate hold out dataset
### write function
preds_list_agree_ho <- purrr::map(train_sets_agree, extract_preds_holdout)

## estimate mean absolute error OR mean weighted CV (multinomial) for each model 
MAE_agree_ho <- map(preds_list_agree_ho, MAE_func) #use existing MAE_func

## unlist
MAE_agree_df_100_n500_ho <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_agree_ho))

save(MAE_agree_df_100_n500_ho, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_agree_df_100_n500_ho_batch2.rda")

rm(train_sets_agree)
rm(val_sets_agree)
rm(MAE_agree)
rm(MAE_agree_ho)

###################################
# Randomly select (by age?) equivalent sample size, use to predict all remaining specimens
## Prepare parameters
train_sets_rand <- list() #empty list
N <- 500 # sample sizeof data to resample (by age in this case but also could be by length bin or with no grouping) 
Iter <- 100 #number of iterations for resample loop, 10 is a stand in but it looks like 2/3 of n is standard for prediction intervals?

## Random sample data, proportional to final_age, and store these as list ###
for (k in 1:Iter){
  train_sets_rand[[k]] <- dat_sg_dr %>%
    #group_by(final_age) %>% # or nothing, this doesn't make sense with sample_n
    sample_n(N, replace = TRUE) 
}

save(train_sets_rand, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/train_sets_rand_n500_batch2.rda")

## For each of these, make a list of validation data sets that have only any data not in each of the training sets. Can use map() with something like this:
val_sets_rand <- map(train_sets_rand, select_val)

## use extract_preds function for passing PLS model to each training dataset
preds_list_rand <- purrr::map2(train_sets_rand, val_sets_rand, extract_preds)

## Use MAE_func to find mean absolute error  
MAE_rand <- map(preds_list_rand, MAE_func)

## unlist
MAE_rand_df_100_n500 <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_rand))

save(MAE_rand_df_100_n500, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_rand_df_100_n500_batch2.rda")

## Use train models to estimate hold out dataset
### Use function
preds_list_rand_ho <- purrr::map(train_sets_rand, extract_preds_holdout)

## estimate mean absolute error OR mean weighted CV (multinomial) for each model 
MAE_rand_ho <- map(preds_list_rand_ho, MAE_func) #use existing MAE_func

## unlist
MAE_rand_df_100_n500_ho <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_rand_ho))

save(MAE_rand_df_100_n500_ho, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_rand_df_100_n500_ho_batch2.rda")


rm(train_sets_rand)
rm(val_sets_rand)
rm(MAE_rand)
rm(MAE_rand_ho)

##################################
# Use kennard-stone to select and equivalent sample size, use to predict all specimens

## First, select random sample with replacement from full double read data set
## Prepare parameters
ks_sets_rand <- list() #empty list
N <- 500 # proportion of data to resample (by age in this case but also could be by length bin or with no grouping) 
Iter <- 100 #number of iterations for resample loop, 10 is a stand in but it looks like 2/3 of n is standard for prediction intervals?

## Random sample data, proportional to final_age, and store these as list ###
for (k in 1:Iter){
  ks_sets_rand[[k]] <- dat_sg_dr %>%
    #group_by(final_age) %>% # or nothing, this doesn't make sense with sample_n
    sample_n(length(dat_sg_dr$final_age), replace = TRUE) 
}

## Then need to write a function to use kenStone to select calibration sets
ks_select <- function(df) {
  ks_inx <- kenStone(df[,c(38:942)], metric = 'euclid', k = N)
  df[ks_inx$model,]
}

train_sets_ks <- purrr::map(ks_sets_rand, ks_select)

save(train_sets_ks, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/train_sets_ks_n500_batch2.rda")

## Make validation sets
test_sets_ks <- map(train_sets_ks, select_val)

## use extract_preds function for passing PLS model to each training dataset
preds_list_ks <- purrr::map2(train_sets_ks, test_sets_ks, extract_preds)

## Use MAE_func to find mean absolute error  

MAE_ks <- map(preds_list_ks, MAE_func)

## unlist
MAE_ks_df_100_n500 <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_ks))

save(MAE_ks_df_100_n500, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_ks_df_doublereaddat_n500_batch2.rda")

## Use train models to estimate hold out dataset
### Use function
preds_list_ks_ho <- purrr::map(train_sets_ks, extract_preds_holdout)

## estimate mean absolute error OR mean weighted CV (multinomial) for each model 
MAE_ks_ho <- map(preds_list_ks_ho, MAE_func) #use existing MAE_func

## unlist
MAE_ks_df_100_n500_ho <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_ks_ho))

save(MAE_ks_df_100_n500_ho, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_ks_df_100_n500_holdout_batch2.rda")

###################################
###################################

# N = 1000
# For this, use same approach as above but instead of calibration sets with n = 2298, have them from 100, 500, 1000, 1500, 2000
# bootstrap select different combinations (by age?) of these and use to estimate all remaining samples

## Prepare parameters
train_sets_agree <- list() #empty list
N <- 1000 # proportion of data to resample (by age in this case but also could be by length bin or with no grouping) 
Iter <- 100 #number of iterations for resample loop, 10 is a stand in but it looks like 2/3 of n is standard for prediction intervals?

## Random sample data, proportional to final_age, and store these as list ###
set.seed(13)
for (k in 1:Iter){
  train_sets_agree[[k]] <- dat_sg_agree %>%
    #group_by(final_age) %>% # or nothing
    sample_n(N, replace = TRUE) #used sample_frac because it allows user to sample by a proportion/percentage instead of a set n. Could also use sample_n() if you prefer setting an n. 
}

save(train_sets_agree, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/train_sets_agree_n1000_batch2.rda")

## For each of these, make a list of validation data sets that have only any data not in each of the training sets. 
val_sets_agree <- map(train_sets_agree, select_val)

## write function for passing PLS model to each training dataset
preds_list_agree <- purrr::map2(train_sets_agree, val_sets_agree, extract_preds)

## estimate mean absolute error OR mean weighted CV (multinomial) for each model 
MAE_agree <- map(preds_list_agree, MAE_func)

## unlist
MAE_agree_df_100_n1000 <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_agree))

save(MAE_agree_df_100_n1000, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_agree_df_100_n1000_batch2.rda")

## Use train models to estimate hold out dataset
### write function
preds_list_agree_ho <- purrr::map(train_sets_agree, extract_preds_holdout)

## estimate mean absolute error OR mean weighted CV (multinomial) for each model 
MAE_agree_ho <- map(preds_list_agree_ho, MAE_func) #use existing MAE_func

## unlist
MAE_agree_df_100_n1000_ho <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_agree_ho))

save(MAE_agree_df_100_n1000_ho, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_agree_df_100_n1000_ho_batch2.rda")

rm(train_sets_agree)
rm(val_sets_agree)
rm(MAE_agree)
rm(MAE_agree_ho)

###################################
# Randomly select (by age?) equivalent sample size, use to predict all remaining specimens
## Prepare parameters
train_sets_rand <- list() #empty list
N <- 1000 # sample sizeof data to resample (by age in this case but also could be by length bin or with no grouping) 
Iter <- 100 #number of iterations for resample loop, 10 is a stand in but it looks like 2/3 of n is standard for prediction intervals?

## Random sample data, proportional to final_age, and store these as list ###
for (k in 1:Iter){
  train_sets_rand[[k]] <- dat_sg_dr %>%
    #group_by(final_age) %>% # or nothing, this doesn't make sense with sample_n
    sample_n(N, replace = TRUE) 
}

save(train_sets_rand, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/train_sets_rand_n1000_batch2.rda")

## For each of these, make a list of validation data sets that have only any data not in each of the training sets. Can use map() with something like this:
val_sets_rand <- map(train_sets_rand, select_val)

## use extract_preds function for passing PLS model to each training dataset
preds_list_rand <- purrr::map2(train_sets_rand, val_sets_rand, extract_preds)

## Use MAE_func to find mean absolute error  
MAE_rand <- map(preds_list_rand, MAE_func)

## unlist
MAE_rand_df_100_n1000 <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_rand))

save(MAE_rand_df_100_n1000, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_rand_df_100_n1000_batch2.rda")

## Use train models to estimate hold out dataset
### Use function
preds_list_rand_ho <- purrr::map(train_sets_rand, extract_preds_holdout)

## estimate mean absolute error OR mean weighted CV (multinomial) for each model 
MAE_rand_ho <- map(preds_list_rand_ho, MAE_func) #use existing MAE_func

## unlist
MAE_rand_df_100_n1000_ho <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_rand_ho))

save(MAE_rand_df_100_n1000_ho, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_rand_df_100_n1000_ho_batch2.rda")


rm(train_sets_rand)
rm(val_sets_rand)
rm(MAE_rand)
rm(MAE_rand_ho)

##################################
# Use kennard-stone to select and equivalent sample size, use to predict all specimens

## First, select random sample with replacement from full double read data set
## Prepare parameters
ks_sets_rand <- list() #empty list
N <- 1000 # proportion of data to resample (by age in this case but also could be by length bin or with no grouping) 
Iter <- 100 #number of iterations for resample loop, 10 is a stand in but it looks like 2/3 of n is standard for prediction intervals?

## Random sample data, proportional to final_age, and store these as list ###
for (k in 1:Iter){
  ks_sets_rand[[k]] <- dat_sg_dr %>%
    #group_by(final_age) %>% # or nothing, this doesn't make sense with sample_n
    sample_n(length(dat_sg_dr$final_age), replace = TRUE) 
}

## Then need to write a function to use kenStone to select calibration sets
ks_select <- function(df) {
  ks_inx <- kenStone(df[,c(38:942)], metric = 'euclid', k = N)
  df[ks_inx$model,]
}

train_sets_ks <- purrr::map(ks_sets_rand, ks_select)

save(train_sets_ks, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/train_sets_ks_n1000_batch2.rda")

load("C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/train_sets_ks_n1000_batch2.rda")

## Make validation sets
test_sets_ks <- map(train_sets_ks, select_val)

## use extract_preds function for passing PLS model to each training dataset
preds_list_ks <- purrr::map2(train_sets_ks, test_sets_ks, extract_preds)

## Use MAE_func to find mean absolute error  

MAE_ks <- map(preds_list_ks, MAE_func)

## unlist
MAE_ks_df_100_n1000 <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_ks))

save(MAE_ks_df_100_n1000, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_ks_df_doublereaddat_n1000_batch2.rda")

## Use train models to estimate hold out dataset
### Use function
preds_list_ks_ho <- purrr::map(train_sets_ks, extract_preds_holdout)

## estimate mean absolute error OR mean weighted CV (multinomial) for each model 
MAE_ks_ho <- map(preds_list_ks_ho, MAE_func) #use existing MAE_func

## unlist
MAE_ks_df_100_n1000_ho <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_ks_ho))

save(MAE_ks_df_100_n1000_ho, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_ks_df_n1000_holdout_batch2.rda")

###################################
###################################

# # N = 1500
# # For this, use same approach as above but instead of calibration sets with n = 2298, have them from 100, 500, 1000, 1500, 2000
# # bootstrap select different combinations (by age?) of these and use to estimate all remaining samples
# 
# ## Prepare parameters
# train_sets_agree <- list() #empty list
# N <- 1500 # proportion of data to resample (by age in this case but also could be by length bin or with no grouping) 
# Iter <- 100 #number of iterations for resample loop, 10 is a stand in but it looks like 2/3 of n is standard for prediction intervals?
# 
# ## Random sample data, proportional to final_age, and store these as list ###
# set.seed(13)
# for (k in 1:Iter){
#   train_sets_agree[[k]] <- dat_sg_agree %>%
#     #group_by(final_age) %>% # or nothing
#     sample_n(N, replace = TRUE) #used sample_frac because it allows user to sample by a proportion/percentage instead of a set n. Could also use sample_n() if you prefer setting an n. 
# }
# 
# save(train_sets_agree, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/train_sets_agree_n1500_batch2.rda")
# 
# ## For each of these, make a list of validation data sets that have only any data not in each of the training sets. 
# val_sets_agree <- map(train_sets_agree, select_val)
# 
# ## write function for passing PLS model to each training dataset
# preds_list_agree <- purrr::map2(train_sets_agree, val_sets_agree, extract_preds)
# 
# ## estimate mean absolute error OR mean weighted CV (multinomial) for each model 
# MAE_agree <- map(preds_list_agree, MAE_func)
# 
# ## unlist
# MAE_agree_df_100_n1500 <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_agree))
# 
# save(MAE_agree_df_100_n1500, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_agree_df_100_n1500_batch2.rda")
# 
# ## Use train models to estimate hold out dataset
# ### write function
# preds_list_agree_ho <- purrr::map(train_sets_agree, extract_preds_holdout)
# 
# ## estimate mean absolute error OR mean weighted CV (multinomial) for each model 
# MAE_agree_ho <- map(preds_list_agree_ho, MAE_func) #use existing MAE_func
# 
# ## unlist
# MAE_agree_df_100_n1500_ho <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_agree_ho))
# 
# save(MAE_agree_df_100_n1500_ho, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_agree_df_100_n1500_ho_batch2.rda")
# 
# rm(train_sets_agree)
# rm(val_sets_agree)
# rm(MAE_agree)
# rm(MAE_agree_ho)
# rm(preds_list_agree)
# rm(preds_list_agree_ho)
# 
# ###################################
# # Randomly select (by age?) equivalent sample size, use to predict all remaining specimens
# ## Prepare parameters
# train_sets_rand <- list() #empty list
# N <- 1500 # sample sizeof data to resample (by age in this case but also could be by length bin or with no grouping) 
# Iter <- 100 #number of iterations for resample loop, 10 is a stand in but it looks like 2/3 of n is standard for prediction intervals?
# 
# ## Random sample data, proportional to final_age, and store these as list ###
# for (k in 1:Iter){
#   train_sets_rand[[k]] <- dat_sg_dr %>%
#     #group_by(final_age) %>% # or nothing, this doesn't make sense with sample_n
#     sample_n(N, replace = TRUE) 
# }
# 
# ## For each of these, make a list of validation data sets that have only any data not in each of the training sets. Can use map() with something like this:
# val_sets_rand <- map(train_sets_rand, select_val)
# 
# ## use extract_preds function for passing PLS model to each training dataset
# preds_list_rand <- purrr::map2(train_sets_rand, val_sets_rand, extract_preds)
# 
# ## Use MAE_func to find mean absolute error  
# MAE_rand <- map(preds_list_rand, MAE_func)
# 
# ## unlist
# MAE_rand_df_100_n1500 <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_rand))
# 
# save(MAE_rand_df_100_n1500, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_rand_df_100_n1500_batch2.rda")
# 
# ## Use train models to estimate hold out dataset
# ### Use function
# preds_list_rand_ho <- purrr::map(train_sets_rand, extract_preds_holdout)
# 
# ## estimate mean absolute error OR mean weighted CV (multinomial) for each model 
# MAE_rand_ho <- map(preds_list_rand_ho, MAE_func) #use existing MAE_func
# 
# ## unlist
# MAE_rand_df_100_n1500_ho <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_rand_ho))
# 
# save(MAE_rand_df_100_n1500_ho, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_rand_df_100_n1500_ho_batch2.rda")
# 
# rm(preds_list_rand)
# rm(preds_list_rand_ho)
# rm(train_sets_rand)
# rm(val_sets_rand)
# rm(MAE_rand)
# rm(MAE_rand_ho)
# 
# ##################################
# # Use kennard-stone to select and equivalent sample size, use to predict all specimens
# 
# ## First, select random sample with replacement from full double read data set
# ## Prepare parameters
# ks_sets_rand <- list() #empty list
# N <- 1500 # proportion of data to resample (by age in this case but also could be by length bin or with no grouping) 
# Iter <- 100 #number of iterations for resample loop, 10 is a stand in but it looks like 2/3 of n is standard for prediction intervals?
# 
# ## Random sample data, proportional to final_age, and store these as list ###
# for (k in 1:Iter){
#   ks_sets_rand[[k]] <- dat_sg_dr %>%
#     #group_by(final_age) %>% # or nothing, this doesn't make sense with sample_n
#     sample_n(length(dat_sg_dr$final_age), replace = TRUE) 
# }
# 
# ## Then need to write a function to use kenStone to select calibration sets
# ks_select <- function(df) {
#   ks_inx <- kenStone(df[,c(38:942)], metric = 'euclid', k = N)
#   df[ks_inx$model,]
# }
# 
# train_sets_ks <- purrr::map(ks_sets_rand, ks_select)
# 
# ## Make validation sets
# test_sets_ks <- map(train_sets_ks, select_val)
# 
# ## use extract_preds function for passing PLS model to each training dataset
# preds_list_ks <- purrr::map2(train_sets_ks, test_sets_ks, extract_preds)
# 
# ## Use MAE_func to find mean absolute error  
# 
# MAE_ks <- map(preds_list_ks, MAE_func)
# 
# ## unlist
# MAE_ks_df_100_n1500 <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_ks))
# 
# save(MAE_ks_df_100_n1500, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_ks_df_doublereaddat_n1500_batch2.rda")
# 
# ## Use train models to estimate hold out dataset
# ### Use function
# preds_list_ks_ho <- purrr::map(train_sets_ks, extract_preds_holdout)
# 
# ## estimate mean absolute error OR mean weighted CV (multinomial) for each model 
# MAE_ks_ho <- map(preds_list_ks_ho, MAE_func) #use existing MAE_func
# 
# ## unlist
# MAE_ks_df_100_n1500_ho <- data.frame(model_iter = seq(1:Iter), MAE = unlist(MAE_ks_ho))
# 
# save(MAE_ks_df_100_n1500_ho, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Model-best-practices/dependency-files-pcod/MAE_ks_df_n1500_holdout_batch2.rda")