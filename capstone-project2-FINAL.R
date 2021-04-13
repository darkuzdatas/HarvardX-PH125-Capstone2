####################################################################################################
# Download Superconductivity Data Set from UCI Machine Learning Repository
####################################################################################################
# UCI Machine Learning Repository dataset:
# https://archive.ics.uci.edu/ml/datasets/Superconductivty+Data
# https://archive.ics.uci.edu/ml/machine-learning-databases/00464/superconduct.zip


if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(viridis)) install.packages("viridis", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(ClusterR)) install.packages("ClusterR", repos = "http://cran.us.r-project.org")
if(!require(glmnet)) install.packages("glmnet", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(DiagrammeR)) install.packages("DiagrammeR", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("DiagrammeR", repos = "http://cran.us.r-project.org")
if(!require(webshot)) install.packages("webshot", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(caret)
library(readr)
library(data.table)
library(ggthemes)
library(broom)
library(viridis)
library(matrixStats)
library(corrplot)
library(ClusterR)
library(glmnet)
library(xgboost)
library(DiagrammeR)
library(kableExtra)
library(webshot)

####################################################################################################

dl <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00464/superconduct.zip", dl)


# get the name of the zipped file
fname_zipped = dl

# list all files names inside of a .zip file
fnames = as.character(unzip(fname_zipped, list = TRUE)$Name)

# read every file into R, assuming they are .csv files
lst = vector("list", length(fnames))
for (i in seq_along(fnames))
  lst[[i]] = read.csv(unz(fname_zipped, fnames[i]), stringsAsFactors = F)

# assign different files from the .zip to different data frames
unique_m <- lst[[1]]
train <- lst[[2]]

# remove unneeded objects
rm(dl,lst,fname_zipped,fnames,i)


mean(is.na(train))
mean(is.na(unique_m))


dim(train)
dim(unique_m)

head(train)
head(unique_m)


# train contains various physical properties of the material
# unique_m contains chemical element decomposition of the material

# critical_temp occurs in both dataframes
# let's drop it in the train before binding them

superconductors <- cbind(select(train,-critical_temp),unique_m)

# remove unneeded objects
rm(train,unique_m)

# after that last by one column contains critical temperature of the semiconductor, and last column 
# contains chemical compound of the superconducting material

# row.names(superconductors) <- superconductors$material
# !!! This above fails! It turns out there are duplicate rows !!!!
# I need to work on the dataset, in order to eliminate these duplicates

# count of the unique rows:
all_unique <- superconductors %>% 
  distinct(material, .keep_all = TRUE)
length(all_unique$material)   #  15542
 
# superconductors$material
# indx <- superconductors$material[ duplicated(superconductors$material) == TRUE ]
# duplicated(indx)


# Interestingly some materials occur twice or more times in the dataframe and the only difference 
# seem to be the critical temperature. For example:
one_dup_eg <- superconductors %>% filter(material == "Zr46.75Ti8.25Cu7.5Ni10Be27.5")
one_dup_eg[1,1:168] - one_dup_eg[2,1:168]
sum(one_dup_eg[1,1:168] - one_dup_eg[2,1:168])


all_duplicated_materials <- superconductors %>% group_by(material) %>% 
  dplyr::summarize(n=n(), critical_temp = critical_temp, mean_ct = mean(critical_temp), std = sd(critical_temp), 
             std_low = ( abs(critical_temp - mean_ct) < std), keep_material_std1 = (mean(std_low) == 1),
             std_low1_5 = ( abs(critical_temp - mean_ct) < 1.5*std),
             keep_material_std1_5 = (mean(std_low1_5) == 1)) %>% 
  filter(n > 1) %>% arrange(desc(n))

# it is 7982 rows


# remove unneeded objects
rm(one_dup_eg,all_unique,indx)

# check how many and which materials to keep based on the criteria that all CTs (Critical Temps)
# are within 1 standard deviation from the mean

to_keep_1sd <- all_duplicated_materials %>% group_by(material) %>% summarise(n = n(), sd = first(std) ,crit_temp = first(mean_ct), keep = first(keep_material_std1))
mean(to_keep_1sd$keep)


# check how many and which materials to keep based on the criteria that all CTs (Critical Temps)
# are within 1.5 standard deviation from the mean
to_keep_1_5sd <- all_duplicated_materials %>% group_by(material) %>% summarise(n = n(), sd = first(std) ,crit_temp = first(mean_ct), keep = first(keep_material_std1_5))
mean(to_keep_1_5sd$keep)  # 0.8796992



# Let's join the table and examine two examples, which we would keep under "1_5 sd" criteria, but reject under "1 sd"

to_keep_compare <- left_join(to_keep_1sd,to_keep_1_5sd, by = c("material","sd","n","crit_temp"))

materials <- to_keep_compare %>% filter(keep.x != keep.y) %>% arrange(desc(n)) %>% pull(material)
materials_to_examine <- materials[1:3]

sup_to_exam <- superconductors %>% filter(material %in% materials_to_examine) %>% select(material,critical_temp) %>% arrange(material)


# Here is the table
all_duplicated_materials %>% filter(material %in% materials_to_examine) %>% knitr::kable("pipe")

# OK I will KEEP all materials, which pass the "1_5*SD" criteria)
# which means mean(to_keep_1_5sd$keep)  # 0.8796992 of the duplicate entries


# here I will make that final dataframe:

# first I will extract names of all duplicated superconductors

all_dups_names <- all_duplicated_materials %>% group_by(material) %>% pull(material)

# filter superconductors only by these
sup_filtered <- superconductors %>% filter(material %in% all_dups_names)

dim(sup_filtered)
# drop all which I will not keep due to "1_5 sd" rule
for_filter <- to_keep_1_5sd %>% filter(keep == TRUE) %>% pull(material)
sup_filtered <- sup_filtered %>% filter(material %in% for_filter)
dim(sup_filtered)

# dplyr::summarize by material and replace critical_temp with the mean
sup_filtered <- sup_filtered %>% group_by(material) %>% mutate(av_temp  = mean(critical_temp)) %>%
                distinct(material, .keep_all= TRUE)
sup_filtered <- sup_filtered %>% mutate(critical_temp = av_temp) %>% select(-av_temp)
dim(sup_filtered)

# now I construct final dataframe by dropping all duplicated material rows in superconductors df 
# and re-adding dataframe constructed in the previous step, which contains only one row for each
# duplicated material, which I will keep, and uses mean critical_temp of the original entries

superc_final <- superconductors %>% filter( !(material %in% all_dups_names))
dim(superc_final)
dim(superconductors)

superc_final <- rbind(superc_final,sup_filtered)

# after binding both dataframes now I have final one with 15270 unique entries
dim(superc_final)

# I double check that all rows are now unique:
superc_final %>% group_by(material) %>% 
  summarise(n=n()) %>% filter(n != 1) 

# this test creates empty dataframe, confirming that cleaning of the data is completed

rm(to_keep_1_5sd, to_keep_1sd, to_keep_compare, for_filter, all_dups_names, 
   sup_to_exam, all_duplicated_materials)
rm(materials_to_examine, materials, sup_filtered)

####################################################################################################
# Additional exploratory data analysis
#
####################################################################################################

summary(superc_final)
dim(superc_final)
glance(superc_final)

# How often given atom appears in the superconductor ?
# First 81 columns describe physical and chemical properties of different semiconductors

# column 82 - 167 describe elements in given superconductor and number of elements forming it
# it can be fractional number - if certain atom is shared across few bigger cells building 
# given chemical compound

# what is distribution of atoms used?

elements_distr <- superc_final[82:167] %>% summarise(across(everything(),
                        list(min = min, max = max, sum = sum, mean = mean, sd = sd)))

ed <- elements_distr %>% gather() %>%  separate(key,c("element", "feature"), sep="_") 

ed_for_graph <- ed %>% spread(element, value)


# count non zero occurrences in superconductor
colSums(superc_final[82:167] != 0)

element_freq <- as_tibble_row(colSums(superc_final[82:167] != 0)) %>% 
  gather("element", "frequency")  %>%
  filter(frequency > 0)



element_freq %>% filter(frequency > 500) %>% ggplot( aes(x = reorder(element,desc(frequency)),y=frequency)) + 
                geom_point() + labs(x = "element", y= "present in how many superconductors")

# elements with no occurrences:

element_not_used <- as_tibble_row(colSums(superc_final[82:167] != 0)) %>% 
  gather("element", "frequency")  %>%  filter(frequency == 0) %>%
  pull(element)

# these elements don't occure at all and will be removed from the dataset

element_not_used 


rm(ed, ed_for_graph, element_freq, elements_distr)



####################################################################################################
# Graph hist of the critical_temp - NEW
####################################################################################################

#general distribution
superc_final %>% ggplot(aes(x=critical_temp)) + geom_density()

# Distribution is not normal
# There is lots of them with crit temp just above 0 K and then there is also bump around 85 K

# let's plot only these, which are close to 0 K  (<10 K)
superc_final %>% filter(critical_temp < 10) %>% ggplot(aes(x=critical_temp)) + 
  geom_density()

# what is amount and percentage of all superconductors = rows, which have critical temp < 10 K ?
total <- dim(superc_final)[1]
less_10K <- superc_final %>% filter(critical_temp < 10) %>% dplyr::summarize(n = n())


####################################################################################################
# Display graph of mean CT based on the element content - NEW
####################################################################################################

element_ct <- superc_final %>% select(82:169) %>% select(-element_not_used) %>% 
  gather(key = element, value = num_of_element, 1:77) %>%   filter(num_of_element > 0)

dim(element_ct)
head(element_ct)

element_ct$element <- as.factor(element_ct$element)

ct_by_element <- element_ct %>% group_by(element) %>% dplyr::summarize(n = n(), element = first(element), 
      mean_ct = mean(critical_temp), sd_ct = sd(critical_temp)) %>% arrange(desc(mean_ct)) 

#ct_by_element$element <- as.character(ct_by_element$element)
ct_by_element %>% ungroup() %>% filter(mean_ct > 25) %>% ggplot( aes(x = reorder(element,desc(mean_ct)), y=mean_ct) ) +
  labs(x = "element", y= "mean critical temperature [K]") + geom_point() 


# check how many supercs have e.g Hg 
aa <- superc_final %>% select("Hg") %>% filter(Hg > 0)
length(aa$Hg)

aa <- superc_final %>% select("Ca") %>% filter(Ca > 0)
length(aa$Ca)

# Now let's see if element content matter - divide by element content
el_ct_with_num_and_material <- element_ct %>% group_by(element, num_of_element) %>% dplyr::summarize(n = n(), element = first(element), 
    mean_ct = mean(critical_temp), sd_ct = sd(critical_temp), material = material) %>% arrange(desc(mean_ct)) 

el_ct_with_num <- element_ct %>% group_by(element, num_of_element) %>% dplyr::summarize(n = n(), element = first(element), 
     mean_ct = mean(critical_temp), sd_ct = sd(critical_temp)) %>% arrange(desc(mean_ct)) 

rm(ct_by_element, element_ct)


####################################################################################################
# I can now prepare train and validation sets for my project
# I use 85% of the superc_final dataframe as training and 15% for validation
####################################################################################################

# Let's add row names first
row.names(superc_final) <- superc_final$material

# Validation set will be 15% of superc_final data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = superc_final$critical_temp, times = 1, p = 0.15, list = FALSE)
training <- superc_final[-test_index,]
validation <- superc_final[test_index,]



rm(test_index)

####################################################################################################
# I now drop irrelevant features in the train and test dataframes
####################################################################################################
####################################################################################################
# graph of the standard deviation per feature
####################################################################################################
col_mean_training <- sweep(training[1:167], 2, colMeans(training[1:167]))
training_standardized <- sweep(col_mean_training, 2, colSds(as.matrix(training[1:167])), FUN = "/")

# there is some NAs due to the fact, that all entries in the column are 0
# ignore for now as I will anyway eliminate these columns in a next step
mean(is.na(training_standardized))  

features_sd <- colSds(as.matrix(training[1:167]))

col_names <- colnames(training[1:167])

# features 1-81 are of type1: various physical properties of the material
# features 82 - 167 are of type2: chemical element decomposition of the material
# I will mark them uniqly, so I can see how they influence CT

sc_type1 <- rep(1,81)
sc_type2 <- rep(2,86)
sc_type <- c(sc_type1,sc_type2)

features_sd_df <- tibble(feature = col_names, sd = features_sd, type = as.factor(sc_type))

# many features have very low standard deviation, therefore not useful for prediction
features_sd_df %>% filter(sd < 3) %>% dplyr::summarize(n = n()) # 105 features have sd > 3
features_sd_df %>% filter(sd > 3) %>% dplyr::summarize(n = n()) # 62 features have sd > 3

# create vector of features to drop
features_to_drop <- features_sd_df %>% filter(sd < 3) %>% pull(feature)

# I will use only these features, which have sd > 3
features_sd_df %>% filter(sd > 3) %>% ggplot(aes(x = reorder(feature,desc(sd)),y = sd,fill=type )) +
  geom_col() +
  theme(axis.title = element_text(angle = 0, vjust = -0.175, size = 15 ),
        axis.text = element_text(size = 6,angle = 90), legend.text = element_text(size = 10)) +
  labs(x = "feature") 



rm(col_names, features_sd, sc_type, sc_type1, sc_type2, col_mean_training)


####################################################################################################
# I now drop irrelevant features in the train and test dataframes
####################################################################################################

training_final <- training %>% select(-features_to_drop)

dim(training_final)


####################################################################################################
# Are these features correlated with each other 
# and with the outcome (CT) ?
####################################################################################################
training_final

for_corplot <- as.data.frame(abs(cor(training_final[1:63],training_final$critical_temp)))  %>% 
  arrange(desc(V1)) %>% filter(V1 > 0.45)  %>% rownames()



corrplot( cor(select(training_final, for_corplot)),
                method = "circle",       
                hclust.method = "ward.D", # If order = "hclust", is the cluster method to be used
                type = "upper",
                addrect = 2,              # If order = "hclust", number of cluster rectangles
                rect.col = 3,             # Color of the rectangles
                tl.col = "black",
                rect.lwd = 3,
                title = "Top features correlated and anti-correlated with critical temperature"
          )             # Line width of the rectangles



cor(training_final[57:62],training_final$critical_temp)

# all type 2 features except Oxygen are not correlated with the outcome: CT
training_final <- training_final %>% select(-c("C","Zr","V","Ti","Nb"))
dim(training_final)


# correlation of all features 
corrplot( cor(training_final[1:58]),
          method = "circle",       
          order = "hclust",         # Ordering method of the matrix
          hclust.method = "ward.D", # If order = "hclust", is the cluster method to be used
          addrect = 2, 
          tl.cex = 0.1,
          tl.srt = 45,
            # If order = "hclust", number of cluster rectangles
          rect.col = 3,             # Color of the rectangles
          rect.lwd = 3)             # Line width of the rectangles


# find features not correlated with CT
corr_with_ct <- abs(cor(training_final[1:57],training_final[58]))
corr_with_ct_filtered <- corr_with_ct[corr_with_ct[,1] < 0.15,]

to_remove <- names(corr_with_ct_filtered)
training_final <- training_final %>% select(-to_remove)
dim(training_final)


# check features highly correlated to each other
corr_with_ea <- abs(cor(training_final[1:49],training_final[1:49]))
corr_with_ea_filtered <- corr_with_ea[corr_with_ea[,1] > 0.85,]

# wtd_mean_atomic_mass is highly correlated with with wtd_gmean_atomic_mass (0.965)
# and wtd_gmean_atomic_mass is better correlated wit CT 0.3816 therefore I will drop
# wtd_mean_atomic_mass column

training_final <- training_final %>% select(-"wtd_mean_atomic_mass")
dim(training_final)


rm(to_remove, features_to_drop, corr_with_ct_filtered, corr_with_ct, corr_with_ea, 
   corr_with_ea_filtered, superconductors)

# after that massaging I am left with 48 usable features, which will be used to build the models

# many features have very low standard deviation, therefore not useful for prediction
sd_m3 <- features_sd_df %>% filter(sd < 3) %>% dplyr::summarize(n = n()) # 105 features have sd < 3
sd_l3 <- features_sd_df %>% filter(sd > 3) %>% dplyr::summarize(n = n()) # 62 features have sd > 3


df1 <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("features with sd < 3", "features with sd > 3")
df1 <- rbind(c(sd_m3 , sd_l3) ) 
colnames(df1) <- x
df1 %>% kable(format = "pipe" )
 
 
####################################################################################################
# define function used to calculate RMSE and tables to store the results
####################################################################################################
RMSE <- function(true, predicted){
  sqrt(mean((true - predicted)^2))
}



# FIRST MODEL
####################################################################################################
# First model - mlr (Multiply Linear Regression) on the entire dataset 
####################################################################################################

data <- training_final %>% select(-"material")
set.seed(1)

# pick the training control parameters
tr_control1 <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE,
  savePredictions = TRUE,
  allowParallel = TRUE
)


# I use caret train function to evaluate all models:
fit_mlr = train( critical_temp ~ ., data=data, 
                 method = "lm", trControl = tr_control1  )

#save model in the ./output directory
saveRDS(fit_mlr,file="./output/model1-fit_mlr")


fit_mlr$results %>% kable(caption = 'Best Tune Fit for model1: mlr', digits = 2, format = "pipe" )
# save it for later reference:
fit_mlr$results %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_MLR_RESULT.md")
fit_mlr$results %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_MLR_RESULT.txt")

fit_mlr$results %>% filter( RMSE == min(RMSE)) %>% knitr::kable("pipe") %>% save_kable("./figures/MODEL1_RMSE.txt")

# Let's check performance on the validation set
####################################################################################################
# Apply my model on the validation set
#################################################################################################
valid <- validation %>% select(-"material")
pred_ct_mlr <- predict(fit_mlr, valid)

model1_rmse <- RMSE(validation$critical_temp,pred_ct_mlr)

# save result to the table:
RMSE_VAL <- tibble(method = "MODEL 1 - MLR", RMSE_VALIDATION = model1_rmse)
RMSE_VAL %>% knitr::kable("pipe")

# save it as an image:
RMSE_VAL %>% knitr::kable("pipe") %>% save_kable("./figures/RMSE_VAL_After_Model1.md")
RMSE_VAL %>% knitr::kable("pipe") %>% save_kable("./figures/RMSE_VAL_After_Model1.txt")


####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
# SECOND MODEL - ELASTIC NET
####################################################################################################
# Let's try different methods of linear regression
# I use now LM with elastic net
####################################################################################################

modelLookup("glmnet")
getModelInfo("glmnet", regex = TRUE)[[1]]$param

# There are two forms of penalized models with this tuneGrid: ridge regression and lasso 
# regression. alpha = 0 is pure ridge regression, and alpha = 1 is pure lasso regression. 
# To fit a mixture of the two models (i.e. an elastic net) using an alpha between 0 and 1. 
# For example, alpha = 0.05 would be 95% ridge regression and 5% lasso regression




# train the model with the default hyperparameters
fit_glmnet = train( critical_temp ~ ., data=data, 
                    method = "glmnet", trControl = tr_control1  )


#save model in the ./output directory
saveRDS(fit_glmnet,file="./output/model2-fit_glmnet_no_tun")

ggplot(fit_glmnet)
# save plot of the fit to the ./figures directory
png(filename="./figures/MODEL2_fit_glmnet_no_tun.png")
ggplot(fit_glmnet)
dev.off()



fit_glmnet$bestTune %>% kable(caption = 'Best Tune Fit for model2: glmnet with no tuning', digits = 2, format = "pipe" )
# save it for later reference:
fit_glmnet$bestTune %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_GLMNET_BEST_TUNE.md")
fit_glmnet$bestTune %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_GLMNET_BEST_TUNE.txt")

fit_glmnet$results %>% kable(caption = 'Results for model2: glmnet', digits = 2, format = "pipe" )
# save it for later reference:
fit_glmnet$results %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_GLMNET_RESULT.md")
fit_glmnet$results %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_GLMNET_RESULT.txt")

fit_glmnet$results %>% filter( RMSE == min(RMSE)) %>% knitr::kable("pipe") %>% save_kable("./figures/MODEL2_RMSE_NO_TUNE.txt")


# now I try to optimize the model, and run it with tuning


glmnet_grid_opt <- expand.grid(
  alpha = seq(0.7, 0.9, 0.02),
  lambda = seq(0.0, 0.01, 0.001)
)


fit_glmnet_opt = train( critical_temp ~ ., data=data, 
                     method = "glmnet", trControl = tr_control1,  tuneGrid = glmnet_grid_opt )



#save model in the ./output directory
saveRDS(fit_glmnet_opt,file="./output/model2-fit_glmnet_opt")

ggplot(fit_glmnet_opt)
# save plot of the fit to the ./figures directory
png(filename="./figures/MODEL2_fit_glmnet_opt.png")
ggplot(fit_glmnet_opt)
dev.off()



fit_glmnet_opt$bestTune %>% kable(caption = 'Best Tune Fit for model2: glmnet with optimization', digits = 2, format = "pipe" )
# save it for later reference:
fit_glmnet_opt$bestTune %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_GLMNET_BEST_TUNE_WITH_OPT.md")
fit_glmnet_opt$bestTune %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_GLMNET_BEST_TUNE_WITH_OPT.txt")

fit_glmnet$results %>% kable(caption = 'Results for model2: glmnet', digits = 2, format = "pipe" )
# save it for later reference:
fit_glmnet_opt$results %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_GLMNET_RESULT_WITH_OPT.md")
fit_glmnet_opt$results %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_GLMNET_RESULT_WITH_OPT.txt")


fit_glmnet_opt$results %>% filter( RMSE == min(RMSE)) %>% knitr::kable("pipe") %>% save_kable("./figures/MODEL2_RMSE_WITH_TUNE.txt")

# Let's check performance on the validation set

####################################################################################################
# Apply my GLMNET model on the validation set 
####################################################################################################

# before hyperparameters tuning
pred_ct_glmnet <- predict(fit_glmnet, valid)
model2_rmse_no_tun <- RMSE(validation$critical_temp,pred_ct_glmnet)

# save result to the table:
RMSE_VAL <- bind_rows(RMSE_VAL, tibble(method = "MODEL 2 - GLMNET - no tuning", RMSE_VALIDATION = model2_rmse_no_tun))
RMSE_VAL %>% knitr::kable("pipe")

# save it for a later use:
RMSE_VAL %>% knitr::kable("pipe") %>% save_kable("./figures/RMSE_VAL_After_Model2_no_tune.md")
RMSE_VAL %>% knitr::kable("pipe") %>% save_kable("./figures/RMSE_VAL_After_Model2_no_tune.txt")


# after hyperparameters tuning:

pred_ct_glmnet_opt <- predict(fit_glmnet_opt, valid)
model2_rmse_opt <- RMSE(validation$critical_temp,pred_ct_glmnet_opt)

# save result to the table:
RMSE_VAL <- bind_rows(RMSE_VAL, tibble(method = "MODEL 2 - GLMNET - optimized", RMSE_VALIDATION = model2_rmse_opt))
RMSE_VAL %>% knitr::kable("pipe")

# save it for a later use:
RMSE_VAL %>% knitr::kable("pipe") %>% save_kable("./figures/RMSE_VAL_After_Model2_optimized.md")
RMSE_VAL %>% knitr::kable("pipe") %>% save_kable("./figures/RMSE_VAL_After_Model2_optimized.txt")



####################################################################################################
# Display graph of predicted vs real CT for GLMNET MODEL and compare to MLR MODEL
####################################################################################################

pred_real_ct_glmnet <- tibble(predicted = pred_ct_glmnet_opt, real = validation$critical_temp ) 
pred_real_ct_mlr <- tibble(predicted = pred_ct_mlr, real = validation$critical_temp ) 

pred_real_ct_glmnet %>% ggplot(aes(x=real, y=predicted)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE)
# there are many predictions, lower than 0 K !
# TODO: do sth with them
# TODO: maybe additional cluster for CTs < 10K ???ß

pred_real_ct_mlr %>% ggplot(aes(x=real, y=predicted)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

# both on the same graph, with regression lines

ggplot(NULL,aes(x=real, y=predicted)) + geom_point(data = pred_real_ct_glmnet, col = "red") + 
  geom_point(data = pred_real_ct_mlr, col = "blue") +
  geom_smooth(data = pred_real_ct_glmnet, method = "lm", se = FALSE,col = "black") +
  geom_smooth(data = pred_real_ct_mlr, method = "lm", se = FALSE, col = "black")





####################################################################################################
# THIRD MODEL - xgbTree (eXtreme Gradient Boosting) on the entire dataset 
####################################################################################################



# HOW TO TUNE HYPERPARAMETERS 
# https://www.kaggle.com/pelkoja/visual-xgboost-tuning-with-caret
# https://www.slideshare.net/OwenZhang2/tips-for-data-science-competitions/14

# 3.1 List of the Tunable Hyperparameters and the Process
# 
# In caret_6.0-80, the tuning parameter grid should have the following columns:
#   
#   nrounds: Number of trees, default: 100
# max_depth: Maximum tree depth, default: 6
# eta: Learning rate, default: 0.3
# gamma: Used for tuning of Regularization, default: 0
# colsample_bytree: Column sampling, default: 1
# min_child_weight: Minimum leaf weight, default: 1
# subsample: Row sampling, default: 1
# 
# We’ll break down the tuning of these into five sections:
#   
# 1 Fixing learning rate eta and number of iterations nrounds
# 2 Maximum depth max_depth and child weight min_child_weight
# 3 Setting column colsample_bytree and row sampling subsample
# 4 Experimenting with different gamma values
# 5 Reducing the learning rate eta
# 
# In general, the idea is to use higher learning rate to tune the hyperparamers 
# (which can be computationally really heavy process) as it involves fitting a 
# model for each configuration of different hyperparameters, and then use these 
# parameters to fit the final model with lower learning rate and higher number
# of trees. We’ll be using values obtained from earlier steps to fit the models
# in the upcoming ones. These values can be obtained from train-class objects 
# from model$bestTune$hyperparameter.
# 


# getModelInfo("lm")
# modelLookup("glm")
# modelLookup("glmnet")
# modelLookup("xgbTree")
# getModelInfo("lm", regex = TRUE)[[1]]$param

data <- training_final %>% select(-"material")
set.seed(421)


# pick the training control parameters
tr_control1 <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE,
  savePredictions = TRUE,
  allowParallel = TRUE
)


# train the model with eXtreme gradient boosting tree using train from caret, 
# using the default parameters
fit_xgbTree_no_tuning = train( critical_temp ~ ., data=data, 
                 method = "xgbTree", trControl = tr_control1, verbose = TRUE, objective = "reg:squarederror" )


#save model in the ./output directory
saveRDS(fit_xgbTree_no_tuning,file="./output/model3-fit_xgbTree_no_tuning")

plot(fit_xgbTree_no_tuning)   ## ggplot throws error:
# The function can only handle <= 4 tuning parameters for scatter plots. Use output = 'ggplot' to create your own
# save plot of the fit to the ./figures directory
png(filename="./figures/MODEL3_fit_xgbTree_no_tuning.png")
plot(fit_xgbTree_no_tuning)
dev.off()



fit_xgbTree_no_tuning$bestTune %>% kable(caption = 'Best Tune Fit for model3: xgbTree with no tuning', digits = 2, format = "pipe" )
# save it for later reference:
fit_xgbTree_no_tuning$bestTune %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_BEST_TUNE_NO_TUNE.md")
fit_xgbTree_no_tuning$bestTune %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_BEST_TUNE_NO_TUNE.txt")

fit_xgbTree_no_tuning$results %>% kable(caption = 'Results for model3: xgbTree with no tuning', digits = 2, format = "pipe" )
# save it for later reference:
fit_xgbTree_no_tuning$results %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_RESULT_NO_TUNE.md")
fit_xgbTree_no_tuning$results %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_RESULT_NO_TUNE.txt")


fit_xgbTree_no_tuning$results %>% filter( RMSE == min(RMSE)) %>% knitr::kable("pipe") %>% save_kable("./figures/MODEL3_RMSE_NO_TUNE.txt")



####################################################################################################
# Apply this first xgbTree model on the validation set 
####################################################################################################

# before hyperparameters tuning
pred_ct_xgb_nt <- predict(fit_xgbTree_no_tuning, valid)
model3_rmse_no_tun <- RMSE(validation$critical_temp,pred_ct_xgb_nt)

# save result to the table:
RMSE_VAL <- bind_rows(RMSE_VAL, tibble(method = "MODEL 3 - XGBTREE - no tuning", RMSE_VALIDATION = model3_rmse_no_tun))
RMSE_VAL %>% knitr::kable("pipe")

# save it for a later use:
RMSE_VAL %>% knitr::kable("pipe") %>% save_kable("./figures/RMSE_VAL_After_Model3_no_tune.md")
RMSE_VAL %>% knitr::kable("pipe") %>% save_kable("./figures/RMSE_VAL_After_Model3_no_tune.txt")






####################################################################################################
# Now perform proper hyperparameters tuning on the eXtreme gradient bootsting tree
####################################################################################################


### STEP1 - fixing tree depth and eta
# at this point I will use fairly high eta to speed up computations. It will be tuned down later, 
# after setting all other parameters

# I will use the same tune_control for the first 5 steps, which will use cross validation with 3 folds
tune_control <- caret::trainControl(
  method = "cv", 
  number = 3,
  verboseIter = TRUE,
  allowParallel = TRUE 
)

tune_grid_step1 <- expand.grid(
  nrounds = seq(from = 200, to = 2000, by = 200),
  eta = c(0.04, 0.05, 0.075),
  max_depth = c(8,9,10,11),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

# run model with tuning step 1
fit_xgbTree_step1 = train( critical_temp ~ ., data=data, 
                          method = "xgbTree", trControl = tune_control,  tuneGrid = tune_grid_step1, verbose = TRUE, objective = "reg:squarederror"  )


#save model in the ./output directory
saveRDS(fit_xgbTree_step1,file="./output/model3-fit_xgbTree_step1")

#plot(fit_xgbTree_step1) 
png(filename="./figures/MODEL3_fit_xgbTree_tune_step1.png")
plot(fit_xgbTree_step1)
dev.off()


fit_xgbTree_step1$bestTune %>% kable(caption = 'Best Tune Fit for model3: xgbTree step1 tuning', digits = 2, format = "pipe" )
# save it for later reference:
fit_xgbTree_step1$bestTune %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_BEST_TUNE_STEP1.md")
fit_xgbTree_step1$bestTune %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_BEST_TUNE_STEP1.txt")

fit_xgbTree_step1$results %>% kable(caption = 'Results for model3: xgbTree step1 tuning', digits = 2, format = "pipe" )
# save it for later reference:
fit_xgbTree_step1$results %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_RESULT_TUNE_STEP1.md")
fit_xgbTree_step1$results %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_RESULT_TUNE_STEP1.txt")


# this will consolidate parameters into one table:
# first let's check min RSME obtained during this step of tuning
min_RMSE <- min(fit_xgbTree_step1$results$RMSE)

# save result to the table:
xgbTree_tune_RMSEs <- tibble(method = "MODEL3 - xgbTree step1 tuning", Best_RMSE = min_RMSE, 
                      nrounds = fit_xgbTree_step1$bestTune$nrounds,
                      max_depth = fit_xgbTree_step1$bestTune$max_depth,
                      eta = fit_xgbTree_step1$bestTune$eta,
                      gamma = fit_xgbTree_step1$bestTune$gamma,
                      colsample_bytree = fit_xgbTree_step1$bestTune$colsample_bytree,
                      min_child_weight = fit_xgbTree_step1$bestTune$min_child_weight,
                      subsample = fit_xgbTree_step1$bestTune$subsample
                      )
xgbTree_tune_RMSEs %>% knitr::kable("pipe")
# save it for later reference:
xgbTree_tune_RMSEs %>% knitr::kable("pipe") %>% save_kable("./figures/MODEL3 - xgbTree Step1 tuning.md")
xgbTree_tune_RMSEs %>% knitr::kable("pipe") %>% save_kable("./figures/MODEL3 - xgbTree Step1 tuning.txt")








### STEP2 - with fixed eta set maximum depth to best_max_depth +-1 
# and optimize for the max_depth and min_child_weight parameters
# I also adjusted boosting iters nrounds to better fit minimum seen on the graphs


best_eta <- fit_xgbTree_step1$bestTune$eta
best_max_depth <- fit_xgbTree_step1$bestTune$max_depth


tune_grid_step2 <- expand.grid(
  nrounds = seq(from = 200, to = 2000, by = 200),
  eta = best_eta,
  max_depth = c(best_max_depth-1,best_max_depth,best_max_depth+1),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = c(1, 2, 3),  
  subsample = 1
)


# run model with tuning step 2
fit_xgbTree_step2 = train( critical_temp ~ ., data=data, 
                           method = "xgbTree", trControl = tune_control,  tuneGrid = tune_grid_step2, verbose = TRUE, objective = "reg:squarederror"  )


#save model in the ./output directory
saveRDS(fit_xgbTree_step2,file="./output/model3-fit_xgbTree_step2")

ggplot(fit_xgbTree_step2) 
png(filename="./figures/MODEL3_fit_xgbTree_tune_step2.png")
ggplot(fit_xgbTree_step2)
dev.off()


fit_xgbTree_step2$bestTune %>% kable(caption = 'Best Tune Fit for model3: xgbTree step2 tuning', digits = 2, format = "pipe" )
# save it for later reference:
fit_xgbTree_step2$bestTune %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_BEST_TUNE_STEP2.md")
fit_xgbTree_step2$bestTune %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_BEST_TUNE_STEP2.txt")

fit_xgbTree_step2$results %>% kable(caption = 'Results for model3: xgbTree step2 tuning', digits = 2, format = "pipe" )
# save it for later reference:
fit_xgbTree_step2$results %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_RESULT_TUNE_STEP2.md")
fit_xgbTree_step2$results %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_RESULT_TUNE_STEP2.txt")


# this will consolidate parameters into one table:
# first let's check min RSME obtained during this step of tuning
min_RMSE <- min(fit_xgbTree_step2$results$RMSE)

# save result to the table:
xgbTree_tune_RMSEs <- bind_rows(xgbTree_tune_RMSEs, tibble(method = "MODEL3 - xgbTree step2 tuning", Best_RMSE = min_RMSE, 
                             nrounds = fit_xgbTree_step2$bestTune$nrounds,
                             max_depth = fit_xgbTree_step2$bestTune$max_depth,
                             eta = fit_xgbTree_step2$bestTune$eta,
                             gamma = fit_xgbTree_step2$bestTune$gamma,
                             colsample_bytree = fit_xgbTree_step2$bestTune$colsample_bytree,
                             min_child_weight = fit_xgbTree_step2$bestTune$min_child_weight,
                             subsample = fit_xgbTree_step2$bestTune$subsample) )

xgbTree_tune_RMSEs %>% knitr::kable("pipe")
# save it for later reference:
xgbTree_tune_RMSEs %>% knitr::kable("pipe") %>% save_kable("./figures/MODEL3 - xgbTree Step2 tuning.md")
xgbTree_tune_RMSEs %>% knitr::kable("pipe") %>% save_kable("./figures/MODEL3 - xgbTree Step2 tuning.txt")




### STEP3 - with fixed eta, max_depth, and min_child_weight,
#  I will try different values for row (subsample) and column sampling (colsample_bytree)
# I adjusted again boosting iters nrounds to better fit minimum seen on the graphs

best_max_depth <- fit_xgbTree_step2$bestTune$max_depth
best_min_child_weight <- fit_xgbTree_step2$bestTune$min_child_weight

tune_grid_step3 <- expand.grid(
  nrounds = seq(from = 200, to = 2000, by = 200),
  eta = best_eta,
  max_depth = best_max_depth,
  gamma = 0,
  colsample_bytree = c(0.6, 0.7, 0.8, 1.0),
  min_child_weight = best_min_child_weight,  
  subsample = c(0.5, 0.75, 0.9 , 1.0)
)

# run model with tuning step 3
fit_xgbTree_step3 = train( critical_temp ~ ., data=data, 
                           method = "xgbTree", trControl = tune_control,  tuneGrid = tune_grid_step3, verbose = TRUE, objective = "reg:squarederror"  )

#save model in the ./output directory
saveRDS(fit_xgbTree_step3,file="./output/model3-fit_xgbTree_step3")

ggplot(fit_xgbTree_step3) 
png(filename="./figures/MODEL3_fit_xgbTree_tune_step3.png")
ggplot(fit_xgbTree_step3)
dev.off()


fit_xgbTree_step3$bestTune %>% kable(caption = 'Best Tune Fit for model3: xgbTree step3 tuning', digits = 2, format = "pipe" )
# save it for later reference:
fit_xgbTree_step3$bestTune %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_BEST_TUNE_Step3.md")
fit_xgbTree_step3$bestTune %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_BEST_TUNE_Step3.txt")

fit_xgbTree_step3$results %>% kable(caption = 'Results for model3: xgbTree - step 3 tuning', digits = 2, format = "pipe" )
# save it for later reference:
fit_xgbTree_step3$results %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_RESULT_TUNE_Step3.md")
fit_xgbTree_step3$results %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_RESULT_TUNE_Step3.txt")


# this will consolidate parameters into one table:
# first let's check min RSME obtained during this step of tuning
min_RMSE <- min(fit_xgbTree_step3$results$RMSE)

# save result to the table:
xgbTree_tune_RMSEs <- bind_rows(xgbTree_tune_RMSEs, tibble(method = "MODEL3 - xgbTree step3 tuning", Best_RMSE = min_RMSE, 
                             nrounds = fit_xgbTree_step3$bestTune$nrounds,
                             max_depth = fit_xgbTree_step3$bestTune$max_depth,
                             eta = fit_xgbTree_step3$bestTune$eta,
                             gamma = fit_xgbTree_step3$bestTune$gamma,
                             colsample_bytree = fit_xgbTree_step3$bestTune$colsample_bytree,
                             min_child_weight = fit_xgbTree_step3$bestTune$min_child_weight,
                             subsample = fit_xgbTree_step3$bestTune$subsample) )

xgbTree_tune_RMSEs %>% knitr::kable("pipe")
# save it for later reference:
xgbTree_tune_RMSEs %>% knitr::kable("pipe") %>% save_kable("./figures/MODEL3 - xgbTree Step3 tuning.md")
xgbTree_tune_RMSEs %>% knitr::kable("pipe") %>% save_kable("./figures/MODEL3 - xgbTree Step3 tuning.txt")




### STEP4 - with fixed eta, max_depth, min_child_weight, colsample_bytree and subsample
#  I will fix gamma parameter now.


best_colsample_bytree <- fit_xgbTree_step3$bestTune$colsample_bytree
best_subsample <- fit_xgbTree_step3$bestTune$subsample

tune_grid_step4 <- expand.grid(
  nrounds = seq(from = 200, to = 3000, by = 200),
  eta = best_eta,
  max_depth = best_max_depth,
  gamma = c( 0.1, 0.5, 0.6 , 0.7, 0.8 , 0.9, 1.0),
  colsample_bytree = best_colsample_bytree,
  min_child_weight = best_min_child_weight, 
  subsample = best_subsample
)

# run model with tuning step 4
fit_xgbTree_step4 = train( critical_temp ~ ., data=data, 
                           method = "xgbTree", trControl = tune_control,  tuneGrid = tune_grid_step4, verbose = TRUE, objective = "reg:squarederror"  )


#save model in the ./output directory
saveRDS(fit_xgbTree_step4,file="./output/model3-fit_xgbTree_step4")

ggplot(fit_xgbTree_step4) 
png(filename="./figures/MODEL3_fit_xgbTree_tune_step4.png")
ggplot(fit_xgbTree_step4)
dev.off()


fit_xgbTree_step4$bestTune %>% kable(caption = 'Best Tune Fit for model3: xgbTree step4 tuning', digits = 2, format = "pipe" )
# save it for later reference:
fit_xgbTree_step4$bestTune %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_BEST_TUNE_Step4.md")
fit_xgbTree_step4$bestTune %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_BEST_TUNE_Step4.txt")

fit_xgbTree_step4$results %>% kable(caption = 'Results for model3: xgbTree - step4 tuning', digits = 2, format = "pipe" )
# save it for later reference:
fit_xgbTree_step4$results %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_RESULT_TUNE_Step4.md")
fit_xgbTree_step4$results %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_RESULT_TUNE_Step4.txt")


# this will consolidate parameters into one table:
# first let's check min RSME obtained during this step of tuning
min_RMSE <- min(fit_xgbTree_step4$results$RMSE)

# save result to the table:
xgbTree_tune_RMSEs <- bind_rows(xgbTree_tune_RMSEs, tibble(method = "MODEL3 - xgbTree step4 tuning", Best_RMSE = min_RMSE, 
                                                           nrounds = fit_xgbTree_step4$bestTune$nrounds,
                                                           max_depth = fit_xgbTree_step4$bestTune$max_depth,
                                                           eta = fit_xgbTree_step4$bestTune$eta,
                                                           gamma = fit_xgbTree_step4$bestTune$gamma,
                                                           colsample_bytree = fit_xgbTree_step4$bestTune$colsample_bytree,
                                                           min_child_weight = fit_xgbTree_step4$bestTune$min_child_weight,
                                                           subsample = fit_xgbTree_step4$bestTune$subsample) )

xgbTree_tune_RMSEs %>% knitr::kable("pipe")
# save it for later reference:
xgbTree_tune_RMSEs %>% knitr::kable("pipe") %>% save_kable("./figures/MODEL3 - xgbTree Step4 tuning.md")
xgbTree_tune_RMSEs %>% knitr::kable("pipe") %>% save_kable("./figures/MODEL3 - xgbTree Step4 tuning.txt")



### STEP5 - with fixed all parameters, I will now reduce the eta (learning rate)

best_gamma <- fit_xgbTree_step4$bestTune$gamma

tune_grid_step5 <- expand.grid(
  nrounds = seq(from = 200, to = 3000, by = 200),
  eta = c( 0.015, 0.02, 0.025, 0.03, 0.05),
  max_depth = best_max_depth,
  gamma = best_gamma,
  colsample_bytree = best_colsample_bytree,
  min_child_weight = best_min_child_weight, 
  subsample = best_subsample
)


# run model with tuning step 5
fit_xgbTree_step5 = train( critical_temp ~ ., data=data, 
                           method = "xgbTree", trControl = tune_control,  tuneGrid = tune_grid_step5, verbose = TRUE, objective = "reg:squarederror"  )



#save model in the ./output directory
saveRDS(fit_xgbTree_step5,file="./output/model3-fit_xgbTree_step5")

ggplot(fit_xgbTree_step5) 
png(filename="./figures/MODEL3_fit_xgbTree_tune_step5.png")
ggplot(fit_xgbTree_step5)
dev.off()


fit_xgbTree_step5$bestTune %>% kable(caption = 'Best Tune Fit for model3: xgbTree step5 tuning', digits = 2, format = "pipe" )
# save it for later reference:
fit_xgbTree_step5$bestTune %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_BEST_TUNE_Step5.md")
fit_xgbTree_step5$bestTune %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_BEST_TUNE_Step5.txt")

fit_xgbTree_step5$results %>% kable(caption = 'Results for model3: xgbTree - step5 tuning', digits = 2, format = "pipe" )
# save it for later reference:
fit_xgbTree_step5$results %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_RESULT_TUNE_Step5.md")
fit_xgbTree_step5$results %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_RESULT_TUNE_Step5.txt")


# this will consolidate parameters into one table:
# first let's check min RSME obtained during this step of tuning
min_RMSE <- min(fit_xgbTree_step5$results$RMSE)

# save result to the table:
xgbTree_tune_RMSEs <- bind_rows(xgbTree_tune_RMSEs, tibble(method = "MODEL3 - xgbTree step5 tuning", Best_RMSE = min_RMSE, 
                                                           nrounds = fit_xgbTree_step5$bestTune$nrounds,
                                                           max_depth = fit_xgbTree_step5$bestTune$max_depth,
                                                           eta = fit_xgbTree_step5$bestTune$eta,
                                                           gamma = fit_xgbTree_step5$bestTune$gamma,
                                                           colsample_bytree = fit_xgbTree_step5$bestTune$colsample_bytree,
                                                           min_child_weight = fit_xgbTree_step5$bestTune$min_child_weight,
                                                           subsample = fit_xgbTree_step5$bestTune$subsample) )

xgbTree_tune_RMSEs %>% knitr::kable("pipe")
# save it for later reference:
xgbTree_tune_RMSEs %>% knitr::kable("pipe") %>% save_kable("./figures/MODEL3 - xgbTree Step5 tuning.md")
xgbTree_tune_RMSEs %>% knitr::kable("pipe") %>% save_kable("./figures/MODEL3 - xgbTree Step5 tuning.txt")




### STEP6 - in this last step I will use the final eta, and increase cv to 10,
# as well as adjusted boosting (nrounds) last time to better fit minimum seen on the graphs
# also increasing cv folds to 10

tune_control <- caret::trainControl(
  method = "cv", 
  number = 10,
  verboseIter = TRUE,
  allowParallel = TRUE 
)


final_eta <- fit_xgbTree_step5$bestTune$eta

tune_grid_step6 <- expand.grid(
  nrounds = seq(from = 2500, to = 4500, by = 100),
  eta = final_eta,
  max_depth = best_max_depth,
  gamma = best_gamma,
  colsample_bytree = best_colsample_bytree,
  min_child_weight = best_min_child_weight, 
  subsample = best_subsample
)



# run model with tuning step 6
fit_xgbTree_step6 = train( critical_temp ~ ., data=data, 
                            method = "xgbTree", trControl = tune_control,  tuneGrid = tune_grid_step6, verbose = TRUE, objective = "reg:squarederror"  )




#save model in the ./output directory
saveRDS(fit_xgbTree_step6,file="./output/model3-fit_xgbTree_step6")

ggplot(fit_xgbTree_step6) 
png(filename="./figures/MODEL3_fit_xgbTree_tune_step6.png")
ggplot(fit_xgbTree_step6)
dev.off()


fit_xgbTree_step6$bestTune %>% kable(caption = 'Best Tune Fit for model3: xgbTree step6 tuning', digits = 2, format = "pipe" )
# save it for later reference:
fit_xgbTree_step6$bestTune %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_BEST_TUNE_Step6.md")
fit_xgbTree_step6$bestTune %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_BEST_TUNE_Step6.txt")

fit_xgbTree_step6$results %>% kable(caption = 'Results for model3: xgbTree - step6 tuning', digits = 2, format = "pipe" )
# save it for later reference:
fit_xgbTree_step6$results %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_RESULT_TUNE_Step6.md")
fit_xgbTree_step6$results %>% knitr::kable("pipe") %>% save_kable("./figures/FIT_XGBTREE_RESULT_TUNE_Step6.txt")


# this will consolidate parameters into one table:
# first let's check min RSME obtained during this step of tuning
min_RMSE <- min(fit_xgbTree_step6$results$RMSE)

# save result to the table:
xgbTree_tune_RMSEs <- bind_rows(xgbTree_tune_RMSEs, tibble(method = "MODEL3 - xgbTree step6 tuning", Best_RMSE = min_RMSE, 
                                                           nrounds = fit_xgbTree_step6$bestTune$nrounds,
                                                           max_depth = fit_xgbTree_step6$bestTune$max_depth,
                                                           eta = fit_xgbTree_step6$bestTune$eta,
                                                           gamma = fit_xgbTree_step6$bestTune$gamma,
                                                           colsample_bytree = fit_xgbTree_step6$bestTune$colsample_bytree,
                                                           min_child_weight = fit_xgbTree_step6$bestTune$min_child_weight,
                                                           subsample = fit_xgbTree_step6$bestTune$subsample) )

xgbTree_tune_RMSEs %>% knitr::kable("pipe")
# save it for later reference:
xgbTree_tune_RMSEs %>% knitr::kable("pipe") %>% save_kable("./figures/MODEL3 - xgbTree Step6 tuning.md")
xgbTree_tune_RMSEs %>% knitr::kable("pipe") %>% save_kable("./figures/MODEL3 - xgbTree Step6 tuning.txt")




# Let's check performance on the validation set of the final model with fully tuned hyperparameters
####################################################################################################
# Apply my xgbTree model on the validation set 
####################################################################################################

# before hyperparameters tuning
pred_xgbTree_tuned <- predict(fit_xgbTree_step6, valid)
model3_rmse_tuned <- RMSE(validation$critical_temp,pred_xgbTree_tuned)       #     on April 11th 18:xx

# save result to the table:
RMSE_VAL <- bind_rows(RMSE_VAL, tibble(method = "MODEL 3 - xgbTree - tuned", RMSE_VALIDATION = model3_rmse_tuned))
RMSE_VAL %>% knitr::kable("pipe")

# save it for a later use:
RMSE_VAL %>% knitr::kable("pipe") %>% save_kable("./figures/RMSE_VAL_After_Model3_final_tuned.md")
RMSE_VAL %>% knitr::kable("pipe") %>% save_kable("./figures/RMSE_VAL_After_Model3_final_tuned.txt")


#check XgbTree importance:
  importance_matrix <- xgb.importance(model = fit_xgbTree_step6)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

# explore feature importance
#varImp(fit_xgbTree_step6,scale=FALSE)
feature_importance <- xgboost::xgb.importance(fit_xgbTree_step6$finalModel$feature_names,model=fit_xgbTree_step6$finalModel)


feature_importance %>% top_n(10) %>% knitr::kable("pipe")
feature_importance %>% top_n(10) %>% knitr::kable("pipe") %>% save_kable("./figures/Final_model_feat_imp.txt")

####################################################################################################
# Display graph of predicted vs real CT
####################################################################################################

pred_real <- tibble(predicted = pred_xgbTree_tuned, real = valid$critical_temp ) 

png(filename="./figures/FINAL_PRED_VS_REAL.png")
ggplot(NULL,aes(x=real, y=predicted)) + geom_point(data = pred_real, col = "red") + 
  geom_point(data = pred_real, col = "blue") +
  geom_smooth(data = pred_real, method = "lm", se = FALSE,col = "black") + 
  labs(x = "real critical temperature [K]", y= "predicted critical temperature [K]")
dev.off()


