rm(list = ls())


# Import packages
packages <- c("dplyr",
              "pls",
              "stringr",
              "tidyverse",
              "ggplot2",
              "Matrix",
              "leaps", #variable selection
              "car", #partial residual plots
              "psych", #pairs.panels
              "blm",
              "betareg",
              "visdat", # graph missing values
              "naniar",
              "VIM",
              "caTools", # to split train data
              "DMwR", # KNN imputation of missing values
              "mice", # Impute missing values both continuous and factor var
              "rpart", # Impute missing values both continuous and factor var
              "aod",
              "randomForest",
              "bst",
              "xgboost",
              "gbm",
              "import",
              "caret",
              "PerformanceAnalytics",
              "forcats",
              "e1071",
              "MLmetrics",
              "pROC",
              "gtools")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = T)) install.packages(x)
  if (! (x %in% (.packages() ))) library(x, character.only = T)
})

stepAIC <- MASS::stepAIC
combinations <- gtools::combinations


# Import data
#train <- read.csv("train.csv", na.strings = "")
train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <- read.csv("test.csv", stringsAsFactors = FALSE)
GPU <- read.csv("GPU_details.csv", stringsAsFactors = FALSE)






#############################################################################################
####################################    Data Exploration    #################################
#############################################################################################

# Extract dimensions
dim(train)
dim(test)

# Descriptive statistics
summary(train)
str(train)

for (i in colnames(train)){
  if (!is.numeric(train[,i])){
    print(i)
    print(factor(train[,i]))
  }
}

# Missingness
train_nona <- train %>% mutate_all(na_if,"")
vis_miss(train_nona,cluster= TRUE) # VIM package for missing values
gg_miss_var(train_nona)
gg_miss_case(train_nona)


# Correlation

#dev.off()
library(corrgram)
nums <- unlist(lapply(train, is.numeric))
tr22 <- train
colnames(tr22)[12] <- "keyboard"
corrgram(tr22[ ,nums], order=TRUE, upper.panel=panel.cor, font.labels=2, cex.labels=1)






#############################################################################################
############################    Preprocessing train and test data    ########################
#############################################################################################

#------------------- adding additional variables ---------------------------#

# Remove whitespaces from GPU and train dataset
train$gpu <- str_trim(train$gpu, "both")
train$gpu <- gsub("  ", " ", train$gpu)
train$gpu <- gsub("[[:space:]]", " ", train$gpu)
test$gpu <- str_trim(test$gpu, "both")
test$gpu <- gsub("  ", " ", test$gpu)
test$gpu <- gsub("[[:space:]]", " ", test$gpu)
GPU$gpu <- paste0(GPU$Brand, " ", GPU$Product.Name)
GPU$gpu <- gsub("  ", " ", GPU$gpu)
GPU$gpu <- gsub("   ", " ", GPU$gpu)
GPU$gpu <- gsub("[[:space:]]", " ", GPU$gpu)
GPU$gpu <- str_trim(GPU$gpu, "both")


# Add videocards without word 'Graphics' in the name (since it is not present in train dataset's GPU column)
fix <- gsub("Graphics", "", GPU$gpu)
fix <- gsub("  ", " ", fix)
fix <- gsub("   ", " ", fix)
fix <- gsub("[[:space:]]", " ", fix)
fix <- str_trim(fix, "both")
#fix <- str_replace_all(fix, fixed(" "), "")
GPU$fix <- fix
GPU_t <- unique(rbind(GPU %>% select(-fix), GPU %>% select(-gpu) %>% rename(gpu = fix)))
GPU_t <- GPU_t[!duplicated(GPU_t$gpu), ]


# Join GPU details with train and test datasets
train_2 <- unique(left_join(train, GPU_t[, -(2:5)], by = "gpu"))
test_2 <- unique(left_join(test, GPU_t[, -(2:5)], by = "gpu"))
#View(train_2[is.na(train_2$Memory) & grepl("AMD", train_2$gpu), ])



### Replace unknown video cards with most popular ones for AMD, NVIDIA, Intel

# Select top 1 most popular gpu per brand
vids <- train_2 %>% group_by(Brand, gpu) %>% summarise(N = n()) %>% top_n(1)
cnames <- colnames(GPU_t[, -c(2:5, 10)])

# Replace unknown AMD GPUs with most popular AMD gpu in dataset
Amd <- GPU_t %>%
  filter(gpu %in% vids$gpu[grep("AMD", vids$Brand)]) %>% select(cnames) %>% as.data.frame()
Amd2 <- Amd[rep(row.names(Amd), dim(train_2[is.na(train_2$Memory) & grepl("AMD", train_2$gpu), cnames])[1]),]

# Replace unknown Intel GPUs with most popular Intel gpu in dataset
Intel <- GPU_t %>%
  filter(gpu %in% vids$gpu[grep("Intel", vids$Brand)]) %>% select(cnames) %>% as.data.frame()
Intel <- Intel[rep(row.names(Intel), dim(train_2[is.na(train_2$Memory) & grepl("Intel", train_2$gpu), cnames])[1]),]

# Replace unknown NVIDIA GPUs with most popular NVIDIA gpu in dataset
Nvidia <- GPU_t %>%
  filter(gpu %in% vids$gpu[grep("NVIDIA", vids$Brand)]) %>% select(cnames) %>% as.data.frame()
Nvidia <- Nvidia[rep(row.names(Nvidia), dim(train_2[is.na(train_2$Memory) & grepl("NVIDIA", train_2$gpu), cnames])[1]),]

train_2[is.na(train_2$Memory) & grepl("AMD", train_2$gpu), cnames] <- Amd2
train_2[is.na(train_2$Memory) & grepl("Intel", train_2$gpu), cnames] <- Intel
train_2[is.na(train_2$Memory) & grepl("NVIDIA", train_2$gpu), cnames] <- Nvidia

# Replace 'Other' GPUs or blank GPUs with most popular AMD gpu (due to AMD being most popular dedicated GPU)
train_2$gpu[train_2$gpu == ""] <- "Unknown"
Other <- Amd[rep(row.names(Amd), dim(train_2[is.na(train_2$Memory), cnames])[1]),]
# Replace brand with brands (1st name in GPU name) from our original dataset
Other$Brand <- unlist(lapply(strsplit(train_2$gpu[is.na(train_2$Memory)], " "), `[[`, 1))
train_2[is.na(train_2$Memory), cnames] <- Other


#Reduce(setdiff, strsplit(c(GPU$fix[1243], test$gpu[234]), split = ""))
#Reduce(setdiff, strsplit(c(GPU$fix[1244], test$gpu[1329]), split = ""))

# SSD is possible substitute for HDD(storage), so these features should not be analyzed independently but in relation with each oth




### Split the string into individual words, find GHz and extract it
splitString <- strsplit(train_2$cpu_details[train_2$cpu_details != ""], " ")
cpu_ghz <- c()
for (i in 1:length(splitString)) {
  loc <- grep("GHz", splitString[[i]])
  if (length(loc) != 0) {
    cpu_ghz[[i]] <- as.numeric(splitString[[i]][loc-1])
  }
}

# Add GHz column to train dataset
train_2$ghz <- NA
train_2$ghz[train_2$cpu_details != ""] <- cpu_ghz




### Add threading variable, remove excess parenthesis
train_2$thread <- sapply(strsplit(train_2$cpu_details, ' ', fixed = TRUE), function(i) 
  paste(grep('Thread', i, value = TRUE, fixed = TRUE), collapse = ' '))
train_2$thread <- gsub(")", "", train_2$thread)
# Blank values replaced with no-threading
train_2$thread[train_2$thread == ""] <- "No-Threading"




### Split the string into individual words, find GPU memory and extract it
splitString_gpu <- strsplit(train_2$Memory, " ")
gpu_mb <- c()
for (i in 1:length(splitString_gpu)) {
  loc_gb <- grep("GB", splitString_gpu[[i]])
  loc_mb <- grep("MB", splitString_gpu[[i]])
  if (length(loc_gb) != 0) {
    gpu_mb[[i]] <- as.numeric(splitString_gpu[[i]][loc_gb-1]) * 1024
  }
  else if (length(loc_mb) != 0) {
    gpu_mb[[i]] <- as.numeric(splitString_gpu[[i]][loc_mb-1])
  } else {
    gpu_mb[[i]] <- 0
  }
}

# View(cpu_ghz)
# Add memory variable to train dataset
train_2$gpu_memory <- gpu_mb
train_2$Memory <- NULL



### Add Shaders, TMU, ROP for gpu
t <- str_split_fixed((train_2$`Shaders...TMUs...ROPs`)," / ", 3) %>% as.data.frame()
train_2$gpu_shaders <- as.numeric(t$V1)
train_2$gpu_tmu <- as.numeric(t$V2)
train_2$gpu_rop <- as.numeric(t$V3)
train_2$Shaders...TMUs...ROPs <- NULL



### Add cores variable to train dataset (extracted from cpu_details)
train_2$cores <- NA
train_2$cores[grep("Dual-Core", train_2$cpu_details)] <- 2  
train_2$cores[grep("Quad-Core", train_2$cpu_details)] <- 4
train_2$cores[grep("Hexa-Core", train_2$cpu_details)] <- 6
train_2$cores[grep("Octa-Core", train_2$cpu_details)] <- 8 



### Create Apple brand premium as dummy variable
train_2$brand_premium <- ifelse(train_2$brand == "Apple", 1, 0)



# Change GPU.clock to numeric variable
train_2$GPU.clock <- as.numeric(gsub(" MHz", "", train_2$GPU.clock))

# Change Memory clock to numeric
train_2$Memory.clock[grepl("System Shared", train_2$Memory.clock)] <- "0"
train_2$Memory.clock <- as.numeric(gsub(" MHz", "", train_2$Memory.clock))



### Create perks variable 

# 1 if bluetooth present
train_2$bluetooth <- as.numeric(str_detect(tolower(train_2$name), "bluetooth"))

# 1 if webcam present
train_2$webcam <- as.numeric(str_detect(tolower(train_2$name), "webcam"))

# 1 if usb present
train_2$usb <- as.numeric(str_detect(tolower(train_2$name), "usb"))

# 1 if wifi present
train_2$wifi <- as.numeric(str_detect(tolower(train_2$name), "wifi"))

# 1 if dvd present
train_2$dvd <- as.numeric(str_detect(tolower(train_2$name), "dvd"))

# 1 if hdmi present
train_2$hdmi <- as.numeric(str_detect(tolower(train_2$name), "hdmi"))

# 1 if fingerprint reader present
train_2$fingerprint <- as.numeric(str_detect(tolower(train_2$name), "fingerprint"))

# 1 if gold colour reader present
train_2$gold <- as.numeric(str_detect(tolower(train_2$name), "gold"))

# weighted number of perks (dvd, hdmi, fingerprint, gold, etc)
train_2$perks <- (train_2$bluetooth + train_2$webcam + train_2$usb +
                    train_2$wifi + train_2$dvd + train_2$hdmi + train_2$fingerprint + 
                    train_2$gold)/8

train_2$bluetooth <- NULL
train_2$webcam <- NULL
train_2$usb <- NULL
train_2$wifi <- NULL
train_2$dvd <- NULL
train_2$hdmi <- NULL
train_2$fingerprint <- NULL
train_2$gold <- NULL



### Rename
train_2 <- train_2 %>%  rename(gpu_brand=Brand, 
                               gpu_clock=GPU.clock,
                               gpu_memory_clock=Memory.clock)









############################ the same procedure with test data #################################


# Replace unknown AMD GPUs with most popular AMD gpu in dataset
Amd <- GPU_t %>%
  filter(gpu %in% vids$gpu[grep("AMD", vids$Brand)]) %>% select(cnames) %>% as.data.frame()
Amd2 <- Amd[rep(row.names(Amd), dim(test_2[is.na(test_2$Memory) & grepl("AMD", test_2$gpu), cnames])[1]),]

# Replace unknown Intel GPUs with most popular Intel gpu in dataset
Intel <- GPU_t %>%
  filter(gpu %in% vids$gpu[grep("Intel", vids$Brand)]) %>% select(cnames) %>% as.data.frame()
Intel <- Intel[rep(row.names(Intel), dim(test_2[is.na(test_2$Memory) & grepl("Intel", test_2$gpu), cnames])[1]),]

# Replace unknown NVIDIA GPUs with most popular NVIDIA gpu in dataset
Nvidia <- GPU_t %>%
  filter(gpu %in% vids$gpu[grep("NVIDIA", vids$Brand)]) %>% select(cnames) %>% as.data.frame()
Nvidia <- Nvidia[rep(row.names(Nvidia), dim(test_2[is.na(test_2$Memory) & grepl("NVIDIA", test_2$gpu), cnames])[1]),]


test_2[is.na(test_2$Memory) & grepl("AMD", test_2$gpu), cnames] <- Amd2
test_2[is.na(test_2$Memory) & grepl("Intel", test_2$gpu), cnames] <- Intel
test_2[is.na(test_2$Memory) & grepl("NVIDIA", test_2$gpu), cnames] <- Nvidia


# Replace Other GPUs or blank GPUs with most popular AMD gpu (due to AMD being most popular dedicated GPU)
test_2$gpu[test_2$gpu == ""] <- "Unknown"
Other <- Amd[rep(row.names(Amd), dim(test_2[is.na(test_2$Memory), cnames])[1]),]

# Replace brand with brands (1st name in GPU name) from our original dataset
Other$Brand <- unlist(lapply(strsplit(test_2$gpu[is.na(test_2$Memory)], " "), `[[`, 1))
test_2[is.na(test_2$Memory), cnames] <- Other



### Split the string into individual words, find GHz and extract it
test_2$cpu_details <- as.character(test_2$cpu_details)
splitString <- strsplit(test_2$cpu_details[test_2$cpu_details != ""], " ")
cpu_ghz <- c()
for (i in 1:length(splitString)) {
  loc <- grep("GHz", splitString[[i]])
  if (length(loc) != 0) {
    cpu_ghz[[i]] <- as.numeric(splitString[[i]][loc-1])
  }
}

# Add GHz column to test dataset
test_2$ghz <- NA
test_2$ghz[test_2$cpu_details != ""] <- cpu_ghz



### Add threading variable, remove excess parenthesis
test_2$thread <- sapply(strsplit(test_2$cpu_details, ' ', fixed = TRUE), function(i) 
  paste(grep('Thread', i, value = TRUE, fixed = TRUE), collapse = ' '))
test_2$thread <- gsub(")", "", test_2$thread)

# Blank values replaced with no-threading
test_2$thread[test_2$thread == ""] <- "No-Threading"



### Split the string into individual words, find GPU memory and extract it
splitString_gpu <- strsplit(test_2$Memory, " ")
gpu_mb <- c()
for (i in 1:length(splitString_gpu)) {
  loc_gb <- grep("GB", splitString_gpu[[i]])
  loc_mb <- grep("MB", splitString_gpu[[i]])
  if (length(loc_gb) != 0) {
    gpu_mb[[i]] <- as.numeric(splitString_gpu[[i]][loc_gb-1]) * 1024
  }
  else if (length(loc_mb) != 0) {
    gpu_mb[[i]] <- as.numeric(splitString_gpu[[i]][loc_mb-1])
  } else {
    gpu_mb[[i]] <- 0
  }
}

# View(cpu_ghz)
# Add memory variable to test dataset
test_2$gpu_memory <- gpu_mb
test_2$Memory <- NULL



### Add Shaders, TMU, ROP for gpu
t <- str_split_fixed((test_2$`Shaders...TMUs...ROPs`)," / ", 3) %>% as.data.frame()
test_2$gpu_shaders <- as.numeric(t$V1)
test_2$gpu_tmu <- as.numeric(t$V2)
test_2$gpu_rop <- as.numeric(t$V3)
test_2$Shaders...TMUs...ROPs <- NULL



# Add cores variable to test dataset (extracted from cpu_details)
test_2$cores <- NA
test_2$cores[grep("Dual-Core", test_2$cpu_details)] <- 2  
test_2$cores[grep("Quad-Core", test_2$cpu_details)] <- 4
test_2$cores[grep("Hexa-Core", test_2$cpu_details)] <- 6
test_2$cores[grep("Octa-Core", test_2$cpu_details)] <- 8 



# Create apple brand premium as dummy variable
test_2$brand_premium <- ifelse(test_2$brand == "Apple", 1, 0)



# Change GPU.clock to numeric variable
test_2$GPU.clock <- as.numeric(gsub(" MHz", "", test_2$GPU.clock))



# Change Memory clock to numeric
test_2$Memory.clock[grepl("System Shared", test_2$Memory.clock)] <- "0"
test_2$Memory.clock <- as.numeric(gsub(" MHz", "", test_2$Memory.clock))



# 1 if bluetooth present
test_2$bluetooth <- as.numeric(str_detect(tolower(test_2$name), "bluetooth"))

# 1 if webcam present
test_2$webcam <- as.numeric(str_detect(tolower(test_2$name), "webcam"))

# 1 if usb present
test_2$usb <- as.numeric(str_detect(tolower(test_2$name), "usb"))

# 1 if wifi present
test_2$wifi <- as.numeric(str_detect(tolower(test_2$name), "wifi"))

# 1 if dvd present
test_2$dvd <- as.numeric(str_detect(tolower(test_2$name), "dvd"))

# 1 if hdmi present
test_2$hdmi <- as.numeric(str_detect(tolower(test_2$name), "hdmi"))

# 1 if fingerprint reader present
test_2$fingerprint <- as.numeric(str_detect(tolower(test_2$name), "fingerprint"))

# 1 if gold colour reader present
test_2$gold <- as.numeric(str_detect(tolower(test_2$name), "gold"))

# Number of perks (dvd, hdmi, fingerprint, gold, etc) as a percentage
test_2$perks <- (test_2$bluetooth + test_2$webcam + test_2$usb +
                   test_2$wifi + test_2$dvd + test_2$hdmi + test_2$fingerprint + 
                   test_2$gold)/8

test_2$bluetooth <- NULL
test_2$webcam <- NULL
test_2$usb <- NULL
test_2$wifi <- NULL
test_2$dvd <- NULL
test_2$hdmi <- NULL
test_2$fingerprint <- NULL
test_2$gold <- NULL




# Rename
test_2 <- test_2 %>%  rename(gpu_brand=Brand, 
                             gpu_clock=GPU.clock,
                             gpu_memory_clock=Memory.clock)






########################### Additional data cleaning #####################################

# Screen_surface - to lowercase (previously Matte and matte as separate classes)
train_2$screen_surface <- tolower(train_2$screen_surface)
test_2$screen_surface <- tolower(test_2$screen_surface)


# Rename
colnames(train_2)[1] <- "id" # renames column
colnames(test_2)[1] <- "id" # renames column


# Change all empty cells to NA
train_2 <- train_2 %>% mutate_all(na_if,"")
test_2 <- test_2 %>% mutate_all(na_if,"")





########################### Missing value imputation #####################################


#-------- Imputation of NA values with kNN on train dataset

train_clean1 <- train_2
train_clean1$name <- NULL
train_clean1$base_name <- NULL
train_clean1$cpu_details <- NULL
train_clean1$os_details <- NULL
train_clean1$gpu <- NULL

train_clean1$brand <- as.factor(train_clean1$brand)
train_clean1$cpu <- as.factor(train_clean1$cpu)
train_clean1$gpu_brand <- as.factor(train_clean1$gpu_brand)
train_clean1$screen_surface <- as.factor(train_clean1$screen_surface)
train_clean1$os <- as.factor(train_clean1$os)
train_clean1$thread <- as.factor(train_clean1$thread)


# View missing values - VIM package
vis_miss(train_clean1,cluster= TRUE) # VIM package for missing values
gg_miss_var(train_clean1)
gg_miss_case(train_clean1)

aggr(x = train_clean1)
glimpse(train_clean1)

set.seed(123)
clean2_knn <- knnImputation(train_clean1, k = 10)

aggr(x=clean2_knn)
clean2_knn %>%
  summarise_if(is.factor,nlevels) # too many levels in cpu -- do not use
glimpse(clean2_knn)





#-------- Check for multicollinearity

cor(clean2_knn$pixels_x, clean2_knn$pixels_y) #-- very high correlation -- use only pixels_y


vars3 <- colnames(clean2_knn)
VIF <- solve(cor(clean2_knn[, vars3[!(vars3 %in% c("id", "brand", "pixels_x", "screen_surface", "cpu", "thread", "os", "min_price", "max_price", "gpu_brand"))]]))
diag(VIF) # gpu_memory_clock VIF 41.62 - delete variable


# Wihout gpu_memory_clock
VIF2 <- solve(cor(clean2_knn[, vars3[!(vars3 %in% c("gpu_memory_clock", "id", "brand", "pixels_x", "screen_surface", "cpu", "thread", "os", "min_price", "max_price", "gpu_brand"))]]))
diag(VIF2) # discrete_gpu VIF 14.32 - delete variable

# Wihout gpu_memory_clock and discrete_gpu
VIF3 <- solve(cor(clean2_knn[, vars3[!(vars3 %in% c("discrete_gpu", "gpu_memory_clock", "id", "brand", "pixels_x", "screen_surface", "cpu", "thread", "os", "min_price", "max_price", "gpu_brand"))]]))
diag(VIF3) # All VIFs < 10 no more multicollinearity


mean(diag(VIF3)) # not much larger than 1 -- good


# Remove
clean2_knn$discrete_gpu <- NULL
clean2_knn$gpu_memory_clock <- NULL







#-------- Imputation of NA values with kNN on test dataset

test_clean1 <- test_2
test_clean1$name <- NULL
test_clean1$base_name <- NULL
test_clean1$cpu_details <- NULL
test_clean1$os_details <- NULL
test_clean1$gpu <- NULL


test_clean1$brand <- as.factor(test_clean1$brand)
test_clean1$cpu <- as.factor(test_clean1$cpu)
test_clean1$gpu_brand <- as.factor(test_clean1$gpu_brand)
test_clean1$screen_surface <- as.factor(test_clean1$screen_surface)
test_clean1$os <- as.factor(test_clean1$os)
test_clean1$thread <- as.factor(test_clean1$thread)


vis_miss(test_clean1,cluster= TRUE) # VIM package for missing values
gg_miss_var(test_clean1)
gg_miss_case(test_clean1)


aggr(x = test_clean1)
glimpse(test_clean1)
dist_data <- train_clean1
dist_data$min_price <- NULL
dist_data$max_price <- NULL
set.seed(123)
clean2_knn_test <- knnImputation(test_clean1, distData = dist_data, k = 10)
aggr(x=clean2_knn_test)

clean2_knn_test %>%
  summarise_if(is.factor,nlevels)



clean2_knn_test$discrete_gpu <- NULL
clean2_knn_test$gpu_memory_clock <- NULL







#############################################################################################
#########################    Cross-validation    ############################################
#############################################################################################

## DO NOT USE THIS, BECAUSE WE USE CROSS-VALIDATION AND TEST MODELS IN THE LEADERBOARD
# set.seed(741)
# train_index <- sample(1:nrow(clean2_knn), 0.8 * nrow(clean2_knn))
# test_index <- setdiff(1:nrow(clean2_knn), train_index)

# data.training <- clean2_knn[train_index,]
# data.test <- clean2_knn[test_index,]



#-------------- 3 different methods for repeated cv:


###### 1 default

# Training control definition
set.seed(123)
train.control <- trainControl(method = "repeatedcv",
                              number = 5, repeats = 3)


# Used this cv to tune parameter ntrees for random forest. Results from the leaderboard:

# ntrees = 50   error: 333.71915917
# ntrees = 100  error: 329.98914464
# ntrees = 150  error: 329.22929872
# ntrees = 200  error: 330.05413688
# ntrees = 250  error: 328.63883385
# ntrees = 300  error: 328.48849774
# ntrees = 350  error: 329.42086368
# ntrees = 500  error: 331.73181822
# ntrees = 1000 error: 333.76290307
# ntrees = 1500 error: 335.35766153

tree.frame <- data.frame(ntrees = c(50,100,150,200,250,300,350,500,1000,1500), 
                         error = c(333.71915917, 329.98914464, 329.22929872,330.05413688,
                                   328.63883385, 328.48849774, 329.42086368, 331.73181822, 
                                   333.76290307, 335.35766153))

ggplot(data=tree.frame, aes(x=ntrees, y=error)) +
  geom_line(color="black") +
  geom_point(color="red") + 
  labs(y= "MAE", x = "Number of Trees") +
  scale_x_continuous(breaks = seq(100, 1500, by = 100)) 


### so chose ntrees = 300





## However, default repeated cv only tries 3 different values for mtry (another parameter of random forest)
## so tried grid search and random search with more checked values for mtry




###### 2 grid search -- better than random, a bit worse than default (error 334.64918312)
## (however, larger grid of mtry values tried, so better generalization)

set.seed(123)
train.control <- trainControl(method = "repeatedcv",
                                number = 5, repeats = 3, search = "grid")
set.seed(123)
tunegrid <- expand.grid(.mtry=c(1:25)) #(AND INSIDE TRAIN FUNCTION NEAR NTREES WRITE tuneGrid=tunegrid)





###### 3 random search -- produced worse results (error 342.93214978)

#set.seed(123)
train.control <- trainControl(method = "repeatedcv",
                              number = 5, repeats = 3, search = "random")
# (AND INSIDE TRAIN FUNCTION NEAR NTREES WRITE tuneLength=25)





#############################################################################################
################################    Modelling    ############################################
#############################################################################################



#------------ Training with MAX_PRICE and MIN_PRICE ------------#

#### Selecting features to use

maxPrice_Clean_Training <- clean2_knn %>% select(brand, screen_size, pixels_y, screen_surface, touchscreen, detachable_keyboard, os, ram, ssd, storage, weight, gpu_clock, ghz, thread, gpu_memory, gpu_shaders, gpu_tmu, gpu_rop, cores, brand_premium, perks, max_price)
minPrice_Clean_Training <- clean2_knn %>% select(brand, screen_size, pixels_y, screen_surface, touchscreen, detachable_keyboard, os, ram, ssd, storage, weight, gpu_clock, ghz, thread, gpu_memory, gpu_shaders, gpu_tmu, gpu_rop, cores, brand_premium, perks, min_price)




#------------ Train for MAX_PRICE

##### Train the model 5 eXtreme Gradient Boosting
set.seed(123)
model5_max <- train(max_price ~ . , data = maxPrice_Clean_Training,
                    method = "xgbTree", trControl = train.control, metric = "MAE")

##### Train the model 6 Parallel Random Forest
set.seed(123)
model6_max <- train(max_price ~ . , data = maxPrice_Clean_Training,
                    method = "parRF", trControl = train.control, metric = "MAE")

##### Train the model 7 Stochastic Gradient Boosting
set.seed(123)
model7_max <- train(max_price ~ . , data = maxPrice_Clean_Training,
                    method = "gbm", trControl = train.control, metric = "MAE")





#------------ Train for MIN_PRICE

##### Train the model 5 eXtreme Gradient Boosting
set.seed(123)
model5_min <- train(min_price ~ . , data = minPrice_Clean_Training,
                    method = "xgbTree", trControl = train.control, metric = "MAE")

##### Train the model 6 Parallel Random Forest
set.seed(123)
model6_min <- train(min_price ~ . , data = minPrice_Clean_Training,
                    method = "parRF", trControl = train.control, metric = "MAE", ntree = 300, tuneGrid=tunegrid)

##### Train the model 7 Stochastic Gradient Boosting
set.seed(123)
model7_min <- train(min_price ~ . , data = minPrice_Clean_Training,
                    method = "gbm", trControl = train.control, metric = "MAE")





#------- Summarize the results ----------------

#print(min(model5_max$results$MAE+model5_min$results$MAE))
#print(min(model6_max$results$MAE+model6_min$results$MAE))
#print(min(model7_max$results$MAE+model7_min$results$MAE))




# -------- Prediction of test data 

# Test data
Price_Test <- clean2_knn_test %>% select(brand, screen_size, pixels_y, screen_surface, touchscreen, detachable_keyboard, os, ram, ssd, storage, weight, gpu_clock, ghz, thread, gpu_memory, gpu_shaders, gpu_tmu, gpu_rop, cores, brand_premium, perks)



# ----------- Results ------------------


#Model 5
bothModels <- list(model5_min ,model5_max)
pred <- data.frame(predict(bothModels, Price_Test, type = "raw"))
names(pred) <- c("MIN","MAX")
 
results <- cbind(id_test,pred)

write.csv(results, file = "Model 5 (Extreme Gradient Boosting) - min max price.csv", row.names = F)


#Model 6
bothModels <- list(model6_min ,model6_max)
pred <- data.frame(predict(bothModels, Price_Test, type = "raw"))
names(pred) <- c("MIN","MAX")

results <- cbind(id_test,pred)

write.csv(results, file = "Model 6 (Parallel Random Forest) - min max price.csv", row.names = F)


#Model 7
bothModels <- list(model7_min ,model7_max)
pred <- data.frame(predict(bothModels, Price_Test, type = "raw"))
names(pred) <- c("MIN","MAX")

results <- cbind(id_test,pred)
results

write.csv(results, file = "Model 7 (Stochastic Gradient Boosting) - min max price.csv", row.names = F)







#------------ Training with RANGE and MIN_PRICE ------------#


# Selecting features to use and creating range variable
clean2_knn$price_range <- abs(clean2_knn$max_price - clean2_knn$min_price)
rangePrice_Clean_Training <- clean2_knn %>% select(brand, screen_size, pixels_y, screen_surface, touchscreen, detachable_keyboard, os, ram, ssd, storage, weight, gpu_clock, ghz, thread, gpu_memory, gpu_shaders, gpu_tmu, gpu_rop, cores, brand_premium, perks, price_range)



##### Train the model 5 eXtreme Gradient Boosting
set.seed(123)
model5_range <- train(price_range ~ . , data = rangePrice_Clean_Training,
                    method = "xgbTree", trControl = train.control, metric = "MAE")

##### Train the model 6 Parallel Random Forest
set.seed(123)
model6_range <- train(price_range ~ . , data = rangePrice_Clean_Training,
                    method = "parRF", trControl = train.control, metric = "MAE", ntree = 300, tuneGrid=tunegrid)
model6_range

##### Train the model 7 Stochastic Gradient Boosting
set.seed(123)
model7_range <- train(price_range ~ . , data = rangePrice_Clean_Training,
                    method = "gbm", trControl = train.control, metric = "MAE")





# -------- Prediction of test data

Price_Test <- clean2_knn_test %>% select(brand, screen_size, pixels_y, screen_surface, touchscreen, detachable_keyboard, os, ram, ssd, storage, weight, gpu_clock, ghz, thread, gpu_memory, gpu_shaders, gpu_tmu, gpu_rop, cores, brand_premium, perks)




# ----------- Results ------------------

#Model 5
bothModels <- list(model5_min ,model5_range)
pred <- data.frame(predict(bothModels, Price_Test, type = "raw"))
names(pred) <- c("MIN","DIFFERENCE")
pred$MAX <- pred$MIN + pred$DIFFERENCE
pred$DIFFERENCE <- NULL

results <- cbind(id_test,pred)
results

write.csv(results, file = "Model 5 (Extreme Gradient Boosting) - min range price.csv", row.names = F)


 
#Model 6
bothModels <- list(model6_min ,model6_range)
pred <- data.frame(predict(bothModels, Price_Test, type = "raw"))
names(pred) <- c("MIN","DIFFERENCE")

pred$MAX <- pred$MIN + pred$DIFFERENCE
pred$DIFFERENCE <- NULL

results <- cbind(id_test,pred)


write.csv(results, file = "Model 6 (Parallel Random Forest) - min range price.csv", row.names = F)

print(model6_min)
print(model6_range)

print(model6_min$finalModel)
print(model6_range$finalModel)



#Model 7
bothModels <- list(model7_min ,model7_range)
pred <- data.frame(predict(bothModels, Price_Test, type = "raw"))
names(pred) <- c("MIN","DIFFERENCE")
pred$MAX <- pred$MIN + pred$DIFFERENCE
pred$DIFFERENCE <- NULL

results <- cbind(id_test,pred)
results

write.csv(results, file = "Model 7 (Stochastic Gradient Boosting) - min range price.csv", row.names = F)







#------------ Feature Importance ------------#


library(Boruta)
set.seed(123)


###### Importance for price_range
str(clean2_knn)
x1<-clean2_knn %>% select(brand, screen_size, pixels_y, screen_surface, touchscreen, detachable_keyboard,
                          os, ram, ssd, storage, weight, gpu_clock, ghz, thread, gpu_memory, gpu_shaders, gpu_tmu,
                          gpu_rop, cores, brand_premium, perks, price_range)
str(x1)

# Train Boruta on the final model
x1_train <- Boruta(x1$price_range~., data = x1, doTrace = 2, ntree = 300, mtry = 2)
print(x1_train)

# Fix tentative importances
boruta.x1 <- TentativeRoughFix(x1_train)
print(boruta.x1)


boruta.x1[["finalDecision"]]
par(mar=c(10,4,1,1))  
plot(boruta.x1,  las=2, cex.lab=1, cex.axis=1, font=1,col.axis="black", xlab = "", ylab = "Importance")
mtext("Attributes", side=1, line=8)
cat("\n\nAttribute Importance Details:\n")
options(width=125)
data<-arrange(cbind(attr=rownames(attStats(boruta.x1)), attStats(boruta.x1)),desc(medianImp))


varImp(model6_range)

###### Importance for min_price
set.seed(123)
str(clean2_knn)
x2<-clean2_knn %>% select(brand, screen_size, pixels_y, screen_surface, touchscreen, detachable_keyboard,
                          os, ram, ssd, storage, weight, gpu_clock, ghz, thread, gpu_memory, gpu_shaders, gpu_tmu,
                          gpu_rop, cores, brand_premium, perks, min_price)
str(x2)


# Train Boruta on the final model
x2_train <- Boruta(x2$min_price~., data = x2, doTrace = 2, ntree = 300, mtry = 2)
print(x2_train)

# Fix tentative importances
boruta.x2 <- TentativeRoughFix(x2_train)
print(boruta.x2)


boruta.x2[["finalDecision"]]
par(mar=c(10,4,1,1))  
plot(boruta.x2,  las=2, cex.lab=1, cex.axis=1, font=1,col.axis="black", xlab = "", ylab = "Importance")
mtext("Attributes", side=1, line=8)
cat("\n\nAttribute Importance Details:\n")
options(width=125)
data2<-arrange(cbind(attr=rownames(attStats(boruta.x2)), attStats(boruta.x2)),desc(medianImp))
# write.csv(data2,"importance ranking2.csv")


varImp(model6_min)
