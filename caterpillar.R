###
### Caterpillar Tube Pricing
###


# setup
setwd("Documents/DataScience/kaggle/Caterpillar/")
library(dplyr)


###
### Model 1. Scripts provided on competition forum with a few minor edits. 
### 

# https://www.kaggle.com/ademyttenaere/caterpillar-tube-pricing/
#     build-complete-train-and-test-db/code
# and
# https://www.kaggle.com/ademyttenaere/caterpillar-tube-pricing/
#    0-2748-with-rf-and-log-transformation/code


# create complete dataset
base <- paste0("/Users/saraweinstein/Documents/DataScience/kaggle/Caterpillar",
               "/competition_data/")

test <- read.csv(paste0(base, "test_set.csv"))
train <- read.csv(paste0(base, "train_set.csv"))

train$id <- -(1:nrow(train))
test$cost <- 0
dFull <- rbind(train, test)

dFull$quote_date <- as.Date(as.character(dFull$quote_date))
# dFull$quote_date_jul <- julian(dFull$quote_date)
# dFull$quote_year <- format(dFull$quote_date, "%Y")
# dFull$quote_month <- format(dFull$quote_date, "%M")


# merge in the data also organized by tube_assembly_id
dFull <- merge(dFull, read.csv(paste0(base, "bill_of_materials.csv")), 
               by = "tube_assembly_id", all.x = TRUE)
dFull <- merge(dFull, read.csv(paste0(base, "specs.csv")), 
               by = "tube_assembly_id", all.x = TRUE)
dFull <- merge(dFull, read.csv(paste0(base, "tube.csv")), 
               by = "tube_assembly_id", all.x = TRUE)


# add columns specifying the form for each tube end
dFull <- left_join(dFull, read.csv(paste0(base, "tube_end_form.csv")),
                   by = c("end_a" = "end_form_id"))
colnames(dFull)[51] <- "end_a_form"

dFull <- left_join(dFull, read.csv(paste0(base, "tube_end_form.csv")),
                   by = c("end_x" = "end_form_id"))
colnames(dFull)[52] <- "end_x_form"


# add columns for each component, specifying the component type
comps <- read.csv(paste0(base, "components.csv"), quote = "")

# this is too ugly, need to use regex & loop
dFull <- left_join(dFull, comps[, -2], by = c("component_id_1" = "component_id"))
dFull <- left_join(dFull, comps[, -2], by = c("component_id_2" = "component_id"))
dFull <- left_join(dFull, comps[, -2], by = c("component_id_3" = "component_id"))
dFull <- left_join(dFull, comps[, -2], by = c("component_id_4" = "component_id"))
dFull <- left_join(dFull, comps[, -2], by = c("component_id_5" = "component_id"))
dFull <- left_join(dFull, comps[, -2], by = c("component_id_6" = "component_id"))
dFull <- left_join(dFull, comps[, -2], by = c("component_id_7" = "component_id"))
dFull <- left_join(dFull, comps[, -2], by = c("component_id_8" = "component_id"))
colnames(dFull)[53:60] <- c("component_1_type", "component_2_type", "component_3_type",
                            "component_4_type", "component_5_type", "component_6_type",
                            "component_7_type", "component_8_type")


# clean NA values: recode numeric NAs t0 -1
for(i in 1:ncol(dFull)) {
      if(is.numeric(dFull[, i])) {
            dFull[is.na(dFull[, i]), i] <- -1  
      } else {
            dFull[, i] <- as.character(dFull[, i])
            dFull[is.na(dFull[, i]), i] <- "NAvalue"
            dFull[, i] <- as.factor(dFull[, i])
      }
}


# clean variables with too many categories
# not comfortable with this step REVISIT/REVISE
for(i in 1:ncol(dFull)){
      if(!is.numeric(dFull[, i])){
            freq <- data.frame(table(dFull[, i]))
            freq <- freq[order(freq$Freq, decreasing = TRUE),]
            dFull[, i] <- as.character(match(dFull[, i], freq$Var1[1:30]))
            dFull[is.na(dFull[, i]), i] <- "rareValue"
            dFull[, i] <- as.factor(dFull[, i])
      }
}


# remove unneeded variables
# dFull <- dFull[, -match(c("tube_assembly_id", "quote_date", "quote_date_jul"), 
#                       names(dFull))]

dFull$tube_assembly_id <- NULL

test <- dFull[which(dFull$id > 0), ]
train <- dFull[which(dFull$id < 0), ]

cat("Final train dataset : ", nrow(train), " rows and ", ncol(train), " columns\n")
cat("Final test dataset : ", nrow(test), " rows and ", ncol(test), " columns\n")


# evaluate RF predictions by splitting training set 80%/20%
library(caTools)

spl <- sample.split(train$cost, 0.8)
dtrain <- subset(train, spl == TRUE)
dtest <- subset(train, spl == FALSE)
 
# train random forest on dtrain and make predictions on dtest
set.seed(123)
rf1 <- randomForest(dtrain$cost ~ . -id, dtrain, ntree = 10, do.trace = 2)
 
pred <- predict(rf1, dtest)
sqrt(mean((log(dtest$cost + 1) - log(pred + 1))^2))   # 0.2299717

# with log transformation
set.seed(123)
rf2 <- randomForest(log(dtrain$cost + 1) ~ . -id, dtrain, ntree = 10, do.trace = 2)
pred <- exp(predict(rf2, dtest)) - 1
sqrt(mean((log(dtest$cost + 1) - log(pred + 1))^2))   # 0.1777412



# train randomForest on the whole training set & make predictions
library(randomForest)

set.seed(123)
rf <- randomForest(log(train$cost + 1) ~ . -id, data = train, 
                   ntree = 20, do.trace = 2, importance = TRUE)

test$cost <- NULL
pred <- exp(predict(rf, newdata = test)) - 1


# check out the model
vu <- varUsed(rf, count = TRUE)
vusorted <- sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(rf$forest$xlevels[vusorted$ix]))


# write out submission file
submitDb <- data.frame(id = test$id, cost = pred)
submitDb <- aggregate(data.frame(cost = submitDb$cost), by = list(id = submitDb$id), mean)

write.csv(submitDb, "submit.csv", row.names = FALSE, quote = FALSE)





###
### Model 2. Reshaped bill_of_materials and specs
###

# re-worked the bill of materials and specs data files, reshaping to long format
# and retaining only the most frequent values (n=11 for bill_of_materials and
# n=25 for specs). created counts for each tube_assembly, indicating the total 
# number of specs (num_specs) and components (num_comps) for each.  also 
# created variable volume, which is volume of the tube cylinder


base <- paste0("/Users/saraweinstein/Documents/DataScience/kaggle/Caterpillar",
               "/competition_data/")

test <- read.csv(paste0(base, "test_set.csv"))
train <- read.csv(paste0(base, "train_set.csv"))

train$id <- -(1:nrow(train))
test$cost <- 0
dFull <- rbind(train, test)


# merge in the data also organized by tube_assembly_id
dFull <- merge(dFull, read.csv(paste0(base, "cleaned_data/bill_materials_ed.csv")), 
               by = "tube_assembly_id", all.x = TRUE)
dFull <- merge(dFull, read.csv(paste0(base, "cleaned_data/specs_ed.csv")), 
               by = "tube_assembly_id", all.x = TRUE)
dFull <- merge(dFull, read.csv(paste0(base, "tube.csv")), 
               by = "tube_assembly_id", all.x = TRUE)


# add columns specifying the form for each tube end
dFull <- left_join(dFull, read.csv(paste0(base, "tube_end_form.csv")),
                   by = c("end_a" = "end_form_id"))
colnames(dFull)[63] <- "end_a_form"

dFull <- left_join(dFull, read.csv(paste0(base, "tube_end_form.csv")),
                   by = c("end_x" = "end_form_id"))
colnames(dFull)[64] <- "end_x_form"

# code below removed as components are now encoded differently in dataset
# add columns for each component, specifying the component type
# comps <- read.csv(paste0(base, "components.csv"), quote = "")

# dFull <- left_join(dFull, comps[, -2], by = c("component_id_1" = "component_id"))
# dFull <- left_join(dFull, comps[, -2], by = c("component_id_2" = "component_id"))
# dFull <- left_join(dFull, comps[, -2], by = c("component_id_3" = "component_id"))
# dFull <- left_join(dFull, comps[, -2], by = c("component_id_4" = "component_id"))
# dFull <- left_join(dFull, comps[, -2], by = c("component_id_5" = "component_id"))
# dFull <- left_join(dFull, comps[, -2], by = c("component_id_6" = "component_id"))
# dFull <- left_join(dFull, comps[, -2], by = c("component_id_7" = "component_id"))
# dFull <- left_join(dFull, comps[, -2], by = c("component_id_8" = "component_id"))
# colnames(dFull)[56:63] <- c("component_1_type", "component_2_type", "component_3_type",
#                             "component_4_type", "component_5_type", "component_6_type",
#                             "component_7_type", "component_8_type")


# clean NA values: recode numeric NAs t0 -1
for(i in 1:ncol(dFull)) {
      if(is.numeric(dFull[, i])) {
            dFull[is.na(dFull[, i]), i] <- -1  
      } else {
            dFull[, i] <- as.character(dFull[, i])
            dFull[is.na(dFull[, i]), i] <- "NAvalue"
            dFull[, i] <- as.factor(dFull[, i])
      }
}


# clean variables with too many categories
for(i in 1:ncol(dFull)){
      if(!is.numeric(dFull[, i])){
            freq <- data.frame(table(dFull[, i]))
            freq <- freq[order(freq$Freq, decreasing = TRUE),]
            dFull[, i] <- as.character(match(dFull[, i], freq$Var1[1:30]))
            dFull[is.na(dFull[, i]), i] <- "rareValue"
            dFull[, i] <- as.factor(dFull[, i])
      }
}


# remove unneeded variables
dFull$tube_assembly_id <- NULL


# split back into train and test
test <- dFull[which(dFull$id > 0), ]
train <- dFull[which(dFull$id < 0), ]


# evaluate RF predictions by splitting training set 80%/20%
library(caTools)

spl <- sample.split(train$cost, 0.8)
dtrain <- subset(train, spl == TRUE)
dtest <- subset(train, spl == FALSE)

# train random forest on dtrain w log transformation and make 
# predictions on dtest
set.seed(123)

rf <- randomForest(log(dtrain$cost + 1) ~ . -id, dtrain, ntree = 10, do.trace = 2)
pred <- exp(predict(rf, dtest)) - 1
sqrt(mean((log(dtest$cost + 1) - log(pred + 1))^2))   # 0.1852868


# train randomForest on the whole training set & make predictions
library(randomForest)

set.seed(123)
rf <- randomForest(log(train$cost + 1) ~ . -id, data = train, 
                   ntree = 20, do.trace = 2, importance = TRUE)

test$cost <- NULL
pred <- exp(predict(rf, newdata = test)) - 1


# check out the model
vu <- varUsed(rf, count = TRUE)
vusorted <- sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(rf$forest$xlevels[vusorted$ix]))


# write out submission file
submitDb <- data.frame(id = test$id, cost = pred)
submitDb <- aggregate(data.frame(cost = submitDb$cost), 
                      by = list(id = submitDb$id), mean)

write.csv(submitDb, "submit2.csv", row.names = FALSE, quote = FALSE)



###
### Model 3. Remove volume, replace date with year and month as separate
### variables
###

base <- paste0("/Users/saraweinstein/Documents/DataScience/kaggle/Caterpillar",
               "/competition_data/")

test <- read.csv(paste0(base, "test_set.csv"))
train <- read.csv(paste0(base, "train_set.csv"))

train$id <- -(1:nrow(train))
test$cost <- 0
dFull <- rbind(train, test)

dFull$quote_date <- as.Date(as.character(dFull$quote_date))
dFull$quote_date_jul <- julian(dFull$quote_date)
dFull$quote_year <- format(dFull$quote_date, "%Y")
dFull$quote_month <- format(dFull$quote_date, "%M")

# merge in the data also organized by tube_assembly_id
dFull <- merge(dFull, read.csv(paste0(base, "cleaned_data/bill_materials_ed.csv")), 
               by = "tube_assembly_id", all.x = TRUE)
dFull <- merge(dFull, read.csv(paste0(base, "cleaned_data/specs_ed.csv")), 
               by = "tube_assembly_id", all.x = TRUE)
dFull <- merge(dFull, read.csv(paste0(base, "tube.csv")), 
               by = "tube_assembly_id", all.x = TRUE)


# add columns specifying the form for each tube end
dFull <- left_join(dFull, read.csv(paste0(base, "tube_end_form.csv")),
                   by = c("end_a" = "end_form_id"))
colnames(dFull)[66] <- "end_a_form"

dFull <- left_join(dFull, read.csv(paste0(base, "tube_end_form.csv")),
                   by = c("end_x" = "end_form_id"))
colnames(dFull)[67] <- "end_x_form"


# clean NA values: recode numeric NAs t0 -1
for(i in 1:ncol(dFull)) {
      if(is.numeric(dFull[, i])) {
            dFull[is.na(dFull[, i]), i] <- -1  
      } else {
            dFull[, i] <- as.character(dFull[, i])
            dFull[is.na(dFull[, i]), i] <- "NAvalue"
            dFull[, i] <- as.factor(dFull[, i])
      }
}


# clean variables with too many categories
for(i in 1:ncol(dFull)){
      if(!is.numeric(dFull[, i])){
            freq <- data.frame(table(dFull[, i]))
            freq <- freq[order(freq$Freq, decreasing = TRUE),]
            dFull[, i] <- as.character(match(dFull[, i], freq$Var1[1:30]))
            dFull[is.na(dFull[, i]), i] <- "rareValue"
            dFull[, i] <- as.factor(dFull[, i])
      }
}


# remove unneeded variables
dFull <- dFull[, -match(c("tube_assembly_id", "quote_date", "quote_date_jul"),
                        names(dFull))]


# split back into train and test
test <- dFull[which(dFull$id > 0), ]
train <- dFull[which(dFull$id < 0), ]


# evaluate RF predictions by splitting training set 80%/20%
library(caTools)

spl <- sample.split(train$cost, 0.8)
dtrain <- subset(train, spl == TRUE)
dtest <- subset(train, spl == FALSE)

# train random forest on dtrain w log transformation and make 
# predictions on dtest
set.seed(123)

rf <- randomForest(log(dtrain$cost + 1) ~ . -id, dtrain, ntree = 10, do.trace = 2)
pred <- exp(predict(rf, dtest)) - 1
sqrt(mean((log(dtest$cost + 1) - log(pred + 1))^2))   # 0.1853548


# train randomForest on the whole training set & make predictions
library(randomForest)

set.seed(123)
rf <- randomForest(log(train$cost + 1) ~ . -id, data = train, 
                   ntree = 20, do.trace = 2, importance = TRUE)

test$cost <- NULL
pred <- exp(predict(rf, newdata = test)) - 1


# check out the model
vu <- varUsed(rf, count = TRUE)
vusorted <- sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(rf$forest$xlevels[vusorted$ix]))
varImpPlot(rf)


# write out submission file
submitDb <- data.frame(id = test$id, cost = pred)
submitDb <- aggregate(data.frame(cost = submitDb$cost), 
                      by = list(id = submitDb$id), mean)

write.csv(submitDb, "submit3.csv", row.names = FALSE, quote = FALSE)



###
### Model 4. Retain date as originally specified. Add component info back in.
###

base <- paste0("/Users/saraweinstein/Documents/DataScience/kaggle/Caterpillar",
               "/competition_data/")

test <- read.csv(paste0(base, "test_set.csv"))
train <- read.csv(paste0(base, "train_set.csv"))

train$id <- -(1:nrow(train))
test$cost <- 0
dFull <- rbind(train, test)


# merge in the data also organized by tube_assembly_id
dFull <- merge(dFull, read.csv(paste0(base, "cleaned_data/bill_materials_ed.csv")), 
               by = "tube_assembly_id", all.x = TRUE)
dFull <- merge(dFull, read.csv(paste0(base, "cleaned_data/specs_ed.csv")), 
               by = "tube_assembly_id", all.x = TRUE)
dFull <- merge(dFull, read.csv(paste0(base, "tube.csv")), 
               by = "tube_assembly_id", all.x = TRUE)


# add columns specifying the form for each tube end
dFull <- left_join(dFull, read.csv(paste0(base, "tube_end_form.csv")),
                   by = c("end_a" = "end_form_id"))
colnames(dFull)[63] <- "end_a_form"

dFull <- left_join(dFull, read.csv(paste0(base, "tube_end_form.csv")),
                   by = c("end_x" = "end_form_id"))
colnames(dFull)[64] <- "end_x_form"


# clean NA values: recode numeric NAs t0 -1
for(i in 1:ncol(dFull)) {
      if(is.numeric(dFull[, i])) {
            dFull[is.na(dFull[, i]), i] <- -1  
      } else {
            dFull[, i] <- as.character(dFull[, i])
            dFull[is.na(dFull[, i]), i] <- "NAvalue"
            dFull[, i] <- as.factor(dFull[, i])
      }
}


# clean variables with too many categories
for(i in 1:ncol(dFull)){
      if(!is.numeric(dFull[, i])){
            freq <- data.frame(table(dFull[, i]))
            freq <- freq[order(freq$Freq, decreasing = TRUE),]
            dFull[, i] <- as.character(match(dFull[, i], freq$Var1[1:30]))
            dFull[is.na(dFull[, i]), i] <- "rareValue"
            dFull[, i] <- as.factor(dFull[, i])
      }
}


# remove unneeded variables
dFull <- dFull[, -match(c("tube_assembly_id"), names(dFull))]


# split back into train and test
test <- dFull[which(dFull$id > 0), ]
train <- dFull[which(dFull$id < 0), ]


# evaluate RF predictions by splitting training set 80%/20%
library(caTools)

spl <- sample.split(train$cost, 0.8)
dtrain <- subset(train, spl == TRUE)
dtest <- subset(train, spl == FALSE)

# train random forest on dtrain w log transformation and make 
# predictions on dtest
set.seed(123)

rf <- randomForest(log(dtrain$cost + 1) ~ . -id, dtrain, ntree = 10, do.trace = 2)
pred <- exp(predict(rf, dtest)) - 1
sqrt(mean((log(dtest$cost + 1) - log(pred + 1))^2))   # 0.1832179


# train randomForest on the whole training set & make predictions
library(randomForest)

set.seed(123)
rf <- randomForest(log(train$cost + 1) ~ . -id, data = train, 
                   ntree = 20, do.trace = 2, importance = TRUE)

test$cost <- NULL
pred <- exp(predict(rf, newdata = test)) - 1


# check out the model
vu <- varUsed(rf, count = TRUE)
vusorted <- sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(rf$forest$xlevels[vusorted$ix]))
varImpPlot(rf)


# write out submission file
submitDb <- data.frame(id = test$id, cost = pred)
submitDb <- aggregate(data.frame(cost = submitDb$cost), 
                      by = list(id = submitDb$id), mean)

write.csv(submitDb, "submit4.csv", row.names = FALSE, quote = FALSE)



