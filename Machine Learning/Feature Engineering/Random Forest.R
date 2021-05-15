library(ggplot2)
library(dplyr)
library(caret) # Package to perform one hot encoding

library(scales)
library(RColorBrewer)
library(caTools)

# Load the dataset
test <- read.csv('/Users/aneruthmohanasundaram/Documents/GitHub/R-Basics/Machine Learning/Datasets/Test_set_values.csv')
train <- read.csv('/Users/aneruthmohanasundaram/Documents/GitHub/R-Basics/Machine Learning/Datasets/Training_set_values.csv')
labels <- read.csv('/Users/aneruthmohanasundaram/Documents/GitHub/R-Basics/Machine Learning/Datasets/Training_set_labels.csv')

train <- data.frame(cbind(train,as.factor(labels$status_group)))
names(train)[41] <- 'status_group'

tb <- with(train,table(extraction_type_class))
lab <- with(labels,table(status_group))
# Analyzing extraction_type_class
ggplot(as.data.frame(tb), aes(factor(extraction_type_class), Freq)) + geom_col(position = 'dodge')

# Analyzing labels
ggplot(data=train_1h, aes(x=status_group)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), fill=brewer.pal(3, "Set1"), alpha = 1/2) + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  theme(axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.y=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, face="bold")) +
  xlab("Operational Status") + ylab("Percent")

# Visualize Regions
color_label <- c("#66FF00", "#0099CC", "#FF6666")
table(train$status_group, train$construction_year)
prop.table(table(train$status_group, train$construction_year), margin = 2)

# "Eliminating" the construction year of 0 (probably indicating missing data for year) for better graph representation
ggplot(subset(train, construction_year > 0),
       aes(x = construction_year, fill = status_group)) + 
  geom_bar() +
  xlab("Construction year")  +
  ylab("Number of waterpoints") +
  labs(fill = "Status of waterpoint") +
  scale_fill_manual(values=color_label)

# Look at distribution of labels per basin
ggplot(train,
       aes(x = basin, fill = status_group)) + 
  geom_bar() +
  xlab("Basin")  +
  theme(axis.text.x=element_text(angle = 20, hjust = 1)) +
  ylab("Number of waterpoints") +
  labs(fill = "Status of waterpoint") +
  theme(legend.position = "top") +
  scale_fill_manual(values=color_label)

# Quantity
ggplot(train,
       aes(x = quantity, fill = status_group)) + 
  geom_bar() +
  xlab("Quantity") +
  ylab("Number of waterpoints") +
  labs(fill = "Status of waterpoint") +
  theme(legend.position = "top") +
  scale_fill_manual(values=color_label)

# Create bar plot for source_type
qplot(source_type, data=train, geom="bar", fill=status_group) + 
  theme(legend.position = "top") + 
  labs(fill = "Status of waterpoint") +
  theme(axis.text.x=element_text(angle = 20, hjust = 1)) + 
  scale_fill_manual(values=color_label) + 
  xlab("Type of water source") +
  ylab("Number of waterpoints")

# Look at distribution of labels per total static head 
ggplot(subset(train, amount_tsh <20000 & amount_tsh >0),
       aes(x = amount_tsh)) + 
  geom_histogram(bins = 20) +
  facet_wrap(~ status_group) +
  xlab("Total Static Head") +
  ylab("Number of waterpoints")

# To check the distribution of date recorded 
# date_record <- as.data.frame(table(train$date_recorded))
# ggplot(data=date_record, aes(x=Var1, y=Freq)) +
#   geom_bar(stat="identity", fill="steelblue")+
#   geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
#   xlab("Unique Values of Date") +
#   ylab("Frequency of creation of pump recorded (by year)")+
#   theme_minimal()


# To get the unique values of recorded_by
unique(train['recorded_by'])
# Since we can see that 'recorded_by' column has no unique elements present so we can drop.
train <- train[ , !names(train) %in% c('recorded_by')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('recorded_by')]

# Checking our dataset dimension after dropping a column 
dim(train) 

# Checking our second column amount_tsh
str(train['amount_tsh'])
table(train$amount_tsh)
# From the below table we can say that it consist of more zeros so we can drop it
train <- train[ , !names(train) %in% c('amount_tsh')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('amount_tsh')]

table(train$public_meeting)
train <- train[ , !names(train) %in% c('public_meeting')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('public_meeting')]

dim(train)

# Checking permit column
table(train$permit)
# From below table we can see that there is a empty string and we can't take the mean values as it is boolean value
# Also it has no relevance to teh prediction
# So drop that column
train <- train[ , !names(train) %in% c('permit')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('permit')]

dim(train)

# Grouping extraction type,extraction type group and extraction type class
head(data.frame(train$extraction_type,train$extraction_type_group,train$extraction_type_class))
# We can say that three columns are correlated and hence we can remove one column
train <- train[ , !names(train) %in% c('extraction_type_group')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('extraction_type_group')]

# Grouping management and management_group
head(data.frame(train$management,train$management_group))
# We can't drop anything from above mentioned columns as they posses different values

# Compare scheme name and scheme management
head(data.frame(train$scheme_name,train$scheme_management))
table(train$scheme_name) # Checking the table of values for scheme_name
table(train$scheme_management) # Checking the table of values for scheme_management
# Since scheme_name consist of too many categorical values we can eliminate it
train <- train[ , !names(train) %in% c('scheme_name')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('scheme_name')]

dim(train)

# Comparing the payment and payment_type
head(train[c('payment','payment_type')])
# Since we can infer that they both have a close relation so we can eliminate either of the column
train <- train[ , !names(train) %in% c('payment')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('payment')]

dim(train)
dim(test)

# Comparing the water_quality and quality_group
head(train[c('water_quality','quality_group')])
# Since we can infer that they both have a close relation so we can eliminate either of the column
train <- train[ , !names(train) %in% c('quality_group')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('quality_group')]

dim(train)
dim(test)

# Comparing the quantity and quantity_group
head(train[c('quantity','quantity_group')])
# Since we can infer that they both have a close relation so we can eliminate either of the column
train <- train[ , !names(train) %in% c('quantity_group')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('quantity_group')]

# Comparing the source_type, source and source_class
head(train[c('source_type','source','source_class')])
# By comparing we can say that source_type and source are same so we can drop either of it but source_class defines type of the water source
train <- train[ , !names(train) %in% c('source_type')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('source_type')]

# Comparing the waterpoint_type and waterpoint_type_group
head(train[c('waterpoint_type','waterpoint_type_group')])
# By comparing we can say that waterpoint type and waterpoint_type_group are same so we can drop either of them
train <- train[ , !names(train) %in% c('waterpoint_type_group')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('waterpoint_type_group')]

# drop columns
####Expalin why dropping those################
train <- train[ , !names(train) %in% c('id','wpt_name','num_private','lga','ward','amount_tsh','subvillage','region','funder','installer')]

# Doing the above for test data
test <- test[ , !names(test) %in% c('id','wpt_name','num_private','lga','ward','amount_tsh','subvillage','region','funder','installer')]

dim(train)
dim(test)

# Converting the date to days
train$date_recorded <- as.numeric(format(as.Date(train$date_recorded), "%Y"))
test$date_recorded <- as.numeric(format(as.Date(test$date_recorded), "%Y"))
head(train)
head(test)

# After analyzing we can say that 0's are present in two columns that is population and construction year so we consider their mean value
replace_zeros <- function(x){
  # converting that particular columns 0 values to NA and then considering the mean
  x$population[x$population == 0] <- NA
  x$construction_year[x$construction_year == 0] <- NA
  
  # Replacing the NA values to its respective mean
  x$population <- ifelse(is.na(x$population), round(mean(x$population, na.rm=TRUE)), x$population)
  x$construction_year <- ifelse(is.na(x$construction_year),round( mean(x$construction_year, na.rm=TRUE)), x$construction_year)
  
  return(x) # returning the dataset
}

df_train <- replace_zeros(train)
df_test <- replace_zeros(test)
head(df_train)
head(df_test)

table(df_train$status_group)

#This function takes as a parameter the dataset
#It changes all the categorical values into lower case
#It considers only the top levels in every categroical column, converting all the rest of the levels into "other"

clean_dataset <- function(x){
  x$basin <- ifelse(x$basin == 'Lake Victoria','lake victoria',x$basin)
  x$basin <- ifelse(x$basin == 'Pangani','pangani',x$basin)
  x$basin <- ifelse(x$basin == 'Rufiji','rufiji',x$basin)
  x$basin <- ifelse(x$basin == 'Internal','internal',x$basin)
  x$basin <- ifelse(x$basin != 'lake victoria' & x$basin != 'pangani' & x$basin != 'rufiji' & x$basin != 'internal' ,'other_basin',x$basin)
  
  # Considering the scheme_management column
  x$scheme_management <- ifelse(x$scheme_management == 'VWC','vwc',x$scheme_management)
  x$scheme_management <- ifelse(x$scheme_management == 'WUG','wug',x$scheme_management)
  x$scheme_management <- ifelse(x$scheme_management == 'Water authority','water_auth',x$scheme_management)
  x$scheme_management <- ifelse(x$scheme_management == 'Water Board','water_board',x$scheme_management)
  x$scheme_management <- ifelse(x$scheme_management != 'vwc' & x$scheme_management != 'wug' & x$scheme_management != 'water_auth' & x$scheme_management != 'water_board' ,'other_schmgt',x$scheme_management)
  
  # Considering the extraction_type column
  x$extraction_type <- ifelse(x$extraction_type != 'gravity','other_exttype',x$extraction_type)
  
  # Considering the extraction_type_class column
  x$extraction_type_class <- ifelse(x$extraction_type_class != 'gravity' & x$extraction_type_class != 'handpump' ,'other_extclass',x$extraction_type_class)
  
  # Considering the management column
  x$management <- ifelse(x$management != 'vwc' & x$management != 'wug' & x$management != 'wua','other_mgt',x$management)
  
  # Considering the management_group column
  x$management_group <- ifelse(x$management_group != 'user-group','other_mgtgroup',x$management_group)
  
  # Considering the payment_type column
  x$payment_type <- ifelse(x$payment_type != 'never pay','other_payemt',x$payment_type)
  
  # Considering the water_quality column
  x$water_quality <- ifelse(x$water_quality != 'soft' ,'other_waterqual',x$water_quality)
  
  # Considering the quantity column
  x$quantity <- ifelse(x$quantity != 'insufficient' & x$quantity != 'enough','other_quantity',x$quantity)
  
  # Considering the source column
  x$source <- ifelse(x$source != 'shallow well' & x$source != 'spring' & x$source != 'machine dbh' & x$source != 'river','other_source',x$source)
  
  # Considering the source_class column and converting unknown to other_sourceClass
  x$source_class <- ifelse(x$source_class == 'unknown','other_sourceClass',x$source_class)
  
  # Considering the waterpoint_type column
  x$waterpoint_type <- ifelse(x$waterpoint_type != 'communal standpipe' & x$waterpoint_type != 'hand pump','other_watertype',x$waterpoint_type)
  
  return(x) # Returns the dataset
}

labels_rename <- function(x){
  x$status_group <- ifelse(x$status_group == "functional",as.numeric(c(0)),x$status_group)
  x$status_group <- ifelse(x$status_group == "non functional",as.numeric(c(1)),x$status_group)
  x$status_group <- ifelse(x$status_group == "functional needs repair",as.numeric(c(2)),x$status_group)
  return(x)
}

cleaned_train <- clean_dataset(df_train)
cleaned_test <- clean_dataset(df_test)
cleaned_train <- labels_rename(cleaned_train)
View(cleaned_train)
View(cleaned_test)
table(cleaned_train$status_group)

cleaned_train$status_group <- ifelse(cleaned_train$status_group == 3,1,cleaned_train$status_group)
table(cleaned_train$status_group)

train_dmy <- caret::dummyVars(' ~ .',data = cleaned_train,fullRank = T)
train_1h <- data.frame(predict(train_dmy, newdata = cleaned_train))

test_dmy <- caret::dummyVars(' ~ .',data = cleaned_test,fullRank = T)
test_1h <- data.frame(predict(test_dmy, newdata = cleaned_test))

train_1h$status_group <- as.factor(train_1h$status_group) # Considering the status as factor column

View(train_1h)

###################################################################################################################################

###################################################### TRAIN TEST SPLIT ###########################################################
tar_variable <- ncol(train_1h)
dt <- sort(sample(nrow(train_1h), nrow(train_1h)*.8))
data_train<-train_1h[dt,]
data_test<-train_1h[-dt,]
###################################################### TRAIN TEST SPLIT ###########################################################

###################################################################################################################################

############################################################### KNN ###############################################################
library(class)
Data<-train_1h[sample(nrow(train_1h)),]

accuracy_arr <- array(0,10)

#Create 10 equally size folds
folds <- cut(seq(1,nrow(Data)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation
for(i in 1:10){
  
  Ypred_knn = knn(data_train,data_test,data_train$status_group,k=i)
  KNN <- table(Ypred_knn,data_test$status_group)
  accuracy_arr[i] <- sum(diag(KNN)/sum(KNN))
}

print(paste0('Accuracy score for 10 values is: ',list(accuracy_arr),' and max value is: ',max(accuracy_arr),' at index: ',which(accuracy_arr == max(accuracy_arr))))

selected_k <- 9
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(Data)),breaks=10,labels=FALSE)
accuracy_arr <- array(0,10)

for (i in 1:10){
  #Segement your data by fold using the which() function
  Idx <- which(folds==i,arr.ind=TRUE)
  testData <- Data[Idx, ]
  trainData <- Data[-Idx, ]
  
  Ypred_knn=knn(trainData,testData,trainData$status_group,k=selected_k)
  KNN <- table(Ypred_knn,testData$status_group)
  accuracy_arr[i] <- sum(diag(KNN)/sum(KNN))
}

print(paste('Average accuracy score for cross validation = ',mean(accuracy_arr)))
############################################################### KNN ###############################################################

###################################################################################################################################

############################################################### ID3 ###############################################################
library(rpart)

id3 <- rpart(status_group ~ ., method="class",data=data_train)
printcp(id3) # display the results
plotcp(id3) # visualize cross-validation results
summary(id3) # detailed summary of splits

# plot tree
plot(id3, uniform=TRUE,
     main="Classification Tree - Rpart")
text(id3, use.n=TRUE, all=TRUE, cex=.8)

Y_pred <- predict(id3,data_test[,-tar_variable])
Y <- data_test[,tar_variable]
threshold <- 0.5

View(Y_pred)

# Y_hat <- ifelse(Y_pred[,2] > threshold,"Yes","No")
Y_hat <- array(0,nrow(Y_pred))
for(i in 1:nrow(Y_pred)){
    if(Y_pred[i,1] > 0.5){
      Y_pred$Y_hat <- 0
    }else if(Y_pred[i,2] > 0.5){
      Y_pred$Y_hat <- 1
    }else if(Y_pred[i,3] > 0.5){
      Y_pred$Y_hat <- 2
    }
}


confusion_matrix <- table(Y_hat,Y)
confusion_matrix

accuracy = sum(diag(confusion_matrix))/sum(confusion_matrix)
misclassification_rate = 1 - accuracy
print(paste0('Misclassification score for ID3 algorithm is: ',misclassification_rate))

# Cross Validation
k = 10
target_variable <- ncol(train_1h)
accuracy_vec <- array(0,k)
threshold <- misclassification_rate

tree_index <- sample(1:nrow(train_1h)) # Arranging the dataset randomly.

# Divide the data set into groups of k
max <- ceiling(nrow(train_1h)/k)
splits <- split(tree_index, ceiling(seq_along(tree_index)/max))

for (i in 1:k){
  # Take the group as a stock or test set
  test_data <- train_1h[splits[[i]],]
  
  # Take the remaining groups as a training data set
  train_data <- train_1h[-splits[[i]],]
  print(paste("Training set size:",dim(train_data)[1],"- Testing set size",dim(test_data)[1]))
  
  # Fitting and evaluate a model in the training set.
  model <- rpart(status_group ~ ., method="class",data=train_data)
  Y_pred <- predict(model,test_data[,-target_variable])
  Y <- test_data[,target_variable]
  
  # Storing our prediction values of the tree
  Y_hat <- ifelse(Y_pred[,2] > threshold,"Yes","No") 
  confusion_matrix <- table(Y_hat,Y)
  print(confusion_matrix)
  
  # Keeping our evaluation score and popping out the completed model
  accuracy_vec[i] = sum(diag(confusion_matrix))/sum(confusion_matrix)
  misclassification_rate = 1 - accuracy_vec[i]
  print(paste("Misclassification rate -",i,"fold:",misclassification_rate))
}

# Accomplish the model's competence by using model assessment scores sample.
print(paste("Mean misclassification score for ID3 algorithm is:",1-mean(accuracy_vec)))
############################################################### ID3 ###############################################################

###################################################################################################################################

############################################################### XGB ###############################################################

library(xgboost)
ind <- sample(2, nrow(train_1h), replace = TRUE, prob= c(0.7,0.3))
data.train <- train_1h[ind==1,]
data.test <- train_1h[ind==2,]
data.train <- sapply(data.train, as.numeric)
data.train[,36] <- data.train[,36] - 1
matrix<-data.matrix(data.train)
data.test <- sapply(data.test, as.numeric)
data.test[,36] <- data.test[,36] - 1
testMatrix <- data.matrix(data.test)
testData <- xgb.DMatrix(testMatrix[,1:35])

Xtreme <- xgboost(data = matrix[,1:35],
                  nfold =5,
                  label = matrix[,36],
                  max_depth = 36,
                  eta = 0.02,
                  nthread = 12,
                  objective = "multi:softprob",
                  num_class =3,
                  subsample = 0.7,
                  colsample_bytree = 0.5,
                  min_child_weight = 3,
                  nrounds = 50,
                  maximize = FALSE)

pred <- predict(Xtreme, testData,reshape = T)
pred = as.data.frame(pred)
colnames(pred) = levels(train_1h$status_group)
pred$prediction = apply(pred,1,function(x) colnames(pred)[which.max(x)])
confusion_matrix <- table(pred$prediction,data.test[,36])
confusion_matrix

acc <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste0('Accuracy score is ',acc*100,'%'))
############################################################### XGB ###############################################################
###################################################################################################################################