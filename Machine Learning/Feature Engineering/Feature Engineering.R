# Libraries importing


# To read our dataset
test <- read.csv('/Users/aneruthmohanasundaram/Documents/GitHub/R-Basics/Machine Learning/Datasets/Test_set_values.csv')
train <- read.csv('/Users/aneruthmohanasundaram/Documents/GitHub/R-Basics/Machine Learning/Datasets/Training_set_values.csv')
labels <- read.csv('/Users/aneruthmohanasundaram/Documents/GitHub/R-Basics/Machine Learning/Datasets/Training_set_labels.csv')

# To check the summary of our dataset.
test_summary <- summary(test)
train_summary <- summary(train)
label_summary <- summary(labels)

# To check the unique values
unique(labels[c(2)])

# To view our dataset.
View(test)

# Creating a dummy variable
# dummy1 <- data.frame(train)
# dummy2 <- data.frame(labels)
# View(dummy2)

# Merging our train values with train labels (Fo r testing we merge dummy2 with dummy1 based on ID)
train_df <- merge(train, labels, by.y = "id") # We are merging this as we can see that their "id" column is same and also assuming that this is the prediction column.
dim(train_df)
View(train_df)

# Visualizing our target column
as.data.frame(table(train_df$status_group))

# Data Preprocessing

# Fetching our dataset information
str(train_df)

###########################################################################################################################
############################################## Data Preprocessing #########################################################
###########################################################################################################################

#  To check missing values in our dataset 
count_amttsh <- table(train_df$amount_tsh)
count_schname <- table(train_df$scheme_name)
count_publicmeet <- table(train_df$public_meeting)
count_permit <- table(train_df$permit)
