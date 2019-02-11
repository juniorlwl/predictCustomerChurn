#########################
# ISyE 6414 Project
#########################


getwd()
setwd("/Users/kunlelawal/Desktop/Masters/ISYE 6414/Project")

# read in the csv file
churn_raw <- read.csv("Churn_Data_csv.csv")

# summary of the data
summary(churn_raw)
dim(churn_raw)

# make 'blanks' into additional factor level
churn_raw[churn_raw == ''] <- 'blank'

# remove columns with too many NAs (more than 5% of observations)
# educ1, income, numbcars, adults, 
# pre_hnd_price, rmcalls, rmmou, rmrev, 
# REF_QTY, tot_ret, tot_acpt, crtcount
columns_to_remove <- NULL
for(i in 1:ncol(churn_raw)) {
  number_of_NAs <- sum(as.numeric(is.na(churn_raw[,i])))
  if(number_of_NAs >= 49999*.05){
    columns_to_remove <- c(i, columns_to_remove)
  }
}

churn <- churn_raw[, -c(columns_to_remove)]

# remove the rows of data with NA's (this removes about 1174 rows)
churn <- na.omit(churn)
#Save the ready file
write.csv(churn, file="Churn_data_clean.csv")





####################################
# data is now ready to use.
# Next section, ridge regression
####################################
install.packages("glmnet")
library(glmnet)

# use model.matrix to make dummy variables
x <- model.matrix(CHURN ~ ., data = churn[, -1]) 
y <- as.factor(churn$CHURN) 
# the 'x' matrix with dummy variables now has 197 dimensions including intercept


# use glm function to fit logistic regression model
# alpha -> elasticnet mixing parameter. 
# alpha=1 for lasso penalty, alpha=0 for ridge penalty
ridge_fit <- glmnet(x, y, family = "binomial", alpha = 0)

# Do ridge regression when you have too many x-variables

#Need help explaining this

# calculate the accurate prediction rate
ridge_pred <- predict(ridge_fit, x, type = 'class')
ridge_pred

# for the sequence of lambda used, find best one
current_rate <- mean(ridge_pred[,1] == churn$CHURN)
best_lambda <- 1
for(lambda in 1:99) {
    if(mean(ridge_pred[,lambda+1] == churn$CHURN) > current_rate){
      current_rate <- mean(ridge_pred[,lambda+1] == churn$CHURN)
      best_lambda <- lambda
      print(paste(lambda, "--", current_rate))
    }
}

# best lambda, s = 99 -> leads to prediction rate of 58.98%
mean(ridge_pred[,99] == churn$CHURN)

####################
#Backwards elimination to determine significant variables 
####################





















