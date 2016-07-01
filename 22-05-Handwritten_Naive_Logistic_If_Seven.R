##############################################
# Naive Logistic Regression - Handwritten Digits
##############################################
# package reader provides function "read_csv"
library(readr)

# Load Training Data
train <- data.frame(read_csv("C:/Users/oq9900/Desktop/train111.csv"))
labels   <- train[,1]

dim(train)



#________________________________________________________
# Logistic Regression on raw data (if number is a 7)
#________________________________________________________


# Divide train-dataset by label (=7 v. !=7)
for (i in (1:nrow(train))){
  if(train[i,1]!=7) train[i,1] <- 0
  if(train[i,1] ==7) train[i,1] <- 1
}

# Update Labels (w.r.t. being (==7 / != 7 unequal to 7)
labels <- train[,1]

# Predictors
predictors <- data.frame(y=labels, x = features)

# Fit a model (pixels as predictors)
predictors <- colnames(train)

summary(fit_seven)
vec <- c("labels",colnames(features))

df <- data.frame(y=labels, x=features)
length(labels)
dim(features)
names(train)
# Logistisches Regressionsmodell (Full Model)
fit_seven <- glm(labels ~ ., data = train, family="binomial")
summary(fit_seven)

#_____________ 
# Short Report
# ____________
# 700 numbers -> 11s to fit But: No convergence! (Many NA's)

# 42000 numbers (full data set) -> ~14min to fit. But: Still no convergence & NA's for several (80) pixels

# ____________Display NA-coefficientss for the full model____________

# Print out NA's
coef <- coef(fit_seven)
show_NA <- rep(0,length(coef))

for(i in 1:length(coef(fit_seven))){
  if(is.na(coef[i])){ show_NA[i] <- 1}
}

# Display the NA coeff on a grid
# Gray-Intensity-Matrix
Nas <- matrix(rep("white",784), ncol=28, nrow=28)
for(i in 1:28){
  for (j in 1:28){
   if(show_NA[(i-1)*28 + j]==1){ Nas[i,29 - j] = "red" }
  }
}

# Plot the number
m <- matrix((1:784), ncol = 28, nrow = 28, byrow = TRUE)
m <- t(m)
image(m, col = Nas)


#______Predict some Values__________________
# Preictions are rubbish: Only ~0 or NA 

# Handelt sich (!) nicht um 7: (train[4,])
predict(fit_seven,train[4,], type="response")

# Handelt sich (!) nicht um 7: (train[89,])
predict(fit_seven,train[89,], type="response")

# Handelt sich (!) nicht um 7: (train[675,])
predict(fit_seven,train[675,], type="response")

# Handelt sich um eine 7: (train[656,1])
predict(fit_seven,train[656,], type="response")

# Handelt sich um eine 7: (train[7,1])
predict(fit_seven,train[7,], type="response")



coef <- coef(fit_seven)
  coef[1]
# Decision Rule (cross validation)
  
par(mfrow=c(1,1))

summary(fit_seven)
  

