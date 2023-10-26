#Team-66
#Batch- Summer 2023
#Project - Macro-economic factors and their impact of S&P-500
#This Coding file is for Linear Regression Part on dependent variable S&P 500
#using independent variables macro-economic indicators(CPI, GDP, Interest Rate, Oil Price Index,
#Unemployment Rate, Housing Price Index)
getwd()
#loading required packages
if (!require(broom))
  install.packages("broom")
library(broom)
if (!require(DAAG))
  install.packages("DAAG")
library(DAAG)
if (!require(stargazer))
  install.packages("stargazer")
library(stargazer)
if (!require(basictabler))
  install.packages("basictabler")
library(basictabler)
if (!require(GGally))
  install.packages("GGally")
library(GGally)

#good practice
rm(list = ls())
set.seed(1)

#loading combined dataset
dataset <- read.csv("project_dataset.csv", header = TRUE)
head(dataset)

#removing date column
dataset <- dplyr::select(dataset,-DATE)

#fit a linear regression model using all factors
model_lm <- lm(ADJCLOSE ~ ., data = dataset)


#print a summary of the model
summary(model_lm)

#calculate the VIF for each predictor variable in the model
vif(model_lm)

#is this a good model, take a glance at non-cv R^2 and non-cv adjusted R^2
glance(model_lm)

# Get the R^2
all_pred_R2 <- summary(model_lm)$r.squared
all_pred_R2

# Get the adjusted R^2
all_pred_adj_R2 <- summary(model_lm)$adj.r.squared
all_pred_adj_R2

#examine p-values for null hypothesis for each predictors significance
#using a function from broom
tidy(model_lm)

#create a data frame mapping each model with R-Squared and Adjusted R-Squared Value
MappingTable <- data.frame()
MappingTable <-
  rbind(MappingTable, c(
    "All Predictors Model",
    round(all_pred_R2, digits = 5),
    round(all_pred_adj_R2, digits = 5)
  ))

####*************Cross-Validation*************####
#the DAAG library has a CV.lm() that can be used to get a more accurate measure quality of the model
#using Cross-Validation
#Perform a 5-fold CV with the linear model
model_lm_cv <- cv.lm(dataset, model_lm, m = 5)

#Calculate the R-squared values
# R-squared = 1 - Sum of Squared Errors of Residuals / Total Sum of Squared Errors
#
# total sum of squared differences between data and its mean

SSTotal <- sum((dataset$ADJCLOSE - mean(dataset$ADJCLOSE)) ^ 2)

#number of observation
numberObserved <- nrow(dataset)

#calculate  sum of squared errors= mean squared error x times number of data points
SSRes_model_lm_cv <- attr(model_lm_cv, "ms") * numberObserved
SSRes_model_lm_cv

#R Squared value for CV model
CV_R2 <- 1 - SSRes_model_lm_cv / SSTotal
CV_R2

#The adjusted R-squared is a modified version of R-squared that adjusts for the number of predictors in a regression model. It is calculated as:

#Adjusted R2 = 1 – [(1-R2)*(n-1)/(n-k-1)] where
#R2: The R2 of the model
#n: The number of observations
#k: The number of predictor variables

#Adjusted R-squared valued for CV model
CV_Adj_R2 <- 1 - ((1 - CV_R2) * (numberObserved - 1) / (numberObserved -
                                                          6 - 1))
CV_Adj_R2

MappingTable <-
  rbind(MappingTable, c(
    "All Predictors Model-CV",
    round(CV_R2, digits = 5),
    round(CV_Adj_R2, digits = 5)
  ))

#####**********End of Cross-Validation*************##########


#removing predictors with p-value > 0.1
model_lm_valuable_pred <-
  lm(ADJCLOSE ~ FEDFUNDS + GDP + CPIAUCSL +
       USSTHPI + WTISPLC , data = dataset)
model_lm_valuable_pred

#print a summary of the model
summary(model_lm_valuable_pred)

#calculate the VIF for each predictor variable in the model
vif(model_lm_valuable_pred)

#is this a good model, take a glance at non-cv R^2 and non-cv adjusted R^2
glance(model_lm_valuable_pred)


# Get the R^2
sel_pred_R2 <- summary(model_lm_valuable_pred)$r.squared
sel_pred_R2

# Get the adjusted R^2
sel_pred_adj_R2 <- summary(model_lm_valuable_pred)$adj.r.squared
sel_pred_adj_R2

MappingTable <-
  rbind(MappingTable, c(
    "Valuable Predictors Model",
    round(sel_pred_R2, digits = 5),
    round(sel_pred_adj_R2, digits = 5)
  ))

####*************Cross-Validation*************####
#Perform a 5-fold CV with the linear model to measure the quality of the model
model_lm_sel_cv <- cv.lm(dataset, model_lm_valuable_pred, m = 5)

#Calculate the R-squared values


#calculate  sum of squared errors= mean squared error x times number of data points
SSRes_model_lm_sel_cv <- attr(model_lm_sel_cv, "ms") * nrow(dataset)
SSRes_model_lm_sel_cv

#R Squared value for CV model
CV_Sel_R2 <- 1 - SSRes_model_lm_sel_cv / SSTotal
CV_Sel_R2

#Adjusted R-squared valued for CV model with 5 predictors
CV_Sel_Adj_R2 <-
  1 - ((1 - CV_Sel_R2) * (numberObserved - 1) / (numberObserved - 5 - 1))
CV_Sel_Adj_R2
MappingTable <-
  rbind(MappingTable,
        c(
          "Valuable Predictors Model-CV",
          round(CV_Sel_R2, digits = 5),
          round(CV_Sel_Adj_R2, digits = 5)
        ))


####**********End of Cross-Validation*************##########

##We anticipated CPI, GDP, and Interest Rate to be the most significant variable##
model_lm_valuable_pred3 <-
  lm(ADJCLOSE ~ FEDFUNDS + GDP + CPIAUCSL
       , data = dataset)
model_lm_valuable_pred3

#print a summary of the model
summary(model_lm_valuable_pred3)

#calculate the VIF for each predictor variable in the model
vif(model_lm_valuable_pred3)

#is this a good model, take a glance at non-cv R^2 and non-cv adjusted R^2
glance(model_lm_valuable_pred3)


# Get the R^2
sel_pred3_R2 <- summary(model_lm_valuable_pred3)$r.squared
sel_pred3_R2

# Get the adjusted R^2
sel_pred3_adj_R2 <- summary(model_lm_valuable_pred3)$adj.r.squared
sel_pred3_adj_R2

MappingTable <-
  rbind(MappingTable, c(
    "Valuable 3 Predictors Model",
    round(sel_pred3_R2, digits = 5),
    round(sel_pred3_adj_R2, digits = 5)
  ))



####*************Cross-Validation*************####
#Perform a 5-fold CV with the linear model to measure the quality of the model
model_lm_sel3_cv <- cv.lm(dataset, model_lm_valuable_pred3, m = 5)

#Calculate the R-squared values


#calculate  sum of squared errors= mean squared error x times number of data points
SSRes_model_lm_sel3_cv <- attr(model_lm_sel3_cv, "ms") * nrow(dataset)
SSRes_model_lm_sel3_cv

#R Squared value for CV model
CV_Sel3_R2 <- 1 - SSRes_model_lm_sel3_cv / SSTotal
CV_Sel3_R2

#Adjusted R-squared valued for CV model with 3 predictors
CV_Sel3_Adj_R2 <-
  1 - ((1 - CV_Sel3_R2) * (numberObserved - 1) / (numberObserved - 3 - 1))
CV_Sel3_Adj_R2
MappingTable <-
  rbind(MappingTable,
        c(
          "Valuable 3 Predictors Model-CV",
          round(CV_Sel3_R2, digits = 5),
          round(CV_Sel3_Adj_R2, digits = 5)
        ))


####**********End of Cross-Validation*************##########

##PART 2 - LINEAR REGRESSION AND CROSS VALIDATION WITH PCA

#Visually Inspect Correlation among Predictors
ggpairs(dataset, columns = c("FEDFUNDS",	"GDP",	"UNRATE", "CPIAUCSL", "USSTHPI", "WTISPLC"))

#correlation matrix of the data
corr <- cor(dataset)
round(corr, 2)


#run PCA on the matrix of scaled predictors
pca_scaled_pred <- prcomp(dataset[,2:7], scale. = TRUE)
summary(pca_scaled_pred)

#pca rotation is the matrix of eigenvectors
pca_scaled_pred$rotation

#use the screeplot function to plot the variances of each of the Principal Components
#to help us decide which one to use

screeplot(pca_scaled_pred, type="lines",col="blue")


# Calculate the variances and proportion of variances from the pca object

pcaVar <- pca_scaled_pred$sdev^2
pcaPropVar <- pcaVar/sum(pcaVar)

# Plot the proportion of variances from PCA

plot(pcaPropVar, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = "b")

# Plot the cumsum proportion of variances from PCA

cumsum(pcaPropVar)
plot(cumsum(pcaPropVar), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained",ylim = c(0,1), type = "b")

#Get First 2 Principal Components
PCs <- pca_scaled_pred$x[,1:2]
pca_scaled_pred$x
PCs

#Build a linear regression model with first 2 Principal  Components
pcData <- cbind(PCs, dataset[,1]) #Create new data matrix with ADJCLOSE  and first 2 Principal Components

as.data.frame(pcData)

model_lm_pc <- lm(V3~., data=as.data.frame(pcData))

summary(model_lm_pc)

## Get coefficients in terms of original data
## from PCA coefficients

# PCA Coefficients for this linear regression model

beta0 <- model_lm_pc$coefficients[1]
betas <- model_lm_pc$coefficients[2:3]
beta0
betas

# Transform the PC coefficients into coefficients for the original variables

pca_scaled_pred$rotation[,1:2]
alphas <- pca_scaled_pred$rotation[,1:2] %*% betas

#using matrix transpose
t(alphas)


#since coefficients above are using scaled data, converting back to the original data

originalAlpha <- alphas/sapply(dataset[,2:7],sd)
originalBeta0 <- beta0 - sum(alphas*sapply(dataset[,2:7],mean)/sapply(dataset[,2:7],sd))

# coefficients for unscaled data:
t(originalAlpha)
originalBeta0

# Calculate quality estimates from this model:

estimates <- as.matrix(dataset[,2:7]) %*% originalAlpha + originalBeta0
estimates

# calculate R^2 and R^2_adj

SSE = sum((estimates - dataset[,1])^2)
SStot = sum((dataset[,1] - mean(dataset[,1]))^2)

R2 <- 1 - SSE/SStot
AdjR2 <- R2 - (1 - R2)*4/(nrow(dataset)-2-1)
R2
AdjR2

# run a regression using the each first i principal components for i=1 to 15


r2 <- numeric(6) # create a vector to store the R-squared values
adjr2 <- numeric(6) # # create a vector to store the Adjusted R-squared values

for (i in 1:6) {
  pclist <- pca_scaled_pred$x[,1:i]  # use the first i principal components
  pcc <- cbind(dataset[,1],pclist)  # create data set
  model <- lm(V1~.,data = as.data.frame(pcc)) # fit model
  r2[i] <- summary(model)$r.squared # calculate R-squared
  adjr2[i] <- summary(model)$adj.r.squared #  Adjusted R-squared value
  
  MappingTable <- rbind(MappingTable, c(paste("All Predictors - Top",i, " Principal Comp Model"),round(r2[i],digits=5),round(adjr2[i],digits=5)))
}

r2
adjr2


plot(r2, xlab = "Principal Component", ylab = "R-squared with this many principal components",
     ylim = c(0,1), type = "b")

#PCA with Cross Validation

# 5-fold cross-validation

r2cross <- numeric(6) # create a vector to store the R-squared values
adjr2cross <- numeric(6) # create a vector to store the Adjusted R-squared values

for (i in 1:6) {
  pclist <- pca_scaled_pred$x[,1:i]  # use the first i principal components
  pcc <- cbind(dataset[,1],pclist)  # create data set
  model <- lm(V1~.,data = as.data.frame(pcc)) # fit model
  c <- cv.lm(as.data.frame(pcc),model,m=5) # cross-validate 
  r2cross[i] <- 1 - attr(c,"ms")*nrow(dataset)/sum((dataset$ADJCLOSE - mean(dataset$ADJCLOSE))^2) # calculate R-squared
  adjr2cross[i] <-1 - ((1-r2cross[i])*(numberObserved-1)/(numberObserved-i-1))
  MappingTable <- rbind(MappingTable, c(paste("All Predictors Cross Validated - Top",i, " Principal Comp Model"),round(r2cross[i],digits=5),round(adjr2cross[i],digits=5)))
}

r2cross
adjr2cross

plot(r2cross, xlab = "Principal Component", ylab = "Cross-validated R-squared with this many principal components",
     ylim = c(0,1), type = "b")

##########summary#################
stargazer(
  model_lm,
  model_lm_valuable_pred,
  model_lm_valuable_pred3,
  align = TRUE,
  type = "text",
  out = "models.txt"
)



tbl <- BasicTable$new()
tbl$addData(
  MappingTable,
  explicitColumnHeaders = c("Model Type", "R-Squared Value", "Adjusted R-Squared Value")
)
# theme the table and render
tbl$theme <- "largeplain"
tbl$renderTable(styleNamePrefix = "t0")