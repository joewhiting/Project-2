ls()
getwd() ##Show working directory
setwd("/Users/josephwhiting/Desktop/College/Intro to Business Analytics /Project 2") ##Setting working directory 
library(readr)
library(ggplot2) #loading ggplot

##Loading data set, and changing the the variables to numeric and storing it as Project_Dataset
Project_Dataset <- read_csv("Project_Dataset_2.csv", 
                            col_types = cols(Quarters = col_number(), 
                                             VOO = col_number(), VGT = col_number(), 
                                             VXUS = col_number(), GDP = col_number(), 
                                             Unemployment = col_number()))
View(Project_Dataset) #view the data set that I am using
summary(Project_Dataset) #looking at summary of variables in the data set 

#INCORPORATING NONLINEAR (POLYNOMIAL) TRANSFORMATIONS OF GDP
Project_Dataset$GDP2<-Project_Dataset$GDP^2 #QUADRATIC TRANSFORMATION (2nd ORDER)
Project_Dataset$GDP3<-Project_Dataset$GDP^3 #CUBIC TRANSFORMATION (3rd ORDER)

#########################
##Partitioning the data##
#########################
#fraction of sample to be used for training
p<-.7

#number of observations (rows) in the dataframe
obs_count<-dim(Project_Dataset)[1]

#number of observations to be selected for the training partition
#the floor() function rounds down to the nearest integer
training_size <- floor(p * obs_count)
training_size
#set the seed to make your partition reproducible
set.seed(1234)
#create a vector with the shuffled row numbers of the original dataset
train_ind <- sample(obs_count, size = training_size)

Training <- Project_Dataset[train_ind, ] #pulls random rows for training
Testing <- Project_Dataset[-train_ind, ] #pulls random rows for testing

#CHECKING THE DIMENSIONS OF THE PARTITIONED DATA
dim(Training)
dim(Testing)

##Checking Max value for each variable
max(Project_Dataset$VOO)
max(Project_Dataset$VGT)
max(Project_Dataset$VXUS)
max(Project_Dataset$GDP)
max(Project_Dataset$Unemployment)

##Checking Min value for each variable
min(Project_Dataset$VOO)
min(Project_Dataset$VGT)
min(Project_Dataset$VXUS)
min(Project_Dataset$GDP)
min(Project_Dataset$Unemployment)


#PLOTTING THE TRAINING AND TESTING PARTITIONS
plot(VOO ~ GDP, Project_Dataset, xlim=c(-35,5), ylim=c(-22,20)) +
  title("Unpartioned Data Set") #PLOT ENTIRE DATASET
plot(VOO ~ GDP, Training, xlim=c(-35,10), ylim=c(-22,22), col ='blue') +
  title("Training Data") #PLOTS THE IN-SAMPLE TRAINING PARTITION
plot(VOO ~ GDP, Testing, xlim=c(-35,10), ylim=c(-22,22),  col ='red', pch=3) +
  title("Testing Data") #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION
plot(VOO ~ GDP, Testing, xlim=c(-35,10), ylim=c(-22,22)) + 
       points(Training$GDP, Training$VOO, col='blue') +
       points(Testing$GDP, Testing$VOO, col='red', pch=3) +
  title("Full Data - Overlaying Partition")#PLOTS THE OUT-OF-SAMPLE TESTING PARTITION & PLOTS THE OUT-OF-SAMPLE TESTING PARTITION

#########################
##Building First Model ##
#########################
M1 <- lm(VOO ~ GDP, Training)
summary(M1)

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_1_IN <- predict(M1, Training) #generate predictions on the (in-sample) training data
View(PRED_1_IN)
View(M1$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_1_OUT <- predict(M1, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$VOO)^2)/length(PRED_1_IN))  #computes in-sample error
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$VOO)^2)/length(PRED_1_OUT)) #computes out-of-sample

RMSE_1_IN #IN-SAMPLE ERROR
RMSE_1_OUT #OUT-OF-SAMPLE ERROR


#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(-35,22,1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M1, list(GDP=x_grid))
plot(Training$VOO ~ Training$GDP, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$VOO ~ Testing$GDP, col='red', pch=3) +
  title("Model 1 Regression")

##########################
##Building Second Model ##
##########################

#BUILDING THE QUADRATIC MODEL FROM THE TRAINING DATA
M2 <- lm(VOO ~ GDP + GDP2, Training)
summary(M2) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_2_IN <- predict(M2, Training) #generate predictions on the (in-sample) training data
View(PRED_2_IN)
View(M2$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_2_OUT <- predict(M2, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_2_IN<-sqrt(sum((PRED_2_IN-Training$VOO)^2)/length(PRED_2_IN))  #computes in-sample error
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$VOO)^2)/length(PRED_2_OUT)) #computes out-of-sample 

RMSE_2_IN #IN-SAMPLE ERROR
RMSE_2_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(-35,22,1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M2, list(GDP=x_grid, GDP2=x_grid^2))
plot(Training$VOO ~ Training$GDP, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$VOO ~ Testing$GDP, col='red', pch=3)  +
  title("Model 2 Regression")

#########################
##Building Third Model ##
#########################

#BUILDING THE CUBIC MODEL FROM THE TRAINING DATA
M3 <- lm(VOO ~ GDP + GDP2 + GDP3, Training)
summary(M3) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_3_IN <- predict(M3, Training) #generate predictions on the (in-sample) training data
View(PRED_3_IN)
View(M3$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_3_OUT <- predict(M3, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_3_IN<-sqrt(sum((PRED_3_IN-Training$VOO)^2)/length(PRED_3_IN))  #computes in-sample error
RMSE_3_OUT<-sqrt(sum((PRED_3_OUT-Testing$VOO)^2)/length(PRED_3_OUT)) #computes out-of-sample 

RMSE_3_IN #IN-SAMPLE ERROR
RMSE_3_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(-35,22,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M3, list(GDP=x_grid, GDP2=x_grid^2, GDP3=x_grid^3))
plot(Training$VOO ~ Training$GDP, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$VOO ~ Testing$GDP, col='red', pch=3)  +
  title("Model 3 Regression")

######################################
###########MODEL COMPARISON###########
######################################

#COMPARISON OF IN-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_IN #MODEL WITH ONLY LINEAR TERM
RMSE_2_IN #MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_IN #MODEL WITH LINEAR, QUADRATIC, AND CUBIC TERM

#COMPARISON OF OUT-OF-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_OUT #MODEL WITH ONLY LINEAR TERM
RMSE_2_OUT #MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_OUT #MODEL WITH LINEAR, QUADRATIC, AND CUBIC TERM


########################################################
###PLOTTING THE REGRESSION MODELS AGAINST ONE ANOTHER###
########################################################

x_grid <- seq(-35,22,1) #CREATES GRID OF X-AXIS VALUES
plot(Training$VOO ~ Training$GDP, col='blue')
predictions_1 <- predict(M1, list(GDP=x_grid))
predictions_2 <- predict(M2, list(GDP=x_grid, GDP2=x_grid^2))
predictions_3 <- predict(M3, list(GDP=x_grid, GDP2=x_grid^2, GDP3=x_grid^3))

lines(x_grid, predictions_1, col='darkgreen', lwd=3) #PLOTS M1
lines(x_grid, predictions_2, col='green', lwd=3) #PLOTS M2
lines(x_grid, predictions_3, col='lightgreen', lwd=3) #PLOTS M3

points(Testing$VOO ~ Testing$GDP, col='red', pch=3)  +
  title("All Regression Models")
