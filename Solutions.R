#loading libraries
library(tidyverse)
library(car)
library(MASS)

#reading the data
#as per the data dictionaries, there are only numeric variables and categorical variables.
#We can assume that all strings are categorical. So, stringsAsFactors can be TRUE.

data <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = TRUE)

#checking the data
View(data)
str(data)

#We can see that a lot of variables are Categorical in the Data Dictionary but they have been read as integers or numbers.

columns <- c(2:9, 15:16, 18)

data[,columns] <- lapply(data[,columns], factor)
str(data)

#Now, our data frame reflects the data dictionary

#### DATA PREPARATION AND CLEANING

###Part 0.1 - Checking data for basic inconsitencies

##Checking for NA
sum(is.na(data))

#There are no NA values

##Checking for duplicates

length(unique(data$car_ID))

#We can see there are no duplicated rows or rows with the same ID.

##Checking for outliers

#We'll check for outliers in numerical variables

#variable: wheelbase

boxplot(data$wheelbase)
wb <- quantile(data$wheelbase, seq(0,1,0.01))
wb
plot(wb)

#Even though our boxplot shows two outliers, the quantile distribution doesn't show a huge spike in the last two values

#variable: carlength

boxplot(data$carlength)
cl <- quantile(data$carlength, seq(0,1,0.01))
cl
plot(cl)

#As with wheelbase, the outliers aren't as extreme for us to treat them.

#variable: carwidth

boxplot(data$carwidth)
cw <- quantile(data$carwidth, seq(0,1,0.01))
cw
plot(cw)

#variable: carheight

boxplot(data$carheight)

#carheight has no outliers

#variable: curbweight

boxplot(data$curbweight)

#curbweight has no outliers

#variable: enginesize

boxplot(data$enginesize)
es <- quantile(data$enginesize, seq(0,1,0.01))
es
plot(es)

#Since enginesize has six outliers, three of which are extremely out of range, we'll cap all values above 91% (183.00) to 183.00

data$enginesize[which(data$enginesize>183.00)] <- 183.00

#Checking again
boxplot(data$enginesize)

#Outliers have been capped

#variable: boreratio

boxplot(data$boreratio)

#boreratio has no outliers

#variable: stroke

boxplot(data$stroke)
s <- quantile(data$stroke, seq(0,1,0.01))
s
plot(s)

#We'll floor the values which are less than 2% (2.6400) to 2.6400

data$stroke[which(data$stroke < 2.6400)] <- 2.6400

#Checking again
boxplot(data$stroke)

#variable: compressionratio

boxplot(data$compressionratio)
cr <- quantile(data$compressionratio, seq(0,1,0.01))
cr
plot(cr)

#There are an extreme number of outliers. We'll cap all values above 90% (10.9400) to 10.9400

data$compressionratio[which(data$compressionratio > 10.9400)] <- 10.9400

#Checking again
boxplot(data$compressionratio)

#variable: price

boxplot(data$price)
p <- quantile(data$price, seq(0,1,0.01))
p
plot(p)

#Even if price has outliers, we will not treat it because it is our dependent variable.

#removing all temporary variables
rm(cl, cr, cw, es, p, s, wb, columns)

###Part 0.3 - Dropping car_ID

#Creating a backup data frame

backup <- data

#restore data, uncomment below line
#data <- backup

#We will be dropping car_ID as it is just a sequence and doesn't help us in predicting anything.

data <- data[,-1]

###Part 0.4 - Splitting CarName into Company and Model

data <- data %>% separate(col = CarName, into = c("company", "model"), sep = " ", extra = "merge")
View(data)

#Checking whether the separate command created NA values

sum(is.na(data))

#There are two NA values now, as shown by our Warning messages for separate function

#Since we cannot determine which models these are, we will fill these with the company name itself.

data[139,"model"] <- data[139,"company"]
data[142,"model"] <- data[142,"company"] 

sum(is.na(data))     

#There are no NA values in the data set anymore. We can proceed with creating dummy variables from the factors.

###Part 0.5 - Creating Dummy Variables

#checking structure of dataset again

str(data)

#Our separate operation made the company and model character type. We have to convert them into factors again.

columns <- c(2,3)
data[,columns] <- lapply(data[,columns], factor)
rm(columns)

str(data)

#Creating another backup to serve as a restore point just in case

backup2 <- data

#restore dataset, uncomment the next line
#data <- backup2

##Let us treat the variables with two levels first

##variable: fueltype
summary(data$fueltype)

#Our assumption is that diesel will be 0, gas (not diesel) will be 1
levels(data$fueltype) <- c(0,1)
data$fueltype <- as.integer(data$fueltype)

##variable: aspiration
summary(data$aspiration)

#Our assumption is that std will be 0, turbo (not std) will be 1
levels(data$aspiration) <- c(0,1)
data$aspiration <- as.integer(data$aspiration)

##variable: doornumber
summary(data$doornumber)

#Our assumption is that four will be 0, two (not four) will be 1
levels(data$doornumber) <- c(0,1)
data$doornumber <- as.integer(data$doornumber)

##variable: enginelocation
summary(data$enginelocation)

#Our assumption is that front will be 0, rear (not front) will be 1
levels(data$enginelocation) <- c(0,1)
data$enginelocation <- as.integer(data$enginelocation)

##This finishes the factors with two levels.

backup3 <- data

#restore backup, uncomment next line
#data <- backup3

##Let us deal with the factors with more than two levels.

##variable: symboling

summary(data$symboling)

dummy_1 <- data.frame(model.matrix( ~symboling, data = data))
dummy_1 <- dummy_1[,-1]
data_1 <- cbind(data[,-1], dummy_1)

##variable: carbody

dummy_2 <- data.frame(model.matrix( ~carbody, data = data))
dummy_2 <- dummy_2[,-1]
data_2 <- cbind(data_1[,-6], dummy_2)

##variable: drivewheel

dummy_3 <- data.frame(model.matrix( ~drivewheel, data = data))
dummy_3 <- dummy_3[,-1]
data_3 <- cbind(data_2[,-6], dummy_3)

##variable: enginetype

dummy_4 <- data.frame(model.matrix( ~enginetype, data = data))
dummy_4 <- dummy_4[,-1]
data_4 <- cbind(data_3[,-12], dummy_4)

##variable: cylindernumber

dummy_5 <- data.frame(model.matrix( ~cylindernumber, data = data))
dummy_5 <- dummy_5[,-1]
data_5 <- cbind(data_4[,-12], dummy_5)

##variable: fuelsystem

dummy_6 <- data.frame(model.matrix( ~fuelsystem, data = data))
dummy_6 <- dummy_6[,-1]
data_6 <- cbind(data_5[,-13], dummy_6)

##variable: company

#Before we treat the company, we can see that there are way too many discrepancies in the company column.

summary(data_6$company)

#We can see that there are value-pairs which should be the same. We'll treat them one-by-one.

##maxda and mazda. This could've happened because "z" and "x" are positioned adjacent on the keyboard.

data_6$company <- gsub("maxda", "mazda", data_6$company)

##Nissan and nissan. This is a simple case error.

data_6$company <- gsub("Nissan", "nissan", data_6$company)

##volkswagen, vokswagen and vw are all the same ones

data_6$company <- gsub("vw", "volkswagen", data_6$company)
data_6$company <- gsub("vokswagen", "volkswagen", data_6$company)

##toyouta and toyota is also a misspelling

data_6$company <- gsub("toyouta", "toyota", data_6$company)

##porsche and porcshce is the last misspelling.

data_6$company <- gsub("porcshce", "porsche", data_6$company)

data_6$company <- as.factor(data_6$company)
summary(data_6$company)


dummy_7 <- data.frame(model.matrix( ~company, data = data_6))
dummy_7 <- dummy_7[,-1]
data_7 <- cbind(data_6[,-1], dummy_7)

###Part 0.6 - Removing model column as required by the problem

data_7 <- data_7[,-1]


###Part 0.7 - Cleaning Environment

data <- data_7
backup <- backup3
rm(backup2,backup3,data_1,data_2,data_3,data_4,data_5,data_6,data_7,dummy_1,dummy_2,dummy_3,dummy_4,dummy_5,dummy_6,dummy_7)
str(data)


###Part 0.8 - Derived Metrics

##carvolume

#We can convert the carheight, carlength, carwidth variables to carvolume. This would decrease the number of variables.

data$carvolume <- data$carlength * data$carheight * data$carwidth
columns <- c(6,7,8)
data <- data[,-columns]
rm(columns)

###Part 0.9 - Splitting Data into Train and Test

sample_size <- floor(0.75 * nrow(data))
set.seed(42)
train_indices <- sample(seq_len(nrow(data)), size = sample_size)
train <- data[train_indices, ]
test <- data[-train_indices, ]

#### MODEL BUILDING

###Part 1.1 - First model

model_1 <- lm(price~., train)
summary(model_1)

#We'll use the first model's summary to only include significant variables in our model.

#Let us apply StepAIC

step <- stepAIC(model_1, direction = "both")

###Part 1.2 - Creating A New Model Iteratively

model_2 <- lm(price ~ fueltype + enginelocation + curbweight + enginesize + 
                boreratio + horsepower + peakrpm + citympg + symboling.1 + 
                symboling0 + symboling1 + symboling2 + symboling3 + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypedohcv + 
                enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                cylindernumberthree + cylindernumbertwelve + fuelsystem2bbl + 
                fuelsystemmpfi + companyaudi + companybmw + companychevrolet + 
                companydodge + companyhonda + companyisuzu + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + 
                companyporsche + companytoyota + companyvolkswagen + companyvolvo + 
                carvolume, train)
summary(model_2)
vif(model_2)

##symboling2 has a high VIF(19.338327) and has relatively low significance (p-value = 0.104208)
##We can remove it.

model_3 <- lm(price ~ fueltype + enginelocation + curbweight + enginesize + 
                boreratio + horsepower + peakrpm + citympg + symboling.1 + 
                symboling0 + symboling1 + symboling3 + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypedohcv + 
                enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                cylindernumberthree + cylindernumbertwelve + fuelsystem2bbl + 
                fuelsystemmpfi + companyaudi + companybmw + companychevrolet + 
                companydodge + companyhonda + companyisuzu + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + 
                companyporsche + companytoyota + companyvolkswagen + companyvolvo + 
                carvolume, train)
summary(model_3)

#The adjusted R-squared is roughly the same. Nothing much has changed in the model.

vif(model_3)

##carvolume has relatively high VIF(15.275585) and has relatively low significance (p-value = 0.147415)
## We can remove it.

model_4 <- lm(price ~ fueltype + enginelocation + curbweight + enginesize + 
                boreratio + horsepower + peakrpm + citympg + symboling.1 + 
                symboling0 + symboling1 + symboling3 + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypedohcv + 
                enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                cylindernumberthree + cylindernumbertwelve + fuelsystem2bbl + 
                fuelsystemmpfi + companyaudi + companybmw + companychevrolet + 
                companydodge + companyhonda + companyisuzu + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + 
                companyporsche + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_4)

#The adjusted R-squared is roughly the same. Nothing much has changed. So, we can proceed further.

vif(model_4)

#symboling3 has now become insignificant and its vif is also more than 2 (4.100132), so we'll remove it.

model_5 <- lm(price ~ fueltype + enginelocation + curbweight + enginesize + 
                boreratio + horsepower + peakrpm + citympg + symboling.1 + 
                symboling0 + symboling1 + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypedohcv + 
                enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                cylindernumberthree + cylindernumbertwelve + fuelsystem2bbl + 
                fuelsystemmpfi + companyaudi + companybmw + companychevrolet + 
                companydodge + companyhonda + companyisuzu + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + 
                companyporsche + companytoyota + companyvolkswagen + companyvolvo, train)

summary(model_5)
vif(model_5)

#enginetypel has become insignificant also and its vif is 4.8, so we'll remove that next.

model_6 <- lm(price ~ fueltype + enginelocation + curbweight + enginesize + 
                boreratio + horsepower + peakrpm + citympg + symboling.1 + 
                symboling0 + symboling1 + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypedohcv + enginetypeohc + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                cylindernumberthree + cylindernumbertwelve + fuelsystem2bbl + 
                fuelsystemmpfi + companyaudi + companybmw + companychevrolet + 
                companydodge + companyhonda + companyisuzu + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + 
                companyporsche + companytoyota + companyvolkswagen + companyvolvo, train)

summary(model_6)
vif(model_6)


#companybmw has been insignificant for the last three models, and its vif is also more than 2 (5+), so we'll remove it.
model_7 <- lm(price ~ fueltype + enginelocation + curbweight + enginesize + 
                boreratio + horsepower + peakrpm + citympg + symboling.1 + symboling1 + symboling0 + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypedohcv + enginetypeohc + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                cylindernumberthree + cylindernumbertwelve + fuelsystem2bbl + 
                fuelsystemmpfi + companyaudi + companychevrolet + 
                companydodge + companyhonda + companyisuzu + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + 
                companyporsche + companytoyota + companyvolkswagen + companyvolvo, train)

summary(model_7)
vif(model_7)

#companyporsche has suddenly become insignificant. Let's check for correlation between companyporsche and companybmw

cor(data$companyporsche, data$companybmw)

#they're not correlated which is good to know. We'll remove companyporsche

model_8 <- lm(price ~ fueltype + enginelocation + curbweight + enginesize + 
                boreratio + horsepower + peakrpm + citympg + symboling.1 + symboling1 + symboling0 + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypedohcv + enginetypeohc + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                cylindernumberthree + cylindernumbertwelve + fuelsystem2bbl + 
                fuelsystemmpfi + companyaudi + companychevrolet + 
                companydodge + companyhonda + companyisuzu + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_8)
vif(model_8)

#fuelsystemmpfi has high vif (8.536430) and low significance, we'll remove it.

model_9 <- lm(price ~ fueltype + enginelocation + curbweight + enginesize + 
                boreratio + horsepower + peakrpm + citympg + symboling.1 + symboling1 + symboling0 + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypedohcv + enginetypeohc + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                cylindernumberthree + cylindernumbertwelve + fuelsystem2bbl + companyaudi + companychevrolet + 
                companydodge + companyhonda + companyisuzu + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_9)
vif(model_9)

#fuelsystem2bbl has suddenly become insignificant. Let us check for correlation between that and fuelsystemmpfi

cor(data$fuelsystem2bbl, data$fuelsystemmpfi)

#They have relatively significant negative correlation (-0.634). We'll remove fuelsystem2bbl as it has low significance now and high vif (4.1)

model_10 <- lm(price ~ fueltype + enginelocation + curbweight + enginesize + 
                 boreratio + horsepower + peakrpm + citympg + symboling.1 + symboling1 + symboling0 + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypedohcv + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 cylindernumberthree + cylindernumbertwelve + companyaudi + companychevrolet + 
                 companydodge + companyhonda + companyisuzu + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_10)
vif(model_10)

#fueltype has suddenly become insignificant. Let us check for correlation between that and fuelsystem2bbl

cor(data$fueltype, data$fuelsystem2bbl)

#The correlation is low (0.226). fueltype is insignifant and it has vif > 2 (3.041), we'll remove it next.

model_11 <- lm(price ~ + enginelocation + curbweight + enginesize + 
                 boreratio + horsepower + peakrpm + citympg + symboling.1 + symboling1 + symboling0 + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypedohcv + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 cylindernumberthree + cylindernumbertwelve + companyaudi + companychevrolet + 
                 companydodge + companyhonda + companyisuzu + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_11)
vif(model_11)

#peakrpm has vif (4.7) and is less significant relatively (p value: 0.062), we'll remove it.

model_12 <- lm(price ~ + enginelocation + curbweight + enginesize + 
                 boreratio + horsepower + citympg + symboling.1 + symboling1 + symboling0 + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypedohcv + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 cylindernumberthree + cylindernumbertwelve + companyaudi + companychevrolet + 
                 companydodge + companyhonda + companyisuzu + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_12)
vif(model_12)

#next up is symboling0 which has had low significance for a lot of previous models, it also has high vif (3.5)

model_13 <- lm(price ~ + enginelocation + curbweight + enginesize + 
                 boreratio + horsepower + citympg + symboling.1 + symboling1 + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypedohcv + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 cylindernumberthree + cylindernumbertwelve + companyaudi + companychevrolet + 
                 companydodge + companyhonda + companyisuzu + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_13)
vif(model_13)

#symboling.1 has suddenly become insignificant. Let us check correlation.

cor(data$symboling.1, data$symboling0)

#The correlation is low. We can remove this.

model_14 <- lm(price ~ + enginelocation + curbweight + enginesize + 
                 boreratio + horsepower + citympg + symboling1 + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypedohcv + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 cylindernumberthree + cylindernumbertwelve + companyaudi + companychevrolet + 
                 companydodge + companyhonda + companyisuzu + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)

summary(model_14)
vif(model_14)

#We can remove any between symboling1 and carbodyhardtop as both have low significance and their vifs are comparable. Let us remove symboling1

model_15 <- lm(price ~ + enginelocation + curbweight + enginesize + 
                 boreratio + horsepower + citympg + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypedohcv + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 cylindernumberthree + cylindernumbertwelve + companyaudi + companychevrolet + 
                 companydodge + companyhonda + companyisuzu + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_15)
vif(model_15)

#next up is carbodyhardtop

model_16 <- lm(price ~ + enginelocation + curbweight + enginesize + 
                 boreratio + horsepower + citympg + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypedohcv + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 cylindernumberthree + cylindernumbertwelve + companyaudi + companychevrolet + 
                 companydodge + companyhonda + companyisuzu + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_16)
vif(model_16)

#carbodysedan has high vif (5.38) and low significance (p-value: 0.04)

model_17 <- lm(price ~ + enginelocation + curbweight + enginesize + 
                 boreratio + horsepower + citympg + carbodyhatchback + carbodywagon + enginetypedohcv + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 cylindernumberthree + cylindernumbertwelve + companyaudi + companychevrolet + 
                 companydodge + companyhonda + companyisuzu + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_17)
vif(model_17)

#carbodyhatchback and carbodywagon are now insignificant. This could be because bodytype matters if all are considered however, if one is removed, all the others become insignificant as well.

#Still, they have vif under 2, so we'll look at citympg which is less significant relatively from the others and has high vif (6.19)

model_18 <- lm(price ~ + enginelocation + curbweight + enginesize + 
                 boreratio + horsepower + carbodyhatchback + carbodywagon + enginetypedohcv + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 cylindernumberthree + cylindernumbertwelve + companyaudi + companychevrolet + 
                 companydodge + companyhonda + companyisuzu + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_18)
vif(model_18)

#now, we will remove carbodyhatchback as it is insignificant

model_19 <- lm(price ~ + enginelocation + curbweight + enginesize + 
                boreratio + horsepower + carbodywagon + enginetypedohcv + enginetypeohc + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                cylindernumberthree + cylindernumbertwelve + companyaudi + companychevrolet + 
                companydodge + companyhonda + companyisuzu + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_19)
vif(model_19)

#now, the lowest significance (p-value: 0.075) is of carbodywagon. Even if it has low vif, every other factor has high significance, so we wil remove this.

model_20 <- lm(price ~ + enginelocation + curbweight + enginesize + 
                 boreratio + horsepower + enginetypedohcv + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 cylindernumberthree + cylindernumbertwelve + companyaudi + companychevrolet + 
                 companydodge + companyhonda + companyisuzu + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_20)
vif(model_20)

#As all our factors now have high significance, we'll eliminate on the basis of highest vif. enginesize has vif 22.146.

model_21 <- lm(price ~ + enginelocation + curbweight + 
                 boreratio + horsepower + enginetypedohcv + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 cylindernumberthree + cylindernumbertwelve + companyaudi + companychevrolet + 
                 companydodge + companyhonda + companyisuzu + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_21)
vif(model_21)

#boreratio has suddenly become insignificanct, we'll check for correlation.

cor(data$boreratio, data$enginesize)

#There is relatively high cor, boreratio only matters if we consider enginesize. Let us remove it.

model_22 <- lm(price ~ + enginelocation + curbweight + horsepower + enginetypedohcv + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 cylindernumberthree + cylindernumbertwelve + companyaudi + companychevrolet + 
                 companydodge + companyhonda + companyisuzu + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_22)
vif(model_22)

#We will now remove cylindernumberthree

model_23 <- lm(price ~ + enginelocation + curbweight + horsepower + enginetypedohcv + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + cylindernumbertwelve + companyaudi + companychevrolet + 
                 companydodge + companyhonda + companyisuzu + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_23)
vif(model_23)

#enginetypeohcf and companyaudi have similar vifs and significance values. We can remove any. We'll remove companyaudi as it has remained low significant for a while.

model_24 <- lm(price ~ + enginelocation + curbweight + horsepower + enginetypedohcv + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + cylindernumbertwelve + companychevrolet + 
                 companydodge + companyhonda + companyisuzu + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_24)
vif(model_24)

#enginetypeohcf will go next.

model_25 <- lm(price ~ + enginelocation + curbweight + horsepower + enginetypedohcv + enginetypeohc + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + cylindernumbertwelve + companychevrolet + 
                 companydodge + companyhonda + companyisuzu + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_25)
vif(model_25)

#cylindernumbertwelve has low significance, we'll remove that next.

model_26 <- lm(price ~ + enginelocation + curbweight + horsepower + enginetypedohcv + enginetypeohc + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + companychevrolet + 
                 companydodge + companyhonda + companyisuzu + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_26)
vif(model_26)

#next we will remove enginetypedohcv

model_27 <- lm(price ~ + enginelocation + curbweight + horsepower + enginetypeohc + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + companychevrolet + 
                 companydodge + companyhonda + companyisuzu + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_27)
vif(model_27)

#enginetypeohc has low significance and vif at 2.7, we wil remove it next.

model_28 <- lm(price ~ + enginelocation + curbweight + horsepower + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + companychevrolet + 
                 companydodge + companyhonda + companyisuzu + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_28)

#Removing enginetypeohc has made a lot of company variables insignificant. This could be because they were correlated. However, the R-squared and adjusted R-squared have dropped significantly.

#We'll try making another model_28 removing the other choice instead.

model_28.1 <- lm(price ~ + enginelocation + curbweight + horsepower + enginetypeohc + enginetyperotor + 
                   cylindernumberfive + cylindernumberfour + cylindernumbersix + companychevrolet + 
                   companydodge + companyhonda + companyisuzu + 
                   companymercury + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_28.1)
vif(model_28.1)

model_28 <- model_28.1
rm(model_28.1)

summary(model_28)
vif(model_28)

#companyisuzu and companymercury have very similar values. Perhaps, they are correlated.

cor(data$companyisuzu, data$companymercury)

#They are not correlated.

#Most of our vifs are now near 2, however, horsepower and curbweight are at 4+. Let us check for correlation

cor(data$horsepower, data$curbweight)

#These variables have relatively high correlation. Let us remove horsepower.

model_29 <- lm(price ~ + enginelocation + curbweight + enginetypeohc + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + companychevrolet + 
                 companydodge + companyhonda + companyisuzu + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_29)
vif(model_29)

#we'll remove companymercury next

model_30 <- lm(price ~ + enginelocation + curbweight + enginetypeohc + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + companychevrolet + 
                 companydodge + companyhonda + companyisuzu + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_30)
vif(model_30)

#Next, we'll remove companyisuzu since it has been insignificant for the last many models.

model_31 <- lm(price ~ + enginelocation + curbweight + enginetypeohc + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + companychevrolet + 
                 companydodge + companyhonda + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_31)
vif(model_31)

#cylindernumbersix and cylindernumberfour have awfully high vif values. cylindernumberfour however is significant beyond doubt. Perhaps, they are correlated.

cor(data$cylindernumbersix, data$cylindernumberfour)

#They are relatively highly negatively correlated. Let us try removing cylindernumbersix

model_32 <- lm(price ~ + enginelocation + curbweight + enginetypeohc + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + companychevrolet + 
                 companydodge + companyhonda + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_32)
vif(model_32)

#companychevrolet has become insignificant. Let us check for correlation

cor(data$companychevrolet, data$cylindernumbersix)

#No correlation. We can remove it.

model_33 <- lm(price ~ + enginelocation + curbweight + enginetypeohc + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + 
                 companydodge + companyhonda + companymitsubishi + companynissan + companyplymouth + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_33)
vif(model_33)

#next, we'll remove companyplymouth

model_34 <- lm(price ~ + enginelocation + curbweight + enginetypeohc + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + 
                 companydodge + companyhonda + companymitsubishi + companynissan + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_34)
vif(model_34)

#Let us try removing companyhonda next

model_35 <- lm(price ~ + enginelocation + curbweight + enginetypeohc + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + 
                 companydodge + companymitsubishi + companynissan + companytoyota + companyvolkswagen + companyvolvo, train)
summary(model_35)
vif(model_35)

#Let us try removing companyvolkswagen next

model_36 <- lm(price ~ + enginelocation + curbweight + enginetypeohc + enginetyperotor + 
                             cylindernumberfive + cylindernumberfour + 
                             companydodge + companymitsubishi + companynissan + companytoyota + companyvolvo, train)
summary(model_36)
vif(model_36)

#Next, we'll remove companydodge

model_37 <- lm(price ~ + enginelocation + curbweight + enginetypeohc + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + companymitsubishi + companynissan + companytoyota + companyvolvo, train)

summary(model_37)
vif(model_37)

#Next, companyvolvo will be removed as it is insignificant.

model_38 <- lm(price ~ + enginelocation + curbweight + enginetypeohc + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + companymitsubishi + companynissan + companytoyota, train)
summary(model_38)
vif(model_38)

#Next companymitsubishi will have to go as it has the least significance

model_39 <- lm(price ~ + enginelocation + curbweight + enginetypeohc + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + companynissan + companytoyota, train)
summary(model_39)
vif(model_39)

#Next, enginetyperotor as it has low significance (p-value: 0.58)

model_40 <- lm(price ~ + enginelocation + curbweight + enginetypeohc +
                 cylindernumberfive + cylindernumberfour + companynissan + companytoyota, train)
summary(model_40)
vif(model_40)

#Next, companynissan

model_41 <- lm(price ~ + enginelocation + curbweight + enginetypeohc +
                 cylindernumberfive + cylindernumberfour + companytoyota, train)
summary(model_41)
vif(model_41)

#Next, we will remove cylindernumberfive as it has low significance and higher vif than companytoyota

model_42 <- lm(price ~ + enginelocation + curbweight + enginetypeohc + cylindernumberfour + companytoyota, train)
summary(model_42)
vif(model_42)

model_43 <- lm(price ~ + enginelocation + curbweight + enginetypeohc + cylindernumberfour, train)
summary(model_43)
vif(model_43)

model_44 <- lm(price ~ + enginelocation + curbweight + cylindernumberfour, train)
summary(model_44)
vif(model_44)

#### We had models above which produced extremely high Adjusted R-Squared where most factors were significant.
#### Now we have reached a model which says that the price of a car depends on where the engine is located, what its curbweight is and whether is has four cylinders or not.
#### Yet, its adjusted R-Squared is relatively less.

#### TESTING MODEL

### Part 1 - Final Model (model_44)

Predict_1 <- predict(model_44,test[,-14])
test$testprice <- Predict_1
r <- cor(test$price,test$testprice)
rsquared <- r^2

### Part 2 - Model 20 which has a lot of factors but all are significant.

Predict_2 <- predict(model_20, test[,-14])
test$testprice <- Predict_2
r_2 <- cor(test$price, test$testprice)
rsquared_2 <- r_2^2

## We can see that model_44 outperforms model_20. Most factors in Model 20 have high vifs and thus multicolinearity is present.

#### SOLUTION

## Model 44 is the final model. As per this model, the price of a car is highly dependent on the curbweight.
## The next factor that determines the price is the enginelocation.
## The number of cylinders being four or not somehow matters also when it comes to finding the price of the car.

summary(model_44)