rm(list = ls())

######################################### 1
dir.create("bigData_lab1")
setwd("/home/gehad/bigData_lab1")
getwd()
#########################################

######################################### 2
dfm <- read.csv("titanic.csv")
View(dfm)
#########################################

######################################### 3
#### a
dfm_dims <- dim(dfm)
dfm_dims
no_rows <- dfm_dims[1]
no_cols <- dfm_dims[2]
###

### b
str(dfm)
###

### c

#dfm_first10Rows <- dfm[1:10,]
#dfm_last10Rows <- dfm[no_rows-10:no_rows,]       ------------------- why not working
#dfm_last10Rows

head(dfm, 10)
tail(dfm, 10)
###

### d
summary_dfm <- summary(dfm)
summary_dfm
###

##################################### 4

### a
#Age_list <- dfm[,c("Age")]
Age_list <- dfm$Age
summary_AgeList <- summary(Age_list)
summary_AgeList
###

### b
first_Qr_Age <- summary_AgeList[2] 
third_Qr_Age <- summary_AgeList[5]
first_Qr_Age
third_Qr_Age
###

### c 

# First method
cat ("no.of NAN's value in Age column is :",summary_AgeList[7])
#

# second method

foundNAN <- FALSE
num_nans_Age <- 0

for (age in dfm$Age) 
{
  if(is.na(age))
  {
    print("NAN value is found")
    foundNAN <- TRUE    
    num_nans_Age = num_nans_Age+1
  }
}


{
  if (foundNAN)
  {
    cat(" Found NAN value and their num is : ",num_nans_Age)   #concatenate and print
    #cat("Found NAN value and their num is : ",summary_AgeList[7])
  }
  else
  {
    print("No NAN value is found")
  }
}
### 

### d
emb_list <- dfm$Embarked
cat("Type of embarked variable is : ",class(emb_list))

cat("Levels of embarked variable is : ",levels(emb_list))

valid_embarked <- c("C","Q","S")
for (emb_val in emb_list)
{
  if(emb_val %in% valid_embarked)
  {
    
  }else
  {
    cat("Unexpected value is found which is : ",emb_val,"\n") 
  }
}

###

### e

# Preprocessing is needed as unexpected values and null values are found 

###

###################################################### 5

### a
dfm <- dfm[!is.na(dfm$Age),]
dim(dfm)  #891-177 = 714
###

### b
valid_embarked <- c("C","Q","S")
dfm <- dfm[dfm$Embarked %in% valid_embarked,]     
dim(dfm)
###

### c
# first method
 #Age_list <- dfm$Age
 #summary_AgeList <- summary(Age_list)
 #summary_AgeList

# second method 
foundNAN <- FALSE

for (age in dfm$Age) 
{
  if(is.na(age))
  {
    print("NAN value is found")
    foundNAN <- TRUE    
  }
}


{
  if (foundNAN)
  {
    print("Found NAN value")
  }
  else
  {
    print("No NAN value is found")
  }
}

#class(dfm$Embarked)
embarked_factor <- factor(dfm$Embarked)

levels(embarked_factor)  #----------------------------------- leeh levels(dfm$Embarked) mn 8er factor byzawed "" maheya factor asln
# Yes that i am expecting

###

### d
print(names(dfm))        #get names of columns
drop_cols <- c("Cabin","Ticket")
dfm <- dfm[,!(names(dfm) %in% drop_cols)]
print(dim(dfm))
print(names(dfm))
### 

################################################# 6

### a
#print(dfm$Gender[1])              ---------------male , female
num_males <- dim(dfm[dfm$Gender == "male",])[1]
cat("Number of males is : ",num_males)

num_females <- dim(dfm[dfm$Gender == "female",])[1]
cat("Number of females is : ",num_females)
###

### b
slices_gender <- c(num_males, num_females)
lbls_gender <- c("male","female")
pie(slices_gender, labels = lbls_gender, main="Pie Chart of Gender")
###

### c
pie(slices_gender, labels = lbls_gender, main="Pie Chart of Gender",col=c("blue","red"))
###

### d
num_Survived_males <- dim(dfm[ dfm$Survived == 1 & dfm$Gender == "male",])[1]
cat("num_Survived_males is : ",num_Survived_males)

num_Survived_females <- dim(dfm[ dfm$Survived == 1 & dfm$Gender == "female",])[1]
cat("num_Survived_females is : ",num_Survived_females)

num_NotSurvived_males <- dim(dfm[ dfm$Survived == 0 & dfm$Gender == "male",])[1]
cat("num_NotSurvived_males is : ",num_NotSurvived_males)

num_NotSurvived_females <- dim(dfm[ dfm$Survived == 0 & dfm$Gender == "female",])[1]
cat("num_NotSurvived_females is : ",num_NotSurvived_females)
###

### d
slices_survived <- c(num_Survived_males, num_Survived_females)
lbls_survived <- c("Survived_males","Survived_females")
pie(slices_survived, labels = lbls_survived, main="Pie Chart of Gender")
###

### e
# number of survived females is greater than survived males
###

### f
rel <- table(dfm$Survived, dfm$Pclass)        #--------------------- joint distribution
rel
###

### g
barplot(rel,main="social class VS number of Survival",xlab="Pclass",ylab = "no.surv",xlim = c(0,20),width = c(3,3,3))
###

### h
barplot(rel,main="social class VS number of Survival",xlab="Pclass",ylab = "no.surv",col=c("red","blue"),legend = rownames(rel),xlim = c(0,20),width = c(3,3,3)) 
###

### i
# as the social class increase the number of not survived increase :(
###

### j
boxplot(dfm$Age)      #---------------------------- what are the circles ? outliers??!!
###

### I
plot(density(dfm$Age))
###

###################################################### 7
print(names(dfm))        #get names of columns
remain_cols <- c("Name","Survived")
dfm <- dfm[,(names(dfm) %in% remain_cols)]
print(names(dfm))
write.csv(dfm, "titanic_preprocessed.csv")
