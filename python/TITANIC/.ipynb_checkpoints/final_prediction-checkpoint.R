## import the data

###############
setwd('~/tensorflow_machine_learning/R_PROJECT/TITANIC/')
train <- read.csv('data/train.csv')
test <- read.csv('data/test.csv')
##############

## how many people survived and death
## python command value_counts()
table(train$Survived)
data1=table(train$Survived)
###### plotting 

#barplot(data1)

## get the percentage of the data
## it will give the percentage 
#barplot(prop.table(data1))



## create out first prediction
## before make a copy of the testing data set


test1 = test
test2 = test

#View(test2)



## first prediction the extreme point
## every body died
## creating the survived column and put 0
test1$Survived <-0
#View(test1)
### and make the test2 and make everybody survived 1
test2$Survived <-1
#View(test2)



## make prediction column
prediction1_1 = data.frame(PassengerId=test1$PassengerId,Survived=test1$Survived)
#View(prediction1_1)


prediction1_2 = data.frame(PassengerId=test2$PassengerId,Survived=test2$Survived)
#View(prediction1_2)



#barplot(table(prediction1_1))
#barplot(table(prediction1_2))



################ SEX
table(train$Sex)
data2=prop.table(table(train$Sex))

#barplot(data2*100)




## second prediction make all the female survived
#View(test1)

test1$Survived[test1$Sex =='female'] <-1
#View(test1)

table(test1$Survived)


#### find relation with the ticket price and passsenger class with death and the survived


#View(test1$Fare)

##making catagory in test1$Fare
## here we use the train data because in the test1 data we changed the actual
##survived person
train$Fare2='30+'
View(test1$Fare2)
train$Fare2[test1$Fare <30 & test1$Fare >=20] <- '20-30'
train$Fare2[test1$Fare <20 & test1$Fare >=10] <- '10-20'
train$Fare2[test1$Fare <10] <- '<10'


#View(train$Fare2)

based_on_fare=table(train$Fare2)
#barplot(based_on_fare)


### go deeper find the cross match between Fare2 Pclass,Sex with Survived

sur_b_on=aggregate(Survived ~ Fare2+Pclass+Sex,data=train,FUN=sum)
#View(sur_b_on)
## now this part is confusing remember we using binary data 0 1
## when we use the sum function 0 become worth less we only can add 1
## so here in the aggregate command we get only survived
## cause we use the sum command

total_relation=aggregate(Survived ~ Fare2+Pclass+Sex,data=train,FUN=length)
View(total_relation)

vvidata=aggregate(Survived ~ Fare2+ Pclass+Sex,data = train,FUN = function(x){sum(x)/length(x)})
#print(vvidata)


############################################
# This is Very Very important
# how many people survived based on the these relation
# how many total people are based on the these relation
# now find the relation of survived and dead based on these relation
# we got the dead too
#############################################

## from the data we can say something that
#--> woman from 2nd class give fare 30+ money from there no one die 100% survived
## so lets reassign our prediction
#--> woman from 1st class give fare 30+ money 97% survived


###############################################

test1$Survived[test1$Pclass==2 & test1$Fare2=='30+' & test1$Sex=='female' ] <- 1
test1$Survived[test1$Pclass==1 & test1$Fare2=='<10' & test1$Sex=='female' ] <- 1
test1$Survived[test1$Pclass==1 & test1$Fare2=='10-20' & test1$Sex=='female' ] <- 1


#View(test1$Survived)
table(test1$Survived)



## apply recurcive partitioning
## make a dicision tree
#install.packages("rattle")
#install.packages("rpart.plot")
#install.packages("RColorBrewer")

##create a tree
library('rpart')
library('rattle')
library('RColorBrewer')



##make a simple tree
mytree <-rpart(Survived ~ Sex,data=train,method = 'class')
#fancyRpartPlot(mytree)

## make another complex tree
mytree1 <- rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=train,method = 'class')
#fancyRpartPlot(mytree1)
#plot(mytree1)
text(mytree1)




## to make this is make understand make it simple

mysimpletree=rpart(Survived ~ Sex+Age,data=train,method = 'class')
#fancyRpartPlot(mysimpletree)

mysimpletree=rpart(Survived ~ Sex+Age+Pclass,data=train,method = 'class')
#fancyRpartPlot(mysimpletree)


mysimpletree=rpart(Survived ~ Age,data=train,method = 'class')
#fancyRpartPlot(mysimpletree)



##################
##################


## for the first time i will predict by an function
prediction_4=predict(mytree1,test,type = 'class')
View(prediction_4)


prediction_data_frame=data.frame(PassengerId=test$PassengerId,Survived=prediction_4)


write.csv(prediction_data_frame,file = "tree1.csv" ,row.names=FALSE)

## we add both train and test data so we can feature engineering
## first adding the 'NA' in the test$survived

test$Survived <- NA
View(train)
train1 <- read.csv('data/train.csv')
View(train1)
combined_set <- rbind(train1,test)
View(combined_set)


## now we can convert the survived$name from class to character
## sometimes it helps

combined_set$Name <- as.character(combined_set$Name)



##### in any disaster there is priority for children and Mother
#### now next target the child and mother from the combined dataset



combined_set$Child[combined_set$Age<14] <- 'Child'
combined_set$Child[combined_set$Age>14] <- 'Adult'


### see the status

table(combined_set$Child,combined_set$Survived)
combined_set$Child <- factor(combined_set$Child)


## find Mother
##how can you find mother
#-> have to be female
#-> more than 18 year
#->Parch(numberofchildren)>0
combined_set$Mother <-'Not Mother'
combined_set$Mother[combined_set$Sex=='female' & combined_set$Parch>0 & combined_set$Age>18] <- 'Mother'

View(combined_set)
table(combined_set$Mother,combined_set$Survived)



mytree4 <-rpart(Survived ~ Mother,data=combined_set,method = 'class')
fancyRpartPlot(mytree4)


mytree5 <-rpart(Survived ~ Mother+Child,data=combined_set,method = 'class')
fancyRpartPlot(mytree5)



mytree6 <-rpart(Survived ~ Child,data=combined_set,method = 'class')
fancyRpartPlot(mytree6)




combined_set$Name[1]
strsplit(combined_set$Name[1],split ='[,.]' )
### its easy we split by the ',' and '.' 
strsplit(combined_set$Name[1],split ='[,.]')[[1]] # taking the element


strsplit(combined_set$Name[1],split ='[,.]')[[1]][[2]] # taking the element




## we do for the total data and return vector and added in the title
combined_set$Title <- sapply(combined_set$Name,FUN = function(x){strsplit(x,split ='[,.]')[[1]][[2]]})
View(combined_set$Title)



#see the table

table(combined_set$Title)



##UPDATE THE MOTHER COLUMN 'MISS' CANT BE  A MOTHER


table(combined_set$Mother)
combined_set$Mother <-'Not Mother'
combined_set$Mother[combined_set$Sex=='female' & combined_set$Parch>0 & combined_set$Age>18 & combined_set$Title != 'Miss'] <- 'Mother'
table(combined_set$Mother)


View(combined_set$Ticket)
table(combined_set$Ticket)




## lets work with the Fate



mytree8 <-rpart(Survived ~ Fare,data=train,method = 'class')
fancyRpartPlot(mytree8)



combined_set$Fare_type <- NA
combined_set$Fare_type [combined_set$Fare<50] <- 'low'
combined_set$Fare_type [combined_set$Fare>50 & combined_set$Fare<=100] <- 'med1'
combined_set$Fare_type [combined_set$Fare>100 & combined_set$Fare<=150] <- 'med2'
combined_set$Fare_type [combined_set$Fare>=150 & combined_set$Fare<500] <- 'high'
combined_set$Fare_type [combined_set$Fare>500] <- 'vhigh'
View(combined_set$Fare_type)
table(combined_set$Fare_type)


mytree8 <-rpart(Fare_type ~ Fare,data=combined_set,method = 'class')
fancyRpartPlot(mytree8)





### the family size is an important because 
## it is inversly proportional to the survived
## if the family size is  big then the Survived is less
## cause the father may not leave their family in the danger
## ot waste time to find the other member

## ok family size is the the the person himself+sibling+child

combined_set$Family_size = combined_set$SibSp +combined_set$Parch +1
table(combined_set$Family_size)



## its very important isight
mytree8 <-rpart(Survived ~ Parch+SibSp,data=combined_set,method = 'class')
fancyRpartPlot(mytree8)



##make another column

combined_set$Family_Size_group <- NA 
combined_set$Family_Size_group[combined_set$Family_size ==1] <-'Single'
combined_set$Family_Size_group[combined_set$Family_size >1 & combined_set$Family_size <=7] <-'Smaller'
combined_set$Family_Size_group[combined_set$Family_size >5] <-'large'
table(combined_set$Family_Size_group)
mosaicplot(table(combined_set$Family_Size_group),shade = TRUE)
mosaicplot(table(combined_set$Family_Size_group,combined_set$Survived),shade = TRUE)




mytree9 <-rpart(Survived ~ Family_Size_group,data=combined_set,method = 'class')
fancyRpartPlot(mytree9)

## split the train and test data from combined data
## we added cause we can change add both simultaniously

View(combined_set)

train = combined_set[1:891,]

test = combined_set[892:1309,]



finaltree <- rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Child+Mother+Title+Fare_type+Family_size+Family_Size_group,data=train,method = 'class')
fancyRpartPlot(finaltree)
View(combined_set) 






prediction_4=predict(finaltree,test,type = 'class')
View(prediction_4)


prediction_data_frame=data.frame(PassengerId=test$PassengerId,Survived=prediction_4)


write.csv(prediction_data_frame,file = "fifth.csv" ,row.names=FALSE)


##there are 263 NA value in the data
View(!is.na(combined_set))
## this command show you is there is NA or not
## if na then True
## if not False


#############################################
# we have a age missing but before that we know
# than we classify the data with decision tree
# there is a way dicision tree will solve everything
# we take all the parameter and this time we predict the 
# Age where Age is missing
##############################################


##############################################
# lets predict the whole age and and put them 
# in the Fillage group
##############################################

#now first create a dicision tree will all the value we have

Fillage <- rpart(Age ~ Pclass + Mother+Family_size+Sex+SibSp+Parch+Fare+Embarked+Family_Size_group,data = combined_set[!is.na(combined_set$Age),],method = "anova")

##this means creat decision tree with the data with the column only this row which is NA

## here data=combined_set[!is.na() combined_set$Age] means only the null value is False and
## the false is going to be predicted with the decision tree



fancyRpartPlot(Fillage)

##now predict and store in the NA value
## now find the null value is.na(combined_set$Age) if it is none then it is true
## then assign the predicted value
#View(combined_set[is.na(combined_set$Age),]) only redirect the value whis ha NA in the Age
combined_set$Age[is.na(combined_set$Age)]<-predict(Fillage,combined_set[is.na(combined_set$Age),])

#Fillage is tree
#and is.na(combined_set$Age) is the data which has NA in Age and with this data we will predict the NA

##predict(dicisiontree,testvalue)
#inthis case decision tree is the Fillage and the test data is
#View(combined_set[is.na(combined_set$Age),]) because we are gonna predict the Age with this data

View(combined_set$Age)

summary(combined_set$Age)
summary(combined_set$Embarked)



## there is two blank value not NA same thing but different pain

#find all the rows with this blank
which(combined_set$Embarked=='')
View(combined_set$Embarked)
combined_set$Embarked[c(62,830)]<-'S'

summary(combined_set$Embarked)



## ok we assign the S to the null value
## all are gone
## now we check the Fare

summary(combined_set$Fare)


### ok there is one NA

## we can do wo things we predict or we can just give the median value
## now we do both for learnign


Fillfare <- rpart(Fare ~ Pclass + Mother+Family_size+Sex+SibSp+Parch+Age+Embarked+Family_Size_group,data = combined_set[!is.na(combined_set$Fare),],method = "anova")
combined_set$Fare[is.na(combined_set$Fare)]<-predict(Fillfare,combined_set[is.na(combined_set$Fare),])





## or we can do the median
which(is.na(combined_set$Embarked))
#if you do not apply the predict command the value is 1044
## and just reassign the value
#combined_set$Fare[1044] <-median(combined_set$Fare,na.rm = TRUE)

##load the library
#install.packages("mice")
library('mice')
library('lattice')


library('randomForest')
##




#####################what is random forest ######################3
###############what is overfit ###############

##what is data over fit
##lets start with a nural network
#you know when you go epoch/itration to 100-200 at some point of time
##you get 100% accuracy thats not something to rejoice because
## or you add 100 layers more 
## what happen they are continuously getting same value and 
## data is gonna overfit
## its like giving the question paper before the exam to the student
## he will memorize everything but when you try to test it with new value
## it will fail because the model in this case dot learn it memorize
## it will be too dependent to test data i mean it will become too familier with the test data
## so after some layer it will say i got it .this have to be this
## but when you predict with new data preformance wil nt be that good



## random forest

## in the decision tree we create the decision by creating more and more branch
## depending on the value of the column
## if we let the tree grow too much it wil eventually overfit
## means dependent on the test data
## some model will be good
## some may be bad
## some may be not that bad
## the random forest take considaretion of all the tree may be take 
## the average of all the result

View(combined_set)


#lets create a lot random forest  with Pclass and Title
#############################################
#############################################3
###########################################
#THIS IS VERY VERY VERY IMPORTANT 
#YOU CANT DO RANDOM FOREST IF THE TITLE IS STRING
#YOU DONT NECESSERILY HAVE TO CONVERT TO A NUMBER
#BUT YOU HAVE TO CHANGE IT AS A FACTOR
#VVI YOU HAVE TO CHANGE IT TO A FACTOR

combined_set$Title<-factor(combined_set$Title)

############################################
############################################
############################################

X_train1<-combined_set[1:891,c('Pclass','Title')]

#Y_train1<-combined_set[1:891,c('Survived')]
#or just take the value from the train 
## same thing 
Y_train1<-as.factor(train$Survived) 
#View(Y_train1)
fit1<-randomForest(x = X_train1,y = Y_train1,importance = TRUE,ntree = 1000)
#######################
fit1

#fit1

#ntree=1000
#means we will create 1000 trees

#####################


## FROM THE RESULT WE HAVE THREE QUESTION
#1) WHAT IS OOB
#2)WHAT IS BAGGING
#3)WHAT IS BOOTSTRAPING


#OOB is called out of the bag observation.that means how good
#is your model to unknown data
#
#now ehat is bag
#### random forest has a cons it can overfit data
###low ammount of data this alogithm sometimes show you 100% 
###accuracy its overfitting
##'Random' is very important in this kind of thing
####################
#################
##'even you randomly making decision tree
## for example if you  making ramdom list of number
## there is a chance that one number repeat more than one
## its like asking the same question again and again in a exam
##like random number(1,3,4,2,4,4,5,4) 
#something like that 4 comes in three
#if it happens then we are actually making same decision tree
#again and again thats cause overfitting
#to prevent this bagginf is important
##bagging means those sample that are considered
##out of the bag means this missing that we missed
## because of the repetation 

###
###
#we can do it with other column but remember
## randomforest doesnt take more than 32 label








## lets make it by all but first make all them facetor
View(combined_set)

combined_set$Sex<-factor(combined_set$Sex)

combined_set$Embarked<-factor(combined_set$Embarked)
combined_set$Survived<-factor(combined_set$Survived)


X_train2<-combined_set[1:891,c('Pclass','Title','Sex','Age','SibSp','Parch','Fare','Embarked'),]
View(X_train2)
Y_train2<-as.factor(train$Survived)

fit2<-randomForest(x = X_train2,y = Y_train2,importance = TRUE,ntree = 1000)
fit2
## thats fantastic result
## less error rate better accuracy
#final_predict<-predict(fit2,test$Survived,OOB=TRUE,type = "response")
YY_test<-combined_set[892:1309,c('Pclass','Title','Sex','Age','SibSp','Parch','Fare','Embarked'),]
final_predict<-predict(fit2,YY_test)
View(final_predict)


####################
####################
prediction_data_frame1=data.frame(PassengerId=test$PassengerId,Survived=final_predict)
write.csv(prediction_data_frame1,file = "FINAL.csv" ,row.names=FALSE)



## thats all