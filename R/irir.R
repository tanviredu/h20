library(h2o)
h2o.init()
## importing data as h20  dataframe
datasets <- h2o.importFile(paste0('h20/python/IRIS-PREDICTION/Iris.csv'))
View(datasets)

## normal import
datasetR <-read.csv('h20/python/IRIS-PREDICTION/Iris.csv')
View(datasetR)

## convert the normal input into h2o input
iris <- as.h2o(datasetR)
head(iris)

data<-datasets[,2:5]
head(data)

## feature engineering

data$ratio1 <- data$PetalLengthCm/data$PetalWidthCm
head(data)

data$ratio2 <- data$SepalLengthCm/data$SepalWidthCm
head(data)


typeof(datasetR$Species)

### normal histo gram wont work here
h2o.hist(data$SepalLengthCm)
h2o.hist(data$PetalLengthCm)
h2o.hist(data$SepalWidthCm)
h2o.hist(data$PetalWidthCm)

## find correlation data
cr <- h2o.cor(data)


## reverse converting the data
df<-as.data.frame(data)        
cr<- cor(df)
crm <- as.matrix(cr)


heatmap(crm, scale = "row")



## splitting data into train test valid

parts <- h2o.splitFrame(data, c(0.6, 0.2) )

## it will convert into three frame
train <- parts[[1]]
test <- parts[[2]]
valid <-parts[[3]]

head(train)

head(test)

head(valid)

View(as.data.frame(train))
