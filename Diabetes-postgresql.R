#Libraries
library(RPostgreSQL)
library(DBI)
library(ggplot2)
library(plotly)
library(dbplyr)
library(caTools)
library(rpart)

#Connection Params.
pw <- "123456789"
drv <- dbDriver("PostgreSQL")
#PostgreSQL Connection
conn <- dbConnect(drv, dbname = "Diabetes",  host = "localhost", port = 5432,
                  password = pw, user = "postgres")

#Listing the exesting Tables in POSTGRESQL DB
print(dbListTables(conn))

#getting data from existing table
diabetes <- dbGetQuery(conn, "SELECT * FROM diabetes")
dbDisconnect(conn)
#Explore data types of data
str(diabetes)

#after finding that data types of "bmi" and "diabetespedigreefunction" are chars which supposed to be Numeric
#and outcome as factor,then we have to convert it to be able to work with it  
diabetes$bmi<-as.numeric(diabetes$bmi)
diabetes$diabetespedigreefunction<-as.numeric(diabetes$diabetespedigreefunction)
diabetes$outcome<-as.factor(diabetes$outcome)
str(diabetes)

#storing data on disk, now we can save CSV file on disk after getting it from DB to work on it 
write.csv(diabetes,'diabetes_data.csv',row.names = FALSE)

#work with Dataset in a normal way

data<-read.csv('diabetes_data.csv')
head(data)
str(data)
summary(data)

#check null values
any(is.null(data))
##we have to remove 0s as it may affect visualizing or modeling data
for (i in 2:6) {
  data <- data[-which(data[, i] == 0), ]
}
data$outcome<-as.factor(data$outcome)
str(data)
#### VISUALIZE DATA

age_out<-ggplot(data,aes(age))+geom_histogram(aes(fill=outcome),color='black')
print(age_out)
#ggplotly(age_out)

#as we can see that not most people may have diabetes, and it's common in aged people

glucose_out<-ggplot(data,aes(glucose))+geom_histogram(aes(fill=outcome),color='black')
print(glucose_out)
#ggplotly(glucose_out)

#As we can also see that most of people who have high levels of glucose may also suffer from diabetes 

bmi_out<-ggplot(data,aes(bmi))+geom_histogram(aes(fill=outcome),color='black')
print(bmi_out)
#ggplotly(bmi_out)

#people who have high BMI may also be vulnerable to diabetes

#corr data
data$outcome<-as.numeric(data$outcome)
numeric.cols<-sapply(data, is.numeric)
corr.data<-cor(data[,numeric.cols])
corrplot(corr.data,method = 'number')
#split data to train and test to train models
data$outcome<-as.factor(data$outcome)
set.seed(80)
sample<-sample.split(data$outcome,SplitRatio = .70)
train<-subset(data,sample=TRUE)
test<-subset(data,sample=FALSE)

######################MODELS########################
#LOGISTIC REGRESSION
log.model<-glm(outcome~.,family = binomial(link = 'logit'),data = train)
summary(log.model)

results <- predict(log.model,newdata=test,type='response')
results <- ifelse(results > 0.5,1,0)
misClasificError <- mean(results != test$outcome)
print(paste('Accuracy',1-misClasificError))

#so bad results for using Logistic model so i'm going to use Decision tree using the most
#effective cols. on our prediction(pregnancies,glucose,diabetespedigreefunction,bmi)

dt.model<-rpart(outcome ~pregnancies+glucose+diabetespedigreefunction+bmi,method = 'class',
                data = train)
#testing model and estimate it's accuracy
treePred <- predict(dt.model, test, type = 'class')
table(treePred, test$outcome)
print(paste('ACCURACY Score: ',mean(treePred==test$outcome)*100))

#Drawing tree
plot(dt.model, uniform=TRUE, 
     main="Classification DECISION Tree for Diabetes")
text(dt.model, use.n=TRUE, all=TRUE)
