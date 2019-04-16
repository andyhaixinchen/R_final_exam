my_binary_vec <- as.logical(1)
my_factors <- factor(c("yes", "no", "yes", "no"))


rawdates <- c("05/27/84", "07/07/05")
formattedDates <- as.Date(rawdates, format = "%m/%d/%y")
numericdates <- c(30829, 38540)
ExcelDates <- as.Date(numericdates, origin = "1899-12-30")


pattern_1 <- seq(from=1, to=20, by=1)
pattern_2 <- rep(c(1,0,1), each=10)
my_mat <- matrix(c(1,2,3,4,5,6,7,8,9,10),nrow=5,ncol=2, byrow=T)


my_df <- as.data.frame(matrix(rep(1:20,2), ncol=5, nrow=8))
ncol(my_df)
nrow(my_df)colnames(my_df)
colnames(my_df) <- c("column_1", "column_2")


# Subsetting
my_mat[1:2,1:2]
subset(my_df, column_1>3)
my_df[which(my_df$column_1 > 3),]
my_df[-which(is.na(my_df$column_1)),]


# Data massaging
replacement_for_gc <- gsub(4, 5, german_credit$history)
replacement_for_gc <- as.numeric(replacement_for_gc)
unique(facebook$file)
facebook_cleaner <- na.omit(facebook)

char_to_num <- function(x){
  x <- as.data.frame(x)#need to make sure that this is a data frame
  n_col_x <- ncol(x) #getting the count of columns for this data frame
  for (i in 1:n_col_x){
    if(is.character(x[,i])){
      my_options <-c()
      my_options <- unique(x[,i])
      for(z in 1:length(my_options)){
        x[,i]<- gsub(as.character(my_options[z]),paste(z), x[,i])
        
      }#closing z loop
      x[,i]<-as.numeric(x[,i])
    }#closing if statement
  }#closing the for loop
  return(x)
}#clsoing char_to_num function


for(i in 1:nrow(facebook)) {
  if( is.na( facebook$like[i])) { facebook$like[i] <- mean(facebook$like[-which(is.na(facebook$like))])}
}



# Stalling SQL package
install.packages("sqldf")
library(sqldf)
sqldf("
      SELECT s.rating, min(s.age)
      FROM sailors s
      WHERE s.age >= 18
      GROUP BY s.rating
      HAVING COUNT(*)>1
     ")
sqldf("
      Select s.sname as sailorname,
      case when s.age>35 then 1
      when s.age<35 then 0
      else 2 
      end as binary
      FROM sailors s
      ")



# For loop
my_df <- as.data.frame(matrix(nrow=4, ncol=3))
for (a in 1:3){#this loop goes over the columns
  for(b in 1:4){#this loop goes over the rows
    my_df[b,a]<- a*b
  }
}
print(my_df)


# While loop
z <- 1
while(z < 20){
  z <- z+1 
  print(z)
}


# If statement
pk <- c(6,5,4,3,2,1)

if(pk[2]==2){pk[1]<-2}else{pk[2]<-2}
if(sum(pk)>10){pk <- NULL}


# Bigquery
install.packages("bigrquery")
library(bigrquery)
proj_id <- "hult-r-project" # replace this with your project ID 
sql <- "SELECT * FROM `german_credit.german_credit`"
tb <- bq_project_query(proj_id, sql)
my_google_dataset <- bq_table_download(tb)
german_google <- as.data.frame(my_google_dataset)


# Optimization model
my_func <- function(m1, m2, m3){
  total <- m1*mix1 + m2*mix2 + m3*mix3
  return(total)
}
objective <- c(1000, 480, 1800, 1000, 990)
library(minpack.lm)
nlsLM(objective ~ my_func(m1, m2, m3))


# Modelling with dices
my_uniform <- sample(1:6, 1544, replace = T)
hist(my_uniform)
mean(my_uniform)
hist(rpois(10000, 10))

# Logistic regression
my_logit <- glm(binary~age, data=german_credit, family="binomial")
logit2prob <- function(x, coeff, intrcpt){
  logit <- intrcpt + x*coeff
  odds <- exp(logit)# a one -unit change of x will result in the odds of being the positive outcome
  prob <- odds / (1 + odds)
  return(c(odds, prob))
}
logit2prob(x=20, coeff=0.018440, intrcpt=0.200919)


# Plotly
library(plotly)
q <- plot_ly(iris, x = ~Petal.Length, y = ~Petal.Width,  type="scatter", mode = "markers" , color = ~Species)
q


# Ggplot
library(ggplot2)
e<-ggplot(mpg, aes(x=cty, y=hwy, color=trans))
e<- e+geom_jitter(height=2, width=2, alpha=0.8)
print(e)

ggplot(data=german_credit)+
  geom_point(aes(x=age, y=amount, alpha=1/20))

ggplot(data=german_credit, aes(age))+
  geom_histogram(binwidth = 5)


# Distribution of means using for loop
my_means <- c()
for (i in 1:500){
  my_means[i] <- mean(sample(1:2, 1000, replace = T))
}


# Exponential distribution 
hist(rexp(1000, rate = 1), breaks = seq(from = 0, to = 50, by = 1))
exp(-0.2*10) # p(x>10)
1-exp(-0.2*2) # p(x<2)



# Decision Tree
library(rpart)
library(rpart.plot)
titanic_tree <- rpart(Survived ~ Pclass + Sex + Age + SibSp, 
                      data = titanic_train, method="class", cp=0.015)
rpart.plot(titanic_tree, type = 1, extra = 1)
plotcp(titanic_tree) # you know the best fit by plotcp(), with smallest error value
# if the tree is too large, it overfit the data. Hence, I need to reduce the model fit. -> prune
# If the tree is too small, it is too general. Hence, I need to improve the model fit. -> grow
# cp adjudts the model. The larger value of cp is, the simpler/smaller the model is.


# Prediction
mydf_train <- as.data.frame(titanic_train)
my_logistic <- glm(Survived ~ Pclass + Sex + Age+ SibSp, data = titanic_train, family="binomial")
summary(my_logistic)
predict_logit <- predict(my_logistic, mydf_train, type="response") #predicting probability of 1
print(predict_logit)

library(ROCR)
mydf <- as.data.frame(titanic_train)
predict_tree <- predict(mytree, mydf, type="prob")#We want to predict probability of 1 for each observations
print(predict_tree)