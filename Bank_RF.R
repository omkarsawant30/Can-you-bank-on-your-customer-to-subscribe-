#First we need to set the directory of the file's location in your computer and then read it so that R can download the data
setwd("E:/DATA ANALYTICS JOURNEY/R Edvancer/PROJECT 5 BANKING") 

bf_train=read.csv("bank-full_train.csv",sep=",",header=T)
bf_test=read.csv("bank-full_test.csv",sep=",",header=T)
library(dplyr)
glimpse(bf_train)

##You will need same set of vars on both train and test,its easier to manage that if you combine train and test
##in the beginning and then separate them once you are done with data preparation
##We'll fill test's response column with NAs.
bf_test$y= NA
bf_train$data = 'train'
bf_test$data = 'test'

all= rbind(bf_train,bf_test)

apply(all,2,function(x) length(unique(x)))

#We remove ID and poutcome as they are not useful in predictions
all=all %>% 
  select(-ID,-poutcome)

#Next we'll create dummy variables for remaining categorical variables
CreateDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
} 

char_logical=sapply(all,is.character)
cat_cols=names(all)[char_logical]
cat_cols

cat_cols=cat_cols[!(cat_cols %in% c('data','y'))]
cat_cols

# we are using frequency cutoff as 50, there is no magic number here,
# lower cutoffs will simply result in more number of dummy variables
for(col in cat_cols){
  all=CreateDummies(all,col,50)
}

#Remove NA values from the data except the response column 
for(col in names(all))
{
  if(sum(is.na(all[,col]))>0 & !(col %in% c("data","y")))
  {
    all[is.na(all[,col]),col]=mean(all[all$data=='train',col],na.rm=T)
  }
}

head(all)

bf_train = all %>% filter(data == 'train') %>% select(-data) 
bf_test= all %>% filter(data == 'test') %>% select(-y, -data) 

any(is.na(bf_train))
any(is.na(bf_test))

library(randomForest)
fit = randomForest(as.factor(y )~ ., data = bf_train) 

### Make predictions on test and submit 
test.predictions = predict(fit, newdata = bf_test)
write.csv(test.predictions,file = "bankingsubs.csv", row.names = F)

#--------------------------------------------------------QUIZ-----------------------------------------------------------------
#(1)- Find mean of the variable age. Round off to 2 decimal places.
#Ans - mean(bank_train$age)
# 40.91
      
#(2) - Total number of outliers present in the variable balance.Use ‘Q1-1.5*IQR’ to calculate lower limit and ‘Q3 + 1.5×IQR’ 
# to calculate upper limit. calculate the count of values in variable balance which are beyond these limits.
#Ans - NA
      
#(3) Find the variance of variable balance.
#Ans -var(bank_train$balance)
# 9273256
      
#(4) which function is used to remove multicollinearity among variables?
#Ans -vif
      
#(5) Model with 'lower AIC' value is a better model or the model with 'higher AIC' value?
#Ans -lower AIC
      
#(6) Should the variable ID be included in building the model?
#Ans -No
      
#(7) Does validation help in generalising the model?
#Ans -Yes
      
#(8) Whether the data given ( train data ) is a balanced or extremely imbalanced data
#( ratio of response class counts even more extreme than 5%:95%)?
#Ans -Balanced
      
#(9) How is box plot upper whisker is calculated ?
#Ans - Q3 + 1.5×IQR
      
#(10) R2 or adjusted R2, which metric to be used to check goodness of the model?
#Ans -Adjusted R2

      



      

