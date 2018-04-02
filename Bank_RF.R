setwd("E:/DATA ANALYTICS JOURNEY/R Edvancer/PROJECT 5 BANKING") 

bf_train=read.csv("bank-full_train.csv",sep=",",header=T)
bf_test=read.csv("bank-full_test.csv",sep=",",header=T)
library(dplyr)
glimpse(bf_train)

bf_test$y= NA
bf_train$data = 'train'
bf_test$data = 'test'

all= rbind(bf_train,bf_test)

apply(all,2,function(x) length(unique(x)))

all=all %>% 
  select(-ID,-poutcome)

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

for(col in cat_cols){
  all=CreateDummies(all,col,50)
}


for(col in names(all)){
  
  if(sum(is.na(all[,col]))>0 & !(col %in% c("data","y"))){
    
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
write.csv(test.predictions,file = "Omkar_Sawant_P5_part2.csv", row.names = F)