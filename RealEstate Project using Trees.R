setwd("D:\\Course\\R\\RealEstateProject")

ld_train=read.csv("housing_train.csv",stringsAsFactors = F)
ld_test= read.csv("housing_test.csv",stringsAsFactors = F)

ld_test$Price=NA
ld_train$data='train'
ld_test$data='test'


ld_all=rbind(ld_train,ld_test)
library(dplyr)
library(lubridate)
glimpse(ld_all)

CreateDummies=function(data,var,freq_cutoff=0){
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
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

# Data Preparation
{
  ld_all$Address=NULL
  ld_all$CouncilArea=NULL
  ld_all$SellerG=NULL

  ld_all=ld_all %>% 
    mutate(YearBuilt=parse_date_time(YearBuilt, "%Y"))
  
  ld_all = CreateDummies(ld_all,"Suburb",100) 
  ld_all = CreateDummies(ld_all,"Type",100)
  
  ld_all = CreateDummies(ld_all,"Method",100)
  
  ld_all = CreateDummies(ld_all,"Postcode",100)
  library(visdat)
  vis_dat(ld_all)
  
  for(col in names(ld_all)){
    
    if(sum(is.na(ld_all[,col]))>0 & !(col %in% c("data","Price"))){
      
      ld_all[is.na(ld_all[,col]),col]=mean(ld_all[,col],na.rm=T)
    }
    
  }
}



vis_dat(ld_all)
## separate train and test
{
ld_train=ld_all %>% filter(data=='train') %>% select(-data)
ld_test=ld_all %>% filter(data=='test') %>% select(-data,-Price)
}

#### 70:30 Train:Test
{
set.seed(2)
s=sample(1:nrow(ld_train),0.7*nrow(ld_train))
ld_train1=ld_train[s,] # 70% 
ld_train2=ld_train[-s,]# 30% of my data
}


# Model Decision tree

library(randomForest)

ld.tree=randomForest(Price~.,data=ld_train1)

val.IR=predict(ld.tree,newdata = ld_train2)

rmse_val=((val.IR)-(ld_train2$Price))^2 %>% mean() %>% sqrt()

Score =212467/rmse_val

library(randomForestSRC)


  o <- tune(Price~., ld_train, doBest = TRUE)
  
  o$optimal
  o

  best_params=data.frame(mtry=35,
                         ntree=500,
                         maxnodes=784,
                         nodesize=1)
  
  ld.rf.final=randomForest(Price~.,
                           mtry=best_params$mtry,
                           ntree=best_params$ntree,
                           maxnodes=best_params$maxnodes,
                           nodesize=best_params$nodesize,
                           data=ld_train)
  
  
  val.IR=predict(ld.rf.final,newdata = ld_train2)
  
  rmse_val=((val.IR)-(ld_train2$Price))^2 %>% mean() %>% sqrt()
  rmse_val
  Score =212467/rmse_val 
  
  test.pred=predict(ld.rf.final,newdata = ld_test) 
  
  
  write.csv(test.pred,"Atahar_Budihal_P1_part2.csv",row.names = F)
  
# ld.tree.final=tree(Interest.Rate~.-ID,data=ld_train)
# test.pred=predict(ld.tree.final,newdata=ld_test)
# write.csv(test.pred,"mysubmission.csv",row.names = F)





