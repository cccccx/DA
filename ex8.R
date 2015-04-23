library("NLP")
library("openNLP")
library("openNLPmodels.en")
library("tm")
library("tau")
library("koRpus")
library("SnowballC", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
library("topicmodels", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
`r2` <- read.csv("~/Desktop/dm/r2.csv")
data<-r2

#Task 1:Explore the data and undertake any cleaning/pre-processing that you deem necessary for the data to be analysed.

#Delete not-used data
used.data<-data[-which(data$purpose == "not-used"),]



#Transformations 
data.text<-VectorSource(used.data[,123])
text<-Corpus(data.text)
text <- tm_map(text,content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),mc.cores=1)
#Strip whitepace
text <- tm_map(text, stripWhitespace)
#Remove numbers
text <- tm_map(text, removeNumbers)
#Remove the puntuation
text <- tm_map(text, removePunctuation)
#Change the words to lower cases
text <- tm_map(text, tolower)
#Remove stopwords
text <- tm_map(text, removeWords, stopwords("english"))
#Stem words in a text document using Porter‘s stemming algorithm
text <- tm_map(text, stemDocument)
text <- tm_map(text, PlainTextDocument)

#Task 2:Obtain feature representations of the documents/news articles as discussed in the text mining lectures.

#Get the Document Term Matrix
dtm <- DocumentTermMatrix(text)

rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm111.new   <- dtm[rowTotals> 0, ]  #remove all docs without words
dtm.del<-dtm[rowTotals==0,]

dtmdel<-as.data.frame(inspect(dtm.del))
#Remove low frequency terms
dtm2 <- removeSparseTerms(dtm111.new,sparse=0.92)
data.dtm2<-as.data.frame(inspect(dtm2))
#Keep all the terms from DTM, and make DTM binary weight
for(h in 1:9849){
  for(i in 1:64){
    if(data.dtm2[h,i]!=0)
      data.dtm2[h,i]<-1
  }
}
write.csv(data.dtm2, "DTM_Feature_fin.csv", row.names = F)
#LDA
data.dtm1<-as.data.frame(inspect(dtm))
rowTotals <- apply(data.dtm1 , 1, sum) #Find the sum of words in each Document
dtm.new   <- data.dtm1[rowTotals> 0, ]  #remove all docs without words
lda <- LDA(dtm.new, control = list(alpha = 0.1), k = 10, method = "VEM")
terms(lda,10)
term<- terms(lda,10)
lda.terms <- c(term[,1], term[,2],term[,3], term[,4], term[,5], term[,6], term[,7], term[,8], term[,9], term[,10])
unique.terms <- unique(lda.terms)
topics <- topics(lda)
length.topics <- length(topics(lda))
col.name <- unique.terms
lda.features <- as.data.frame(matrix(NA, length.topics, length(col.name)))
colnames(lda.features) <- col.name
#Get LDA features matrix
for(i in 1:length(topics)){
  txt<-match(term[,topics[i]], unique.terms)
  lda.features[i,]<-0
  lda.features[i,txt]<-1
}


write.csv(lda.features, "LDA_Feature_fin.csv", row.names = F)


#Combine DTM features with LDA features

fin.data.col.name<-c(colnames(lda.features),colnames(data.dtm2))
#Remove the the same features 
uniqcol.name<-unique(fin.data.col.name)

fin.features.data<-as.data.frame(matrix(0,9849,length(uniqcol.name)))

colnames(fin.features.data)<-uniqcol.name
fincolname<-colnames(fin.features.data)

#Get the final features data
for( i in 1:9849){
  matchDTM <- match(fincolname, colnames(data.dtm2)[which(data.dtm2[i,] == 1)], nomatch = 0) != 0
  matchLDA<- match(fincolname, colnames(lda.features)[which(lda.features[i,] == 1)], nomatch = 0) != 0 
  fin.features.data[i,matchDTM]<-1
  fin.features.data[i,matchLDA]<-1
}

write.csv(fin.features.data, "finfeatures.csv", row.names = F)



#Task3: Build classifiers, using R  libraries to predict the TOPICS tags for documents.

#Get the data after NLP
used.data2<-used.data
dataframe<-data.frame(text=unlist(sapply(text, `[`, "content")), 
                      stringsAsFactors=F)
used.data2[,124]<-NA
used.data2[,124]<-dataframe
row.del<-c(which(used.data2[,124] == ""),which(is.na(used.data2[,124])))
data.final<-used.data2[-row.del,]
#Get ten topics
ten.top<-c("topic.earn", "topic.acq", 
           "topic.money.fx", "topic.grain", 
           "topic.crude", "topic.trade", 
           "topic.interest", "topic.ship", 
           "topic.wheat", "topic.corn")

#Get these ten topic rows from the data after NLP
col.data<-c()
for(i in 1:10){
  col.data<-c(col.data, which(colnames(data.final) == ten.top[i]))
}
#Link the data with seclected features
data.for.cla<-cbind(data.final[,c(3,col.data)],fin.features.data)

#Since some docs belong to more than one topic, we need to extend the data set
data.for.cla[,"class"]<-0
cla<-ncol(data.for.cla)
for(i in 1:9849){
  if(sum(data.for.cla[i,2:11]) > 1){
    for(j in 2:11){
      if(data.for.cla[i,j] == 1){
        newr<-data.for.cla[i,]
        newr[,2:11]<-0
        newr[cla]<-colnames(data.for.cla)[j]
        data.for.cla<-rbind(data.for.cla,newr)
      }  
    }
  } 
  if(sum(data.for.cla[i,2:11]) == 1){
    data.for.cla[i,cla]<-colnames(data.for.cla)[which(data.for.cla[i,1:11] == 1)]
  }
}
#Clean data
data.for.cla1<-data.for.cla[-which(data.for.cla[,cla] == 0),]
drops<-c("topic.earn", "topic.acq", 
        "topic.money.fx", "topic.grain", 
        "topic.crude", "topic.trade", 
        "topic.interest", "topic.ship", 
        "topic.wheat", "topic.corn")
data.df<-data.for.cla1[,!(names(data.for.cla1) %in% drops)]

for(i in 1:length(data.df)){
  data.df[,i]<-as.factor(data.df[,i])
}

train<-data.df[which(data.df$purpose == "train"),]
test<-data.df[which(data.df$purpose == "test"),]


library("e1071")
library("randomForest")
########FOR TRAIN DATA#########
########Naivebayes#######

naivebayes.model<-naiveBayes(class ~ ., data = train)
predict.nb<-predict(naivebayes.model, newdata = train)
nb.table<-table(predict.nb, train[,length(train)])
nb.data<-as.data.frame(nb.table)

coln<-c("TP", "FN", "FP", "Recall", "Precision", "Accuracy","F-measure")
NB.parameters = matrix(0, 10, 7, dimnames=list(ten.top,coln))
for(i in 1:10){
   NB.parameters[i,1] = nb.table[i,i];
   NB.parameters[i,2] = sum(nb.table[-i,i]);
   NB.parameters[i,3] = sum(nb.table[i,-i]);
   NB.parameters[i,4] = NB.parameters[i,1]/(NB.parameters[i,1]+NB.parameters[i,2]);
   NB.parameters[i,5] = NB.parameters[i,1]/(NB.parameters[i,1]+NB.parameters[i,3]);
   NB.parameters[i,6] = NB.parameters[i,1]/sum(nb.table);
   NB.parameters[i,7]=(2*NB.parameters[i,5]*NB.parameters[i,4])/(NB.parameters[i,5]+NB.parameters[i,4])
  }
NB.train.parameters<-NB.parameters
nb.overall.acc<-sum(NB.parameters[,6])
nb.marco.recall<-sum(NB.parameters[,4])/10
nb.marco.precision<-sum(NB.parameters[,5])/10
nb.micro.recall<-sum(NB.parameters[,1])/(sum(NB.parameters[,1])+sum(NB.parameters[,2]))
nb.micro.precision<-sum(NB.parameters[,1])/(sum(NB.parameters[,1])+sum(NB.parameters[,3]))


###Randomforest###

rf.model <- randomForest(class~., data = train)
rf.predict <- predict(rf.model, newdata = train)
RF.table <- table(observed=train[,length(train)], predicted = rf.predict)
RF.parameters = matrix(0, 10, 7, dimnames=list(ten.top,coln))
for(i in 1:10){
  RF.parameters[i,1] = RF.table[i,i];
  RF.parameters[i,2] = sum(RF.table[-i,i]);
  RF.parameters[i,3] = sum(RF.table[i,-i]);
  RF.parameters[i,4] = RF.parameters[i,1]/(RF.parameters[i,1]+RF.parameters[i,2]);
  RF.parameters[i,5] = RF.parameters[i,1]/(RF.parameters[i,1]+RF.parameters[i,3]);
  RF.parameters[i,6] = RF.parameters[i,1]/sum(RF.table);
  RF.parameters[i,7]=(2*RF.parameters[i,5]*RF.parameters[i,4])/(RF.parameters[i,5]+RF.parameters[i,4])
}
RF.train.parameters<-RF.parameters
RF.overall.acc<-sum(RF.parameters[,6])
RF.marco.recall<-sum(RF.parameters[,4])/10
RF.marco.precision<-sum(RF.parameters[,5])/10
RF.micro.recall<-sum(RF.parameters[,1])/(sum(RF.parameters[,1])+sum(RF.parameters[,2]))
RF.micro.precision<-sum(RF.parameters[,1])/(sum(RF.parameters[,1])+sum(RF.parameters[,3]))



####SVM#####

svm.model <- svm(class~., data = train,kernel="linear" )
svm.predict <- predict(svm.model, newdata = train)
SVM.table <- table(observed=train[,length(train)], predicted = svm.predict)
SVM.parameters = matrix(0, 10, 7, dimnames=list(ten.top,coln))
for(i in 1:10){
  SVM.parameters[i,1] = SVM.table[i,i];
  SVM.parameters[i,2] = sum(SVM.table[-i,i]);
  SVM.parameters[i,3] = sum(SVM.table[i,-i]);
  SVM.parameters[i,4] = SVM.parameters[i,1]/(SVM.parameters[i,1]+SVM.parameters[i,2]);
  SVM.parameters[i,5] = SVM.parameters[i,1]/(SVM.parameters[i,1]+SVM.parameters[i,3]);
  SVM.parameters[i,6] = SVM.parameters[i,1]/sum(SVM.table);
  SVM.parameters[i,7]=(2*SVM.parameters[i,5]*SVM.parameters[i,4])/(SVM.parameters[i,5]+SVM.parameters[i,4])
}
SVM.train.parameters<-SVM.parameters
SVM.parameters[2,4]<-0
SVM.parameters[10,4]<-0
SVM.parameters
SVM.overall.acc<-sum(SVM.parameters[,6])
SVM.marco.recall<-sum(SVM.parameters[,4])/10
SVM.marco.precision<-sum(SVM.parameters[,5])/10
SVM.micro.recall<-sum(SVM.parameters[,1])/(sum(SVM.parameters[,1])+sum(SVM.parameters[,2]))
SVM.micro.precision<-sum(SVM.parameters[,1])/(sum(SVM.parameters[,1])+sum(SVM.parameters[,3]))



####Use these classifiters for test data##


##Naivebayes
naivebayes.model<-naiveBayes(class ~ ., data = train)
predict.nb<-predict(naivebayes.model, newdata = test)
nb.test.table<-table(predict.nb, test[,length(test)])


coln<-c("TP", "FN", "FP", "Recall", "Precision", "Accuracy")
NB.parameters1 = matrix(0, 10, 6, dimnames=list(ten.top,coln))
for(i in 1:10){
  NB.parameters1[i,1] = nb.test.table[i,i];
  NB.parameters1[i,2] = sum(nb.test.table[-i,i]);
  NB.parameters1[i,3] = sum(nb.test.table[i,-i]);
  NB.parameters1[i,4] = NB.parameters1[i,1]/(NB.parameters1[i,1]+NB.parameters1[i,2]);
  NB.parameters1[i,5] = NB.parameters1[i,1]/(NB.parameters1[i,1]+NB.parameters1[i,3]);
  NB.parameters1[i,6] = NB.parameters1[i,1]/sum(nb.test.table);
  
}

nb.accuracy<-sum(NB.parameters1[,6])


#####Randomforest###
rf.model1 <- randomForest(class~., data = train)
rf.predict1 <- predict(rf.model1, newdata = test)
RF.table1 <- table(observed=test[,length(test)], predicted = rf.predict1)


coln<-c("TP", "FN", "FP", "Recall", "Precision", "Accuracy")
RF.parameters1 = matrix(0, 10, 6, dimnames=list(ten.top,coln))
for(i in 1:10){
  RF.parameters1[i,1] = RF.table1[i,i];
  RF.parameters1[i,2] = sum(RF.table1[-i,i]);
  RF.parameters1[i,3] = sum(RF.table1[i,-i]);
  RF.parameters1[i,4] = RF.parameters1[i,1]/(RF.parameters1[i,1]+RF.parameters1[i,2]);
  RF.parameters1[i,5] = RF.parameters1[i,1]/(RF.parameters1[i,1]+RF.parameters1[i,3]);
  RF.parameters1[i,6] = RF.parameters1[i,1]/sum(RF.table1);
  
}

RF.parameters1[2,4]<-0
RF.parameters1[10,4]<-0
RF.accuracy<-sum(RF.parameters1[,6])
RF.precision<-sum(RF.parameters1[,5])/10
RF.recall<-sum(RF.parameters1[,4])/10


#####SVM###
svm.model1 <- svm(class~., data = train,kernel="linear" )
svm.predict1 <- predict(svm.model1, newdata = test)
SVM.table1 <- table(observed=test[,length(test)], predicted = svm.predict1)

SVM.parameters1 = matrix(0, 10, 6, dimnames=list(ten.top,coln))
for(i in 1:10){
  SVM.parameters1[i,1] = SVM.table1[i,i];
  SVM.parameters1[i,2] = sum(SVM.table1[-i,i]);
  SVM.parameters1[i,3] = sum(SVM.table1[i,-i]);
  SVM.parameters1[i,4] = SVM.parameters1[i,1]/(SVM.parameters1[i,1]+SVM.parameters1[i,2]);
  SVM.parameters1[i,5] = SVM.parameters1[i,1]/(SVM.parameters1[i,1]+SVM.parameters1[i,3]);
  SVM.parameters1[i,6] = SVM.parameters1[i,1]/sum(SVM.table1);
  
}

SVM.accuracy<-sum(SVM.parameters1[,6])


###Task4:cluster####

###kmeans###

#Remove the purpose and class columns
drops123 <- c("purpose","class")
data.cluster<-data.df[,!(names(data.df) %in% drops123)]
library("cluster"）
# Determine number of clusters
wss <- (nrow(data.cluster)-1)*sum(apply(data.cluster,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data.cluster, centers=i)$withinss)
 plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")    
fit <- kmeans(data.cluster, 10)

# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph
library(cluster) 
clusplot(data.cluster, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(data.cluster, fit$cluster)
plot(fit)


#### Model Based Clustering###
library(mclust)
fit1 <- Mclust(data.cluster)
plot(fit1) # plot results 
summary(fit1) # display the best model


#### Ward Hierarchical Clustering####
d <- dist(data.cluster, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=10) # cut tree into 10 clusters
# draw dendogram with red borders around the 10 clusters 
rect.hclust(fit, k=10, border="red")
