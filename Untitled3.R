library(Boruta)
library(class)
library(e1071)
library(randomForest)
library(dplyr)
library(caret)
library(adabag)
library(gmodels)

daf <- read.csv("/Users/xulongxuan/Downloads/proj2/train1.csv")
tr <- daf[,-1]

a = svm(class~.,data = tr)


daf1 <- read.csv("/Users/xulongxuan/Downloads/proj2/test2.csv")
te <- daf1[,c(-1,-456)]

daf2 <- read.csv("/Users/xulongxuan/Downloads/proj2/dev-best200.csv")
daf2 <- daf2[,-1]
daf21 <- summarise_if(group_by(daf2,user.id,class),is.numeric,sum)
write.csv(daf21,"/Users/xulongxuan/Downloads/proj2/daf21.csv",row.names = FALSE)
daf22 <- read.csv("/Users/xulongxuan/Downloads/proj2/daf21.csv")
te <- daf22[,-c(1,2)]

a = svm(class~.,data = tr)
pd <- predict(a,te)

b<- randomForest(class~.,data = tr)
pd <- predict(b,te)

write.csv(pd,"/Users/xulongxuan/Downloads/proj2/pd.csv",row.names = FALSE)




comp <- read.csv("/Users/xulongxuan/Downloads/proj2/pd.csv")
comp1 <- read.csv("/Users/xulongxuan/Downloads/proj2/dev-best200.csv")
comp1 <- comp1[,c(1,2)]

nrow(comp)
nrow(comp1)

for (i in 1:34028) {
  for(k in 1:794){
    if(comp1[i,2] == comp[k,1]){
      comp1[i,3] <- comp[k,2]
    }
  }
}
comp1
write.csv(comp1,"/Users/xulongxuan/Downloads/proj2/pdt.csv",row.names = FALSE)

pred.label <- comp1[,3]
pred.label
dev.label <- daf2[,456]

CrossTable(pred.label,dev.label,prop.r = F,prop.t = F,prop.chisq = F)
