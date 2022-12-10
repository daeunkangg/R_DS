#0. ��ó�� ����
Sys.setlocale("LC_ALL", "ko_KR.UTF-8")
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(ggplot2)
library(lattice)

#������ �ҷ�����
df = read.csv('wisconsin.csv')

#������ �⺻ ���� �ľ�
View(df)
str(df)
dim(df)
summary(df)

#�̻�ġ Ȯ��- ���������� �ʾ���
boxplot(df[,c(3:12)])$stats
boxplot(df[,c(13:22)])$stats
boxplot(df[,c(23:32)])$stats
#df$Area_mean=ifelse(df$Area_mean<143.5|df$Area_mean>1326, NA, df$Area_mean)
#df$Area_se=ifelse(df$Area_se<6.802|df$Area_se>83.5, NA, df$Area_se)
#df$Area_extreme=ifelse(df$Area_extreme<185.2|df$Area_extreme>1938, NA, df$Area_extreme)


#����ġ Ȯ�� �� ����
summary(is.na(df))
df=na.omit(df)
table(is.na(df))

#������ ���� ��ȯ
df$Diagnosis=as.factor(df$Diagnosis)
summary(df)

#1 ������ ������� �м�

#Diagnosis ~ Radius_mean
df %>% 
  ggplot(aes(Radius_mean, Diagnosis)) + 
  geom_jitter(aes(col=Diagnosis), height=0.1, width=0.0) +
  geom_boxplot(alpha=0.2)

#Diagnosis ~ Radius_se
df %>% 
  ggplot(aes(Radius_se, Diagnosis)) + 
  geom_jitter(aes(col=Diagnosis), height=0.1, width=0.0) +
  geom_boxplot(alpha=0.2)

#Diagnosis ~ Radius_extreme
df %>% 
  ggplot(aes(Radius_extreme, Diagnosis)) + 
  geom_jitter(aes(col=Diagnosis), height=0.1, width=0.0) +
  geom_boxplot(alpha=0.2)

#Diagnosis ~ Area_mean
df %>% 
  ggplot(aes(Area_mean, Diagnosis)) + 
  geom_jitter(aes(col=Diagnosis), height=0.1, width=0.0) +
  geom_boxplot(alpha=0.2)

#Diagnosis ~ Area_se
df %>% 
  ggplot(aes(Area_se, Diagnosis)) + 
  geom_jitter(aes(col=Diagnosis), height=0.1, width=0.0) +
  geom_boxplot(alpha=0.2)

#Diagnosis ~ Area_extreme
df %>% 
  ggplot(aes(Area_extreme, Diagnosis)) + 
  geom_jitter(aes(col=Diagnosis), height=0.1, width=0.0) +
  geom_boxplot(alpha=0.2)


#2 rpart, randomForest

#���ο� ������������ �����
new_df = df %>%
  select(-c(No,Radius_se,Texture_se,Perimeter_se,Area_se,Smoothness_se,
         Compactness_se,Concavity_se,Nconcave_se,Symmetry_se,Fractaldim_se))

#����Ʈ��
r = rpart(Diagnosis~., data=new_df)
print(r)
summary(r)
table(new_df$Diagnosis)

#����Ʈ�� �ð�ȭ
rpart.plot(r, extra=2,digits=3)

#�������չ��� cp Ȯ��
printcp(r)

#�Ʒ����տ� ���� ����
r_p = predict(r, new_df, type='class')
table(r_p)
table(r_p, new_df$Diagnosis)

#����������Ʈ
f=randomForest(Diagnosis~., data=new_df)
print(f)
plot(f)
varUsed(f)
varImpPlot(f)
treesize(f)
summary(f)


#3. ���� ���� ���ɺ� : rpart, randomForest, knn, svmRadial, glmn
control=trainControl(method='cv', number=5)
r=train(Diagnosis~., data=new_df, method='rpart', metric='Accuracy', trControl=control)
f=train(Diagnosis~., data=new_df, method='rf', metric='Accuracy', trControl=control)
k=train(Diagnosis~., data=new_df, method='knn', metric='Accuracy', trControl=control)
s=train(Diagnosis~., data=new_df, method='svmRadial', metric='Accuracy', trControl=control)
g=train(Diagnosis~., data=new_df, method='glm', metric='Accuracy', trControl=control)

#��� ��� ����
resamp=resamples(list(DecisionTree=r, RandomForest=f, SVM=s, KNN=k, GLM=g))
summary(resamp)
sort(resamp, decreasing=TRUE)
dotplot(resamp)