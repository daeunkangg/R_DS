#0. 전처리 수행
Sys.setlocale("LC_ALL", "ko_KR.UTF-8")
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(ggplot2)
library(lattice)

#데이터 불러오기
df = read.csv('wisconsin.csv')

#데이터 기본 구조 파악
View(df)
str(df)
dim(df)
summary(df)

#이상치 확인- 제거하지는 않았음
boxplot(df[,c(3:12)])$stats
boxplot(df[,c(13:22)])$stats
boxplot(df[,c(23:32)])$stats
#df$Area_mean=ifelse(df$Area_mean<143.5|df$Area_mean>1326, NA, df$Area_mean)
#df$Area_se=ifelse(df$Area_se<6.802|df$Area_se>83.5, NA, df$Area_se)
#df$Area_extreme=ifelse(df$Area_extreme<185.2|df$Area_extreme>1938, NA, df$Area_extreme)


#결측치 확인 및 제거
summary(is.na(df))
df=na.omit(df)
table(is.na(df))

#범주형 변수 변환
df$Diagnosis=as.factor(df$Diagnosis)
summary(df)

#1 데이터 상관관계 분석

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

#새로운 데이터프레임 만들기
new_df = df %>%
  select(-c(No,Radius_se,Texture_se,Perimeter_se,Area_se,Smoothness_se,
         Compactness_se,Concavity_se,Nconcave_se,Symmetry_se,Fractaldim_se))

#결정트리
r = rpart(Diagnosis~., data=new_df)
print(r)
summary(r)
table(new_df$Diagnosis)

#결정트리 시각화
rpart.plot(r, extra=2,digits=3)

#과잉적합방지 cp 확인
printcp(r)

#훈련집합에 대한 예측
r_p = predict(r, new_df, type='class')
table(r_p)
table(r_p, new_df$Diagnosis)

#랜덤포리스트
f=randomForest(Diagnosis~., data=new_df)
print(f)
plot(f)
varUsed(f)
varImpPlot(f)
treesize(f)
summary(f)


#3. 여러 모델의 성능비교 : rpart, randomForest, knn, svmRadial, glmn
control=trainControl(method='cv', number=5)
r=train(Diagnosis~., data=new_df, method='rpart', metric='Accuracy', trControl=control)
f=train(Diagnosis~., data=new_df, method='rf', metric='Accuracy', trControl=control)
k=train(Diagnosis~., data=new_df, method='knn', metric='Accuracy', trControl=control)
s=train(Diagnosis~., data=new_df, method='svmRadial', metric='Accuracy', trControl=control)
g=train(Diagnosis~., data=new_df, method='glm', metric='Accuracy', trControl=control)

#결과 모아 보기
resamp=resamples(list(DecisionTree=r, RandomForest=f, SVM=s, KNN=k, GLM=g))
summary(resamp)
sort(resamp, decreasing=TRUE)
dotplot(resamp)
