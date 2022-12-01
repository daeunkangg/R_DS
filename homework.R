#1. csv file 불러오기
working = read.csv('working.csv', fileEncoding='euc-kr')

#2. 데이터 파악하기
head(working)
str(working)
dim(working)

summary(working)
table(working$지역)
table(working$업종)
table(working$사업체규모)
table(working$연령대)
table(working$성별)
table(working$학력)
table(working$직업)
table(working$월평균소득구간)

#3. 데이터 정제
#결측치 여부 확인
table(is.na(working))
table(is.na(working$지역))
table(is.na(working$업종))
table(is.na(working$사업체규모))
table(is.na(working$연차휴가부여))
table(is.na(working$연차휴가사용))
table(is.na(working$연령대))
table(is.na(working$성별))
table(is.na(working$학력))
table(is.na(working$직업))
table(is.na(working$월평균소득구간))
#확인 결과 연차휴가부여, 연차휴가사용 열에서 결측값이 각각 5,9개 존재함 확인

#결측치 제거
working_narm = working[!is.na(working$연차휴가부여), ]
dim(working_narm)
working_new = working_narm[!is.na(working_narm$연차휴가사용), ]
dim(working_new)
#총 10개의 행이 제거 되었음; 이를 미루어 보아 결측치가 중복되는 행에 있었음을 짐작할 수 있음.


#4. 데이터 분석
#4-1. 범주 변수별 연차휴가부여, 연차휴가사용 평균 비교

#지역
working_new %>%
  select(지역,연차휴가부여,연차휴가사용) %>%
  group_by(지역) %>%
  summarise(given_vac = mean(연차휴가부여), used_vac = mean(연차휴가사용), vac_ratio = used_vac/given_vac*100)
#17개 모든 지역에서 부여된 연차에 비해 사용된 연차가 적음. 대체로 대도시에서 연차 부여와 사용이 많음.

#업종
working_new %>%
  select(업종,연차휴가부여,연차휴가사용) %>%
  group_by(업종) %>%
  summarise(given_vac = mean(연차휴가부여), used_vac = mean(연차휴가사용), vac_ratio = used_vac/given_vac*100)
#전기, 가스, 증기 및 수도사업 종사자들의 연차 부여와 사용이 가장 많았으며, 건설업 종사자의 연차 부여와 사용이 가장 적었음.

#사업체규모
working_new %>%
  select(사업체규모,연차휴가부여,연차휴가사용) %>%
  group_by(사업체규모) %>%
  summarise(given_vac = mean(연차휴가부여), used_vac = mean(연차휴가사용),vac_ratio = used_vac/given_vac*100)
#종사자 규모가 클수록 평균적으로 부여한 연차 일수가 증가함.

#성별
working_new %>%
  select(성별,연차휴가부여,연차휴가사용) %>%
  group_by(성별) %>%
  summarise(given_vac = mean(연차휴가부여), used_vac = mean(연차휴가사용), vac_ratio = used_vac/given_vac*100)
#평균적으로 남성에게 더 많은 연차가 부여되었으며, 남성이 더 많은 연차를 사용했다.

#학력
working_new %>%
  select(학력,연차휴가부여,연차휴가사용) %>%
  group_by(학력) %>%
  summarise(given_vac = mean(연차휴가부여), used_vac = mean(연차휴가사용), vac_ratio = used_vac/given_vac*100)
#학력이 높을수록 연차부여와 사용이 확연하게 증가함을 확인할 수 있음.

#직업
working_new %>%
  select(직업,연차휴가부여,연차휴가사용) %>%
  group_by(직업) %>%
  summarise(given_vac = mean(연차휴가부여), used_vac = mean(연차휴가사용), vac_ratio = used_vac/given_vac*100)
#전문직 및 관리직에서 연차 사용이 많았으며, 판매 종사자 및 단순노무 종사자의 연차 사용 비율이 낮았음

#연령대
working_new %>%
  select(연령대,연차휴가부여,연차휴가사용) %>%
  group_by(연령대) %>%
  summarise(given_vac = mean(연차휴가부여), used_vac = mean(연차휴가사용), vac_ratio = used_vac/given_vac*100)
#30대가 가장 많은 연차휴가부여와 연차휴가사용을 기록했지만, 사용된 비율은 그렇게 유의미하게 높지 않음.

#월평균소득구간
working_new %>%
  select(월평균소득구간,연차휴가부여,연차휴가사용) %>%
  group_by(월평균소득구간) %>%
  summarise(given_vac = mean(연차휴가부여), used_vac = mean(연차휴가사용), vac_ratio = used_vac/given_vac*100)
#월평균소득구간이 높음에 따라 연차부여와 사용이 많음

#4-2. 2030세대와 5060세대의 학력별 분포

#2030대 학력 분포 확인
working2030 <- working_new %>%
  filter(연령대=='20대'|연령대=='30대') %>%
  group_by(학력) %>%
  summarise(number=n(),ratio=number/3036*100)
working2030

#막대 그래프로 시각화
ggplot(working2030, aes(x = 학력, y = number)) +
  xlab('education') +
  geom_col()
#확인 결과 대졸이 가장 많았으며, 그 다음으로 전문대졸, 고졸, 대학원, 중졸이하 순
  
#5060대 학력 본포 확인
working5060 <- working_new %>%
  filter(연령대=='50대'|연령대=='60대') %>%
  group_by(학력) %>%
  summarise(number=n(),ratio=number/617*100)
working5060

#막대그래프로 시각화
ggplot(working5060, aes(x = 학력, y = number)) +
  xlab('education') +
  geom_col()
#확인 결과 대졸과 고졸의 비율이 비슷함.

#4-3. 사업체규모에 따른 성별 월평균소득구간 평균 비교

#사업체규모 20인 미만 회사의 성별 월평균소득구간
working_new %>%
  filter(사업체규모>=1&사업체규모<=3) %>%
  select(성별,월평균소득구간) %>%
  group_by(성별) %>%
  summarise(earning_mean=mean(월평균소득구간))
#남성의 평균적인 월평균소득구간이 여성의 평균적인 월평균소득구간보다 높음.
  
#사업체규모 20인 이상 회사의 성별 월평균소득구간
working_new %>%
  filter(사업체규모>=4&사업체규모<=6) %>%
  select(성별,월평균소득구간) %>%
  group_by(성별) %>%
  summarise(earning_mean=mean(월평균소득구간))
#앞선 결과와 비슷하지만, 여성과 남성 간의 차이가 적음

#성별 무관한 사업체 규모 비교
working_new %>%
  select(사업체규모,월평균소득구간) %>%
  group_by(사업체규모) %>%
  summarise(earning_mean=mean(월평균소득구간))

#사업체 규모와 무관한 성별 비교
working_new %>%
  select(성별,월평균소득구간) %>%
  group_by(성별) %>%
  summarise(earning_mean=mean(월평균소득구간))

#제주도 추가 분석
working_new %>%
  filter(지역==17) %>%
  select(업종,연차휴가부여,연차휴가사용) %>%
  group_by(업종) %>%
  summarise(given_vac=mean(연차휴가부여), used_vac=mean(연차휴가사용), n=n())
#운수업, 숙박 음식점 등의 영세업자 다수
working_new %>%
  filter(지역==17) %>%
  select(사업체규모,연차휴가부여,연차휴가사용) %>%
  group_by(사업체규모) %>%
  summarise(given_vac=mean(연차휴가부여), used_vac=mean(연차휴가사용), n=n())
# 사업체규모가 작은 기업 밀집