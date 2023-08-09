metro <- read_excel("C:/Users/USER/Downloads/서울교통공사_관할역별_일별_시간대별_이용인원_20181231.xlsx", 
                    skip = 1)
head(metro)
names(metro)
dim(metro)
# Q1. 출근시간의 이용객과 퇴근시간의 이용객 중 더 큰 쪽은?

metro %>% 
  filter(구분...6 =="하차") %>% 
  select(`08 ~ 09`) %>% pull %>% sum

metro %>% 
  filter(구분...6 =="승차") %>% 
  select(`18 ~ 19`) %>% pull %>% sum

# Q2. 2018년 일 평균 ’종로3가’의 승차인원과 하차인원은?
metro %>% 
  filter(역명 == "종로3가") %>% 
  group_by(구분...6) %>% 
  summarise(n=n(), mean=mean(`합 계`))

metro %>% 
  filter(역명 == "종로3가") %>% 
  group_by(호선, 구분...6) %>% 
  summarise(n=n(), mean=mean(`합 계`))

# Q3. 출근시간대인 8시와 9시 사이에 가장 붐비는 역과 한가한 역은?
metro %>% 
  select(`08 ~ 09`, 구분...6, 역번호) %>% 
  filter(구분...6 == "하차") %>% 
  group_by(역번호) %>% 
  summarise(n=n(), sum=sum(`08 ~ 09`)) %>% 
  arrange(-sum) %>% 
  head(5)

# Q4. ’고속터미널’역의 평일, 토요일, 휴일 평균 이용고객은?
metro %>% 
  filter(역명 =="강남") %>% 
  group_by(호선, 구분...2) %>% 
  summarise(n=n(), mean=mean(`합 계`))

# Q5. 서울에서 가장 이용객이 많은 라인(호선)은?
metro %>% 
  group_by(호선) %>% 
  summarise(n=n(), sum=sum(`합 계`)) %>% 
  arrange(-sum)

# Q6. 시간대별 평균 승차인원과 하차인원은?
library(tidyr)
names(metro)
metro %>% 
  pivot_longer(cols=7:27) -> metro1
head(metro1) %>% data.frame

t.apt7 %>% 
  group_by(year, mon) %>% 
  summarise(mean=mean(price)) %>% 
  pivot_wider(id_cols=year, names_from=mon,
              values_from=mean) %>% View

