library(haven)
welfare_raw <- read_sav("C:/Users/USER/Downloads/Koweps_etc/데이터/Koweps_hpc10_2015_beta1.sav")
head(welfare_raw)
dim(welfare_raw)

welfare <- rename(welfare_raw, gender = h10_g3, # 성별 
                  birth = h10_g4, # 태어난 연도 
                  marriage = h10_g10, # 혼인 상태 
                  religion = h10_g11, # 종교 
                  income = p1002_8aq1, # 월급 
                  code_job = h10_eco9, # 직종 코드 
                  code_region = h10_reg7) %>% 
  select(gender, birth, marriage, religion, income,
         code_job, code_region)

names(welfare)

welfare$gender %>% table -> tab1
tab1/sum(tab1)

welfare$birth %>% hist
welfare %>% 
  mutate(age= 2015 -birth) %>% 
  mutate(age.c = age %/% 10) %>% 
  mutate(age.c = ifelse(age.c>=8,8,age.c)) -> welfare1
table(welfare1$age.c)

welfare$marriage %>% table
welfare$religion %>% table
welfare$income %>% hist
welfare$income %>% sqrt %>% hist
welfare$income %>% log %>% hist

welfare$code_job %>% table
welfare1 %>% 
  mutate(code_job2 = code_job %/% 100) -> welfare1
welfare1$code_job2 %>% table

welfare1$code_region %>% table

# "성별에 따라 월급이 다를까?"

# 성별(gender) 문자형, 월급(income) 수치형
welfare1$gender %>% table
sqrt(welfare1$income) %>% hist

# 시각적
boxplot(sqrt(income)~gender, welfare1)
vioplot(sqrt(income)~gender, welfare1)

welfare1 %>% 
  filter(!is.na(income)) %>% 
  group_by(gender) %>% 
  summarise(n=n(), mean=mean(income), sd=sd(income))

t.test(income~gender, welfare1)
# 나이에 따른 월급
boxplot(income~age, welfare1)
boxplot(income~gender+age.c, welfare1)

welfare1 %>% 
  filter(!is.na(income)) %>% 
  group_by(age.c) %>% 
  summarise(n=n(), mean=sum(income))
welfare1$age.c %>% table


284018/1720 # 30대
406309/2295 # 40대
248413/2113 # 50대

boxplot(income~gender+age.c, welfare1,col=c(2,3))
boxplot(income~age.c+gender, welfare1)

welfare1 %>% 
  filter(!is.na(income)) %>% 
  filter(age.c !=1) %>% 
  group_by(age.c, gender) %>% 
  summarise(n=n(), mean=round(mean(income),2)) %>% 
  pivot_wider(id_cols=age.c, 
              names_from=gender, values_from=mean) %>% 
  data.frame

boxplot(income~code_job2, welfare1)
welfare1 %>% 
  filter(!is.na(income)) %>% 
  group_by(code_job2) %>% 
  summarise(n=n(), mean=round(mean(income),2))

welfare1 %>% 
  filter(!is.na(income)) %>% 
  filter(code_job ==261) %>% 
  summarise(mean=mean(income))

table(welfare1$gender, welfare1$code_job2) -> tab1
round(tab1/rowSums(tab1)*100,1) %>% 
  barplot(beside=T, legend=rownames(.))

table(welfare1$marriage, welfare$religion) %>% 
  chisq.test
table(welfare1$marriage, welfare$religion) %>% 
  barplot(beside=T, legend=rownames(.))










