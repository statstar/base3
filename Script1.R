diet <- read.csv("C:/Users/USER/Downloads/base3-main/base3-main/diet.csv")
head(diet)

library(dplyr)
table(diet$gender)
table(diet$gender) %>% barplot

diet$Age %>% summary
diet$Age %>% hist
diet$Age %>% boxplot

diet$Height %>% summary
diet$Height %>% hist

library(dplyr)
diet %>% 
  filter(gender == 0, Height >=170)

x <- c("b","a","d","c","a","b")
factor(x, levels=c("b","a","d","c"))

diet[,2:3] %>% head
diet[,c(3,2)] %>% head

diet %>% head

diet %>% 
  mutate(diff = pre.weight - weight6weeks) -> diet1
boxplot(diff~Diet, diet1)

library(vioplot)
vioplot(diff~Diet, diet1,
        col=c("cornflowerblue","tomato3", "gold"))

diet1 %>% 
  group_by(Diet) %>% 
  summarise(n=n(), mean=mean(diff), sd=sd(diff))

diet1 %>% 
  mutate(BMI.pre = pre.weight/((Height/100)^2)) %>% 
  mutate(BMI.pre.c = cut(BMI.pre, c(0,18.5,23,25,40),
          c("저체중","정상","과제충","비만"))) -> diet2
diet2$BMI.pre.c %>% table

# diet2 자료에 weight6weeks 변수가 있습니다.
# 사후 몸무게와 키를 이용하여 BMI.after를 생성후
# BMI.after.c를 생성해보세요.

diet2 %>% 
  mutate(BMI.after = weight6weeks/((Height/100)^2)) %>% 
  mutate(BMI.after.c = cut(BMI.after, c(0,18.5,23,25,40),
                         c("저체중","정상","과제충","비만"))) -> diet3

table(diet3$BMI.pre.c, diet3$BMI.after.c, diet3$Diet)
