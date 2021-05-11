# ====== 13.1 ======
# one-way ANOVA, multiple comparison

# Assigning data
damage <- matrix(c(681, 643, 469, 384, 728, 655, 727, 
                   656, 917, 742, 525, 602, 898, 514,
                   454, 687, 620, 525, 459, 360),
                   ncol=4, nrow=5, byrow=T,
                   dimnames=list(NULL, c("소형", "준중형", "중형", "대형")))
damage
damage.df <- data.frame(damage)
(damage.Means <- colMeans(damage))

# Graph
par(mfrow=c(1,2))
plot(damage.Means, pch=19, lwd=5) # 점그래프: 위치에 따른 상대적인 차이
barplot(damage.Means) #막대그래프: 0부터, 절대적인 차이
# 시각적인 느낌이 다름

## 평균값 크기에 따라 자료 순서를 바꾸려면 
### 명목형 자료는 이렇게 하는 것이 분석이 더 용이
# damage.df <- damage.df[,c(1,2,4,3)] #cf) 첫번째 comma == to keep all the rows (지금 열의 순서를 바꾸고 싶은 상황임)
# damage.df
# (damage.Means <- colMeans(damage.df))

damage <- as.vector(damage)
## gl == generate factor levels 
## n == the number of levels
## k == the number of replications
(size <- gl(n=4, k=5, length=20, labels=c("1소형", "2준중형", "3중형", "4대형")))
                
(damage.df <- data.frame(damage, size))
# 자료들이 겹치는 부분 주의! 완벽하게 겹칠 수도 있음
## 이 경우 중형에서 겹치는 자료 존재
stripchart(damage ~ size, vertical = T, main="head damage by the size of car")


## gplot 라이브러리
library(gplots)
par(mfrow=c(1,2))
plotmeans(damage ~ size) # 95% confidence intervals
plotmeans(damage ~ size, bars=F)

# ANOVA
damage.aov <- aov(damage ~ size)
summary(damage.aov)
## H0 기각, 유의한 차이

# Multiple comparison
## (소형평균 - 중형평균)의 신뢰구간
damage.Means[1]-damage.Means[2] - qtukey(0.95, nmeans = 4, df=16) / sqrt(2) * sqrt(15995* ( 1/5 + 1/5))
damage.Means[1]-damage.Means[2] + qtukey(0.95, nmeans=4, df=16) / sqrt(2) * sqrt(15995 * (1/5 + 1/5))
## 이 결과는 아래의 TukeyHSD 결과와 약간의 차이가 나는데
## 그 이유는 분산분석표에서 소숫점 아래 자리까지 정확한 값이 표시되지 못했기 때문이다.
## SSE=255923.2 MSE=15995.2 

### 소형 - 중형 신뢰구간 (-75.84582 , 381.8458)
### 0을 포함하고 있기 때문에 그 차이가 유의하지 않다고 볼 수 있음

## Tukey HSD (Honest Significant Differences)
TukeyHSD(damage.aov)
plot(TukeyHSD(damage.aov))

## cf) 이표본 t 검정
# pairwise.t.test(damage, size, "bonferroni")
## bonferroni: 보수적, 차이를 잘 못찾아내서 선호되지 않음


# ==== 13.2 ====
# two-way ANOVA

# Assigning data 
## matrix로도 가능하긴 한데 여기선 그냥 c로 함
damage2 <- c(700, 540, 450, 460, 820,
             680, 590, 600, 710, 530,
             470, 470, 830, 710, 590, 610)

size2 <- gl(n=4, k=1, length = 16, 
            labels=c("1소형", "2준중형", "3중형", "4대형"))
make2 <- gl(n=2, k=8, length = 16, 
            labels=c("제조사 A", "제조사 B"))

data2 <- data.frame (damage2, size2, make2)
data2

# Plot means
library(gplots)
plotmeans(damage2 ~ interaction (make2, size2), connect = list(1:4, 5:8),
          bars=F, main="제조사 요인")
x11()
plotmeans(damage2 ~ interaction (make2, size2), connect = list(1:2, 3:4, 5:6, 7:8),
          bars=F, main="크기 요인")

## cf) Plotmeans 말고 Plot함수 이용 가능
## interaction을 '*' 표시로 나타낼 수 있음
## plot(damage 2 ~ size2 * make2)
 
## plot(damage2 ~ interaction(size2, make2), connect = list(1:4, 5:8),bars=F, main="제조사 요인"))
## x11()
## plot(damage2 ~ interaction(size2, make2), connect = list(1:2, 3:4, 5:6, 7:8),bars=F, main="크기 요인"))

# ANOVA table
# 이원분석 aov 함수에서 교차항 모형 (interaction term) '*' 사용

damage2.aov <- aov(damage2 ~ size2 * make2)
summary(damage2.aov)
## 검사 결과 f통계량 = 0, 두 요인 간 상호작용이 없음을 확인 가능
## 각 요인 별로 확인 결과 size는 유의미한 상관관계 있음, make는 상관관계 없음

# ==== 13.3 ====
# randomized block design

## 운영체제 A,B,C / 사용자 별로 한글 입력 시간이 다른데 이로 인한 차이를 배제하고자 함

# Assigning data
time <- c(16, 16, 19, 19, 17, 18, 14, 13, 15, 
          13, 12, 14, 18, 17, 19)

OS <- gl(n=3, k=1, length=15, labels=c("A","B","C")) # Operating System
subject <- gl(n=5, k=3, length = 15, labels = 1:5) # Subject -> Block!

data3 <- data.frame (time, OS, subject)
data3

# Plotmeans 
library(gplots)
plotmeans(time ~ interaction(OS, subject), connect = list(1:3, 4:6, 7:9, 10:12,13:15), main="운영체계별 입력 시간")

# randomizaed block design
## two-way ANOVA는 '*'로 interation term 반영
## 확률화블록설계는 interaction term 사용 X '+'
time.aov <- aov(time ~ OS + subject) 
summary(time.aov)

### 결과 보면 subject에 의한 f통계량 매우 작지만 이 경우 고려 대상 아님
### OS 별로도 통계적으로 유의미한 차이가 있다고 볼 수 있음
### 그래프에서도 다 브이자 형태 보여줌

## cf) If there were no blocks, 즉 one-way ANOVA 형태로 검정한다면? 
## time.aov2 <- aov(time ~ OS) -> OS만 고려
## summary(time.aov2)
## 결과를 보면, 개인 차를 고려해주지 않았을 때는 OS 간의 차이가 없는 것으로 나옴
## 개인 차가 워낙 심해서 개인 차를 배제 (block)하지 않으면 OS 간의 차이는 묻혀 버려서 이런 결과의 차이가 발생한다! 


