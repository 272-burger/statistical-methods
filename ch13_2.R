# ====13.28====
# Assigning Data 
engaged_time <- c(14, 10, 13, 15, 15, 12, 10, 8, 8, 6, 7, 4, 5, 10, 9, 11,
                  11, 13, 12, 10, 14, 8, 15, 9, 10, 13, 9, 6, 12, 9, 7, 13)
drug <- gl(2, 8, 32, labels = list("Y", "N"))
counseling <- gl(2, 16, 32, labels = list("Y", "N"))

engaged_time.df <- data.frame(engaged_time, drug, counseling)

# Interaction Visaulization
interaction.plot(x.factor = engaged_time.df$drug, 
                 trace.factor = engaged_time.df$counseling,
                 response = engaged_time.df$engaged_time, fun = mean, type="b",
                 pch = c(2,4), col = c(2,4), xlab = "약물요인", ylab = "집중시간")


# two-way ANOVA
aov.time <- aov(engaged_time ~ drug * counseling)
summary(aov.time)

## Calculation
### Means
(group.means <- aggregate(engaged_time ~ drug + counseling, data = engaged_time.df, mean)) # x_ij_bar
(matrix.means <- matrix(group.means$engaged_time, nrow = 2, ncol = 2))
(obs.mean <- mean(engaged_time)) # x_bar


### Sum of Squares
(sst.time <- sum((engaged_time.df$engaged_time - obs.mean) ** 2))

sstr.drug <- 0
for (i in 1:2){
    sstr.drug <- 2 * 8 * (mean(matrix.means[i,]) - obs.mean) ** 2 + sstr.drug
}
sstr.drug

sstr.counseling <- 0
for (i in 1:2){
    sstr.counseling <- 2 * 8 * (mean(matrix.means[,i]) - obs.mean) ** 2 + sstr.counseling
}
sstr.counseling

ssint.time <- 0
for (i in 1:2){
    for (j in 1:2){
        ssint.time = ssint.time + 8 * (matrix.means[i,j] - mean(matrix.means[i,]) - mean(matrix.means[,j]) + obs.mean) ** 2
    }
}
ssint.time

sse.time <- sst.time - sum(sstr.drug, sstr.counseling, ssint.time)

### Mean sum of squares
mstr.durg <- sstr.drug/(2-1)
mstr.counseling <- sstr.counseling/(2-1)
msint.time <- ssint.time/((2-1) * (2-1))
mse.time <- sse/(2*2*7)

### F.value
f_value.drug <- mstr.durg / mse.time
f_value.counseling <- mstr.counseling / mse.time
(f_value.interaction <- msint.time / mse.time)

### P.value
(p_value.drug <- pf(f_value.drug, 1, 28, lower.tail = F))
(p_value.counseling <- pf(f_value.counseling, 1, 28, lower.tail = F))
(p_value.interaction <- pf(f_value.interaction, 1, 28, lower.tail = F))

# Visualization
library(gplots)
plotmeans(engaged_time ~ interaction(drug, counseling), data = engaged_time.df)


# ====13.29====
# Assigning Data
tv <- c(62,  68, 64, 51, 47, 48, 88, 85, 81,
                67, 60, 66, 127, 120, 126, 105, 101, 107)

position <- gl(2, 3, 18, labels = list("프로그램 시작 전", "프로그램 종료 후"))
time <- gl(3, 6, 18, labels = list("오전 11시대", "오후 3시대", "오후 8시대"))

(tv.df <- data.frame(tv, position, time))

# Interaction Visualization
interaction.plot(x.factor = tv.df$position, trace.factor = tv.df$time,
                 response = tv.df$tv, fun = mean, type = "b",
                 pch = c(2,4), col = c(2,4), xlab = "방영 기점", ylab = "광고 반응도")

interaction.plot(x.factor = tv.df$time, trace.factor = tv.df$position,
                 response = tv.df$tv, fun = mean, type = "b",
                 pch = c(2,4), col = c(2,4), xlab = "방영 시점", ylab = "광고 반응도")

# two-way ANOVA
aov.tv <- aov(tv ~ position * time, data = tv.df)
summary(aov.tv)

## Calculation
### Mean
(group.means <- aggregate(tv ~ position + time, data = tv.df, mean))
(matrix.means <- matrix(group.means$tv, byrow = T, nrow = 3, ncol = 2)) # x_ij_bar
obs.mean <- mean(tv) # x_bar


### Sum of Square
(sst.tv <- sum((tv.df$tv - obs.mean) ** 2))

sstr.position <- 0
for (i in 1:2){
    sstr.position <- 9 * sum((mean(matrix.means[,i]) - obs.mean) ** 2) + sstr.position
}

sstr.position

sstr.time <- 0
for (i in 1:3){
    sstr.time <- 6 * sum((mean(matrix.means[i,]) - obs.mean) ** 2) + sstr.time
}
sstr.time

ssint.tv <- 0
for (i in 1:3){
    for (j in 1:2){
        ssint.tv <- 3 * (matrix.means[i,j] - mean(matrix.means[i,]) - mean(matrix.means[,j]) + obs.mean) ** 2 + ssint.tv
    }
}

ssint.tv

sse.tv <- sst.tv - sum(sstr.position, sstr.time, ssint.tv)
sse.tv

### Mean Sum of Square
mstr.position <- sstr.position / (2 - 1)
mstr.time <- sstr.time / (3 - 1)
mstr.interaction <- ssint.tv / ((3 - 1) * (2 - 1))
mse.tv <- sse.tv / (2 * 3 * (3 - 1))

### F.value
f_value.position <- mstr.position / mse.tv
f_value.time <- mstr.time / mse.tv
f_value.interaction <- mstr.interaction / mse.tv

### P.value
(p_value.position <- pf(f_value.position, 1, 12, lower.tail = F))
(p_value.time <- pf(f_value.time, 2, 12, lower.tail = F))
(p_value.interaction <- pf(f_value.interaction, 2, 12, lower.tail = F))

# Visualization
library(gplots)
plotmeans(tv ~ interaction(position, time), data = tv.df)




# ====13.32====
# Assigning Data
meal_time <- c(12.6, 23.1, 33.0, 41.2, 43.2,
               11.2, 21.8, 28.2, 34.5, 45.2,
               10.7, 18.2, 18.4, 29.7, 35.9,
               8.8, 15.9, 20.1, 31.2, 37.4,
               15.1, 28.2, 39.8, 46.9, 53.2,
               14.8, 23.4, 31.4, 39.7, 49.8)
ppl <- gl(5, 1, 30, labels = list('1', '2', '3', '4', '5 이상'))
location <- gl(3, 10, 30, labels = list("신촌", "대학로", "강남역"))

meal_time.df <- data.frame(meal_time, ppl, location)
meal_time.df

# Interaction Visualization
interaction.plot(x.factor = ppl, trace.factor = location, response = meal_time,
                 type = "b", pch = c(2,4,6), col = c(2,4,6), xlab = "사람 수", ylab = "식사시간")

# two-way ANOVA
aov.meal_time <- aov(meal_time ~ ppl * location, data = meal_time.df)
summary(aov.meal_time)

## Calculation
### Means
(group.means <- aggregate(meal_time ~ ppl + location, data = meal_time.df, mean))
(matrix.meals <- matrix(group.means$meal_time, nrow = 3, ncol = 5, byrow = T)) # x_ij_bar
obs.mean <- mean(meal_time) # x_bar

### Sum of Squares
### a = 5, b = 3, m = 2
(sst.meal <- sum((meal_time - obs.mean) ** 2))

sstr.ppl <- 0
for (i in 1:5){
    sstr.ppl <- 6 * sum((mean(matrix.meals[,i]) - obs.mean) ** 2) + sstr.ppl
}
sstr.ppl

sstr.location <- 0
for (i in 1:3){
    sstr.location <- 10 * sum((mean(matrix.meals[i,]) - obs.mean) ** 2) + sstr.location
}
sstr.location

ssint.meal <- 0
for (i in 1:3){
    for(j in 1:5){
        ssint.meal <- 2 * sum((matrix.meals[i,j] - mean(matrix.meals[i,]) - mean(matrix.meals[,j]) + obs.mean) ** 2) + ssint.meal
    }
}
ssint.meal

sse.meal <- sst.meal - sum(sstr.ppl, sstr.location, ssint.meal)

### Mean Sum of Square
(mstr.ppl <- sstr.ppl / (5 - 1))
(mstr.location <- sstr.location / (3 - 1))
(mstr.interaction <- ssint.meal / 8)
(mse.meal <- sse.meal / 15)

### F.value
(f_value.ppl <- mstr.ppl / mse.meal)
(f_value.location <- mstr.location / mse.meal)
(f_value.interaction <- mstr.interaction / mse.meal)

### P.vlaue
(p_value.location <- pf(f_value.location, 2, 15, lower.tail = F))

# Visualization
library(gplots)
plotmeans(meal_time ~ interaction(ppl, location), data = meal_time.df,
          , connect = list(1:5, 6:10, 11:15), bars = F)

# ====13.33====
# Assigning Data
heartbeat <- c(205, 199, 180, 177, 181, 164, 166, 184, 167, 
               152, 169, 153, 142, 162, 159, 172, 193, 205, 
               191, 184, 156, 170, 207, 160, 181, 177, 175, 
               154, 174, 171)
machine <- gl(3, 1, 30, labels = list("exercise bike", "treadmill", "stair stepper"))
subject <- gl(10, 3, 30, labels = list('1','2','3','4','5','6','7','8','9','10'))

(heartbeat.df <- data.frame(heartbeat, machine, subject))

# Visualization
library(gplots)
plotmeans(heartbeat ~ interaction(machine, subject), data = heartbeat.df, bars = F,
          connect = list(1:3,4:6, 7:9, 10:12, 13:15, 16:18, 19:21, 22:24, 25:27, 28:30))

# No Blocks
# one-way ANOVA
summary(aov(heartbeat ~ machine, data = heartbeat.df))

## Means
(group.means_1 <- aggregate(heartbeat ~ machine, data = heartbeat.df, mean)) # x_i_bar
(group.means_1 <- as.vector(group.means_1$heartbeat))
(obs.mean <- mean(heartbeat)) # x_bar

## Sum of Squares
(sst.heartbeat <- sum((heartbeat - obs.mean) ** 2))
sstr_1 <- 0 
for (i in 1:3) {
       sstr_1 <-  sum(10 * (group.means_1[i] - obs.mean) ** 2) + sstr_1
}

sstr_1

(sse_1 <- sst - sstr_1)

## Mean sum of squares
## k = 3, n = 30
(mstr_1 <- sstr_1 / 2)
(mse_1 <- sse_1 / 27)

## F.value
(f_value_1 <- mstr_1 / mse_1)

## P.value
(p_value_1 <- pf(f_value_1, 2, 27, lower.tail = F))

# Randomized Block Design
summary(aov(heartbeat ~ machine + subject, data = heartbeat.df))

## Calculataion
### Means 
(machine.means <- as.vector((aggregate(heartbeat ~ machine, data = heartbeat.df, mean))$heartbeat))
subject.means <- as.vector((aggregate(heartbeat ~ subject, data = heartbeat.df, mean))$heartbeat)                        
### Sum of Squares

sstr_2 <- 0
for (i in 1:3){
   sstr_2 <- 10 * (machine.means[i] - obs.mean) ** 2 + sstr_2
}
sstr_2

ssb <- 0
for (i in 1:10){
    ssb <- 3 * (subject.means[i] - obs.mean) ** 2 + ssb
}
ssb

(sse_2 <- sst.heartbeat - sum(sstr_2, ssb))

### Mean sum of squares
### k = 3, b = 10
mstr_2 <- sstr_2 / 2
msb <- ssb / 9
mse_2 <- sse_2 / 18

### F.value
f_value_2 <- mstr_2 / mse_2

### P.value
(p_value_2 <- pf(f_value_2, 2, 18, lower.tail = F))
