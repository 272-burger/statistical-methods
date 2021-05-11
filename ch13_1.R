# ==== 13.5 ====

# F-Quantile value
qf(p = 0.95, df1 = 3, df2 = 28, lower.tail = T)

# Visualization
curve(df(x, 3, 28),  from = 0, to = 10)
abline(v = qf(0.95, 3, 28), col = "blue")
abline(v = 4, col = "red")

# q-Quantile value
qtukey(0.95, nmeans = 4, df = 28)

# ==== 13.11 ====

# Assigning data
(painkiller.data <- data.frame("진통제1" = c(25, 38, 40, 65, 47, 52),
                            "진통제2" = c(15, 21, 19, 23, NA, NA),
                            "진통제3" = c(44, 39, 52, 58, 73, NA)))

# Calculating means
(painkiller.means <- colMeans(painkiller.data, na.rm = T))

par(mfrow=c(1,2))
plot(painkiller.means, pch=19, lwd=5) # 점 그래프
barplot(painkiller.means) # 막대그래프

# Dataframe
obs <- na.omit(as.vector(as.matrix(painkiller.data)))
group <- c(rep("진통제1", 6), rep("진통제2", 4), rep("진통제3", 5))
painkiller.df <- data.frame(obs, group)


# ANOVA
aov.painkiller <- aov(obs ~ group, data = painkiller.df)
summary(aov.painkiller)

## Calculation
group.means <- painkiller.means # x_j_bar
obs.mean <- mean(obs) # x_bar 

tr_1 <- 6 * (group.means[1] - obs.mean) ** 2
tr_2 <- 4 * (group.means[2] - obs.mean) ** 2
tr_3 <- 5 * (group.means[3] - obs.mean) ** 2
sstr.painkiller <- sum(tr_1, tr_2, tr_3)
sst.painkiller <- sum((obs - obs.mean) ** 2)

### Decomposition
sse.painkiller <- sst.painkiller - sstr.painkiller

## p.value
(F.value <- (sstr.painkiller/2)/(sse.painkiller/12))
(p.value <- 1 - pf(F.value, 2, 12))

# Data Visualization
## Strip chart
stripchart1 <- stripchart(obs ~ group, data = painkiller.df, vertical=T)

## Confidence interval
library(gplots)
plotmeans(obs ~ group, data = painkiller.df)

# ==== Additional 11.a ====
# Assigning data
painkiller.data <- data.frame("진통제1" = c(25, 38, 40, 65, 47, 52),
                               "진통제2" = c(15, 21, 19, 23, NA, NA),
                               "진통제3" = c(44, 39, 52, 58, 73, NA))

(painkiller.data_mul <- painkiller.data * 60)

# Calculating means
(painkiller.means_mul <- colMeans(painkiller.data_mul, na.rm = T))

par(mfrow=c(1,2))
plot(painkiller.means_mul, pch=19, lwd=5) # 점 그래프
barplot(painkiller.means_mul) # 막대그래프

# Dataframe
obs_mul <- na.omit(as.vector(as.matrix(painkiller.data_mul)))
group_mul <- c(rep("A", 6), rep("B", 4), rep("C", 5))
painkiller.df_mul <- data.frame(obs_mul, group_mul)


# ANOVA
aov.painkiller_mul <- aov(obs_mul ~ group_mul, data = painkiller.df_mul)
summary(aov.painkiller_mul)


# ==== Additional 11.b ====
# Assigning data
painkiller.data <- data.frame("진통제1" = c(25, 38, 40, 65, 47, 52),
                               "진통제2" = c(15, 21, 19, 23, NA, NA),
                               "진통제3" = c(44, 39, 52, 58, 73, NA))

(painkiller.data_add <- painkiller.data + 2)

# Calculating means
(painkiller.means_add <- colMeans(painkiller.data_add, na.rm = T))

par(mfrow=c(1,2))
plot(painkiller.means_add, pch=19, lwd=5) # 점 그래프
barplot(painkiller.means_add) # 막대그래프

# Dataframe
obs_add <- na.omit(as.vector(as.matrix(painkiller.data_add)))
group_add <- c(rep("A", 6), rep("B", 4), rep("C", 5))
painkiller.df_add <- data.frame(obs_add, group_add)


# ANOVA
aov.painkiller_add <- aov(obs_add ~ group_add, data = painkiller.df_add)
summary(aov.painkiller_add)

# Comparison
par(mfrow = c(1,3))
## Origin
stripchart(obs ~ group, data = painkiller.df, vertical=T)
## Multiplication
stripchart(obs_mul ~ group_mul, data = painkiller.df_mul, vertical=T)
## Addition
stripchart(obs_add ~ group_add, data = painkiller.df_add, vertical=T)


# ==== 13.12 ====
# Assigning data
(drink <- matrix(c(19, 30, 23, 17, 32, 26,
                   18, 31, 27, 14, 29, 24,
                   15, 35, 22), nrow = 5, ncol = 3, byrow = T,
                 dimnames = list(NULL, c("A", "B", "C"))))

# Calculating means
(drink.means <- colMeans(drink))
 
## Means Visualization
par(mfrow=c(1,2))
plot(drink.means, pch=19, lwd=5) # 점 그래프
barplot(drink.means) # 막대그래프

# Dataframe
obs <- as.vector(drink)
group <- gl(n=3, k=5, length=15, labels=c("A", "B", "C"))
drink.df <- data.frame(obs, group)

# Data Visualization
## Strip chart
stripchart(obs ~ group, data = drink.df, vertical=T)

## Confidence interval
library(gplots)
plotmeans(obs ~ group, data = drink.df)

# ANOVA
aov.drink <- aov(obs ~ group, data = drink.df)
summary(aov.drink)

## Calculation
group.means <- drink.means # x_j_bar
obs.mean <- mean(group.means) # x_bar 
sstr.drink <- sum(5 * (group.means - obs.mean) ** 2)
sse.drink <- 0
for (j in 1:3) {
    for (i in 1:5) {
        sse.drink = (data.frame(drink)[i, j] - group.means[j]) ** 2 + sse.drink
    }
}
(F.value <- (sstr.drink/2)/(sse.drink/12))
(p.value <- 1 - pf(F.value, 2, 12))

## ANOVA Visualizaiton
curve(df(x, df1 = 2, df2 = 12), from = 0, to = 60)
abline(v = qf(0.95, 3, 12), col = "blue")
abline(v = F.value, col="red")


# Multiple comparison
## Tukey HSD
TukeyHSD(aov.drink)
plot(TukeyHSD(aov.drink))

## Calculation
mse.drink <- sse.drink / 12
q.value <- qtukey(0.95, 3, 12)

low_1 <- (drink.means[1] - drink.means[2]) - q.value/sqrt(2)*sqrt(mse.drink*(1/5 + 1/5))
up_1 <- (drink.means[1] - drink.means[2]) + q.value/sqrt(2)*sqrt(mse.drink*(1/5 + 1/5))
low_2 <- (drink.means[2] - drink.means[3]) - q.value/sqrt(2)*sqrt(mse.drink*(1/5 + 1/5))
up_2 <- (drink.means[2] - drink.means[3]) + q.value/sqrt(2)*sqrt(mse.drink*(1/5 + 1/5))
low_3 <- (drink.means[1] - drink.means[3]) - q.value/sqrt(2)*sqrt(mse.drink*(1/5 + 1/5))
up_3 <- (drink.means[1] - drink.means[3]) + q.value/sqrt(2)*sqrt(mse.drink*(1/5 + 1/5))
(tukey_drinks <- matrix(c(low_1, up_1, low_2, up_2, low_3, up_3), nrow = 3, ncol = 2, byrow = T))

# ==== 13.20 ====
# p.value
round(1 - pf(11.327, 2, 15), 4)

# F-Quantile value
qf(p = 0.95, 2, 15, lower.tail = T)

## Visualization
curve(df(x, 2, 15),  from = 0, to = 5)
abline(v = qf(0.95, 2, 15), col = "blue")

# ANOVA visualization
curve(df(x, 2, 15), from = 0, to = 15)
abline(v = qf(0.95, 2, 15), col = "blue")
abline(v = 11.327, col="red")

# Multiple comparison
## Calculation
mse.prod <- 0.544
q.value <- qtukey(0.99, 3, 15)

(low <- (43.333 - 41.500) - q.value/sqrt(2)*sqrt(mse.prod*(1/6 + 1/6)))
(up <- (43.333 - 41.500) + q.value/sqrt(2)*sqrt(mse.prod*(1/6 + 1/6)))

