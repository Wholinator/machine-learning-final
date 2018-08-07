library(readxl)
NC_Birth <- read_excel("Class Documents/Machine Learning/NC Birth.xlsx")
data = NC_Birth
data = na.omit(data)
dataSplit <- split(data, data$Monthofbirth)

january <- dataSplit[1]$'1'
february <- dataSplit[2]$'2'
march <- dataSplit[3]$'3'
april <- dataSplit[4]$'4'
may <- dataSplit[5]$'5'
june <- dataSplit[6]$'6'
july <- dataSplit[7]$'7'
august <- dataSplit[8]$'8'
september <- dataSplit[9]$'9'
october <- dataSplit[10]$'10'
november <- dataSplit[11]$'11'
december <- dataSplit[12]$'12'

set.seed(101)
library(caTools)
s <- 0

s.1 <- sample.split(january$Plurality, SplitRatio=0.9)
s.2 <- sample.split(february$Plurality, SplitRatio=0.9)
s.3 <- sample.split(march$Plurality, SplitRatio=0.9)
s.4 <- sample.split(april$Plurality, SplitRatio=0.9)
s.5 <- sample.split(may$Plurality, SplitRatio=0.9)
s.6 <- sample.split(june$Plurality, SplitRatio=0.9)
s.7 <- sample.split(july$Plurality, SplitRatio=0.9)
s.8 <- sample.split(august$Plurality, SplitRatio=0.9)
s.9 <- sample.split(september$Plurality, SplitRatio=0.9)
s.10 <- sample.split(october$Plurality, SplitRatio=0.9)
s.11 <- sample.split(november$Plurality, SplitRatio=0.9)
s.12 <- sample.split(december$Plurality, SplitRatio=0.9)

january.train = subset(january, s.1 == TRUE)
january.test = subset(january, s.1 == FALSE)
february.train = subset(february, s.2 == TRUE)
february.test = subset(february, s.2 == FALSE)
march.train = subset(march, s.3 == TRUE)
march.test = subset(march, s.3 == FALSE)
april.train = subset(april, s.4 == TRUE )
april.test = subset(april, s.4 == FALSE)
may.train = subset(may, s.5 == TRUE)
may.test = subset(may, s.5 == FALSE)
june.train = subset(june, s.6 == TRUE)
june.test = subset(june, s.6 == FALSE)
july.train = subset(july, s.7 == TRUE)
july.test = subset(july, s.7 == FALSE)
august.train = subset(august, s.8 == TRUE)
august.test = subset(august, s.8 == FALSE)
september.train = subset(september, s.9 == TRUE)
september.test = subset(september, s.9 == FALSE)
october.train = subset(october, s.10 == TRUE)
october.test = subset(october, s.10 == FALSE)
november.train = subset(november, s.11 == TRUE)
november.test = subset(november, s.11 == FALSE)
december.train = subset(december, s.12 == TRUE)
december.test = subset(december, s.12 == FALSE)

require(miscTools)
library(MASS)

#############JANUARY#############
lm.fit <- lm(january.train$BirthWeight~., data=january.train)
janM <- stepAIC(lm.fit, direction="backward")
summary(janM)
janM <- update(janM, . ~ . -ASPIRATE)

p <- predict(janM, january.test)
janM.r2 <- rSquared(january.test$BirthWeight, resid=january.test$BirthWeight - p)
janM.rmse <- sqrt(mean((january.test$BirthWeight - p)^2))
plot(p, january.test$BirthWeight)


################February###############
lm.fit <- lm(february.train$BirthWeight~., data=february.train)
febM <- stepAIC(lm.fit, direction="both")
febM <- update(febM, .~. -CEPHALO)
summary(febM)
p <- predict(febM, february.test)
febM.r2 <- rSquared(february.test$BirthWeight, resid=february.test$BirthWeight - p)
febM.rmse <- sqrt(mean((february.test$BirthWeight - p)^2))
plot(p, february.test$BirthWeight)


################March##################
lm.fit <- lm(march.train$BirthWeight~., data=march.train)
marM <- stepAIC(lm.fit, direction="both")
marM <- update(marM, .~.-MONITOR)
summary(marM)
p <- predict(marM, march.test)
marM.r2 <- rSquared(march.test$BirthWeight, resid=march.test$BirthWeight - p)
marM.rmse <- sqrt(mean((march.test$BirthWeight - p)^2))
plot(p, march.test$BirthWeight)


################April##################
lm.fit <- lm(april.train$BirthWeight~., data=april.train)
aprM <- stepAIC(lm.fit, direction="both")
aprM <- update(aprM, .~.-Terms)
summary(aprM)
p <- predict(aprM, april.test)
aprM.r2 <- rSquared(april.test$BirthWeight, resid=april.test$BirthWeight - p)
aprM.rmse <- sqrt(mean((april.test$BirthWeight - p)^2))
plot(p, april.test$BirthWeight)


################May##################
lm.fit <- lm(may.train$BirthWeight~., data=may.train)
mayM <- stepAIC(lm.fit, direction="both")
mayM <- update(mayM, .~.-Smoker)
summary(mayM)
p <- predict(mayM, may.test)
mayM.r2 <- rSquared(may.test$BirthWeight, resid=may.test$BirthWeight - p)
mayM.rmse <- sqrt(mean((may.test$BirthWeight - p)^2))
plot(p, may.test$BirthWeight)


################june##################
lm.fit <- lm(june.train$BirthWeight~., data=june.train)
junM <- stepAIC(lm.fit, direction="backward")
junM <- update(junM, .~.-VENTMORE)
summary(junM)
p <- predict(junM, june.test)
junM.r2 <- rSquared(june.test$BirthWeight, resid=june.test$BirthWeight - p)
junM.rmse <- sqrt(mean((june.test$BirthWeight - p)^2))
plot(p, june.test$BirthWeight)


################july##################
lm.fit <- lm(july.train$BirthWeight~., data=july.train)
julM <- stepAIC(lm.fit, direction="both")
julM <- update(julM, .~.-Visits)
summary(julM)
p <- predict(julM, july.test)
julM.r2 <- rSquared(july.test$BirthWeight, resid=july.test$BirthWeight - p)
julM.rmse <- sqrt(mean((july.test$BirthWeight - p)^2))
plot(p, july.test$BirthWeight)


################august##################
lm.fit <- lm(august.train$BirthWeight~., data=august.train)
augM <- stepAIC(lm.fit, direction="both")
augM <- update(augM, .~.-InfantTran)
summary(augM)
p <- predict(augM, august.test)
augM.r2 <- rSquared(august.test$BirthWeight, resid=august.test$BirthWeight - p)
augM.rmse <- sqrt(mean((august.test$BirthWeight - p)^2))
plot(p, august.test$BirthWeight)


################september##################
lm.fit <- lm(september.train$BirthWeight~., data=september.train)
sepM <- stepAIC(lm.fit, direction="both")
sepM <- update(sepM, .~. -HYDRAM)
summary(sepM)
p <- predict(sepM, september.test)
sepM.r2 <- rSquared(september.test$BirthWeight, resid=september.test$BirthWeight - p)
sepM.rmse <- sqrt(mean((september.test$BirthWeight - p)^2))
plot(p, september.test$BirthWeight)


################october##################
lm.fit <- lm(october.train$BirthWeight~., data=october.train)
octM <- stepAIC(lm.fit, direction="both")
octM <- update(octM, .~.-HYDRAM)
summary(octM)
p <- predict(octM, october.test)
octM.r2 <- rSquared(october.test$BirthWeight, resid=october.test$BirthWeight - p)
octM.rmse <- sqrt(mean((october.test$BirthWeight - p)^2))
plot(p, october.test$BirthWeight)


################november##################
lm.fit <- lm(november.train$BirthWeight~., data=november.train)
novM <- stepAIC(lm.fit, direction="both")
novM <- update(novM, .~.-AMNIO)
summary(novM)
p <- predict(novM, november.test)
novM.r2 <- rSquared(november.test$BirthWeight, resid=november.test$BirthWeight - p)
novM.rmse <- sqrt(mean((november.test$BirthWeight - p)^2))
plot(p, november.test$BirthWeight)


################december##################
lm.fit <- lm(december.train$BirthWeight~., data=december.train)
decM <- stepAIC(lm.fit, direction="both")
decM <- update(decM, .~.-RENAL)
summary(decM)
p <- predict(decM, december.test)
decM.r2 <- rSquared(december.test$BirthWeight, resid=december.test$BirthWeight - p)
decM.rmse <- sqrt(mean((december.test$BirthWeight - p)^2))
plot(p, december.test$BirthWeight)



for(i in name.dec){
  #browser()
  if (!is.na(p[i]==0)) {
    p[i] = p[i] + 1
  } else {
    p[i]= 1
  }
}
p=0

#sbirthWeight = 0
#ubirthWeight[12] <- mean(december.test$BirthWeight)
#sbirthWeight[12] <- sd(december.test$BirthWeight)



