########################################################################
## opgave a
# Indlæs 'bmi2_data.csv' filen med data
D <- read.table("bmi2_data.csv", header = TRUE, sep = ";")


# Tilføj log-BMI til datasættet
D$logbmi <- log(D$bmi)


# Deldatasæt med de første 840 observationer (til model)
D_model <- subset(D, id <= 840)

# Deldatasæt med de sidste 7 observationer (til validering)
D_test <- subset(D, id >= 841)

# histogeram af logbmi, age og fastfood
hist(D$logbmi, main = "Histogram log BMI", xlab = "log BMI", prob = TRUE)

hist(D$age, main = "Histogram Age", xlab = "Age", prob = TRUE)

hist(D$fastfood, main = "Histogram Fastfood", xlab = "Fastfood", prob = TRUE)

#Plot over Logbmi, fastfood og alder
plot(D$logbm ~ D$age)
abline(lm(D$logbmi ~ D$age))

plot(D$logbmi ~D$fastfood)
abline(lm(D$logbmi ~ D$fastfood))

#Boxplot af logbmi, age og fastfood
boxplot(D$logbmi, D$age, D$fastfood, names = c("Logbmi", "Age", "Fastfood"), outline = FALSE)

boxplot(D$logbmi, names = c("Logbmi"))

#Tabel udregninger for log bmi
sum(!is.na(D$logbmi))
summary(D$logbmi)
mean(D$logbmi, na.rm = TRUE)
sd(D$logbmi)

#Tabel udregning for Age
sum(!is.na(D$age))
mean(D$age)
sd(D$age)
summary(D$age)

#Tabel udregning for fastfood
sum(!is.na(D$fastfood))
mean(D$fastfood, na.rm=TRUE)
sd(D$fastfood)
summary(D$fastfood)

#######################################################################
## Opgave C
# Estimer multipel lineær regressionsmodel
fit <- lm(logbmi ~ age + fastfood, data = D_model)

# Vis estimerede parametre mm.
summary(fit)

######################################################################
## Opgave d
# Plots til modelkontrol

# Observationer mod fittede værdier
plot(fit$fitted.values, D_model$logbmi, xlab = "Fittede værdier",     
       ylab = "log(BMI)")

# Residualer mod hver af de forklarende variable
plot(D_model$age, fit$residuals, 
        xlab = "Age", ylab = "Residualer")

plot(D_model$fastfood, fit$residuals, 
     xlab = "Fastfood", ylab = "Residualer")

# Residualer mod fittede værdier
plot(fit$fitted.values, fit$residuals, xlab = "Fittede værdier", 
     ylab = "Residualer")

# Normal QQ-plot af residualerne
qqnorm(fit$residuals, ylab = "Residualer", xlab = "Z-scores", 
       main = "")
qqline(fit$residuals)

##################################################################

# Konfidensintervaller for modellens koefficienter
confint(fit, level = 0.95)


# Prædiktioner og 95% prædiktionsintervaller
pred <- predict(SLUTMODEL, newdata = D_test, interval = "prediction", 
              level = 0.95)

# Observerede værdier sammen med prædiktioner
cbind(id = D_test$id, logbmi = D_test$logbmi, pred)

