###Librer?as en general
library(esquisse)
library(faraway)
library(boot)
library(carData)
library(car)
library(tidyverse)
library(QuantPsyc)
library(readr)
library(ggplot2)
library(MASS)
library(stats)
library(lmtest)
library(psychometric)
library(RVAideMemoire)
library(dplyr)
library(hrbrthemes)
library(pls)
library(GGally)
library(PerformanceAnalytics)
library(Hmisc)
library(picante)
library(summarytools)
library(ggpubr)
library(modelr)
library(broom)
library(AICcmodavg)
library(psych)
library(gridExtra)
library(cowplot)
library(modelsummary)
library(gridExtra)
library(HH)
library(multcompView)
library(ggpubr)
library(multcomp)
library(gmodels)
library(janitor)
library(lubridate)
library(wesanderson)
library(data.table)
library(ssym)
library(lmtest)
library(flexmix)
library(oddsratio)
library(aod)
library(arm)
library(gapminder)
library(ggstatsplot)

###Librer?as sobrevida
library(survival)
library(KMsurv)
library(survMisc)
library(survminer)
library(flexsurv)
library(actuar)
library(dplyr)
library(ggfortify)
library(datasets)
library(ranger)
library(texreg)
library(skimr)
library(ggalt)
library(ggcorrplot)
library(remotes)
remotes::install_github("arcruz0/paqueteadp")
library(paqueteadp)
library(SMPracticals)
library(ellipse)
library(haven)
library(gridExtra)
library(mstate)
library(rpivotTable)
library(haven)
library(ranger)
library(texreg)
library(remotes)
library(ggcorrplot)
library(paqueteadp)
library(cmprsk)
library(mstate)
library(riskRegression)
library("RColorBrewer")
library(splines)

###librer?as series de tiempo###
library(ggridges)
library(ggplot2)
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(esquisse)
library(PASWR)
library(TSstudio)
library(caret)
library(rattle)
library(tseries)
library(forecast)
library(tsdl)
library(MLmetrics)
library(astsa)
library(dplyr)
library(stats)
library(devtools)
devtools::install_github("FinYang/tsdl")
library(tsdl)
library(foreign)
library(skimr)
library(ggfortify)
library(survMisc)
library(lubridate)
library(dplyr)
library(astsa)
library(fable)
library(tsibble)

##Alistamiento base de datos##
library(readxl)
View(pol_2018)
dim(pol_2018)
names(pol_2018)
str(pol_2018)
attach(pol_2018)
skimr::skim(pol_2018)
sum(is.na(pol_2018))
colSums(is.na(pol_2018))

##An?lisis descriptivo##
esquisse::esquisser(pol_2018)
ggplot(pol_2018) +
  aes(x = Months, y = Temperature_C) +
  geom_boxplot(aes(color = Months)) +
  geom_jitter(aes(color = Months), width = 0.15, alpha = 1/10) +
  theme_ggstatsplot() +
  theme(axis.text.x = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20))
kruskal.test(Temperature_C ~ Months, data = pol_2018)

my_breaks<- c(0, 4, 10, 14, 18, 22, 28)
ggplot(pol_2018, aes(x = Temperature_C, y = Military_hours, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 5, rel_min_height = 0.03) +
  theme_ggstatsplot() +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text.y = element_text(size = 15)) +
  theme(plot.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 18)) +
  scale_fill_viridis_c(name = "Temp. [°C]", option = "C", breaks = my_breaks) +
  labs(title = 'Temperatures in Bogotá-Suba, 2018')
kruskal.test(Temperature_C ~ Military_hours, data = pol_2018)

ggplot(pol_2018, aes(x = Temperature_C, y = Military_hours, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 5, rel_min_height = 0.03) +
  theme_ggstatsplot() +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text.y = element_text(size = 7)) +
  theme(axis.text = element_text(face="bold")) +
  theme(plot.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 18)) +
  scale_fill_viridis_c(name = "Temp. [°C]", option = "C", breaks = my_breaks) +
  facet_wrap(~Months, nrow = 4) +
  labs(title = 'Temperatures in Bogotá-Suba, 2018')

#Serie NO diferenciada, estad?stica descriptiva#
hist(Temperature_C)
ks.test(pol_2018$Temperature_C, "pnorm")
descr(pol_2018$Temperature_C)
var_pol_2018_nodiff<-(4.11)^2
var_pol_2018_nodiff
stby(Temperature_C, Months, FUN=descr)
tapply(Temperature_C, Months, quantile)
stby(Temperature_C, Military_hours, FUN=descr)
tapply(Temperature_C, Military_hours, quantile)

##Serie de tiempo NO diferenciada, frecuencia 24_horas##
serie_pol_24<-ts(pol_2018$Temperature_C, start=0, end =23, frequency = 24)
par(mfrow=c(1,1))
plot.ts(serie_pol_24)
abline(h=15, col="red")
adf.test(serie_pol_24) #No requiere diferenciaci?n por estacionariedad, pero aparentemente si por periodicidad#
ndiffs(serie_pol_24)
decomp_ts_pol_24 = stl(serie_pol_24, s.window = "p")
plot(decomp_ts_pol_24)
fit_serie_pol_24 <- ts(pol_2018$Temperature_C, start=0, end =23, frequency = 24)
fit_serie_pol_24
checkresiduals(fit_serie_pol_24)

#Exploraci?n de posible modelo, serie NO diferenciada#
acf(serie_pol_24, lag.max =24, plot =T)
pacf(serie_pol_24, lag.max =24, plot =T)
ts_decompose(serie_pol_24)
ts_cor(serie_pol_24)

##Serie de tiempo diferenciada para mejorar "seasonal", frecuencia 24_horas##
serie_pol_24_diff<-diff(serie_pol_24, 1)
par(mfrow=c(1,1))
plot.ts(serie_pol_24_diff)
descr(serie_pol_24_diff)
var_serie_pol_24_diff<-(1.67)^2
var_serie_pol_24_diff
ks.test(serie_pol_24_diff, "pnorm")
adf.test(serie_pol_24_diff)
ndiffs(serie_pol_24_diff)
decomp_ts_pol_24_diff = stl(serie_pol_24_diff, s.window = "p")
plot(decomp_ts_pol_24_diff)
#Exploraci?n de posible modelo serie diferenciada#
acf(serie_pol_24_diff, lag.max =24, plot =T)
pacf(serie_pol_24_diff, lag.max =24, plot =T)
ts_decompose(serie_pol_24_diff)
ts_cor(serie_pol_24_diff)

##set entrenamiento, prueba= diferenciada, 1##
Train_pol_24_diff<-window(serie_pol_24_diff, start = 0, end = 17)
valid_pol_24_diff<-window(serie_pol_24_diff, start = 18)

descr(Train_pol_24_diff)
var_Train_pol_24_diff<-(1.67)^2
Train_pol_24_diff
ks.test(Train_pol_24_diff, "pnorm")
adf.test(Train_pol_24_diff)
ndiffs(Train_pol_24_diff)
decomp_ts_pol_24_diff_train = stl(Train_pol_24_diff, s.window = "p")
plot(decomp_ts_pol_24_diff_train)
ggAcf(Train_pol_24_diff)
ggAcf(Train_pol_24_diff, lag=48)
pacf(Train_pol_24_diff)
ts_decompose(Train_pol_24_diff)
ts_cor(Train_pol_24_diff)

##AutoARIMA- Train, Serie diferenciada##
autoARM_Train_diff<-auto.arima(Train_pol_24_diff, trace = T)
autoARM_Train_diff<-arima(Train_pol_24_diff, order=c(2,0,2), seasonal = list(order=c(1,0,1), frequency=24))
plot(autoARM_Train_diff)
fit <- Arima(Train_pol_24_diff, order=c(2,0,2), seasonal = list(order=c(1,0,1), frequency=24))
fit
checkresiduals(fit)
residuals_fit<-residuals(fit)
autoplot(forecast(fit))

##AutoARIMA- Train, Serie diferenciada; modelo 1-0-3##
autoARM_Train_diff_nnn<-arima(Train_pol_24_diff, order=c(1,0,3), seasonal = list(order=c(1,0,1), frequency=24))
plot(autoARM_Train_diff_nnn)
fit_nnn <- Arima(Train_pol_24_diff, order=c(1,0,3), seasonal = list(order=c(1,0,1), frequency=24))

fit_nnn
checkresiduals(fit_nnn)
residuals_fit<-residuals(fit_nnn)
autoplot(forecast(fit_nnn))

Train_autoARM <- window(Train_pol_24_diff, order=c(2,0,2), seasonal = list(order=c(1,0,1), frequency=24))
Train_autoARM_fit1 <- meanf(Train_autoARM,h=72)
Train_autoARM_fit2 <- rwf(Train_autoARM,h=72)
Train_autoARM_fit3 <- snaive(Train_autoARM,h=72)
autoplot(window(Train_pol_24_diff, start=0)) +
  autolayer(Train_autoARM_fit1, series="Mean", PI=FALSE) +
  autolayer(Train_autoARM_fit2, series="Na?ve", PI=FALSE) +
  autolayer(Train_autoARM_fit3, series="Seasonal na?ve", PI=FALSE) +
  xlab("Hours") + ylab("Temperature[?C]") +
  ggtitle("Temperatures in Bogot?-Suba, 2018") +
  guides(colour=guide_legend(title="Forecast"))

summary(Train_autoARM_fit1 <- meanf(Train_autoARM,h=72))
summary(Train_autoARM_fit2 <- rwf(Train_autoARM,h=72))
summary(Train_autoARM_fit3 <- snaive(Train_autoARM,h=72))

##Naive model##
naive = snaive(Train_pol_24_diff, h=length(valid_pol_24_diff))
MAPE(naive$mean, valid_pol_24_diff)
RMSE(naive$mean, valid_pol_24_diff)
plot(Train_pol_24_diff, col="blue", xlab="Hours", ylab="Temperature [?C]", main="Temperature [?C]", type='l')
lines(naive$mean, col="red", lwd=2)

##Otro m?todo para forecast##
ggsubseriesplot(diff(Train_pol_24_diff,1)) + 
  ggtitle("Subseries Plot")
model = auto.arima(Train_pol_24_diff,d = 0,seasonal = TRUE, stepwise = TRUE, ic = "aic")
frcst = forecast(model,h = 72)
plot(frcst)
plot(model$residuals)
ts.plot(model$x, model$fitted, col=1:2,gpars = list(xlab = "Hours",ylab = "Temperature[?C]",main = "Real v/s Fitted values"))


##ARIMA ?ptimo##
arima_optimal = auto.arima(Train_pol_24_diff, trace=T)
sarima_forecast = sarima.for(Train_pol_24_diff, n.ahead=length(valid_pol_24_diff),p=2,d=0,q=2,P=1,D=0,Q=1,S=72)
MAPE(sarima_forecast$pred, valid_pol_24_diff)
plot(serie_pol_24_diff, col="blue", xlab="Hours", ylab="Temperature [?C]", main="SARIMA", type='l')
lines(sarima_forecast$pred, col="red", lwd=2)

##ETS mmodel##
ets_model = ets(Train_pol_24_diff, allow.multiplicative.trend = TRUE)
summary(ets_model)
ets_forecast = forecast(ets_model, h=length(valid_pol_24_diff))
MAPE(ets_forecast$mean, valid_pol_24_diff)
RMSE(ets_forecast$mean, valid_pol_24_diff)
plot(Train_pol_24_diff, col="blue", xlab="Hours", ylab="Temperature [?C]", main="Seasonal Exponential Smoot", type='l')
lines(ets_forecast$mean, col="red", lwd=2)
Train_pol_24_diff %>% ets() %>% forecast() %>% autoplot()

##Double-Seasonal Holt-Winters Forecasting##
HW1 <- HoltWinters(Train_pol_24_diff)
HW2 <- HoltWinters(Train_pol_24_diff, alpha=0.2, beta=0.1, gamma=0.1)
plot(Train_pol_24_diff, ylab="Temperature [?C]", xlim=c(0,17))
lines(HW1$fitted[,1], lty=2, col="blue")
lines(HW2$fitted[,1], lty=2, col="red")
HW1.pred_train <- predict(HW1, 36, prediction.interval = TRUE, level=0.95)
plot(Train_pol_24_diff, ylab="Temperature [?C]", xlim=c(0,20))
lines(HW1$fitted[,1], lty=2, col="blue")
lines(HW1.pred_train[,1], col="red")
lines(HW1.pred_train[,2], lty=2, col="brown")
lines(HW1.pred_train[,3], lty=2, col="brown")

HW1_v <- HoltWinters(valid_pol_24_diff)
HW2_v <- HoltWinters(valid_pol_24_diff, alpha=0.2, beta=0.1, gamma=0.1)
plot(valid_pol_24_diff, ylab="Temperature [?C]", xlim=c(19,23))
lines(HW1_v$fitted[,1], lty=2, col="blue")
lines(HW2_v$fitted[,1], lty=2, col="red")
HW1.pred_valid <- predict(HW1_v, 36, prediction.interval = TRUE, level=0.95)
plot(valid_pol_24_diff, ylab="Temperature [?C]", xlim=c(19,24))
lines(HW1_v$fitted[,1], lty=2, col="blue")
lines(HW1.pred_valid[,1], col="red")
lines(HW1.pred_valid[,2], lty=2, col="brown")
lines(HW1.pred_valid[,3], lty=2, col="brown")