#################################################
# MODELOS DE PROYECCIONES
#################################################
# CARTERA ACUMULADA- SALDOS

setwd("C:/1 Pasivos")
datos <- read.csv("carteraacumulada.csv" , sep = ";", dec="." )
names(datos)
View(datos)

y = ts(datos$V18, frequency = 12, start = c(2013,07))
fechas = seq(as.Date("2013/7/1"), length.out = length(y), by = "months")

ts.plot(y)

# Muestra de a??os para prueba y predicci??n
T =length(y)
yi = y[1:(T-12)]
yf = y[(T-12+1):T]

# Ajustar 4 modelos: lineal, cuadr??atico, c??ubico, log-lin

t = seq(1:(T-12))
t2 = t^2
t3 = t^3
lyi = log(yi)

mod.lin = lm(yi ~ t)          # modelo lineal
mod.cuad = lm(yi ~ t + t2)    # modelo cuadr??tico
mod.cub = lm(yi ~ t+ t2 +t3)  # modelo cubico
mod.llin = lm(lyi ~ t)        # modelo log-lineal
# modelo exponencial
bo.est=mod.llin$coefficients[1]
b1.est=mod.llin$coefficients[2]
Ds = data.frame(yi,t)
mod.exp = nls( yi ~ exp(beta0+beta1*t), 
             data=Ds, 
             start= list(beta0=bo.est , beta1=b1.est))

summary(mod.lin)
summary(mod.cuad)
summary(mod.cub)
summary(mod.exp)

# Medida para elegir el modelo 
medidas = function(m,y,k){
                T = length(y)
                yest = fitted(m)
                sse = sum((yest-y)^2)
                ssr = sum((y-mean(y))^2)
                mse = sse/(T-k)
                R2 = 1 - sse/ssr
                Ra2 = 1 - (T-1)*(1-R2)/(T-k)
                aic = log((T-k)*exp(2*k/T)*mse/T)
                bic = log(T^(k/T)*(T-k)*mse/T)
                M = c(Ra2, mse, aic, bic)
                names(M) = c("R2-ajus","MSE","logAIC","logBIC")
                return(M)}

M.lin = medidas(mod.lin,yi,2)
M.cuad = medidas(mod.cuad,yi,3)
M.cub = medidas(mod.cub,yi,4)
M.exp = medidas(mod.exp,yi,2)
M = cbind(M.lin,M.cuad,M.cub,M.exp)
M

# Chequeo de las Hip??otesis del Modelo de Regresi ??on
# chequeo de residuos del modelo

r = mod.lin$residuals  # cambiar este modelo para verificar por resultados.

par(mfrow=c(2,2))
plot(t,r,type='o',ylab='residuo')
abline(h=0,lty=2)
plot(density(r),xlab='x',main= '')
qqnorm(r)
qqline(r,col=2)
acf(r,ci.type="ma",60)

# Pronosticos
tt = seq(45,56,1)
tt2 = tt^2
tt3 = tt^3
ttl = log(tt)  
  
pr2 = predict(mod.cuad,data.frame(t=tt,t2=tt2))
pr3 = predict(mod.cub,data.frame(t=tt,t3=tt3))
prl = predict(mod.lin,data.frame(t=tt,tl=ttl))


length(tt)
length(yf)
plot(tt,yf,type="b")
lines(tt,pr2,col="red")
plot(ttl,yf,type="b")
lines(ttl,pr2,col="red")
par(mfrow=c(1,1))

descomp1 = decompose(y) # descomposici??n de factores de la serie
plot(descomp1)
# View(descomp1$seasonal)

# Analisis de gr??ficos de correlaciones parciales
par(mfrow=c(3,2))
acf(y,48)
pacf(y,48)
ydif = diff(y)
#plot(ydif)
acf(ydif,48)
pacf(ydif,48)
acf(diff(diff(y)),12)
pacf(diff(diff(y)),12)

#View(ydif)

# Modelos arima
#library(forecast)

ajuste_manual=arima(y,order=c(1,1,0))
ajuste_manual
tsdiag(ajuste_manual)
prediccion = as.data.frame(predict(ajuste_manual,n.ahead=12))
prediccion$yest=exp(prediccion$pred)
View(prediccion_1)

ajuste_manual_1=arima(y,order=c(1,1,1))
ajuste_manual_1
tsdiag(ajuste_manual_1)
prediccion_1= as.data.frame(predict(ajuste_manual_1,n.ahead=12))
prediccion_1$yest=exp(prediccion_1$pred)

ajuste_manual_2=arima(y,order=c(1,1,2))
ajuste_manual_2
tsdiag(ajuste_manual_2)
prediccion_2= as.data.frame(predict(ajuste_manual_2,n.ahead=12))
prediccion_2$yest=exp(prediccion_2$pred)

ajuste_manual_3=arima(y,order=c(2,1,0),seasonal = list(order=c(1,1,0),period=12))
ajuste_manual_3
tsdiag(ajuste_manual_3)
prediccion_3= as.data.frame(predict(ajuste_manual_3,n.ahead=12))
View(prediccion_3)

ajuste_manual_4=arima(y,order=c(1,1,0),seasonal = list(order=c(1,1,0),period=12))
ajuste_manual_4
tsdiag(ajuste_manual_4)
prediccion_4= as.data.frame(predict(ajuste_manual_4,n.ahead=12))
View(prediccion_4)


residuos_3=as.data.frame(y+ajuste_manual_3$residuals)
View(residuos_3)

residuos_4=as.data.frame(y+ajuste_manual_4$residuals)
View(residuos_4)

# trabajando sin la serie estacional
yest = y - descomp1$seasonal
par(mfrow=c(3,2))
acf(yest,48)
pacf(yest,48)
acf(diff(yest),48)
pacf(diff(yest),48)
acf(diff(diff(yest)),12)
pacf(diff(diff(yest)),12)

ajuste_manual_5=arima(yest,order=c(1,1,0),seasonal = list(order=c(0,0,0),period=12))
ajuste_manual_5
tsdiag(ajuste_manual_5)
prediccion_5= as.data.frame(predict(ajuste_manual_5,n.ahead=12))
View(prediccion_5)

ajuste_manual_6=arima(yest,order=c(1,1,0),seasonal = list(order=c(1,1,0),period=12))
ajuste_manual_6
tsdiag(ajuste_manual_6)
prediccion_6= as.data.frame(predict(ajuste_manual_6,n.ahead=12))
View(prediccion_6)

residuos_6=as.data.frame(y+ajuste_manual_6$residuals)
View(residuos_6)

par(mfrow=c(1,1))

###############################################################################
# SALDOS NUEVOS
###############################################################################
datos1 <- read.csv("saldosnuevos.csv", sep = ";")
summary(datos1)
str(datos1)

x = ts(datos1$V18, frequency = 12, start = c(2013,07))
fechas = seq(as.Date("2013/7/1"), length.out = length(y), by = "months")

ts.plot(x)
descomp2 = decompose(x) # descomposici??n de factores de la serie
plot(descomp2)
descomp2$seasonal
# Analisis de gr??ficos de correlaciones parciales
par(mfrow=c(3,2))
acf(x,48)
pacf(x,48)
acf(diff(x),48)
pacf(diff(x),48)
acf(diff(diff(x)),12)
pacf(diff(diff(x)),12)

ajuste_manual=arima(x,order=c(1,1,0))
ajuste_manual
tsdiag(ajuste_manual)
prediccion_x = as.data.frame(predict(ajuste_manual,n.ahead=12))
View(prediccion_x)

ajuste_manual_1=arima(x,order=c(1,1,1))
ajuste_manual_1
tsdiag(ajuste_manual_1)
prediccion_x_1= as.data.frame(predict(ajuste_manual_1,n.ahead=12))
View(prediccion_x_1)


ajuste_manual_2=arima(x,order=c(1,1,2))
ajuste_manual_2
tsdiag(ajuste_manual_2)
prediccion_x_2= as.data.frame(predict(ajuste_manual_2,n.ahead=12))
View(prediccion_x_2)


ajuste_manual_3=arima(x,order=c(2,1,0),seasonal = list(order=c(1,1,0),period=12))
ajuste_manual_3
tsdiag(ajuste_manual_3)
prediccion_x_3= as.data.frame(predict(ajuste_manual_3,n.ahead=12))
View(prediccion_x_3)

ajuste_manual_4=arima(x,order=c(1,1,0),seasonal = list(order=c(1,1,0),period=12))
ajuste_manual_4
tsdiag(ajuste_manual_4)
prediccion_x_4= as.data.frame(predict(ajuste_manual_4,n.ahead=12))
View(prediccion_x_4)

residuos_3=as.data.frame(x+ajuste_manual_3$residuals)
View(residuos_3)

residuos_4=as.data.frame(x+ajuste_manual_4$residuals)
View(residuos_4)

##################################################################
ajuste_manual_7_log=arima(log(xest),order=c(1,1,2),seasonal = list(order=c(1,1,0),period=12))
ajuste_manual_7_log
tsdiag(ajuste_manual_7_log)
prediccion_x_7_log= as.data.frame(predict(ajuste_manual_7_log,n.ahead=12))
View(prediccion_x_7_log)

residuos_7_log=as.data.frame(log(xest)+ajuste_manual_7_log$residuals)
View(residuos_7_log)
RSME= sum(((residuos_7_log$x - log(xest))^2))/length(x)
RSME*100
#################################################################

# trabajando sin la serie estacional
xest = x - descomp2$seasonal
par(mfrow=c(3,2))
acf(xest,48)
pacf(xest,48)
acf(diff(xest),48)
pacf(diff(xest),48)
acf(diff(diff(xest)),12)
pacf(diff(diff(xest)),12)

ajuste_manual_5=arima(xest,order=c(1,1,0),seasonal = list(order=c(1,1,0),period=12))
ajuste_manual_5
tsdiag(ajuste_manual_5)
prediccion_x_5= as.data.frame(predict(ajuste_manual_5,n.ahead=12))
View(prediccion_x_5)

ajuste_manual_6=arima(xest,order=c(1,1,0),seasonal = list(order=c(1,1,1),period=12))
ajuste_manual_6
tsdiag(ajuste_manual_6)
prediccion_x_6= as.data.frame(predict(ajuste_manual_6,n.ahead=12))
View(prediccion_x_6)

ajuste_manual_7=arima(xest,order=c(1,1,2),seasonal = list(order=c(1,1,0),period=12))
ajuste_manual_7
tsdiag(ajuste_manual_7)
prediccion_x_7= as.data.frame(predict(ajuste_manual_7,n.ahead=12))
View(prediccion_x_7)


residuos_7=as.data.frame(x+ajuste_manual_7$residuals)
View(residuos_7)

residuos_6=as.data.frame(x+ajuste_manual_6$residuals)
View(residuos_6)

par(mfrow=c(1,1))

###############################################################################
# Analisis de series Ctas Acumuladas
###############################################################################
# Ctas Corrientes
z = ts(datos$V7, frequency = 12, start = c(2013,07))
descomp_Cta_corrientes = decompose(z) # descomposici??n de factores de la serie
plot(descomp_Cta_corrientes)

z_natural = ts(datos$V5, frequency = 12, start = c(2013,07))
descomp_z_natural = decompose(z_natural) # descomposici??n de factores de la serie
plot(descomp_z_natural)

z_juridica = ts(datos$V6, frequency = 12, start = c(2013,07))
descomp_z_juridica = decompose(z_juridica) # descomposici??n de factores de la serie
plot(descomp_z_juridica)

# Ctas ahorro
w = ts(datos$V10, frequency = 12, start = c(2013,07))
descomp_Cta_ahorros = decompose(w) # descomposici??n de factores de la serie
plot(descomp_Cta_ahorros)

w_natural = ts(datos$V8, frequency = 12, start = c(2013,07))
descomp_w_natural = decompose(w_natural) # descomposici??n de factores de la serie
plot(descomp_w_natural)

w_juridica = ts(datos$V9, frequency = 12, start = c(2013,07))
descomp_w_juridica = decompose(w_juridica) # descomposici??n de factores de la serie
plot(descomp_w_juridica)

# Miscelaneos
v = ts(datos$V2, frequency = 12, start = c(2013,07))
descomp_crecer = decompose(v) # descomposici??n de factores de la serie
plot(descomp_crecer)

u = ts(datos$V4, frequency = 12, start = c(2013,07))
descomp_acumulacion = decompose(u) # descomposici??n de factores de la serie
plot(descomp_acumulacion)

t = ts(datos$V13, frequency = 12, start = c(2013,07))
descomp_dpf = decompose(t) # descomposici??n de factores de la serie
plot(descomp_dpf)

t_natural = ts(datos$V11, frequency = 12, start = c(2013,07))
descomp_dpf_natural = decompose(t_natural) # descomposici??n de factores de la serie
plot(descomp_dpf_natural)

t_juridico = ts(datos$V12, frequency = 12, start = c(2013,07))
descomp_dpf_juridico = decompose(t_juridico) # descomposici??n de factores de la serie
plot(descomp_dpf_juridico)


###############################################################################
# Analisis de series Ctas Nuevas
###############################################################################
# Ctas Corrientes
zx = ts(datos1$V7, frequency = 12, start = c(2013,07))
descomp_Cta_corrientes_x = decompose(zx) # descomposici??n de factores de la serie
plot(descomp_Cta_corrientes_x)

zx_natural = ts(datos1$V5, frequency = 12, start = c(2013,07))
descomp_z_natural_x = decompose(zx_natural) # descomposici??n de factores de la serie
plot(descomp_z_natural_x)

zx_juridica = ts(datos1$V6, frequency = 12, start = c(2013,07))
descomp_z_juridica_x = decompose(zx_juridica) # descomposici??n de factores de la serie
plot(descomp_z_juridica_x)

# Ctas ahorro
wx = ts(datos1$V10, frequency = 12, start = c(2013,07))
descomp_Cta_ahorros_x = decompose(wx) # descomposici??n de factores de la serie
plot(descomp_Cta_ahorros_x)

wx_natural = ts(datos1$V8, frequency = 12, start = c(2013,07))
descomp_w_natural_x = decompose(wx_natural) # descomposici??n de factores de la serie
plot(descomp_w_natural_x)

wx_juridica = ts(datos1$V9, frequency = 12, start = c(2013,07))
descomp_w_juridica_x = decompose(wx_juridica) # descomposici??n de factores de la serie
plot(descomp_w_juridica_x)

# Miscelaneos
vx = ts(datos1$V2, frequency = 12, start = c(2013,07))
descomp_crecer_x = decompose(vx) # descomposici??n de factores de la serie
plot(descomp_crecer_x)

u = ts(datos$V4, frequency = 12, start = c(2013,07))
descomp_acumulacion = decompose(u) # descomposici??n de factores de la serie
plot(descomp_acumulacion)

tx = ts(datos1$V13, frequency = 12, start = c(2013,07))
descomp_dpf_x = decompose(tx) # descomposici??n de factores de la serie
plot(descomp_dpf_x)

tx_natural = ts(datos1$V11, frequency = 12, start = c(2013,07))
descomp_dpf_natural_x = decompose(tx_natural) # descomposici??n de factores de la serie
plot(descomp_dpf_natural_x)

tx_juridico = ts(datos1$V12, frequency = 12, start = c(2013,07))
descomp_dpf_juridico_x = decompose(tx_juridico) # descomposici??n de factores de la serie
plot(descomp_dpf_juridico_x)

###########################################################################
# Modelando la variable saldos nuevos pero acumulados
###########################################################################

xi = ts(datos1$V19, frequency = 12, start = c(2013,07))
ts.plot(xi)
descomp_xi = decompose(xi) # descomposici??n de factores de la serie
plot(descomp_xi)
#descomp2$seasonal
# Analisis de gr??ficos de correlaciones parciales
par(mfrow=c(3,2))
acf(xi,48)
pacf(xi,48)
acf(diff(xi),48)
pacf(diff(xi),48)
acf(diff(diff(xi)),12)
pacf(diff(diff(xi)),12)

ajuste_manual_1_xi=arima(xi,order=c(1,1,1))
ajuste_manual_1_xi
tsdiag(ajuste_manual_1_xi)
prediccion_xi_1= as.data.frame(predict(ajuste_manual_1_xi,n.ahead=12))
View(prediccion_xi_1)

ajuste_manual_1_xi_log=arima(log(xi),order=c(1,1,1))
residuos_1_log=as.data.frame(log(xi)+ajuste_manual_1_xi_log$residuals)
RSME= sum(((residuos_1_log$x - log(xi))^2))/length(xi)
RSME*100
#################################################################
#################################################################
# MERCADO
#################################################################
#################################################################
datos2 <- read.csv("mercado.csv", sep=";")
names(datos2)
str(datos2)
library(ClustOfVar)
library(cluster)
library(reshape2)
library(plyr)

# Usando el m??todo de clasificaci??n jer??rquica para unificar comportamientos
tree <- hclustvar(datos2[,-c(1,9:11,58)])
names(tree)
plot.new()
plot(tree)
groupsV <- cutree(tree, k=8)
rect.hclust(tree, k=8, border="red")
stab <- stability(tree,B=10)
write.csv(groupsV,"clustervariables.csv")

variables_cluster <- read.csv("clustervariables.csv" , sep=",")
View(variables_cluster)

datos <- read.csv("carteraacumulada.csv" , sep = ";", dec="." )


# correlacionando variables
length(datos$V18)
length(datos2$v1)

library(psych)
datos_corr <- cbind(datos2[7:55,],datos[1:49,19])
plot.new()
pairs.panels(datos_corr[,c(2:10,65)])
pairs.panels(datos_corr[,c(11:20,65)])
pairs.panels(datos_corr[,c(21:30,65)])
pairs.panels(datos_corr[,c(31:40,65)])
pairs.panels(datos_corr[,c(41:50,65)])
pairs.panels(datos_corr[,c(51:60,65)])
pairs.panels(datos_corr[,c(61:65)])

# graficando las variables con mayor correlacion con la respuesta
library(corrplot)
names(datos_corr)
corr_matrix <- cor(datos_corr[c(65,3,4,13,14,17,19,24,
                              28,32,36,40,43,45,47,48,49,50,56)])
corrplot(corr_matrix, method = "color",type="upper", order = "original")

datos3 <- datos_corr[c(1,65,3,4,13,14,17,19,24,
                       28,32,36,40,43,45,47,48,49,50,56)]
names(datos3)[2] <- "pasivos"
str(datos3)

# Verificando las mejores variables para explicar

library(MASS)
fit <- lm(pasivos~., data=datos3[-1] )
model01 <- stepAIC( fit, direction = "both") 
summary(model01)
par(mfrow=c(2,2))
plot(model01)
par(mfrow=c(1,1))

# Carga de archivo datos3
write.csv(datos3,"datos3.csv")
datos3 <- read.csv("datos3.csv",sep=",")

# Convirtiendo los datos en series
y = ts(datos3$pasivos, frequency = 12, start = c(2013,07))
fechas = seq(as.Date("2013/7/1"), length.out = length(y), by = "months")
ts.plot(y)
descom_pasivos <- decompose(y)
plot(descom_pasivos)

x1 = ts(datos3$v31, frequency = 12, start = c(2013,07))
fechas = seq(as.Date("2013/7/1"), length.out = length(y), by = "months")
ts.plot(x1)
descom_x1 <- decompose(x1)
plot(descom_x1)

# Creando variables dummy
dummy <- as.data.frame(descom_pasivos$seasonal)
d7 <- ifelse(round(dummy$x)== 2099675,1,0)
d8 <- ifelse(round(dummy$x)== 7414230,1,0)
d9 <- ifelse(round(dummy$x)== -5984551,1,0)
d10 <- ifelse(round(dummy$x)== -4700888,1,0)
d11 <- ifelse(round(dummy$x)== -5218966,1,0)
d12 <- ifelse(round(dummy$x)== 9023306,1,0)
d1 <- ifelse(round(dummy$x)== 6497734,1,0)
d2 <- ifelse(round(dummy$x)== 7639119,1,0)
d3 <- ifelse(round(dummy$x)== -2346559,1,0)
d4 <- ifelse(round(dummy$x)== -3421483,1,0)
d5 <- ifelse(round(dummy$x)== -3329419,1,0)
d6 <- ifelse(round(dummy$x)== -7672197,1,0)


library(dlm)
library(tseries)
library(dynlm)

print(adf1 <- adf.test(datos3$pasivos, 'stationary', 
                       k = trunc((length(datos3$pasivos-1)^(1/3)))))

print(adf2 <- adf.test(datos3$v31, 'stationary', 
                       k = trunc((length(datos3$V2-1)^(1/3)))))


# Como ambos valores del estadistico no son mayores a 3.5
# ambas variables no son estacionarias

######################################################################
# Modelo con V31: Depostios - Dep??sitos de particulares
######################################################################

model02 <- dynlm( y ~ d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 + d11 + d12+
                          L(y,1) + L(y,2) +
                      x1 + L(x1,1) + L(x1,2) + L(x1,3) + L(x1,4) +
                           L(x1,5) + L(x1,6) + L(x1,7) + L(x1,8) + L(x1,9) +
                           L(x1,10) + L(x1,11) + L(x1,12)  
                 )

print(ADL.summary <- summary(model02))
par(mfrow=c(2,2))
plot(model02)
par(mfrow=c(1,1))
names(model02)
plot(y,col="red")
lines(model02$fitted.values, col="blue")


# Modelo limpiando t??rminos no significativos

model021 <- dynlm( y ~ d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 + d11 + d12+
                     L(y,1) + x1 + L(x1,5) + L(x1,9) + L(x1,11)  
)

model021 <- dynlm( y ~ d9 + d12+
                     L(y,1) + x1 + L(x1,5) + L(x1,9) + L(x1,11)  
)

                  
print(ADL.summary <- summary(model021))
par(mfrow=c(2,2))
plot(model021)
par(mfrow=c(1,1))
names(model021)
plot(y,col="red")
lines(model021$fitted.values, col="blue")
par(mfrow=c(2,2))
ei <- model021$residuals
plot(ei)
acf(ei)
pacf(ei)
hist(ei)

View(model021$fitted.values)

######################################################################
# Modelo con V35: Total Activos Dep??sitos de Bancos
######################################################################

x2 = ts(datos3$v35, frequency = 12, start = c(2013,07))
fechas = seq(as.Date("2013/7/1"), length.out = length(y), by = "months")
descom_x2 <- decompose(x2)
plot(descom_x2)

print(adf3 <- adf.test(datos3$v35, 'stationary', 
                       k = trunc((length(datos3$V35-1)^(1/3)))))


model03 <- dynlm( y ~ d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 + d11 + d12+
                    L(y,1) + L(y,2) +
                    x2 + L(x2,1) + L(x2,2) + L(x2,3) + L(x2,4) +
                    L(x2,5) + L(x2,6) + L(x2,7) + L(x2,8) + L(x2,9) +
                    L(x2,10) + L(x2,11) + L(x2,12)  
)

print(ADL.summary <- summary(model03))
par(mfrow=c(2,2))
plot(model03)
par(mfrow=c(1,1))
names(model02)
plot(y,col="red")
lines(model03$fitted.values, col="blue")

# Modelo limpiando t??rminos no significativos

model031 <- dynlm( y ~ d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 + d11 + d12+
                     L(y,1) + L(y,2) +
                     x2 + L(x2,1) + L(x2,3) + L(x2,8) + L(x2,9) +
                     L(x2,10) + L(x2,11) + L(x2,12)  
)

print(ADL.summary <- summary(model031))
par(mfrow=c(2,2))
plot(model031)
par(mfrow=c(1,1))
names(model021)
plot(y,col="red")
lines(model031$fitted.values, col="blue")
par(mfrow=c(2,2))
ei <- model031$residuals
plot(ei)
acf(ei)
pacf(ei)
hist(ei)

View(model031$fitted.values)

######################################################################
# Modelo con V42: Dep??sitos a plazo
######################################################################

x3 = ts(datos3$v42, frequency = 12, start = c(2013,07))
fechas = seq(as.Date("2013/7/1"), length.out = length(y), by = "months")
descom_x3 <- decompose(x3)
plot(descom_x3)

print(adf3 <- adf.test(datos3$v42, 'stationary', 
                       k = trunc((length(datos3$V42-1)^(1/3)))))


model04 <- dynlm( y ~ d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 + d11 + d12+
                    L(y,1) + L(y,2) +
                    x3 + L(x3,1) + L(x3,2) + L(x3,3) + L(x3,4) +
                    L(x3,5) + L(x3,6) + L(x3,7) + L(x3,8) + L(x3,9) +
                    L(x3,10) + L(x3,11) + L(x3,12)  
)

print(ADL.summary <- summary(model04))
par(mfrow=c(2,2))
plot(model04)
par(mfrow=c(1,1))
names(model02)
plot(y,col="red")
lines(model04$fitted.values, col="blue")

# Modelo limpiando t??rminos no significativos

model041 <- dynlm( y ~ d3 + d4 + d5 + d6 + d9 + d11 +
                     L(y,1) + L(x3,2) + L(x3,4) + L(x3,9)
                     
)


print(ADL.summary <- summary(model041))
par(mfrow=c(2,2))
plot(model041)
par(mfrow=c(1,1))
names(model041)
plot(y,col="red")
lines(model041$fitted.values, col="blue")
par(mfrow=c(2,2))
ei <- model041$residuals
plot(ei)
acf(ei)
pacf(ei)
hist(ei)

######################################################################
# Modelo con V18: Inversiones en Valores
######################################################################

x4 = ts(datos3$v18, frequency = 12, start = c(2013,07))
fechas = seq(as.Date("2013/7/1"), length.out = length(y), by = "months")
descom_x4 <- decompose(x4)
plot(descom_x4)

print(adf4 <- adf.test(datos3$v18, 'stationary', 
                       k = trunc((length(datos3$V18-1)^(1/3)))))


model05 <- dynlm( y ~ d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 + d11 + d12+
                    L(y,1) + L(y,2) +
                    x4 + L(x4,1) + L(x4,2) + L(x4,3) + L(x4,4) +
                    L(x4,5) + L(x4,6) + L(x4,7) + L(x4,8) + L(x4,9) +
                    L(x4,10) + L(x4,11) + L(x4,12)  
)

print(ADL.summary <- summary(model05))
par(mfrow=c(2,2))
plot(model05)
par(mfrow=c(1,1))
names(model02)
plot(y,col="red")
lines(model05$fitted.values, col="blue")

# Modelo limpiando t??rminos no significativos

model051 <- dynlm( y ~ d9 + d12 + L(y,1) + L(x4,4))

print(ADL.summary <- summary(model051))
par(mfrow=c(2,2))
plot(model051)
par(mfrow=c(1,1))
names(model041)
plot(y,col="red")
lines(model051$fitted.values, col="blue")
par(mfrow=c(2,2))
ei <- model051$residuals
plot(ei)
acf(ei)
pacf(ei)
hist(ei)

#########################################################################
# Carga de indice de produccion
#########################################################################
produccion <- read.csv("indiceproduccion.csv",sep=";")
View(produccion)

p1 = ts(produccion$??NDICE_original, frequency = 12, start = c(2013,07))
fechas = seq(as.Date("2013/7/1"), length.out = length(y), by = "months")
ts.plot(p1)
descom_p1 <- decompose(p1)
plot(descom_p1)

# Analisis de gr??ficos de correlaciones parciales
par(mfrow=c(3,2))
acf(p1,48)
pacf(p1,48)
acf(diff(p1),48)
pacf(diff(p1),48)
acf(diff(diff(p1)),12)
pacf(diff(diff(p1)),12)

##########################################################################
# Modelo Multivariante
##########################################################################
x5 = ts(datos3$v23, frequency = 12, start = c(2013,07))
fechas = seq(as.Date("2013/7/1"), length.out = length(y), by = "months")
x6 = ts(datos3$v27, frequency = 12, start = c(2013,07))
fechas = seq(as.Date("2013/7/1"), length.out = length(y), by = "months")
descomp_x6 <- decompose(x6)


model06 <- dynlm( y ~ d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 + d11 + d12+
                    L(y,1) + L(y,2) +
                    x1  + L(x1,1) + L(x1,2) + L(x1,3) + L(x1,4) + L(x1,5) + L(x1,6) + 
                    x2  + L(x2,1) + L(x2,2) + L(x2,3) + L(x2,4) + L(x2,5) + L(x2,6) +
                    x4  + L(x4,1) + L(x4,2) + L(x4,3) + L(x4,4) + L(x4,5) + L(x4,6) +
                    p1  + L(p1,1) + L(p1,2) + L(p1,3) + L(p1,4) + L(p1,5) + L(p1,6) 
                    )
                  
print(ADL.summary <- summary(model06))

model061 <- dynlm( y ~ d6 + d7 + d8 + d9 + d11 + 
                    L(y,1) + L(y,2) +
                          L(x1,1) + L(x1,5) +  
                    x2  + L(x2,4) + L(x2,5) + 
                    x4  +    
                          L(p1,1) + L(p1,3) + L(p1,6) )



print(ADL.summary <- summary(model061))
par(mfrow=c(2,2))
plot(model061)
par(mfrow=c(1,1))
#names(model041)
plot(y,col="red")
lines(model061$fitted.values, col="blue")
lines(model021$fitted.values, col="green")
lines(model031$fitted.values, col="grey")
lines(model041$fitted.values, col="black")
lines(model051$fitted.values, col="violet")

View(model061$fitted.values)

par(mfrow=c(2,2))
ei <- model061$residuals
plot(ei)
acf(ei)
pacf(ei)
hist(ei)

plot(descom_pasivos)
plot(descom_x1)
plot(descom_x2)
plot(descom_x4)
plot(descom_p1)


#########################################################################
# sustituyendo X2
###########################################################################

model07 <- dynlm( y ~ d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 + d11 + d12+
                    L(y,1) + L(y,2) +
                    x1  + L(x1,1) + L(x1,2) + L(x1,3) + L(x1,4) + L(x1,5) + L(x1,6) + 
                    x6  + L(x6,1) + L(x6,2) + L(x6,3) + L(x6,4) + L(x6,5) + L(x6,6) +
                    x4  + L(x4,1) + L(x4,2) + L(x4,3) + L(x4,4) + L(x4,5) + L(x4,6) +
                    p1  + L(p1,1) + L(p1,2) + L(p1,3) + L(p1,4) + L(p1,5) + L(p1,6) 
)

print(ADL.summary <- summary(model07))


model071 <- dynlm( y ~ d2 + d3 + d4 + d5 + d7 + d8 + d9 + d10 + d11 + d12+
                     L(y,1) + 
                     L(x1,4) + L(x1,5) + L(x1,6) + 
                     L(x6,1) + L(x6,5) + 
                     x4  + L(x4,1) + L(x4,2) + L(x4,4) + L(x4,5) + 
                     L(p1,1) + L(p1,2) + L(p1,3) + L(p1,5)   
)

print(ADL.summary <- summary(model071))
plot(y,col="red")
lines(model071$fitted.values, col="blue")
par(mfrow=c(2,2))
plot(model071)
View(model071$fitted.values)

par(mfrow=c(1,1))
plot(descomp_x6)

# http://www.capitalspectator.com/modeling-what-if-scenarios-with-impulse-response-simulations/
# http://r-forge.r-project.org/projects/vars/

library(vars)

datos3.1 <- cbind(y,x1,x2,x4,p1)
VARselect(datos3.1,lag.max = 6,type="const")
var_model061 <- VAR(datos3.1 , p=6, type="const")
summary(var_model061)
impulso <- irf(var_model061, impulse="y", cumulative = TRUE)
plot(impulso)
