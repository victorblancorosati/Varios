###############################################################
# Financial Modeling, Summer 2017
##############################################################

# https://rpubs.com/DanielSLee/ImportingManagingFinancialData

library("quantmod")
library("TTR")

myportafolio <- c("DD",	  "KO",	  "PG",	  "UNH",	"IBM",	"JPM",	"MRK",	"VZ",	
                  "AXP",	"XOM",	"DIS",	"TRV",	"V",	  "NKE",	"MCD",	"INTC",	
                  "CAT",	"MSFT",	"WMT",	"PFE",	"HD",	  "CSCO",	"CVX",	"GS",	
                  "JNJ",	"UTX",	"GE",	  "MMM",	"AAPL",	"MMM",	"ABT",	"ABBV",	
                  "ACN",	"ATVI",	"AYI",	"ADBE",	"AMD",	"AAP",	"AES",	"AET",	
                  "AMG",	"AFL",	"A",	  "APD",	"AKAM",	"ALK",	"ALB",	"ARE",	
                  "ALXN",	"ALGN",	"ALLE",	"AGN",	"ADS",	"LNT",	"ALL",	"GOOGL",	
                  "GOOG",	"MO",	  "AMZN",	"AEE",	"AAL",	"AEP",	"AXP",	"AIG",	
                  "AMT",	"AWK",	"AMP",	"ABC",	"AME",	"AMGN",	"APH",	"APC",	
                  "ADI",	"ANDV",	"ANSS",	"ANTM",	"AON",	"AOS",	"APA",	"AIV",	
                  "AAPL",	"AMAT",	"APTV",	"ADM",	"ARNC",	"AJG",	"AIZ",	"T",	
                  "ADSK",	"ADP",	"AZO",	"AVB",	"AVY",	"BHGE",	"BLL",	"BAC",
                  "BK",	  "BAX",	"BBT",	"BDX",	"BRK.B","BBY",	"BIIB",	"BLK",
                  "HRB",	"BA",	  "BWA",	"BXP",	"BSX",	"BHF",	"BMY",	"AVGO",
                  "BF.B",	"CHRW",	"CA",	  "COG",	"CDNS",	"CPB",	"COF",	"CAH",
                  "CBOE",	"KMX",	"CCL",	"CAT",	"CBG",	"CBS",	"CELG",	"CNC",
                  "CNP",	"CTL",	"CERN",	"CF",	  "SCHW",	"CHTR",	"CHK",	"CVX",
                  "CMG",	"CB",	  "CHD",	"CI",	  "XEC",	"CINF",	"CTAS",	"CSCO",
                  "C",	  "CFG",	"CTXS",	"CLX",	"CME",	"CMS",	"KO",	  "CTSH",
                  "CL",	  "CMCSA",	"CMA",	"CAG",	"CXO",	"COP","ED", "STZ",
                  "COO",	"GLW",	"COST",	"COTY",	"CCI",	"CSRA",	"CSX",	"CMI",
                  "CVS",	"DHI",	"DHR",	"DRI",	"DVA",	"DE",	  "DAL",	"XRAY",
                  "DVN",	"DLR",	"DFS",	"DISCA","DISCK","DISH",	"DG",	  "DLTR",
                  "D",	  "DOV",	"DWDP",	"DPS",	"DTE",	"DRE",	"DUK",	"DXC",
                  "ETFC",	"EMN",	"ETN",	"EBAY",	"ECL",	"EIX",	"EW",	  "EA",	
                  "EMR",	"ETR",	"EVHC",	"EOG",	"EQT",	"EFX",	"EQIX",	"EQR",
                  "ESS",	"EL",	  "ES",	  "RE",	  "EXC",	"EXPE",	"EXPD",	"ESRX",
                  "EXR",	"XOM",	"FFIV",	"FB",	  "FAST",	"FRT",	"FDX",	"FIS",
                  "FITB",	"FE",	  "FISV",	"FLIR",	"FLS",	"FLR",	"FMC",	"FL",
                  "F",	  "FTV",	"FBHS",	"BEN",	"FCX",	"GPS",	"GRMN",	"IT",
                  "GD",	  "GE",	  "GGP",	"GIS",	"GM",	  "GPC",	"GILD",	"GPN",
                  "GS",	  "GT",	  "GWW",	"HAL",	"HBI",	"HOG",	"HRS",	"HIG",
                  "HAS",	"HCA",	"HCP",	"HP",	  "HSIC",	"HSY",	"HES",	"HPE",
                  "HLT",	"HOLX",	"HD",	  "HON",	"HRL",	"HST",	"HPQ",	"HUM",
                  "HBAN",	"HII",	"IDXX",	"INFO",	"ITW",	"ILMN",	"IR",	  "INTC",
                  "ICE",	"IBM",	"INCY",	"IP",	  "IPG",	"IFF",	"INTU",	"ISRG",	
                  "IVZ",	"IQV",	"IRM",	"JEC",	"JBHT",	"SJM",	"JNJ",	"JCI",
                  "JPM",	"JNPR",	"KSU",	"K",	  "KEY",	"KMB",	"KIM",	"KMI",
                  "KLAC",	"KSS",	"KHC",	"KR",	  "LB",	  "LLL",	"LH",	  "LRCX",	
                  "LEG",	"LEN",	"LUK",	"LLY",	"LNC",	"LKQ",	"LMT",	"L",	
                  "LOW",	"LYB",	"MTB",	"MAC",	"M",	  "MRO",	"MPC",	"MAR",
                  "MMC",	"MLM",	"MAS",	"MA",	  "MAT",	"MKC",	"MCD",	"MCK",
                  "MDT",	"MRK",	"MET",	"MTD",	"MGM",	"KORS",	"MCHP",	"MU",
                  "MSFT",	"MAA",	"MHK",	"TAP",	"MDLZ",	"MON",	"MNST",	"MCO",
                  "MS",	  "MOS",	"MSI",	"MYL",	"NDAQ",	"NOV",	"NAVI",	"NTAP",
                  "NFLX",	"NWL",	"NFX",	"NEM",	"NWSA",	"NWS",	"NEE",	"NLSN",	
                  "NKE",	"NI",	  "NBL",	"JWN",	"NSC",	"NTRS",	"NOC",	"NCLH",
                  "NRG",	"NUE",	"NVDA",	"ORLY",	"OXY",	"OMC",	"OKE",	"ORCL",
                  "PCAR",	"PKG",	"PH",	  "PDCO",	"PAYX",	"PYPL",	"PNR",	"PBCT",
                  "PEP",	"PKI",	"PRGO",	"PFE",	"PCG",	"PM",	  "PSX",	"PNW",
                  "PXD",	"PNC",	"RL",	  "PPG",	"PPL",  "PX",	  "PCLN",	"PFG",
                  "PG",	  "PGR",	"PLD",	"PRU",	"PEG",	"PSA",	"PHM",	"PVH",
                  "QRVO",	"PWR",	"QCOM",	"DGX",	"RRC",	"RJF",	"RTN",	"O",
                  "RHT",	"REG",	"REGN",	"RF",	  "RSG",	"RMD",	"RHI",	"ROK",
                  "COL",	"ROP",	"ROST",	"RCL",	"CRM",	"SBAC",	"SCG",	"SLB",
                  "SNI",	"STX",	"SEE",	"SRE",	"SHW",	"SIG",	"SPG",	"SWKS",
                  "SLG",	"SNA",	"SO",	  "LUV",	"SPGI",	"SWK",	"SBUX",	"STT",
                  "SRCL",	"SYK",	"STI",	"SYMC",	"SYF",	"SNPS",	"SYY",	"TROW",
                  "TPR",	"TGT",	"TEL",	"FTI",	"TXN",	"TXT",	"TMO",	"TIF",
                  "TWX",	"TJX",	"TMK",	"TSS",	"TSCO",	"TDG",	"TRV",	"TRIP",
                  "FOXA",	"FOX",	"TSN",	"UDR",	"ULTA",	"USB",	"UAA",	"UA",	
                  "UNP",	"UAL",	"UNH",	"UPS",	"URI",	"UTX",	"UHS",	"UNM",
                  "VFC",	"VLO",	"VAR",	"VTR",	"VRSN",	"VRSK",	"VZ",	"VRTX",	
                  "VIAB",	"V",	  "VNO",	"VMC",	"WMT",	"WBA",	"DIS",	"WM",
                  "WAT",	"WEC",	"WFC",	"HCN",	"WDC",	"WU",	  "WRK",	"WY",	
                  "WHR",	"WMB",	"WLTW",	"WYN",	"WYNN",	"XEL",	"XRX",	"XLNX",
                  "XL",	"XYL",	"YUM",	"ZBH",	"ZION",	"ZTS")

ls.str()

data <- data.frame(as.xts(merge(A,	C,	V,	D,	F,	K,	T,	BK,	
                                CB,	DE,	DG,	ED,	ES,	EW,	FL,	GE,	
                                GS,	IP,	IT,	KR,	LH,	MO,	PG,	RE,	
                                VZ,	BA,	CA,	CF,	CI,	CL,	DD,	EA,	
                                EL,	FB,	FE,	GD,	GM,	GT,	HD,	HP,	
                                IR,	KO,	LB,	AAP,	ABT,	ADM,	ADS,	
                                AEE,	AES,	AFL,	AIG,	AIZ,	ALL,	AMD,	AMG,
                                AMP,	AON,	APA,	APD,	AVB,	AVY,	AXP,	AZO,
                                BAC,	BBT,	BDX,	BLL,	BWA,	CAG,	CCI,	CFG,
                                CHK,	CLX,	CMG,	CMS,	CNP,	COG,	COP,	CPB,
                                CVS,	CXO,	DAL,	DHR,	DPS,	DRI,	DUK,	DVN,
                                DXC,	EIX,	EMN,	EOG,	EQR,	ESS,	ETN,	EXR,
                                FDX,	FLR,	FMC,	FTV,	GLW,	GPC,	GPS,	GWW,
                                HAS,	HBI,	HCP,	HES,	HII,	HOG,	HON,	HPE,
                                HRB,	HRS,	HST,	HUM,	ICE,	IFF,	IQV,	IRM,
                                IVZ,	JCI,	JNJ,	JPM,	KEY,	KIM,	KMB,	KMX,
                                KSU,	LEG,	LLL,	LNC,	MCD,	NKE,	STZ,	TRV,
                                UNH,	XEC,	AAL,	ABC,	ACN,	ADI,	ADP,	AEP,
                                AET,	AGN,	AIV,	AJG,	ALB,	ALK,	AME,	AMT,
                                AOS,	APC,	APH,	ARE,	AWK,	AYI,	BAX,	BBY,
                                BEN,	BHF,	BLK,	BMY,	BSX,	BXP,	CAH,	CAT,
                                CBG,	CBS,	CCL,	CHD,	CMA,	CME,	CMI,	CNC,
                                COF,	COO,	CSX,	CTL,	CVX,	DFS,	DHI,	DIS,
                                DLR,	DOV,	DRE,	DTE,	DVA,	ECL,	EFX,	EMR,
                                EQT,	ETR,	EXC,	FCX,	FIS,	FLS,	FRT,	GGP,
                                GIS,	GPN,	HAL,	HCA,	HIG,	HLT,	HPQ,	HRL,
                                HSY,	IBM,	IPG,	ITW,	JEC,	KHC,	KMI,	KSS,
                                LEN,	LKQ,	LLY,	LNT,	LUK,	MMM,	MRK,	PFE,
                                SJM,	UTX,	WMT,	XOM,	ABBV,	ADBE,	AKAM,	ALGN,
                                ALXN,	AMZN,	ANSS,	APTV,	ARNC,	BF.B,	BHGE,	CBOE,
                                CDNS,	CERN,	CHTR,	CINF,	COST,	CSRA,	CTAS,	CTSH,
                                DISH,	DLTR,	EBAY,	EXPD,	FAST,	FBHS,	FFIV,	FISV,
                                GILD,	GOOG,	INCY,	INTC,	LRCX,	MSFT,	SCHW,	XRAY,
                                AAPL,	ADSK,	ALLE,	AMAT,	AMGN,	ANDV,	ANTM,	ATVI,
                                AVGO,	BIIB,	CELG,	CHRW,	COTY,	CSCO,	CTXS,	DWDP,
                                EQIX,	ESRX,	ETFC,	EVHC,	EXPE,	FITB,	FLIR,	GRMN,
                                HBAN,	HOLX,	HSIC,	IDXX,	ILMN,	INFO,	INTU,	ISRG,
                                JBHT,	JNPR,	KLAC,	ZION,	BRK.B,	CMCSA,	DISCA,
                                DISCK, GOOGL)))




getSymbols("MYMMF" , src = "google")
candleChart(MYMMF, multi.col = TRUE , subset='last 2 month')
chartSeries(MYMMF) 
addMACD() 
addBBands()
head(YHOO)
tail(YHOO)
plot(diff(log(MYMMF)))
chartSeries(YHOO, subset = 'last 3 months')



############################################################
# http://amunategui.github.io/wallstreet/
############################################################

myportafolio <- c("AAPL", "ADBE", "ADI", "ADP", "ADSK", "AKAM", "ALTR", "ALXN",
                  "AMAT", "AMGN", "AMZN", "ATVI", "AVGO", "BBBY", "BIDU", "BIIB",
                  "BRCM", "CA", "CELG", "CERN", "CHKP", "CHRW", "CHTR", "CMCSA",
                  "COST", "CSCO", "CTRX", "CTSH", "CTXS", "DISCA", "DISCK", "DISH",
                  "DLTR", "EBAY", "EQIX", "ESRX", "EXPD", "EXPE", "FAST",
                  "FB", "FFIV", "FISV", "FOXA", "GILD", "GMCR", "GOOG", "GOOGL",
                  "GRMN", "HSIC", "ILMN", "INTC", "INTU", "ISRG", "KLAC", "KRFT",
                  "LBTYA", "LLTC", "LMCA", "LMCK", "LVNTA", "MAR", "MAT", "MDLZ",
                  "MNST", "MSFT", "MU", "MXIM", "MYL", "NFLX", "NTAP", "NVDA",
                  "NXPI", "ORLY", "PAYX", "PCAR", "PCLN", "QCOM", "QVCA", "REGN",
                  "ROST", "SBAC", "SBUX", "SIAL", "SIRI", "SNDK", "SPLS", "SRCL",
                  "STX", "SYMC", "TRIP", "TSCO", "TSLA", "TXN", "VIAB", "VIP",
                  "VOD", "VRSK", "VRTX", "WDC", "WFM", "WYNN", "XLNX", "YHOO")

getSymbols(myportafolio, src = "google")
#  se elimino "DTV"

data <- data.frame(as.xts(merge(AAPL, ADBE, ADI, ADP, ADSK, AKAM, ALTR, ALXN,
                                AMAT, AMGN, AMZN, ATVI, AVGO, BBBY, BIDU, BIIB,
                                BRCM, CA, CELG, CERN, CHKP, CHRW, CHTR, CMCSA,
                                COST, CSCO, CTRX, CTSH, CTXS, DISCA, DISCK, DISH,
                                DLTR, EBAY, EQIX, ESRX, EXPD, EXPE, FAST,
                                FB, FFIV, FISV, FOXA, GILD, GMCR, GOOG, GOOGL,
                                GRMN, HSIC, ILMN, INTC, INTU, ISRG, KLAC, KRFT,
                                LBTYA, LLTC, LMCA, LMCK, LVNTA, MAR, MAT, MDLZ,
                                MNST, MSFT, MU, MXIM, MYL, NFLX, NTAP, NVDA,
                                NXPI, ORLY, PAYX, PCAR, PCLN, QCOM, QVCA, REGN,
                                ROST, SBAC, SBUX, SIAL, SIRI, SNDK, SPLS, SRCL,
                                STX, SYMC, TRIP, TSCO, TSLA, TXN, VIAB, VIP,
                                VOD, VRSK, VRTX, WDC, WFM, WYNN, XLNX, YHOO)))
#head(data,2)
#names(data)

saveSymbolLookup(file="mysymbols.rda") 
loadSymbolLookup(file="mysymbols.rda") 
#summary(data)
#str(data)

# Variables con terminaci??n .Open
open = dput(names(data)[grepl('.Open',names(data))])
data1 = data[,open]
#View(data1)
#str(data1)

# Trabajando con NA
SI_1 = function(x){
          ifelse(is.na(x),na.locf(x , fromLast = TRUE),x)
}
data2=data.frame(sapply(data1,SI_1))
head(data2)

setwd("C:/Users/Vblanco/Desktop/Personal Acciones")
write.csv(data2,"data2.csv")

# Archivo de lectura
data2 <- read.csv("data2.csv", sep = ",")
data3 <- as.ts(data2,frequency = 1,
                              start=c(2007,01,01), end=c(2017,01,12))

summary(data3)
#View(data3)
# Graficos m??ltiples
#plot.ts(data3[,2:4], plot.type = "multiple")

# Trabajando sobre muestra de dias
plot.ts(diff(log(data3[2062:2792,2:11]),lag=1), plot.type = "multiple",yax.flip = TRUE)
plot.ts(lag(data3[2062:2792,2:11],1), plot.type = "multiple",yax.flip = TRUE)
#plot.ts(diff(lag(data3[2062:2792,2],lag=1)), plot.type = "multiple",yax.flip = TRUE)
lag.plot(data3[2062:2792,2:10],1)
acf(data3[2062:2792,2:11])

# Calibrando el modelo que se utilizar??
par(mfrow=c(2,2))
acf(data3[2062:2792,2])
pacf(data3[2062:2792,2])
acf(diff(log(data3[2062:2792,2])),lag=1)
pacf(diff(data3[2062:2792,2],lag=1))
model01 <- arima(log(data3[2062:2792,2]), order = c(1,1,1))
names(model01)
model01$series
e <- model01$residuals
hist(e)
acf(e)
pacf(e)
p <- predict(model01,n.ahead=30)
par(mfrow=c(1,1))
plot.ts(log(data3[2062:2792,2]))
lines(p$pred, col="blue")
lines(p$pred+2*p$se, col="red")
lines(p$pred-2*p$se, col="red")
names(model01)
tsdiag(model01)
hist(model01$residuals)
acf(model01$residuals^2)

# generanado estimaciones futuras por serie
# Total de prediciones
n.ahead = 30
model02<- function(i,j,x){
                          as.data.frame(predict(
                            arima(log(data3[i:j,x]), order=c(1,1,1)), n.ahead=n.ahead))
}

# guardando las predicciones
columnas=ncol(data2)-1
predicciones <- data.frame(1:n.ahead,t(1:columnas))
names(predicciones) <- names(data2)
predicciones_se <- data.frame(1:n.ahead,t(1:columnas))
names(predicciones_se) <- names(data2)

for (x in 2:ncol(data3)) predicciones[x] <- model02(2062,2792,x)$pred 
for (x in 2:ncol(data3)) predicciones_se[x] <- model02(2062,2792,x)$se 

resultados <- rbind(log(data3), predicciones)
resultados <- as.data.frame(as.ts(data3,frequency = 1,
                     start=c(2007,01,01), end=c(2017,31,12)))

####################################################################

# Evaluando comportamiento de los últimos 20 días

####################################################################
resultados_dif <- apply(resultados, 2, diff)
# Diferencias de los últimos 20 días
diferencias <- as.data.frame(tail(resultados_dif,20))
# Promedio de variaciones de los últimos 20 días por variavle
media_dif_var <- apply(diferencias, 2, mean)

######################################################################
# MEJOR DESEMPEÑO
######################################################################
# Var con mejor desempeño
mejor_desempeño <- order(media_dif_var[media_dif_var > 0 ])
mejor_desempeño
#head(resultados[45])
mejor_desempeño_10 <- mejor_desempeño[c(1:10)]
plot.ts(resultados[c(mejor_desempeño_10)], plot.type = "multiple")

# Màximo de variaciones de los últimos 20 días por por día
maximo_dif_dia <- apply(diferencias, 1, max)
# Máximo de todas las variables
max(maximo_dif_dia)
which.max(maximo_dif_dia)
plot.ts(resultados[which.max(maximo_dif_dia)])


######################################################################
# PEOR DESEMPEÑO
######################################################################

# Var con peor desempeño
peor_desempeño <- order(media_dif_var[media_dif_var <=0 ] , decreasing = TRUE)
peor_desempeño
#head(resultados[22])
#head(resultados[45])
peor_desempeño_10 <- peor_desempeño[c(1:10)]
plot.ts(resultados[c(peor_desempeño_10)], plot.type = "multiple")

# Mínimo de variaciones de los últimos 20 días por por día
minimo_dif_dia <- apply(diferencias, 1, min)
# Menor de todas las variables
min(minimo_dif_dia)
which.min(minimo_dif_dia)
plot.ts(resultados[which.min(minimo_dif_dia)])

names(resultados)
####################################################################
# Evaluacion gràfica
####################################################################
plot.ts(resultados$NVDA.Open)
plot.ts(data2$NVDA.Open, col="blue")
plot.ts(diff(resultados$NVDA.Open,1))
plot.ts(diff(resultados$NVDA.Open,12))

# Información directo del mercado
candleChart(NVDA, multi.col = TRUE)
chartSeries(NVDA) 
addMACD() 
addBBands()
head(NVDA)
tail(NVDA)
plot(diff(log(NVDA)))
chartSeries(NVDA, subset = 'last 3 months')

# Calcular los dias con mayores perdidas
diasperdidas <- apply(resultados,2,min)
diasperdidas
hist(resultados$BIIB.Open, breaks=10)

# Los cuantiles de la distribuci??n de retorno son interesantes desde la 
# #perspectiva de la gesti??n de riesgo. Podemos por ejemplo, determinar con
# #un nivel de confianza del 99%, el valor en riesgo (VaR)  a un d??a desde 
# #un enfoque hist??rico.

cuantil = function(x) {quantile(x, probs = 0.01, na.rm = TRUE)}
cuantil_resultados <- apply(resultados,2,cuantil)
cuantil_resultados

# # Por lo tanto, la probabilidad de que el retorno sea inferior al -6.524868% 
# # en un d??a determinado es tan s??lo el 1%. Pero si se produce ese d??a 
# # (y ??sto ocurre 11 veces en los 1047 datos analizados) entonces 6.5% es 
# #la cantidad m??nima que puede perder. Para ver los d??as que ??sto ocurre hacemos:
resultados$AAPL.Open[resultados$AAPL.Open <= cuantil_resultados["APPL.Open"] ]


# Metales

metales <- c("GOLD","silver","platinum","palladium")
getMetals(metales, src= 'oanda')
data_metal <- data.frame(as.xts(merge(XAUUSD, XAGUSD, XPTUSD, XPDUSD)))


getSymbols("EUR/JPY", src='oanda')

"AA"
"AAPL"
"AIG"
"AXP"
"BA"
"C"
"CAT"
"CC"
"CL"
"COMPQ"
"CT"
"DAX"
"DD"
"DIS"
"EK"
"FTSE"
"GC"
"GE"
"GFF"
"GM"
"HD"
"HE"
"HGG"
"HO"
"HON"
"HPQ"
"HSI"
"IBM"
"INDU"
"INTC"
"IP"
"JNJ"
"JO"
"JPM"
"KC"
"KO"
"LB"
"LE"
"MCD"
"MMM"
"MO"
"MRK"
"MSFT"
"N100"
"N225"
"NG"
"PAF"
"PFE"
"PG"
"PLF"
"QGG"
"QM"
"QQQ"
"SB"
"SI"
"SPX"
"SPY"
"T"
"USDX"
"UTX"
"VMT"
"VZ"
"XB"
"XOM"
"XR"
"ZC"
"ZL"
"ZM"
"ZS"
"ZW"
"AUD/CAD"
"AUD/CHF"
"AUD/CZK"
"AUD/DKK"
"AUD/HKD"
"AUD/HUF"
"AUD/JPY"
"AUD/MXN"
"AUD/NOK"
"AUD/NZD"
"AUD/PLN"
"AUD/SEK"
"AUD/SGD"
"AUD/USD"
"AUD/ZAR"
"CAD/CHF"
"CAD/CZK"
"CAD/DKK"
"CAD/HKD"
"CAD/HUF"
"CAD/JPY"
"CAD/MXN"
"CAD/NOK"
"CAD/PLN"
"CAD/SEK"
"CAD/SGD"
"CAD/ZAR"
"CHF/CZK"
"CHF/DKK"
"CHF/HKD"
"CHF/HUF"
"CHF/JPY"
"CHF/MXN"
"CHF/NOK"
"CHF/PLN"
"CHF/SEK"
"CHF/SGD"
"CHF/ZAR"
"CZK/JPY"
"DKK/JPY"
"EUR/AUD"
"EUR/CAD"
"EUR/CHF"
"EUR/CZK"
"EUR/DKK"
"EUR/GBP"
"EUR/HKD"
"EUR/HUF"
"EUR/JPY"
"EUR/MXN"
"EUR/NOK"
"EUR/NZD"
"EUR/PLN"
"EUR/SEK"
"EUR/SGD"
"EUR/USD"
"EUR/ZAR"
"GBP/AUD"
"GBP/CAD"
"GBP/CHF"
"GBP/CZK"
"GBP/DKK"
"GBP/HKD"
"GBP/HUF"
"GBP/JPY"
"GBP/MXN"
"GBP/NOK"
"GBP/NZD"
"GBP/PLN"
"GBP/SEK"
"GBP/SGD"
"GBP/USD"
"GBP/ZAR"
"GOLD"
"HKD/JPY"
"HUF/JPY"
"MXN/JPY"
"NOK/JPY"
"NZD/CAD"
"NZD/CHF"
"NZD/CZK"
"NZD/DKK"
"NZD/HKD"
"NZD/HUF"
"NZD/JPY"
"NZD/MXN"
"NZD/NOK"
"NZD/PLN"
"NZD/SEK"
"NZD/SGD"
"NZD/USD"
"NZD/ZAR"
"SEK/JPY"
"SGD/JPY"
"USD/CAD"
"USD/CHF"
"USD/CZK"
"USD/DKK"
"USD/HKD"
"USD/HUF"
"USD/JPY"
"USD/MXN"
"USD/NOK"
"USD/PLN"
"USD/SEK"
"USD/SGD"
"USD/ZAR"
"ZAR/JPY"



# 
# setwd("C:/Users/Vblanco/Desktop/Personal Acciones/predicciones")
# for (x in 2:ncol(data3)) write.csv(model02(2062,2792,x), 
#                                    as.character(paste("MODELO",x,".csv",sep=",")))
# 
# 
# # cargando las predicciones
# # Listar documentos
# files  <- list.files(pattern = '\\.csv')
# View(files)
# 
# # Crear una lista
# tables <- as.data.frame(lapply(files, read.csv, header = TRUE))
# str(tables)
# 
# # Variables con nombre pre
# pre = dput(names(tables)[grepl('pred',names(tables))])
# data_pre = tables[,pre]
# 
# 
# View(log(data3))
# ncol(data2)
# ncol(data_pre)
# names(data_pre)<- names(data2)
# 
# data4 <- rbind(log(data3),data_pre,make.row.names=FALSE)
# View(data4)
# 
# data5 <- as.data.frame(as.ts(data4,frequency = 1,
#                start=c(2007,01,01), end=c(2017,01,12)))
# 
# attributes(data5)
# 
# candleChart(AAPL, multi.col = TRUE,subset='last 4 months')
# chartSeries(AAPL)
# plot.ts(data5["AAPL.Open"], plot.type = "multiple",yax.flip = TRUE)
# 
# data5 [1]
# plot(data5$AAPL.Open, type = "l")
# plot(data5$ADP.Open, type="l")




#





#library("bigtime")
#VARMAfit <- sparseVARMA(as.matrix(data3[,c(2,4)]))



#library(TSclust)
#ncol(data3)
#summary(data3)
#INT.PER" Integrate Periodogram-based method.
# data3_dis <- diss(data3, METHOD="INT.PER")
# fit1_1<-hclust(d=data3_dis, method= "complete" )
# fit1_2<-hclust(d=data3_dis, method= "ward.D2" )
# fit1_3<-hclust(d=data3_dis, method= "ward.D" )
# fit1_4<-hclust(d=data3_dis, method= "median" )
# 
# plot.new()
# par(mfrow=c(2,2))
# 
# plot(fit1_1,hang=1)
# plot(fit1_2,hang=1)
# plot(fit1_3,hang=1)
# plot(fit1_4,hang=1)
# 
# groups1<-cutree(fit1_1,k=7)
# groups2<-cutree(fit1_2,k=5)
# groups3<-cutree(fit1_3,k=6)
# groups4<-cutree(fit1_4,k=7)
# 
# # Autocorrelation-based Dissimilarity
# data3_dis_ACF <- diss(data3, METHOD="ACF",p=0.05)
# fit2_1<-hclust(d=data3_dis_ACF, method= "complete" )
# fit2_2<-hclust(d=data3_dis_ACF, method= "ward.D2" )
# fit2_3<-hclust(d=data3_dis_ACF, method= "ward.D" )
# fit2_4<-hclust(d=data3_dis_ACF, method= "median" )
# 
# plot.new()
# par(mfrow=c(2,2))
# 
# plot(fit2_1,hang=1)
# plot(fit2_2,hang=1)
# plot(fit2_3,hang=1)
# plot(fit2_4,hang=1)
# 
# groups1<-cutree(fit2_1,k=7)
# groups2<-cutree(fit2_2,k=5)
# groups3<-cutree(fit2_3,k=6)
# groups4<-cutree(fit2_4,k=7)


# Model-based Dissimilarity Proposed by Maharaj (1996, 2000)
# data3_dis_AR.MAH <- diss(data3, METHOD="AR.MAH")$statistic
# fit3_1<-hclust(d=data3_dis_AR.MAH, method= "complete" )
# fit3_2<-hclust(d=data3_dis_AR.MAH, method= "ward.D2" )
# fit3_3<-hclust(d=data3_dis_AR.MAH, method= "ward.D" )
# fit3_4<-hclust(d=data3_dis_AR.MAH, method= "median" )
# 
# plot.new()
# par(mfrow=c(2,2))
# 
# plot(fit3_1,hang=1)
# plot(fit3_2,hang=1)
# plot(fit3_3,hang=1)
# plot(fit3_4,hang=1)
# 
# groups1<-cutree(fit3_1,k=4)
# groups2<-cutree(fit3_2,k=6)
# groups3<-cutree(fit3_3,k=4)
# groups4<-cutree(fit3_4,k=3)
# 

# # Complexity-Invariant Distance Measure For Time Series
# data3_dis_CID <- diss(data3, METHOD="CID")
# fit4_1<-hclust(d=data3_dis_CID, method= "complete" )
# fit4_2<-hclust(d=data3_dis_CID, method= "ward.D2" )
# fit4_3<-hclust(d=data3_dis_CID, method= "ward.D" )
# fit4_4<-hclust(d=data3_dis_CID, method= "median" )
# 
# plot.new()
# par(mfrow=c(2,2))
# 
# plot(fit4_1,hang=1)
# plot(fit4_2,hang=1)
# plot(fit4_3,hang=1)
# plot(fit4_4,hang=1)
# 
# groups1<-cutree(fit4_1,k=4)
# groups2<-cutree(fit4_2,k=6)
# groups3<-cutree(fit4_3,k=4)
# groups4<-cutree(fit4_4,k=3)
# 
# # Complexity-Invariant Distance Measure For Time Series
# data3_dis_PER <- diss(data3, METHOD="PER", logarithm=TRUE, normalize=TRUE)
# fit5_1<-hclust(d=data3_dis_PER, method= "complete" )
# fit5_2<-hclust(d=data3_dis_PER, method= "ward.D2" )
# fit5_3<-hclust(d=data3_dis_PER, method= "ward.D" )
# fit5_4<-hclust(d=data3_dis_PER, method= "median" )
# 
# plot.new()
# par(mfrow=c(2,2))
# 
# plot(fit5_1,hang=1)
# plot(fit5_2,hang=1)
# plot(fit5_3,hang=1)
# plot(fit5_4,hang=1)
# 
# groups1<-cutree(fit5_1,k=7)
# groups2<-cutree(fit5_2,k=7)
# groups3<-cutree(fit5_3,k=4)
# groups4<-cutree(fit5_4,k=3)
# 
# # Dissimilarity Measure Based on Nonparametric Forecast
# diffs <- rep(1, ncol(data3))
# logs <- rep(TRUE, ncol(data3))
# data3_dis_PRED <- diss(data3, METHOD="PRED", h=3, B=200,
#                        logarithm=logs, differences=diffs, plot=TRUE)
# 
# 
# fit6_1<-hclust(d=data3_dis_PER, method= "complete" )
# fit6_2<-hclust(d=data3_dis_PER, method= "ward.D2" )
# fit6_3<-hclust(d=data3_dis_PER, method= "ward.D" )
# fit6_4<-hclust(d=data3_dis_PER, method= "median" )
# 
# plot.new()
# par(mfrow=c(2,2))
# 
# plot(fit5_6,hang=1)
# plot(fit5_6,hang=1)
# plot(fit5_6,hang=1)
# plot(fit5_6,hang=1)
# 
# groups1<-cutree(fit5_1,k=7)
# groups2<-cutree(fit5_2,k=7)
# groups3<-cutree(fit5_3,k=4)
# groups4<-cutree(fit5_4,k=3)
# 

# table(groups4)

# Comparando cluster
# true_cluster_1 <- as.data.frame(rep(1,41))
# true_cluster_2 <- as.data.frame(rep(2,39))
# true_cluster_3 <- as.data.frame(rep(3,19))
# true_cluster_4 <- as.data.frame(rep(4,3))
# true_cluster_5 <- as.data.frame(rep(5,1))
# 
# names(true_cluster_1)[1]="cluster"
# names(true_cluster_2)[1]="cluster"
# names(true_cluster_3)[1]="cluster"
# names(true_cluster_4)[1]="cluster"
# names(true_cluster_5)[1]="cluster"
# 
# true_cluster <- rbind(true_cluster_1,
#                       true_cluster_2,
#                       true_cluster_3,
#                       true_cluster_4,
#                       true_cluster_5)
#                       
# cluster.evaluation(t(true_cluster), groups1)
# cluster.evaluation(t(true_cluster), groups2)
# cluster.evaluation(t(true_cluster), groups3)
# cluster.evaluation(t(true_cluster), groups4)
# 
# # Model-based Dissimilarity Proposed by Maharaj (1996, 2000)
# data3_dis_AR.MAH <- diss(data3, METHOD="AR.MAH")$p_value
# data3_dis_AR.MAH_clus <- pvalues.clust(data3_dis_AR.MAH,significance = 0.05)
# cluster.evaluation(t(true_cluster), data3_dis_AR.MAH_clus)
# 
# par(mfrow=c(1,1))
# data3_dis_AR.MAH_clus

 # Evidencia de heterocedasticidad en los residuos #GARCH

# https://repositorio.unican.es/xmlui/bitstream/handle/10902/10331/ABASCALNEGUERUELAMARIO.pdf?sequence=1

# install.packages('rugarch')
# library('rugarch')
# 
# # GARCH(1,1)
# modelo.garch11 <- ugarchspec(variance.model=list(garchOrder=c(1,1)),
#                              mean.model =list(armaOrder=c(0,0)))
# modelo.garch11.fit <- ugarchfit(spec=modelo.garch11,
#                                 data=diff(log(data3[2062:2792,2]),lag=1),
#                                 solver.control= list(trace=1))
# summary(modelo.garch11.fit)
# modelo.garch11.fit
# # los optimal parameters son todos positivos implica varianza positiva
# # la suma de alpha1+beta1 es menor a la unidad
# # solo alpha1 y omega son estadisticamente significativos
# # este modelo explica los cambios en la serie, se nota al superponer
# # la serie con la desviaci??n muestra Conditional SD (vs |returns|)
# # Si nos fijamos en la curva representada, observaremos que esta es sim??trica, por
# # ende podemos afirmar que la cr??tica hacia el modelo GARCH queda evidenciada pues
# # este no es capaz de estimar convenientemente el efecto apalancamiento
# 
# plot(modelo.garch11.fit)
# 
# # eGARCH(1,1)
# modelo.egarch1_1 <-
#   ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),
#              mean.model =list(armaOrder=c(1,1)))
# 
# fit <- ugarchfit(data=as.ts(diff(log(data3[2062:2792,2]),lag=1)),
#                                   spec=modelo.egarch1_1)
# summary(fit)
# fit
# plot(fit)
# 
# # Si aplicamos este mismo test sobre los residuos al cuadrado, obtendremos los
# # En este caso, no se rechazara la hip??tesis de ausencia de correlaci??n serial y
# # podemos afirmar que los residuos se distribuyen de forma independiente
# 
# # Prediccion del modelo
# prediccion <- ugarchforecast(fit,data=as.data(log(data3[2062:2792,2])),
#                                        n.roll = 0,
#                                        n.ahead = 50)
# 
# 
# fitted(prediccion)
# sigma(prediccion)
# plot(prediccion)

# https://eeecon.uibk.ac.at/~zeileis/teaching/AER/Ex-FinancialEconometrics.pdf

# install.packages('fGarch')
# library(fGarch)
# 
# mp_gf <- garchFit(~ arma(1,1) + aparch(1,1), 
#                   data = as.ts(diff(log(data3[2062:2792,2]),lag=1)), 
#                   trace = FALSE)
# summary(mp_gf)
# plot(mp_gf)
# prediccion <- predict(mp_gf, n.ahead=50, plot=TRUE, mse="uncond")
# View(prediccion)
# 
# prueba <- as.data.frame(rbind.xts(ts(diff(log(data3[2062:2792,2]),lag=1)
#                                      ,prediccion$meanForecast)))
# 
# nrow(as.data.frame(diff(log(data3[2062:2792,2]),lag=1)))
# nrow(as.data.frame(prediccion))
# 
# names(as.data.frame(diff(log(data3[2062:2792,2]),lag=1)))[1]="prediccion"
# names(as.data.frame(prediccion$meanForecast))[1]="prediccion"
# 
# 
# prediccion_1 <- diffinv(as.vector(prediccion),xi= 1)  
# 
# 
# 
# 
# write.csv(prueba,"prueba.csv")
# write.csv(log(data3[2062:2792,2]),"real.csv")
# 
# 
# prediccion <- predict(model01, n.ahead=50)
# prueba <- as.data.frame(rbind.xts(ts(log(data3[2062:2792,2])),prediccion$pred))
# #View(prueba)
# #View(log(data3[2062:2792,2]))
# #View(prediccion$pred)
# par(mfrow=c(1,1))
# plot.ts(prediccion$pred, plot.type = "single",yax.flip = TRUE)
# plot.ts(prueba, plot.type = "single",yax.flip = TRUE)
# plot.ts(log(data3[2062:2792,2]), plot.type = "single",yax.flip = TRUE)
# 
# str(prueba)
# str(as.ts(log(data3[2062:2792,2])))
# str(prediccion)
# 
# 
# # Generalizando el modelo a toda las series
# 
# model01<- function(i,j,x){
#           arima(log(data3[i:j,x]), order=c(1,1,1))
# }
# 
# # perido de tiempo
# i= 2062
# j=2792
# 



# Combinar en una tabla
# combined.df <- do.call(cbind , tables)
# View(combined.df)


# 
# 
# 
# 
# 
# 
# for (x in 2:ncol(data3)) z <- as.data.frame(print(model02(2062,2792,x)))
# 
# for (x in 2:ncol(data3)) z <- as.data.frame(print(model02(2062,2792,x)))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# for (x in 2:ncol(data3)) (read.csv(paste("MODELO",x,".csv",sep=","),sep=","))
#                           
# 
# 
# predicho <- function(x){
#                   for (x in 2: ncol(data3))
#                   z[x] <- read.csv(paste("MODELO",x,".csv"),sep=",")
#                   predicciones <- as.data.frame(cbind(z))
# }
# 
# predicho(1)
# 
# 
# 
# 
# 
# 
# apply(data3,2, arima(log(data3[2062:2792,]),order=c(1,1,1)))
# 
# modelo2 <- function(i,j,k) {
#   apply(data3,2, arima(log(data3[i:j,x]), order=c(1,1,1))
#         
# }
# 
# 
# # http://finanzasquants.blogspot.com/2016/07/
# 
# # Proyeccion
# library(quantmod) #carga la librer??a
# getSymbols("FB", from="2012-05-18", to="2016-07-18")
# library(tseries)
# z1 = ts(log(data3[2062:2792,2]))
# View(z1)
# adf.test(z1, alternative="explosive")
# adf.test(z1, alternative="stationary")
# r =  diff(z1)
# adf.test(r[-1], alternative="explosive")
# adf.test(r[-1], alternative="stationary")
# acf(r[-1], ylim=c(-0.1,0.1)) 
# pacf(r[-1], ylim=c(-0.1,0.1))
# library(rugarch)
# arma.especificacion = arfimaspec(
#   mean.model = list(armaOrder = c(10,25), include.mean = TRUE),
#   distribution.model = "norm", 
#   fixed.pars = list(ma1=0, ma2=0, ma3=0, ma4=0))
# arma.estimar = arfimafit(arma.especificacion, r[-1])
# arma.estimar
# acf(residuals(arma.estimar), ylim=c(-0.1,0.1))
# pacf(residuals(arma.estimar), ylim=c(-0.1,0.1))
# r.pron = arfimaforecast(arma.estimar, n.ahead=7)
# r.pron
# plot.ts(r)
# # Calcular los dias con mayores perdidas
# r[which.min(r)]
# hist(r, breaks=100)
# # Los cuantiles de la distribuci??n de retorno son interesantes desde la 
# #perspectiva de la gesti??n de riesgo. Podemos por ejemplo, determinar con
# #un nivel de confianza del 99%, el valor en riesgo (VaR)  a un d??a desde 
# #un enfoque hist??rico.
# cuantil = quantile(r, probs = 0.01, na.rm = TRUE)
# # Por lo tanto, la probabilidad de que el retorno sea inferior al -6.524868% 
# # en un d??a determinado es tan s??lo el 1%. Pero si se produce ese d??a 
# # (y ??sto ocurre 11 veces en los 1047 datos analizados) entonces 6.5% es 
# #la cantidad m??nima que puede perder. Para ver los d??as que ??sto ocurre hacemos:
# r[r<=(cuantil)]
# 
# proyeccion = fitted(arfimaforecast(arma.estimar, n.ahead = 15))
# plot.ts(proyeccion)
# 
# x<-as.data.frame(as.numeric(r))
# names(x)="r"
# x1<-as.data.frame(as.numeric(proyeccion))
# names(x1)="r"
# View(x1)
# x2 <- rbind(x,x1)
# plot.ts(x2)
# 
# diffinv((x2),lag=1,xi=z1[1])

