#Installing & loading packages that we need to use
pack<-c("car","readxl","ggplot2", "vtable","tidyverse", "jtools", "e1071", "tseries", 
        "ggplot2", "plotly", "fRegression", "rugarch", "rmgarch","vars","fUnitRoots")

#uncomment the below command to install packages if not already installed
#install.packages(pack)
install.packages(pack)
lapply(pack, require, character.only = TRUE)

#===================================================
#                    Task 1
#===================================================

Stocks<- read.csv("C:\\Users\\ssatn\\Documents\\02 - Cours\\04 - NEOMA\\Econometrics and Time Series Applied to Finance\\Group Project\\Stocks_AC.csv", header=TRUE,skipNul = TRUE)
Stocks<- data.frame(Stocks)

#1.1) Print Variables names & plot GM and Ford

#Variables names:
Stocks_names = colnames(Stocks)
print(Stocks_names)

#Plot GM & Ford
plot(Stocks$GM_AC, type = "l", col="red",  main = "Stock Prices Movement GP & Ford",
     xlab= "Evolution by days", ylab= "Stock Prices")
par(new=TRUE)
plot(Stocks$F_AC, type = "l", col="green",  main = "Stock Prices Movement GP & Ford",
     xlab= "Evolution by days", ylab= "Stock Prices")
legend("topleft", legend=c("GM", "Ford"),
       col=c("red", "green"), lty=1, cex=1.2,
       box.lty=1)

#1.2) Sample size T, returns of GM & Ford, Plot the returns against each other, how correlated
#are the two series ?

T = nrow(Stocks)
print(T)
GM_return <- diff(Stocks$GM_AC)/Stocks$GM_AC[1:(T-1)]
F_return <- diff(Stocks$F)/Stocks$F[1:(T-1)]

plot(GM_return, F_return, type = "p", col="red",  main = "Ford Return Against GM Return",
     xlab= "GM Return Rate", ylab= "Ford Return Rate")
#VERY STRONG POSITIVE CORRELATION BETWEEN BOTH STOCKS RETURNS
cor(GM_return, F_return)

#1.3) Create a boxplot of the General Motors and Ford returns
lmts <- range(GM_return,F_return)

par(mfrow = c(1, 2))
boxplot(GM_return,ylim=lmts, main="Boxplot GM Return")
boxplot( F_return,ylim=lmts, main="Boxplot Ford Return")

#1.4) Repeat Tasks 1.2 & 1.3 with MSFT & MRK
MSFT_return <- diff(Stocks$MSFT_AC)/Stocks$MSFT_AC[1:(T-1)]
MRK_return <- diff(Stocks$MRK_AC)/Stocks$MRK_AC[1:(T-1)]
par(mfrow = c(1, 1))
plot(MSFT_return, MRK_return, type = "p", col="red",  main = "Merck Return Against Microsoft",
     xlab= "MSFT Return Rate", ylab= "MRK Return Rate")

lmts <- range(MSFT_return,MRK_return)
par(mfrow = c(1, 2))
boxplot(MSFT_return,ylim=lmts, main="Boxplot MSFT Return")
boxplot( MRK_return,ylim=lmts, main="Boxplot MRK Return")

#correlation
cor(MSFT_return, MRK_return)

#1.5) Descriptive Statistics
return_dataset <- data.frame(GM_return, F_return, MSFT_return, MRK_return)

summary(return_dataset)
stdev(return_dataset$GM_return)
stdev(return_dataset$F_return)
stdev(return_dataset$MSFT_return)
stdev(return_dataset$MRK_return)

skewness(return_dataset$GM_return)
skewness(return_dataset$F_return)
skewness(return_dataset$MSFT_return)
skewness(return_dataset$MRK_return)

kurtosis(return_dataset$GM_return)
kurtosis(return_dataset$F_return)
kurtosis(return_dataset$MSFT_return)
kurtosis(return_dataset$MRK_return)

#===================================================
#                    Task 2
#===================================================

#2.1) ACF & PACF Plots for prices & returns
#Prices:
par(mfrow = c(2, 2))
acf(Stocks$GM_AC, main = 'AC GM ')
acf(Stocks$F_AC, main = 'AC Ford ')
acf(Stocks$MRK_AC, main = 'AC Merck ')
acf(Stocks$MSFT_AC, main = 'AC MSFT ')


par(mfrow = c(2, 2))
pacf(Stocks$GM_AC, main = 'PAC GM ')
pacf(Stocks$F_AC, main = 'PAC Ford ')
pacf(Stocks$MRK_AC, main = 'PAC Merck ')
pacf(Stocks$MSFT_AC, main = 'PAC MSFT ')

#Returns:
par(mfrow = c(2, 2))
acf(return_dataset$GM_return, main = 'AC GM ')
acf(return_dataset$F_return, main = 'AC Ford ')
acf(return_dataset$MRK_return, main = 'AC Merck ')
acf(return_dataset$MSFT_return, main = 'AC MSFT ')

pacf(return_dataset$GM_return, main = 'PAC GM ')
pacf(return_dataset$F_return, main = 'PAC Ford ')
pacf(return_dataset$MRK_return, main = 'PAC Merck ')
pacf(return_dataset$MSFT_return, main = 'PAC MSFT ')

#2.2) Stationarity, serial correlation, homoskedasticity, normal distribution tests:
#Stationarity Dickey-fuller: Null Hyphothesis (H0) : The time series displays a unit-root. This means that the time series is nonstationary
#Prices
adf.test(Stocks$GM_AC)
adf.test(Stocks$F_AC)
adf.test(Stocks$MRK_AC)
adf.test(Stocks$MSFT_AC)
#returns
adf.test(return_dataset$GM_return)
adf.test(return_dataset$F_return)
adf.test(return_dataset$MSFT_return)
adf.test(return_dataset$MRK_return)

#Serial correlation Ljung-box test: null hypothesis no serial correlation
#Prices
Box.test(Stocks$GM_AC, type= "Ljung-Box")
Box.test(Stocks$F_AC, type= "Ljung-Box")
Box.test(Stocks$MSFT_AC, type= "Ljung-Box")
Box.test(Stocks$MRK_AC, type= "Ljung-Box")

#returns
Box.test(return_dataset$GM_return, type= "Ljung-Box")
Box.test(return_dataset$F_return, type= "Ljung-Box")
Box.test(return_dataset$MSFT_return, type= "Ljung-Box")
Box.test(return_dataset$MRK_return, type= "Ljung-Box")

#Normality test:  Shapiro-Wilk's test null hypothesis is normal distribution
#Prices:
shapiro.test(Stocks$GM_AC)
shapiro.test(Stocks$F_AC)
shapiro.test(Stocks$MSFT_AC)
shapiro.test(Stocks$MRK_AC)


#returns
shapiro.test(return_dataset$GM_return)
shapiro.test(return_dataset$F_return)
shapiro.test(return_dataset$MSFT_return)
shapiro.test(return_dataset$MRK_return)

#homoskedasticity test:

#2.3) AR(1) for GM series:
GM_AR1<- arima(x = return_dataset$GM_return, order = c(1, 0, 0))
GM_AR1

Fit_GM_AR1 <- fitted(GM_AR1) #fitted values

Res_GM_AR1 <-   residuals(GM_AR1) #residuals

#2.4) ARMA-GARCH for GM series:
ARMA_Garch = ugarchspec(mean.model=list(armaOrder=c(1,0)),
                       variance.model=list(garchOrder=c(1,1)))

# Estimate our GARCH model
GM.arma.garch = ugarchfit(data=return_dataset$GM_return, spec=ARMA_Garch)
show(GM.arma.garch)

# Check if the standardized residuals are normally distributed and not serially correlated 
GM_garch_StandRes <- ts(residuals(GM.arma.garch, standardize=TRUE))
plot(Date[2:1827], GM_garch_StandRes,type="l", xlab =" Date ")

Box.test(GM_garch_StandRes)

jarque.bera.test(GM_garch_StandRes)


#2.5) EGARCH model for GM series & MSFT series
egarch.spec = ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)), 
                                                    mean.model=list(armaOrder=c(0,0)))
GM_egarch.fit = ugarchfit(egarch.spec, return_dataset$GM_return) 
coef(GM_egarch.fit) 
GM_egarch.fit

MSFT_egarch.fit = ugarchfit(egarch.spec, return_dataset$MSFT_return) 
coef(GM_egarch.fit)
MSFT_egarch.fit

#2.6) GARCH-DCC
# GARCH(1,1) model for each of both series
garch1.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                            variance.model = list(garchOrder = c(1,1),
                                                     model = "sGARCH"),
                            distribution.model = "norm" )


dcc.garch11.spec = dccspec(uspec = multispec( replicate(2,garch1.spec) ),dccOrder = c(1,1),distribution = "mvnorm")


return_dataset2 <- data.frame(GM_return, MSFT_return)
GM_MSFT_DCC_fit = dccfit(dcc.garch11.spec, data = return_dataset2[1:4962, 1:2])
GM_MSFT_DCC_fit
plot(GM_MSFT_DCC_fit)

#===================================================
#                    Task 3
#===================================================


T_Yields = read.csv("C:\\Users\\ssatn\\Documents\\02 - Cours\\04 - NEOMA\\Econometrics and Time Series Applied to Finance\\Group Project\\TreasuryYields.txt", header=TRUE, sep = " ")



#3.1) 

Yield_3M= T_Yields$X.1
Yield_6M= T_Yields$X.2
Yield_1Y= T_Yields$X.3
Yield_2Y= T_Yields$X1mo
Yield_3Y= T_Yields$X3mo

plot(Yield_3M, type = "l", col="red",  main = "Treasury Yield",
     xlab= "Evolution by days", ylab= "Yield %")
par(new=TRUE)
plot(Yield_6M, type = "l", col="green",  main = "Treasury Yield",
     xlab= "Evolution by days", ylab= "Yield %")
par(new=TRUE)
plot(Yield_1Y, type = "l", col="blue",  main = "Treasury Yield",
     xlab= "Evolution by days", ylab= "Yield %")
par(new=TRUE)
plot(Yield_2Y, type = "l", col="grey",  main = "Treasury Yield",
     xlab= "Evolution by days", ylab= "Yield %")
par(new=TRUE)
plot(Yield_3Y, type = "l", col="orange",  main = "Treasury Yield",
     xlab= "Evolution by days", ylab= "Yield %")
legend("bottomleft", legend=c("3M", "6M","1Y","2Y","3Y"),
       col=c("red", "green","blue","grey","orange"), lty=1, cex=1.2,
       box.lty=1)


#3.2)

acf(Yield_3M, na.action = na.pass, main = '3M Yield')
acf(Yield_6M, na.action = na.pass, main = '6M Yield')
acf(Yield_1Y, na.action = na.pass, main = '1Y Yield')
acf(Yield_2Y, na.action = na.pass, main = '2Y Yield')
acf(Yield_3Y, na.action = na.pass, main = '3Y Yield')

# here we remove the NA in the DF 
Yield_3M=na.omit(Yield_3M)
Yield_6M=na.omit(Yield_6M)
Yield_1Y=na.omit(Yield_1Y)
Yield_2Y=na.omit(Yield_2Y)
Yield_3Y=na.omit(Yield_3Y)

adf.test(Yield_3M)
adf.test(Yield_6M) 
adf.test(Yield_1Y)
adf.test(Yield_2Y)
adf.test(Yield_3Y)

#3.3)
df = data.frame(Yield_3M,Yield_6M,Yield_1Y,Yield_2Y, Yield_3Y)
cor(df)
OLS =lm(Yield_3Y~Yield_3M, data=df)
summ(OLS)
summary(OLS)

#3.4)

Resids=OLS$residuals
URTestRes= urersTest(Resids, type = c("DF-GLS"))
URTestRes
Resids

#3.5) 
DL_3Y = diff(Yield_3Y)
DL_3M =diff(Yield_3M)


ECM=lm(DL_3Y ~ DL_3M + Resids)
summary(ECM)

#3.6) 
OLS =lm(Yield_3M~Yield_3Y, data=data)
summ(OLS)
summary(OLS)

#===================================================
#                    Task 4
#===================================================

#4.1)

plot(Stocks$PFE_AC, type = "l", col="red",  main = "Stock Prices Movement GP & Ford",
     xlab= "Evolution by days", ylab= "Stock Prices")
par(new=TRUE)

plot(GM_return, type = "l", col="blue",  main = "",
     xlab= "Evolution by days", ylab= "Stock return")
par(new=TRUE)
plot(MSFT_return, type = "l", col="red",  main = "",
     xlab= "Evolution by days", ylab= "Stock return")
par(new=TRUE)
plot(PFE_return, type = "l", col="green",  main = "",
     xlab= "Evolution by days", ylab= "Stock return")

#4.2)
PFE_return <- diff(Stocks$PFE_AC)/Stocks$PFE_AC[1:(T-1)]

df2= data.frame(GM_return, MSFT_return, PFE_return)
VARselect(df2)
Var_Statistical_Model=VAR(df2, p = 1)
summary(Var_Statistical_Model)

#4.3)

causality(Var_Statistical_Model, cause = "GM_return")
causality(Var_Statistical_Model, cause = "MSFT_return")
causality(Var_Statistical_Model, cause = "PFE_return")

#4.4)
impulse_response = irf(Var_Statistical_Model ,n.ahead = 20)
plot (impulse_response)

#4.5)
testInheritedMethods() = fevd (Var_Statistical_Model ,n.ahead = 20)
plot (test)





