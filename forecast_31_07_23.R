##############################
# forecasting retail sales for USA
##############################
#load libraries
##############################
library(readxl)
library(CADFtest)
library(urca)
library(CADFtest)
library(ggplot2)
library(ggfortify)
library(uroot)
library(tseries)
library(aTSA)
library(vars)
library(Metrics)
library(ecm)
library(uroot)
library(tidyverse)
library(ffp)
library(foreign)
library(quantmod)
library(ffp)
library(fpp2)
library(fpp3)
library(fable)
library(mable)
library(forecast)
library(foreign)
library(readxl)
library(tsibble)
library(tibble)
library(zoo)
library(stargazer)
library(ggplot2)
#############################
#load data
##################################
rm(list=ls())
governmentcons <- read_csv("governmentcons.csv")
monthly_dates <- seq(governmentcons[1], by = "1 month", length = 12)
realretail <- read_excel("real_retail_sales.xlsx")
realretail$DATE <- as.Date(realretail$DATE)
consentiment <- read_excel("consentiment.xls")
expinfl <- read_csv("expinflation.csv")
uncertainty <- read_csv("uncertainty.csv")
sentiment <- read_csv("UMCSENT92.csv")
government <- read_csv("governmentcons.csv")
grangerinfl <- read_excel("granger_infl.xlsx")
grangerunemp <- read_excel("granger_unemp.xlsx")
grangersent <- read_excel("granger_sent.xlsx")
grangerpers <- read_excel("granger_perscons.xlsx")
unemp <- read_csv("UNRATE.csv")
remove(grangerunemp)
grangerunemp <- read_csv("UNRATE.csv")
persons <- read_csv("perscons.csv")
govcons <- read.csv("governmentcons.csv")
sent <- read_csv("csent.csv")
emp <- read_csv("emp.csv")
cpi <- read_csv("cpi.csv")
consconf <- read_csv("consumerconfidence.csv")
retail <- read.csv("retailsales.csv")
retail$DATE <- as.Date(retail$DATE)
uncertainty <- read_csv("UMCSENT92.csv")
mich <- read_csv("MICH.csv")
stickycpi92 <- read_csv("stickycpi.csv")
sent <- read_csv("csent.csv")
grangersales <- read_excel("real_retail_sales.xlsx")
grangerpers <- read_csv("PCEPI.csv")
sticky <- read_csv("corestickyprice.csv")
confidence <- read_csv("confidence.csv")
confidence <- na.omit(confidence)
corecpi <- read_csv("corestickyprice.csv")
sales20 <- read_excel("sales20.xlsx")
income <- read_excel("realincome.xlsx")
sales_sticky <- read_excel("sales_sticky.xlsx")
dispincome <- read_excel("disposable_income.xlsx")
dispincome$DATE <- as.Date(dispincome$DATE)
persincome <- read_excel("realpersincome.xlsx")
persincome$DATE <- as.Date(persincome$DATE)
realsales22 <- read_excel("realsales22.xlsx")
realsales22$DATE <- as.Date(realsales22$DATE)
sale_income22 <- read_excel("sales_income.xlsx")
sale_income22$DATE <- as.Date(sale_income22$DATE)
##########################
# testing covariates for granger causality test
##########################
grangersales22_ts <- ts(realsales22$REALSALES, start = c(1992,1), frequency = 12)
dgrangersales22_ts <- diff(grangersales22_ts)
persincome_ts <- ts(persincome$realpersincome, start = c(1992,1), frequency = 12)
dpersincome_ts <- diff(persincome_ts)
varpersincome <- cbind(dgrangersales22_ts, dpersincome_ts)
lagselect <- VARselect(varpersincome, lag.max = 10, type = "const")
lagselect$selection
incomepersvar <- VAR(varpersincome, p = 10, type = "const", season = NULL, exog = NULL)
grangerpersincome <- causality(incomepersvar, cause = "dpersincome_ts")
grangerpersincome1 <- causality(incomepersvar2, cause = "dpersincome_ts")
incomepersvar2 <- VAR(varpersincome, p = 1, type = "const")
grangerpersincome1

grangersales_ts <- ts(grangersales$sales, start = c(1992,1), frequency = 12)
dgrangersales_ts <- diff(grangersales_ts)
dispincome_ts <- ts(dispincome$disposableincome, start = c(1992,1), frequency = 12)
ddispincome_ts <- diff(dispincome_ts)
autoplot(ddispincome_ts)
autoplot(dgrangersales_ts)
vardisp <- cbind(dgrangersales_ts, ddispincome_ts)
lagselectdisp <- VARselect(vardisp, lag.max = 10, type = "const")
lagselectdisp$selection
lagselect$selection
dispvar <- VAR(vardisp, p = 2, type = "const", season = NULL, exog = NULL)
grangerdisp <- causality(dispvar, cause = "ddispincome_ts")
grangerdisp
confidence_ts <- ts(confidence$CONFIDENCE, start= c(1992,1), frequency = 12)
dconfidence_ts <- diff(confidence_ts)
autoplot(dconfidence_ts)
remove(dconfidence_ts)
varconfidence <- cbind(dgrangersales_ts,dconfidence_ts)
lagselect <- VARselect(varconfidence, lag.max = 10, type = "const")
lagselect$selection
confidencevar <- VAR(varconfidence, p = 3, type ="const", season = NULL)
grangerconf <- causality(confidencevar, cause = "dconfidence_ts")
grangerconf
sticky_ts <- ts(sticky$CORE, start = c(1992,1), frequency = 12)
autoplot(sticky_ts)
dsticky_ts <- diff(sticky_ts)
varsticky <- cbind(dgrangersales_ts, dsticky_ts)
lagselect <- VARselect(varsticky, lag.max = 10, type = "const")
lagselect$selection
stickyvar <- VAR(varsticky, p = 6, type = "const", season = NULL, exog = NULL)
grangersticky <- causality(stickyvar, cause = "dsticky_ts")
grangersticky
autoplot(dgrangersales_ts)
mich_ts <- ts(mich$MICH, start = c(1992,1), frequency = 12)
dmich_ts <- diff(mich_ts)
varmich <- cbind(dgrangersales_ts, dmich_ts)
lagselect <- VARselect(varmich, lag.max = 10, type = "none")
lagselect$selection
michvar <- VAR(varmich, p = 6, type = "none", season = NULL, exog = NULL)
grangermich <- causality(michvar, cause = "dmich_ts")
grangermich
grangersentiment <- ts(sentiment$UMCSENT, start = c(1992,1), frequency = 12)
autoplot(grangersentiment)
dgrangersentiment <- diff(grangersentiment)
autoplot(dgrangersentiment)
varmodelsentiment <- cbind(dgrangersales_ts, dgrangersentiment)
lagselect <- VARselect(varmodelsentiment, lag.max = 10, type = "const")
lagselect$selection
modelvarsentiment <- VAR(varmodelsentiment, p = 5, type = "const", season = NULL, exog = NULL)
grangersentimenttest <- causality(modelvarsentiment, cause = "dgrangersentiment")
grangersentimenttest
grangerunemp_ts <- ts(grangerunemp$UNRATE, start = c(1992,1), frequency = 12)
autoplot(grangerunemp_ts)
dgrangerunemp_ts <- diff(grangerunemp_ts)
varmodelunemp <- cbind(dgrangersales_ts, dgrangerunemp_ts)
lagselect <- VARselect(varmodelunemp, lag.max = 10, type = "const")
lagselect$selection
modelvarunemp <- VAR(varmodelunemp, p = 1, type = "const", season = NULL, exog = NULL)
grangerunemptest <- causality(modelvarunemp, cause = "dgrangerunemp_ts")
grangerunemptest
grangerpers_ts <- ts(grangerpers$PCEPI, start = c(1992,1), frequency = 12)
grangersales_ts <- ts(grangersales$sales, start = c(1992,1), frequency = 12) 
grangerinfl_ts <- ts(grangerinfl$expinfl, start = c(1992,1), frequency = 12)
dgrangersales_ts <- diff(grangersales_ts)
autoplot(dgrangersales_ts)
remove(dgrangersales_ts)
dgrangerinfl_ts <- diff(grangerinfl_ts)
autoplot(dgrangerinfl_ts)
VARmodel <- cbind(dgrangersales_ts, dgrangerinfl_ts)
lagselect <- VARselect(VARmodel, lag.max = 10, type = "const")
lagselect$selection
VARinfl <- VAR(VARmodel, p = 1, type = "const", season = NULL, exog = NULL)
grangertestinfl <- causality(VARinfl, cause = "dgrangerinfl_ts")
grangertestinfl
dgrangerpers_ts <- diff(grangerpers_ts)
autoplot(dgrangerpers_ts)
Varmodelpers <- cbind(dgrangersales_ts, dgrangerpers_ts)
lagspers <- VARselect(Varmodelpers, lag.max = 10, type = "const")
lagspers$selection
VARpers <- VAR(Varmodelpers, p = 1, type = "const", season = NULL, exog = NULL)
grangerpers <- causality(VARpers, cause = "dgrangerpers_ts")
grangerpers

sales20_ts <- ts(sales20$realsales, start = c(1992,1), frequency = 12)
income_ts <- ts(income$REALINCOME, start = c(1992,1), frequency = 12)
dsales20_ts <- diff(sales20_ts)
dincome_ts <- diff(income_ts)
varincome <- cbind(dsales20_ts, dincome_ts)
lagselect <- VARselect(varincome, lag.max = 10, type = "const")
lagselect$selection
incomevar <- VAR(varincome, p = 2, type = "const", season = NULL, exog = NULL)
grangerincome <- causality(incomevar, cause = "dincome_ts")

#########################
#create tibble
########################

retail <- as.Date.character(retail$DATE)
industrydif <- diff(industry)
industrydif <- diff(industry$INDPRO)
class(industry)
as_tibble (industry)
as_tibble(oil)
as_tibble(sent)
as_tibble(emp)
as_tibble(cpi)
as_tibble(consconf)
as_tibble(elec)
as_tibble(retail)
as_tibble(retail92)
as_tibble(stickycpi92)
as_tibble(industry22)
as_tibble(sent)
as_tibble(govcons)
as_tibble(realretail)
as_tibble(expinfl)
as_tibble(unemp)
as_tibble(corecpi)
as_tibble(dispincome)
as_tibble(realsales22)
as_tibble(persincome)
as_tibble(sale_income22)
##########################
#from tibble to tsibble 
#########################
sales_sticky_sales_ts <- sales_sticky$sales %>% 
  as_tsibble(index = "DATE")
expinfl_ts <- expinfl %>% 
  as_tsibble(index = "date")
expinfl_ts <- expinfl %>% 
  mutate(date = yearmonth(date)) %>% 
  as_tsibble(index = date)

realsales_ts <- realretail%>% 
  as_tsibble(index = "DATE")
realsales_ts <- realretail %>% 
  mutate(DATE = yearmonth(DATE)) %>%
  as_tsibble(index = "DATE")
realsales_ts_diff <- diff(realsales_ts)
govcons_ts <- govcons %>% 
  as_tsibble(index = "DATE")
realsales_ts
govcons$DATE <- as.year(govcons$DATE, format = "%Y-%q-%d")
govcons <- na.omit(govcons)
govcons$DATE <- as.Date(yearquarter(govcons$DATE))
govcons_ts <- govcons %>% 
  as_tsibble(index = "DATE")
govcons_ts <- govcons %>% 
  as_tsibble(index = "DATE")

sent_ts <- sent %>%
  as_tsibble(index = "DATE")
sent_ts <- sent %>%
  mutate(DATE = yearmonth(DATE)) %>%
  as_tsibble(index = "DATE")

emp_ts <- emp %>%
  as_tsibble(index = "DATE")
emp_ts <- emp %>%
  mutate(DATE = yearmonth(DATE)) %>%
  as_tsibble(index = "DATE")



retail_ts <- retail %>% 
  as_tsibble(index = "DATE") 
retail_ts <- retail %>% 
  mutate(DATE = yearmonth(DATE)) %>%
  as_tsibble(index = "DATE")
retail_ts92 <- retail92 %>% 
  as_tsibble(index = "DATE")
retail_ts92 <- retail92 %>% 
  mutate(DATE = yearmonth(DATE)) %>% 
  as_tsibble(index = "DATE")

stickycpi92_ts <- stickycpi92 %>% 
  as_tsibble(index = DATE)
stickycpi92_ts <- stickycpi92 %>% 
  mutate(DATE = yearmonth(DATE)) %>% 
  as_tsibble(index = "DATE")
industry22_ts <- industry22 %>% 
  as_tsibble(index = "DATE")

sent_ts <- sent%>%
  as_tsibble(index = DATE)
sent_ts <- sent %>% 
  mutate(DATE = yearmonth(DATE)) %>% 
  as_tsibble(index = "DATE")
corecpi_ts <- corecpi %>% 
  as_tsibble(index = "DATE")
corecpi_ts <- corecpi %>% 
  mutate(DATE = yearmonth(DATE)) %>% 
  as_tsibble(index = "DATE")
dispincome_ts <- dispincome %>% 
  as_tsibble(index = "DATE")
dispincome_ts <- dispincome %>%
  mutate(DATE = yearmonth(DATE)) %>% 
  as_tsibble(index = "DATE")
realsales22_ts <- realsales22 %>% 
  as_tsibble(index = "DATE")
realsales22_ts <- realsales22 %>% 
  mutate(DATE = yearmonth(DATE)) %>% 
  as_tsibble(index = "DATE")
persincome22_ts <- persincome %>% 
  as_tsibble(index = "DATE")
persincome22_ts <- persincome %>% 
  mutate(DATE = yearmonth(DATE)) %>% 
  as_tsibble(index = "DATE")
sales_income22_ts <- sale_income22 %>% 
  as_tsibble(index = "DATE")
sales_income22_ts <- sale_income22 %>% 
  mutate(DATE = yearmonth(DATE)) %>% 
  as_tsibble(index = "DATE")
##############################
# seasonal treatment 
#############################

retailts <- ts(realretail[,2], start = c(1991,1), frequency = 12)
diffretailsts <- diff(retailts)
ggseasonplot(diffretailsts)+ ggtitle("Time Plot: Change in Real US Retail Sales Per Day")+ ylab("Millions of 2023 Dollars")
ggsubseriesplot(diffretailsts)+ ggtitle("Time Plot: Change in Real US Retail Sales Per Day")+ ylab("Millions of 2023 Dollars")

##########################
#visualize
##########################
autoplot(realsales_ts)+ 
  labs(title = "Advanced Retail sales" , 
                          y = "sales",
                          x = "Years")+theme(plot.title=element_text(hjust=0.5))
autoplot(retail_ts92)

retail_ts92 %>% gg_tsdisplay(difference(sales, 12) %>% difference(),plot_type = 'partial', lag=36)
autoplot(corecpi_ts)+ 
  labs(title = "Core CPI", 
       y = "rate",
       x = "Years")+theme(plot.title=element_text(hjust=0.5))

autoplot(realsales22_ts)+ 
  labs(title = "Advanced Retail Sales (CPI adjusted)" , 
       y = "sales",
       x = "Years")+theme(plot.title=element_text(hjust=0.5))
autoplot(realsales22_ts)
##################################
difference
##################################
realsales_ts %>% 
  autoplot(log(sales))
#################################
#decomposition
################################

realsales_ts %>% 
  model(classical_decomposition(sales, type = "additive")) %>% 
  components() %>% 
  autoplot()+ xlab("Year") + ggtitle("Classical additive decomposition of Retail Sales US")

realsales22_ts %>% 
  model(classical_decomposition(REALSALES, type = "additive")) %>% 
  components() %>% 
  autoplot()+ xlab("Year") + ggtitle("Classical additive decomposition of Retail Sales US")
corecpi_ts %>% 
  model(classical_decomposition(CORE, type = "additive")) %>% 
  components() %>% 
  autoplot()+ xlab("Year") + ggtitle("Classical additive decomposition of Core CPI US")
#############################
# Perform preliminary analysis
################################
realsales22_ts %>% 
  gg_tsdisplay(difference(log(REALSALES),12) %>% difference(), plot_type = "partial", lag = 36)+
  labs(title = "Seasonally differenced", y = "")

  realsales22fitnoseason <- realsales22_ts %>% 
  model(arima012011 = ARIMA(log(REALSALES) ~ pdq(0,1,2)+ PDQ(0,0,1)),
        arima110210 = ARIMA(log(REALSALES) ~ pdq(1,1,0)+ PDQ(2,0,0)),
        stepwise = ARIMA(REALSALES),
        search = ARIMA(log(REALSALES), stepwise = FALSE, approximation = FALSE))

        realsales22fitbig <- realsales22_ts %>% 
  model(best = ARIMA(log(REALSALES), stepwise = FALSE, approximation = FALSE, 
                 order_constraint = p + q + P + Q <=9))
  
report(realsales22fitbig)
realsales22fitbig %>%

augment(realsales22fitbig) %>% 
  features(.innov, ljung_box, lag = 12, dof = 6)
glance(realsales22fitnoseason)

realsales22fitnoseason %>% 
  pivot_longer(everything(),
               names_to = "Model name",
               values_to = "Orders")

realsales22fit %>% 
  pivot_longer(everything(),
               names_to = "Model name",
               values_to = "Orders")
realsales_ts %>%
  gg_tsdisplay(difference(sales, 12) %>% difference(), plot_type = "partial", lag = 36)+
  labs(title = "Seasonally differenced", y = "")
realsales_ts %>%
  gg_tsdisplay(difference(sales) %>% difference(), plot_type = "partial")
realretail_fit1 <- realsales_ts %>% 
  model(arima021 = ARIMA(sales ~ pdq(0,2,1)),
        arima120 = ARIMA(sales ~ pdq(1,2,0)),
        stepwise = ARIMA(sales),
        search = ARIMA(sales, stepwise = FALSE, approximation = FALSE,
                       order_constraint = p + q <= 9))
realretail_large_fit <- realsales_ts %>% 
  model(best = ARIMA(log(sales), 
                     stepwise = FALSE, approximation = FALSE,
                     order_constraint = p + q <= 9))
report(realretail_large_fit)
augment(realretail_large_fit) %>% 
  features(.innov, ljung_box, lag = 24, dof = 9)

realretail_fit1 %>% 
  forecast(h = 7) %>% 
  filter(.model =="search") %>% 
  model(naive = NAIVE(sales) %>% 
  autoplot(realsales_ts)

seasonal_fit <- realsales_ts %>% 
  model(arima110210 = ARIMA(sales ~ pdq(1,1,0) + PDQ(1,1,0)),
        arima012011 = ARIMA(sales ~ pdq(0,1,2) + PDQ(0,1,1)),
        auto = ARIMA(sales, stepwise = FALSE, approx = FALSE))
glance(seasonal_fit)
augment(seasonal_fit) %>% 
  filter(.model == "arima110210") %>% 
  features(.innov, ljung_box, lag = 12, dof = 3)
seasonal_fit %>% 
  pivot_longer(everything(),
    names_to = "Model name",
    values_to = "Orders")
glance(seasonal_fit) %>% 
  arrange(AICc) %>% 
  select(.model:BIC)
asymp_fit_seasonal <- realsales_ts %>% 
  model(best = ARIMA(log(sales), 
                     stepwise = FALSE, approximation = FALSE,
                     order_constraint = p + q + P + Q <= 9))

Sales22 <- realsales22_ts %>% 
      select(REALSALES) %>% 
      stretch_tsibble(.init = 6, .step = 1)

Sales_22_stretch_1 <- sales_income22_ts %>% 
  stretch_tsibble(.init = 6, .step = 1) %>% 
  filter(.id != max(.id))

fitsalesincome <- Sales_22_stretch_1 %>% 
  model(ARIMAX = ARIMA(log(REALSALES) ~ pdq(2,1,3)+ PDQ(0,0,1)+ log(PERSINCOME)),
        ARIMA = ARIMA(log(REALSALES) ~ pdq(2,1,3)+ PDQ(0,0,1)),
        RW(REALSALES ~ drift())
        )
test <- new_data(Sales_22_stretch_1, n=4) %>% 
  left_join(sales_income22_ts, by = "DATE")

fc <- forecast(fitsalesincome, new_data = test) 
  group_by(.id) %>%
  mutate(h = row_number()) %>%
  ungroup()

autoplot(fc, include = 60)

fc %>% accuracy(sales_income22_ts, include = 60)
  
fc %>% accuracy(sales_income22_ts, by=c("h", ".model")) %>% 
  select(h, RMSE)

fc %>% accuracy(sales_income22_ts)

Sales22_stretch <- realsales22_ts %>% 
  stretch_tsibble(.init = 6, .step = 1) %>% 
  filter(.id != max(.id))

sales_income_stretch <- sales_income %>% 
  stretch_tsibble(.init = 6, .step =1) %>% 
  filter(.id != max(.id))

fit22wincome <- sales_income_stretch %>% 
  model(RW(REALSALES ~ drift()),
        ARIMA = ARIMA(log(REALSALES)~ pdq(2,1,3)+PDQ(0,0,1)),
        ARIMAX = ARIMA(log(REALSALES)~realpersincome + pdq(2,1,3)+PDQ(0,0,1))
        )
fcincome <- fit22wincome %>% 
  forecast(h =1)
fcincome %>% accuracy(sales_income)

fit22 <- Sales22_stretch %>% 
  model(RW(REALSALES ~ drift()),
        ARIMA = ARIMA(log(REALSALES)~ pdq(2,1,3)+PDQ(0,0,1)))
fit22 %>% 
  forecast() %>% 
  autoplot(realsales22_ts)
fc22 <- fit22 %>% 
  forecast(h=3)
fc22 %>% accuracy(realsales22_ts)
autoplot(fc22) + theme(plot.width = 50,
                       plot.height = 50)
ggsave("C:\Users\adria\Documents\Uni\Uni_Gießen\seminar_statistics_and_econometrics\data\plots", fc, width = 200, height = 100)
dev.off()

stretch_income <- persincome22_ts %>% 
  stretch_tsibble(.init = 6, .step = 1)

fit <- stretch_income %>% 
  model(ARIMA(R))

forecast_income <- persincome22_ts %>% 
  model(ARIMA(realpersincome ~ pdq(1,0,0)))%>% 
  forecast(h = 1)

sales_income <- realsales22_ts %>% 
  left_join(forecast_income, by = "DATE")




