library(readxl)
joined_prices <- read_excel("Downloads/joined_prices.xlsx", 
                            skip = 1)
#massaging this data to a correct format

joined_prices$dates_fixed <- as.Date(joined_prices$Date)

#removing redundant vars
joined_prices$...1 <- NULL

# Using Yahoo Finance API to download live data for my tickers
library(quantmod)
#Step1: using the API
getSymbols("WFC")
#giving our WFC a proper more generic name
stock1 <- getSymbols("WFC", auto.assign=FALSE)

stock2 <- getSymbols("SPY", auto.assign=FALSE)

fixed_income <- getSymbols("AGG", auto.assign=FALSE)

#Step2: we are going to join all 3 tables together:
joined_prices <- merge.xts(stock1, stock2, fixed_income)

#step3: selecting only adjusted prices - which are in 6th,12th, 18th var
joined_prices_only <- joined_prices[ , c(6,12,18)]

# visualizing our prices
chartSeries(stock1['2022-09'])
chartSeries(stock2['2022-09'])
chartSeries(fixed_income['2022-09'])
 
#session 3 
#step 4 :: intermediate version with dplyr
library(dplyr)
joined_returns_dplyr <- as.data.frame(joined_prices_only) %>%
   mutate(WFC_ROR = log(WFC.Adjusted/lag(WFC.Adjusted))) %>%
   mutate(SPY_ROR = log(SPY.Adjusted/lag(SPY.Adjusted))) %>%
   mutate(AGG_ROR = log(AGG.Adjusted/lag(AGG.Adjusted)))

min(joined_returns_dplyr[3996,4:6])

#going for different time windows
#step5:
n <- 21
joined_returns_dplyr <- as.data.frame(joined_prices_only) %>%
  mutate(WFC_ROR = log(WFC.Adjusted/lag(WFC.Adjusted, n ))) %>%
  mutate(SPY_ROR = log(SPY.Adjusted/lag(SPY.Adjusted, n ))) %>%
  mutate(AGG_ROR = log(AGG.Adjusted/lag(AGG.Adjusted, n )))

### Alternative to the above Step 4 and 5:

stock1_returns <- dailyReturn(getSymbols("WFC", auto.assign=FALSE))
stock2_returns <- dailyReturn(getSymbols("SPY", auto.assign=FALSE))
fixed_income_returns <- dailyReturn(getSymbols("AGG", auto.assign=FALSE))

joined_dailyreturns <- merge.xts(stock1_returns, stock2_returns, fixed_income_returns)

#monthly
stock1_returns <- monthlyReturn(getSymbols("WFC", auto.assign=FALSE))
stock2_returns <- monthlyReturn(getSymbols("SPY", auto.assign=FALSE))
fixed_income_returns <- monthlyReturn(getSymbols("AGG", auto.assign=FALSE))

joined_monthlyreturns <- merge.xts(stock1_returns, stock2_returns, fixed_income_returns)

#annual
#stock1_returns <- annualReturn(getSymbols("WFC", auto.assign=FALSE))
#stock2_returns <- annualReturn(getSymbols("SPY", auto.assign=FALSE))
#fixed_income_returns <- annualReturn(getSymbols("AGG", auto.assign=FALSE))

#joined_annualreturns <- merge.xts(stock1_returns, stock2_returns, fixed_income_returns)

#the next is optional - but real life
joined_returns_loop <- as.data.frame(joined_prices_only)
joined_returns_loop$log_ret <- c()
joined_returns_loop$log_ret[1] <- NA

for(i in 2:nrow(joined_returns_loop)){
  joined_returns_loop$log_ret[i]<- 
    log(joined_returns_loop$WFC.Adjusted[i]/joined_returns_loop$WFC.Adjusted[i-1])
}

#session 4
#calculating portfolio returns

WFC_alloc <- 0.3
SPY_alloc <- 0.25
AGG_alloc <- 0.45
#these have to add to 1

joined_portfolio_ret <- as.data.frame(joined_monthlyreturns) %>%
  mutate(portfolio= WFC_alloc*monthly.returns+
                    SPY_alloc * monthly.returns.1 +
                    AGG_alloc*monthly.returns.2)

# adding the Russell 1000 VONE to our returns as a benchmark

benchmark_returns <- monthlyReturn(getSymbols("VONE", auto.assign=FALSE))

joined_monthlyreturns <- merge.xts(stock1_returns, stock2_returns,
                                   fixed_income_returns, benchmark_returns)

#calculating sigma for the last 12 months
time_index <- nrow(joined_monthlyreturns)

joined_monthlyreturns <- as.data.frame(joined_monthlyreturns)

WFC_sigma <- sd(joined_monthlyreturns$monthly.returns[time_index:(time_index-11)])*sqrt(12)

SPY_sigma <- sd(joined_monthlyreturns$monthly.returns.1[time_index:(time_index-11)])*sqrt(12)

AGG_sigma <- sd(joined_monthlyreturns$monthly.returns.2[time_index:(time_index-11)])*sqrt(12)

VONE_sigma <- sd(joined_monthlyreturns$monthly.returns.3[time_index:(time_index-11)])*sqrt(12)

portfolio_sigma <- sd(joined_portfolio_ret$portfolio[time_index:(time_index-11)])*sqrt(12)

#calculating tracking error 

WFC_te <- sd(joined_monthlyreturns$monthly.returns[time_index:(time_index-11)]-
                  joined_monthlyreturns$monthly.returns.3[time_index:(time_index-11)])*sqrt(12)

SPY_te <- sd(joined_monthlyreturns$monthly.returns.1[time_index:(time_index-11)]-
                  joined_monthlyreturns$monthly.returns.3[time_index:(time_index-11)])*sqrt(12)

AGG_te <- sd(joined_monthlyreturns$monthly.returns.2[time_index:(time_index-11)]-
                  joined_monthlyreturns$monthly.returns.3[time_index:(time_index-11)])*sqrt(12)

VONE_te <- sd(joined_monthlyreturns$monthly.returns.3[time_index:(time_index-11)]-
                   joined_monthlyreturns$monthly.returns.3[time_index:(time_index-11)])*sqrt(12)

portfolio_te <- sd(joined_portfolio_ret$portfolio[time_index:(time_index-11)]-
                        joined_monthlyreturns$monthly.returns.3[time_index:(time_index-11)])*sqrt(12)

### Sharpe risk metric

riskfree <- 0.001

WFC_sharpe <- (mean(joined_monthlyreturns$monthly.returns[time_index:(time_index-11)])-riskfree)/WFC_sigma

SPY_sharpe <- (mean(joined_monthlyreturns$monthly.returns.1[time_index:(time_index-11)])-riskfree)/SPY_sigma

AGG_sharpe <- (mean(joined_monthlyreturns$monthly.returns.2[time_index:(time_index-11)])-riskfree)/AGG_sigma

bench_sharpe <- (mean(joined_monthlyreturns$monthly.returns.3[time_index:(time_index-11)])-riskfree)/VONE_sigma

portfolio_sharpe <- (mean(joined_portfolio_ret$portfolio[time_index:(time_index-11)])-riskfree)/portfolio_sigma

#try looking at the correlation/covariance

cor(joined_monthlyreturns[time_index:(time_index-59),], use='complete.obs')

#session 6
#designing quasi CAPM models

time_index <- nrow(joined_monthlyreturns)

last_12_months <- joined_monthlyreturns[time_index:(time_index-11),]
# designing the CAPM model for WFC

WFC_reg <- lm(monthly.returns~monthly.returns.3 , data=last_12_months)
summary(WFC_reg)

SPY_reg <- lm(monthly.returns.1~monthly.returns.3 , data=last_12_months)
summary(SPY_reg)

AGG_reg <- lm(monthly.returns.2~monthly.returns.3 , data=last_12_months)
summary(AGG_reg)

#random sampling out of the large data frame
#testing our model

sample_indx <- sample(1:nrow(joined_monthlyreturns), size=5)

sample_data <- joined_monthlyreturns[sample_indx,]

predict(WFC_reg, sample_data)

predict(AGG_reg, sample_data)

