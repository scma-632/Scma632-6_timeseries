#https://kamran-afzali.github.io/posts/2021-09-25/PortfolioOptimization.html
#https://www.codingfinance.com/post/2018-05-31-portfolio-opt-in-r/
#https://medium.com/@junko_hutahaean/modern-finance-portfolio-optimization-with-r-aa4a10becd78

library(tidyquant) 
library(timetk) 
library(forcats)
library(tidyr)
install.packages('kableExtra')
library(kableExtra)
library(ggplot2)
library(dplyr)

tick <- c('3MINDIA.NS','INFY.NS', 'TCS.NS', 'WIPRO.NS','^NSEI','TATAELXSI.NS', 'RAMCOSYS.NS', 'NUCLEUS.NS','SONATSOFTW.NS',
              'ONWARDTEC.NS', 'AFFLE.NS', 'ROLTA.NS','COFORGE.NS','HAPPSTMNDS.NS',
               'BSOFT.NS','GODREJPROP.NS', 'GODREJPROP.NS', 'OBEROIRLTY.NS', 'SOBHA.NS','BRIGADE.NS','LT.NS','PURVA.NS',
               'PURVA.NS','PRESTIGE.NS','MAHLIFE.NS','NH.NS', 'MAXHEALTH.NS', 'APOLLOHOSP.NS', 'FORTIS.NS',  
               'ASTERDM.NS', 'DRAGARWQ.BO','HDFCBANK.NS', 'BANDHANBNK.NS', 'IDFCFIRSTB.NS', 'ICICIBANK.NS', 'FEDERALBNK.NS', 
               'AXISBANK.NS', 'INDUSINDBK.NS','RBLBANK.NS', 'AUBANK.NS', 'KOTAKBANK.NS', 'DCBBANK.NS', 'CUB.NS',
               'PSB.NS', 'BANKINDIA.NS', 'UNIONBANK.NS', 'UCOBANK.NS','IOB.NS', 'INDIANB.NS','CENTRALBK.NS','BANKBARODA.NS',
               'MAHABANK.NS','PNB.NS','CANBK.NS','CIPLA.NS', 'ABBOTINDIA.NS', 'ABB.NS', 'LT.NS','LTIM.NS', 
               'VIMTALABS.NS', 'ALKEM.NS', 'TATASTEEL.NS', 'TATAMOTORS.NS', 'M&M.NS', 'BAJFINANCE.NS', 'BAJAJFINSV.NS',
               'M&MFIN.NS','BAJAJ-AUTO.NS', 'TVSMOTOR.NS', 'BHEL.NS','TATAPOWER.NS', 'WONDERLA.NS', 
               'JSWSTEEL.NS', 'HINDALCO.NS','BOSCHLTD.NS', 'CONCOR.NS', 'BRITANNIA.NS', 'EICHERMOT.NS', 
               'HDFCBANK.NS', 'ICICIBANK.NS', 'HEROMOTOCO.NS', 'PAGEIND.NS','SRF.NS', 'COLPAL.NS', 'SHREECEM.NS', 
               'MCDOWELL-N.NS', 'VBL.NS','ADANIENT.NS','ADANIPORTS.NS', 'ADANIGREEN.NS', 'PAGEIND.NS', 'HONAUT.NS','INDHOTEL.NS',
               'ITC.NS','VIPIND.NS','PIIND.NS','PERSISTENT.NS','JUBLFOOD.NS','AVANTIFEED.NS','DMART.NS','AGI.NS','ALKEM.NS',
               'AKZOINDIA.NS','MOTHERSON.NS','ESCORTS.NS','DIVISLAB.NS', 'ICICIBANK.NS', 'HEROMOTOCO.NS', 'PAGEIND.NS','SRF.NS', 
               'COLPAL.NS', 'SHREECEM.NS','ASHOKLEY.NS','TATACHEM.NS','PIDILITIND.NS','VIPIND.NS','PIIND.NS','PERSISTENT.NS',
               'JUBLFOOD.NS','AVANTIFEED.NS','DMART.NS','AGI.NS','ALKEM.NS','AKZOINDIA.NS', 'VBL.NS','ADANIENT.NS','ADANIPORTS.NS',
               'ADANIGREEN.NS', 'PAGEIND.NS', 'HONAUT.NS','INDHOTEL.NS')

price_data <- tq_get(tick,
                     from = '2019-07-22',
                     to = '2024-07-22',
                     get = 'stock.prices')

log_ret_tidy <- price_data %>%
  dplyr::group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ret',
               type = 'log')

head(log_ret_tidy)%>%kable()

log_ret_xts <- log_ret_tidy %>%
  spread(symbol, value = ret) %>%
  tk_xts()

log_ret_xts=log_ret_xts%>%as.data.frame()%>%drop_na()

summary(log_ret_xts)%>%kable()

head(log_ret_xts)%>%kable()
mean_ret <- colMeans(log_ret_xts,na.rm = T)
print(round(mean_ret, 5))%>%kable()

cov_mat <- cov(log_ret_xts) * 252
print(round(cov_mat,4))%>%kable()

wts <- runif(n = length(tick))
wts <- wts/sum(wts)
print(wts)%>%kable()


port_returns <- (sum(wts * mean_ret) + 1)^252 - 1


port_risk <- sqrt(t(wts) %*%(cov_mat %*% wts) )
print(port_risk)%>%kable()

sharpe_ratio <- port_returns/port_risk
print(sharpe_ratio)%>%kable()


num_port <- 10000

# Creating a matrix to store the weights

all_wts <- matrix(nrow = num_port,
                  ncol = length(tick))

# Creating an empty vector to store
# Portfolio returns

port_returns <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Standard deviation

port_risk <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Sharpe Ratio

sharpe_ratio <- vector('numeric', length = num_port)



for (i in seq_along(port_returns)) {
  
  wts <- runif(length(tick))
  wts <- wts/sum(wts)
  
  # Storing weight in the matrix
  all_wts[i,] <- wts
  
  # Portfolio returns
  
  port_ret <- sum(wts * mean_ret)
  port_ret <- ((port_ret + 1)^252) - 1
  
  # Storing Portfolio Returns values
  port_returns[i] <- port_ret
  
  
  # Creating and storing portfolio risk
  port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
  port_risk[i] <- port_sd
  
  # Creating and storing Portfolio Sharpe Ratios
  # Assuming 0% Risk free rate
  
  sr <- port_ret/port_sd
  sharpe_ratio[i] <- sr
  
}

portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)


# Converting matrix to a tibble and changing column names
all_wts <- tk_tbl(all_wts)


colnames(all_wts) <- colnames(log_ret_xts)

# Combing all the values together
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))



head(portfolio_values)%>%kable()


min_var <- portfolio_values[which.min(portfolio_values$Risk),]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]


p <- min_var %>%
  gather(EXI:RXI, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

p


p <- max_sr %>%
  gather(EXI:RXI, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Tangency Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

p



p <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Annualized Risk',
       y = 'Annualized Returns',
       title = "Portfolio Optimization & Efficient Frontier") +
  geom_point(aes(x = Risk,
                 y = Return), data = min_var, color = 'red') +
  geom_point(aes(x = Risk,
                 y = Return), data = max_sr, color = 'red')
p

