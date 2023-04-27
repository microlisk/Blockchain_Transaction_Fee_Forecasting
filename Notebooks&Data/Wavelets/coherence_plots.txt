# Below used to generate wavelet coherence plots in r studio
library(biwavelet)
library(lubridate)
library(repr)

#Load merged data at 5 min sample rate
Data <- read.csv("C:\\Users\\conal\\Desktop\\MCM\\Practicum - Copy\\data\\all_data_5min 11-26 to 05-26.csv")



# Columns to series
datetime = as.POSIXct(Data$datetime, format="%Y-%m-%d %H:%M:%S")
gas_limit_  = Data$gas_limit
gas_used_  = Data$gas_used
Open_  = Data$Open
avg_gas_price_  = Data$avg_gas_price
max_gas_price_  = Data$max_gas_price
min_gas_price_ = Data$min_gas_price
block_utilisation_  = Data$block_utilisation
contracts_ =Data$contracts
transaction_count_ =Data$transaction_count
base_fee_per_gas_ =Data$base_fee_per_gas
block_gas_5th_percentile_=Data$block_gas_5th_percentile
block_gas_95th_percentile_=Data$block_gas_95th_percentile

#To time series with datetime
Gas_Limit = cbind(datetime, gas_limit_)
Gas_Used = cbind(datetime, gas_used_)
Open_ts = cbind(datetime, Open_)
Minimum_Gas_Price  = cbind(datetime, min_gas_price_)
Average_Gas_Price = cbind(datetime, avg_gas_price_)
Maximum_Gas_Price = cbind(datetime, max_gas_price_)
Block_Utilisation = cbind(datetime, block_utilisation_)
Contract_Counts = cbind(datetime,contracts_)
Transaction_Counts = cbind(datetime,transaction_count_)
Base_Fee = cbind(datetime,base_fee_per_gas_)
Ninety_Fifth_Percentile_Gas_Price = cbind(datetime,block_gas_95th_percentile_)
Fifth_Percentile_Gas_Price = cbind(datetime,block_gas_5th_percentile_)



str <- function(v1) {
  deparse(substitute(v1))
}

val=Contract_Counts
#Select series for coherence
t1 = na.omit(Minimum_Gas_Price)
t2 = na.omit(Fifth_Percentile_Gas_Price)
nrands = 50

# Use biwavelet library to generate coherence matrix
wtc.t1t2 = wtc(t1, t2, nrands = nrands, s0 = 3600, max.scale = 1209600, mother = 'morlet')
wtc.t1t2$period <- wtc.t1t2$period/60



# Plotting a graph
dev.new(width=12,height=6,noRStudioGD = TRUE)
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 10) + 0.1)

title=paste("Wavelet Coherence: Minimum Gas Price vs ", gsub("_", " ", str(95th Percentile)), "(Morlet)")
title=paste("Wavelet Coherence: Minimum Gas Price vs ", "5th Percentile Gas Price", "(Morlet)")
plot(wtc.t1t2, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
    lwd.sig = 2, arrow.lwd = 0.03, arrow.len = 0.12, ylab = "Scale", xlab = "Period",xaxt = "n", 
    plot.cb = TRUE, main = title)


# Adding grid lines

verticle_lines <- as.numeric(seq(as.POSIXct("2021/11/26"), as.POSIXct("2022/5/26"), "weeks"))
abline(v = verticle_lines, h = 1:16, col = "brown", lty = 1, lwd = 1)

# Defining x labels


axis.POSIXct(1, at=seq(datetime[1], tail(datetime,1), by='168 hours'), format = "%m-%d")






