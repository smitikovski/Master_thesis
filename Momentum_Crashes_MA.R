### Script for the Master Thesis "Momentum Crashes" by Mirko Smit at Kiel University

library(rugarch)
library(data.table)
library(ggplot2)
library(stargazer)
library(readxl)

### Pathway to data
path_to_data = "/Users/mirkosmit/Documents/CAU/Master_Thesis/Data/"
xls_tickers = "DAX_Ticker_ISIN.xlsx"

### Read in relevant ticker names
Stocks = read_xlsx(paste0(path_to_data,xls_tickers))

### Get Stock data from Yahoo Finance API and store it





### Read in the data used in Daniel & Moskowitz
constituents = (,)


