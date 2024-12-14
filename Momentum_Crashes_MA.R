### Script for the Master Thesis "Momentum Crashes" by Mirko Smit at Kiel University

library(rugarch)
library(data.table)
library(ggplot2)
library(stargazer)
library(readxl)
library(sandwich)
library(stringr)
library(lubridate)

### Pathway to data
path_to_data = "/Users/mirkosmit/Documents/CAU/Master_Thesis/Data/"
raw_Dax_Data = "DAX_All.xlsx"

raw_FSE_monthly = "FSE_Monthly.xlsx"

### Read in relevant ticker names
Stocks = fread(paste0(path_to_data,raw_Dax_Data))

FSE_Stocks = read_xlsx(paste0(path_to_data, raw_FSE_monthly))

### Data Cleansing of monthly FSE Stock Data

columns_to_keep = !grepl("#ERROR", colnames(FSE_Stocks))
FSE_Stocks = FSE_Stocks[, ..columns_to_keep]
setnames(FSE_Stocks, gsub("\\.\\.\\..*", "", colnames(FSE_Stocks)))


#Create new variable just for the Prices to calc returns etc.
FSE_monthly_p = copy(FSE_Stocks)
FSE_monthly_p = as.data.table(FSE_monthly_p)

search_to_drop = c(" - EARNINGS PER SHR"," - TOT RETURN IND")
pattern = paste(search_to_drop, collapse = "|")
price_columns_to_keep = !grepl(pattern, colnames(FSE_monthly_p))

FSE_monthly_p = FSE_monthly_p[, ..price_columns_to_keep]

unique_cols = !duplicated(colnames(FSE_monthly_p))

FSE_monthly_p = FSE_monthly_p[, ..unique_cols]
colnames(FSE_monthly_p)
setkey(FSE_monthly_p, Date)
setcolorder(FSE_monthly_p, sort(colnames(FSE_monthly_p)))
columns_to_keep2 = !grepl("#ERROR", colnames(FSE_monthly_p))
FSE_monthly_p = FSE_monthly_p[, ..columns_to_keep2]
setkey(FSE_monthly_p, Date)



# Stocks with 




### Momentum Portfolio
## 

#Data_copy[, Metro := Data_copy[Date <= "1996-07-12" & Date >= "", Metro ]



### Read in the data used in Daniel & Moskowitz




