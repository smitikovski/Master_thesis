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

### Read in relevant ticker names
Stocks = fread(paste0(path_to_data,raw_Dax_Data))

### Data cleansing

colnames(Stocks)

Stocknames = colnames(Stocks)
Stocknames_1 = list()
#### extract the Stocknames from the colnames
Stocknames = Stocknames[1:length(Stocknames)]
Stocknames = gsub(" - TOT RETURN IND","",Stocknames)
Stocknames = gsub(" - EARNINGS PER SHR","",Stocknames)
Stocknames = gsub(" (XET) ","",Stocknames)
##Stocknames = gsub("\\(.*?)","",Stocknames)
Stocknames = str_trim(Stocknames, side = "both")
Stocknames = unique(Stocknames)
Stocknames

### Filter for relevant Data
Stocks = as.data.table(Stocks)
setindex(Stocks, ...1)
rel_dax_data = Stocks[, ..Stocknames]
setnames(rel_dax_data, old = c("...1"), new = c("Date"))
rel_dax_data[, Date := date(Date)]



### 





### Read in the data used in Daniel & Moskowitz




