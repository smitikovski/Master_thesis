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


### Data cleansing
## Dax Stocks
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

Data_copy = copy(rel_dax_data)



## Cleanse for the XETRA-Data provided in the export file from EIKON
colnames(Data_copy)
Data_copy = Data_copy[, c(79:length(colnames(Data_copy)) ) := NULL]
colnames(Data_copy)

Adidas = Data_copy[, "ADIDAS"]
Adidas[, ADIDAS_XET := rel_dax_data[,"ADIDAS (XET)"]]
Adidas[, Date := rel_dax_data[,"Date"]]
## Capping the Data to the dates at which Stocks entered and left the DAX
colnames(rel_dax_data)

Cec = Data_copy[,"CECONOMY"]
Cec[,Date :=  Data_copy[, "Date"] ]

Stock_names = colnames(Data_copy)
writexl::write_xlsx(as.data.frame(Stock_names), path = path_to_data)

### Data Cleansing of monthly FSE Stock Data

colnames(FSE_Stocks)
FSE_stocknames = colnames(FSE_Stocks)

FSE_stocknames = gsub(" - TOT RETURN IND", "", FSE_stocknames)
FSE_stocknames = gsub(" - EARNINGS PER SHR", "", FSE_stocknames)

FSE_stocknames = unique(FSE_stocknames)
FSE_stocknames = FSE_stocknames[!grepl("#ERROR", FSE_stocknames)]
FSE_stocknames = sub("\\.\\.\\.\\..*$", "", FSE_stocknames)
FSE_stocknames = sub("\\.\\.\\..*$", "", FSE_stocknames)
FSE_stocknames = unique(FSE_stocknames)
FSE_stocknames


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

price_columns_to_keep


# Names of the stocks are now cleanly extracted
# -> Extract relevant Data from Import data.table

valid_stocknames <- FSE_stocknames[!FSE_stocknames %in% colnames(FSE_Stocks)]
valid_stocknames

rel_FSE_Stocks = FSE_Stocks[, ..FSE_stocknames]

FSE_Stocks[, "BAYWA"]

colnames(FSE_Stocks)


### Momentum Portfolio
## 

#Data_copy[, Metro := Data_copy[Date <= "1996-07-12" & Date >= "", Metro ]



### Read in the data used in Daniel & Moskowitz




