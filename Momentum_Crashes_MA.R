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

### Read in the Data ´

raw_Dax_Data = "DAX_All.xlsx"

raw_FSE_monthly = "FSE_Monthly.xlsx"

EURIBOR_name = "EURIBOR.xlsx"

FIBOR_name = "FIBOR.xlsx"


### Read in relevant data
Stocks = fread(paste0(path_to_data,raw_Dax_Data))

FSE_Stocks = read_xlsx(paste0(path_to_data, raw_FSE_monthly))

# Import EURIBOR data and format it
EURIBOR = read_xlsx(paste0(path_to_data, EURIBOR_name))
setnames(EURIBOR, c("DATE", "RF_1M", "RF_3M"))
EURIBOR = as.data.table(EURIBOR)
EURIBOR[, DATE := ymd(DATE)]
EURIBOR[, "RF_3M" := NULL]

#Import FIBOR data and format it
FIBOR = read_xlsx(paste0(path_to_data, FIBOR_name))
setnames(FIBOR, c("DATE", "RF_1M"))
FIBOR = as.data.table(FIBOR)
FIBOR[, DATE := ymd(DATE)]
FIBOR = FIBOR[DATE <= "1999-01-01"]

# Transform FIBOR AND EURIBOR data to one continuous time series
RF_Interest = rbind.data.frame(FIBOR, EURIBOR)
setorder(RF_Interest, DATE)

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

# Calculate the Stock returns for the portfolio returns
 # Melt DT to calc the monthly returns

FSE_monthly_p_l = melt(FSE_monthly_p, id.vars = c("Date"))
FSE_monthly_p_l[, value := as.numeric(value)]
FSE_monthly_p_l[, Return := as.numeric((value - shift(value, n = 1)) / shift(value, n = 1)), by = variable]

# Calculate the cumulated return of month t-12 to t-2

#FSE_monthly_p_l[, Return_2L := as.numeric(shift(Return, n = 2, type = "lead")), by = variable]
FSE_monthly_p_l[, Return_cs := frollsum(Return, 11)] 

### Momentum Portfolio
# Rank stocks according to their return and put them into deciles

# Calculate deciles for each Date based on the Return column -> only rank non-NA returns 
FSE_monthly_p_l[, Decile := fifelse(
  !is.na(Return_cs),  
  as.integer(cut(Return_cs,
                 breaks = quantile(Return_cs, probs = seq(0, 1, 0.1), na.rm = TRUE),
                 include.lowest = TRUE, labels = FALSE)),
  NA_integer_), by = Date]

FSE_monthly_p_l[, Date := ymd(Date)]
FSE_monthly_p_l[ Date == "2019-05-03", .SD]


## Testing whether breaking up into deciles actually worked
FSE_monthly_p_l[ Date == "2019-05-03" & !is.na(Decile), .N]
FSE_monthly_p_l[ Date == "2019-05-03" & Decile == 1, .N]
FSE_monthly_p_l[ Date == "2019-05-03" & Decile ==10, .(Return_cs, Decile)]
FSE_monthly_p_l[ Date == "2019-05-03" & Decile ==1, .(Return_cs, Decile)]


## Testing another date
FSE_monthly_p_l[ Date == "2023-10-03" & !is.na(Decile), .N]
FSE_monthly_p_l[ Date == "2023-10-03" & Decile == 1, .N]
FSE_monthly_p_l[ Date == "2023-10-03" & Decile ==10, .(Return_cs, Decile)]
FSE_monthly_p_l[ Date == "2023-10-03" & Decile ==1, .(Return_cs, Decile)]

2022-10-03
FSE_monthly_p_l[ Date == "2022-10-03" & Decile == 10, .N]

688/77

# Seems to work
## Now calculate the return of the winners minus the return of the losers

# Formation one-month after evaluating the deciles of the returns
FSE_monthly_p_l[, PF_Formation := shift(Decile, n=1)]

WML_Data = FSE_monthly_p_l[PF_Formation == 10, .(WINNER = mean(Return_cs, na.rm = T) ), by = Date]
setorder(WML_Data, Date)
LOSERS = FSE_monthly_p_l[PF_Formation == 1, .(LOSER = mean(Return_cs, na.rm = T)), by = Date]
WML_Data = merge(WML_Data, LOSERS, by = "Date")
WML_Data[, WML := WINNER - LOSER]

WML_Data[LOSER == min(LOSER), ]

WML_Data[, max(WINNER)]
#1: 1988-11-03         NA    NA
#2: 2014-02-03   72.43583    NA
#3: 2016-01-03   83.98931    NA
#4: 2019-05-03 1814.81072    NA
#5: 2023-10-03  395.05834    NA

FSE_monthly_p_l[Date == "2014-02-03" & PF_Formation == 1] sum()


## Investigation of results
## 2024-12-03






# Create wide table again visual comparability
FSE_monthly_p_w = dcast(FSE_monthly_p_l, Date~variable, value.var = c("Return_cs"))








#Data_copy[, Metro := Data_copy[Date <= "1996-07-12" & Date >= "", Metro ]



### Read in the data used in Daniel & Moskowitz




