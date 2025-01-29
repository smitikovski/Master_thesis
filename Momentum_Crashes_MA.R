### Script for the Master Thesis "Momentum Crashes" by Mirko Smit at Kiel University

library(rugarch)
library(data.table)
library(ggplot2)
library(stargazer)
library(readxl)
library(sandwich)
library(stringr)
library(lubridate)
library(timeDate)
library(tseries)
library(sjPlot)
library(PerformanceAnalytics)
library(zoo)
### PathsjPlot### Pathway to data
path_to_data = "/Users/mirkosmit/Documents/CAU/Master_Thesis/Data/"
path_to_output = "/Users/mirkosmit/Documents/CAU/Master_Thesis/Output/"
### Read in the Data ´

CDAX_IND = "CDAX_IND.xlsx"
raw_FSE_daily = "FGKURS_MS.xlsx"

EURIBOR_name = "EURIBOR.xlsx"
FIBOR_name = "FIBOR.xlsx"

### Read in relevant data
FSE_Daily = read_xlsx(paste0(path_to_data, raw_FSE_daily))

### Import CDAX Performance Index data and format it
CDAX_IND = read_xlsx(paste0(path_to_data, CDAX_IND))
CDAX_IND = as.data.table(CDAX_IND)
CDAX_IND[, DATE := as.Date(DATE)]
CDAX_IND[, DATE := ymd(DATE)]
setnames(CDAX_IND, c("DATE","M_IND"))

# Import EURIBOR data and format it
EURIBOR = read_xlsx(paste0(path_to_data, EURIBOR_name))
setnames(EURIBOR, c("DATE", "RF_1M", "RF_3M"))
EURIBOR = as.data.table(EURIBOR)
EURIBOR[, DATE := as.Date(DATE)]
EURIBOR[, DATE := format(DATE, "%Y-%m")]
EURIBOR[, "RF_3M" := NULL]

#Import FIBOR data and format it
FIBOR = read_xlsx(paste0(path_to_data, FIBOR_name))
setnames(FIBOR, c("DATE", "RF_1M"))
FIBOR = as.data.table(FIBOR)
FIBOR[, DATE := as.Date(DATE)]
FIBOR[, DATE := format(DATE, "%Y-%m")]
FIBOR = FIBOR[DATE <= "1999-01-01"]

# Transform FIBOR AND EURIBOR data to one continuous time series
RF_Interest = rbind.data.frame(FIBOR, EURIBOR)
setorder(RF_Interest, DATE)

# Transform FIBOR and EURIBOR Rates into percentage points
RF_Interest[, RF_1M := as.numeric(RF_1M)]
RF_Interest[, RF_1M_M := (1 + RF_1M / 100) ^ (1/12) - 1]
RF_Interest[, RF_1M_D := (1 + RF_1M / 100) ^ (1/252) - 1]
#RF_Interest[, DATE := format(DATE, "%Y-%m")]

###### Data Cleansing ############################################

## Daily FSE Stock Data
FSE_Daily = as.data.table(FSE_Daily)
setnames(FSE_Daily, c("DATE"),c( "DATE"))
FSE_Daily[, DATE := ymd(DATE)]
col_names <- colnames(FSE_Daily)
columns_to_keep_daily = !grepl("#ERROR", colnames(FSE_Daily))
FSE_Daily = FSE_Daily[, ..columns_to_keep_daily]
setnames(FSE_Daily, gsub("\\.\\.\\..*", "", colnames(FSE_Daily)))

# Drop unnecessary data to make processing of data quicker
search_to_drop_daily = c(" - EARNINGS PER SHR"," - TOT RETURN IND", " - TURNOVER BY VALUE", " - TURNOVER BY VOLUME")
pattern_d = paste(search_to_drop_daily, collapse = "|")
price_columns_to_keep_daily = !grepl(pattern_d, colnames(FSE_Daily))
FSE_Daily = FSE_Daily[, ..price_columns_to_keep_daily]


######## Remove any data that is collected on public holidays (or remnants of it)
# Ensure your start_date and end_date are properly set
start_date <- as.Date(min(FSE_Daily$DATE))  
end_date <- as.Date(max(FSE_Daily$DATE))

# Function to calculate specified holidays for Frankfurt Stock Exchange
frankfurt_selected_holidays <- function(start_date, end_date) {
  # Generate Easter-related holidays
  rel_years <- seq(as.numeric(format(start_date, "%Y")), as.numeric(format(end_date, "%Y")))
  easter <- timeDate::Easter(rel_years)
  good_friday <- as.Date(easter) - 2
  easter_monday <- as.Date(easter) + 1

  fixed_holidays <- c(
    "01-01", # New Year's Day
    "05-01", # Labour Day
    "12-24", # Christmas Eve
    "12-25", # Christmas Day
    "12-26", # Boxing Day
    "12-31"  # New Year's Eve
  )
  fixed_holidays <- as.Date(paste0(rel_years, "-", fixed_holidays), format = "%Y-%m-%d")
  holidays <- unique(c(good_friday, easter_monday, fixed_holidays))
  holidays <- holidays[holidays >= start_date & holidays <= end_date]
  return(holidays)
}
frankfurt_selected_holidays_list <- frankfurt_selected_holidays(start_date, end_date)
FSE_Daily <- FSE_Daily[!(DATE %in% frankfurt_selected_holidays_list)]

# Replace values after delisting date with NA
delist_pattern <- "DELIST\\.(\\d{2}/\\d{2}/\\d{2})$"
delist_cols <- grep(delist_pattern, col_names, value = TRUE)

# Iterate over delisted columns and fill in NA for each value after delisting date
for (col in delist_cols) {
  delist_date = sub(delist_pattern, "\\1", col)
  delist_date = dmy(delist_date)
  FSE_Daily[DATE > delist_date, (col) := NA]
}

### Excluding all stocks that have less than 9 months of data

stock_columns <- setdiff(names(FSE_Daily), "DATE")
valid_stocks <- sapply(stock_columns, function(col) {
  sum(!is.na(FSE_Daily[[col]]))
})
stocks_to_keep <- names(valid_stocks[valid_stocks >= 270])
FSE_Daily <- FSE_Daily[, c("DATE", stocks_to_keep), with = FALSE]
colnames(FSE_Daily)

stock_columns = setdiff(names(FSE_Daily), "DATE")

first_values <- FSE_Daily[, lapply(.SD, function(x) x[which(!is.na(x))[1]]), .SDcols = stock_columns]
first_values_vector <- unlist(first_values, use.names = TRUE)
valid_stocks <- names(first_values_vector[first_values_vector >= 1])
FSE_Daily = FSE_Daily[, c("DATE", valid_stocks), with = FALSE]

## Make a data copy so data transformations will not have an impact on the initial file ### to be removed - check
FSE_daily_p = copy(FSE_Daily)
FSE_daily_p = as.data.table(FSE_daily_p)

# Making sure there is only one column per stock
unique_cols_d = !duplicated(colnames(FSE_daily_p))
FSE_daily_p = FSE_daily_p[, ..unique_cols]
colnames(FSE_daily_p)
setcolorder(FSE_daily_p, sort(colnames(FSE_daily_p)))
columns_to_keep2_d = !grepl("#ERROR", colnames(FSE_daily_p))
FSE_daily_p = FSE_daily_p[, ..columns_to_keep2_d]
setnames(FSE_daily_p, c(""),c( "DATE"))
setkey(FSE_daily_p, DATE)

###Calculate the Stock returns for the portfolio returns
 # Melt DT to calc the monthly returns

#FSE_monthly_p_l = melt(FSE_monthly_p, id.vars = c("Date"))
#FSE_monthly_p_l[, value := as.numeric(value)]
#FSE_monthly_p_l[, Return := as.numeric((value - shift(value, n = 1)) / shift(value, n = 1)), by = variable]

# Calculate the daily stock returns
Market_value_cols = grepl("MARKET VALUE", colnames(FSE_daily_p))
Market_value_cols = colnames(FSE_daily_p[, ..Market_value_cols])
Price_cols = !grepl("MARKET VALUE", colnames(FSE_daily_p))
Price_cols = colnames(FSE_daily_p[, ..Price_cols])
FSE_daily_p_l = melt(FSE_daily_p, id.vars = c("DATE"), measure.vars = list(Price_cols), variable.name = "Stock", value.name = c("Price"))
MV_to_merge = melt(FSE_daily_p, id.vars = c("DATE"), measure.vars = list(Market_value_cols), variable.name = "Stock", value.name = c("MV"))
MV_to_merge[, Stock := gsub(" - MARKET VALUE", "", MV_to_merge$Stock)]
FSE_daily_p_l = merge(FSE_daily_p_l, MV_to_merge, by = c("DATE", "Stock"))
setorder(FSE_daily_p_l, Stock, DATE)
FSE_daily_p_l[, DATE := ymd(DATE)]
FSE_daily_p_l[, Stock := as.character(Stock)]

######## TO DO - Testen!!!! 
uniqueN(FSE_daily_p_l$Stock)
FSE_daily_p_l[, Price := as.numeric(Price)]
FSE_daily_p_l[, MV := as.numeric(MV)]

#FSE_daily_p_l[, Daily_Return := as.numeric((Price - shift(Price, n = 1)) / shift(Price, n = 1)), by = Stock]
#FSE_daily_p_l[!is.na(Daily_Return)]

# Get the monthly returns, market value at the end of the months and the 11 month returns from daily data

### new new new first record per month extraction since ceiling date does not work
FSE_Daily[, DATE := as.Date(DATE)]

# Sort the dataset by DATE to ensure proper order
setorder(FSE_Daily, DATE)

# Function to get the first available date in each month
get_last_in_month <- function(dates) {
  # Extract the year-month for grouping
  dates_dt <- data.table(DATE = dates, YearMonth = floor_date(dates, "month"))
  
  # Find the last date in each month
  result <- dates_dt[, .(Last_DATE = max(DATE)), by = YearMonth]
  
  return(result)
}

### Get ceiling dates for the daily data of German stocks
last_dates <- get_last_in_month(FSE_daily_p_l$DATE)
last_dates[, Last_DATE := as.Date(Last_DATE)] 

# Merge with main data table
FSE_ceiling = FSE_daily_p_l[last_dates, on = .(DATE = Last_DATE)]
FSE_ceiling[, YearMonth := NULL]
FSE_ceiling[, DATE := format(DATE, "%Y-%m")]


### Create 11 month return as defined in Jegadeesh 1993 and monthly return of the stocks
FSE_ceiling[, Return_11M := as.numeric(log(1+((Price - shift(Price, n = 11)) / shift(Price, n = 11)))), by = Stock]
FSE_ceiling[, REL_RET := shift(Return_11M, n = 1, type = "lag"), by = Stock]
FSE_ceiling[, REL_MV := shift(MV, n = 1, type = "lag"), by = Stock]
FSE_ceiling[, Return_1M := as.numeric(log(1+((Price - shift(Price, n = 1)) / shift(Price, n = 1)))), by = Stock]
 
##### Cleansing of stock return data
## Cleanse return spices
FSE_ceiling = FSE_ceiling[Return_1M <= 9.9]




### Get ceiling dates for the daily data of CDAX Performance Index for the calc of monthly returns
last_dates = get_last_in_month(CDAX_IND$DATE)
last_dates[, Last_DATE := as.Date(Last_DATE)]
CDAX_M = CDAX_IND[last_dates, on = .(DATE = Last_DATE)]
CDAX_M[,YearMonth := NULL]
CDAX_M[,DATE := format(DATE, "%Y-%m")]
CDAX_M[, RM_1M := as.numeric(log(1+((M_IND - shift(M_IND, n = 1)) / shift(M_IND, n = 1))))]

# Assign the stocks of each month to a decile according to their 11 month return
FSE_ceiling[, Decile := fifelse(
  !is.na(REL_RET),  
  as.integer(cut(REL_RET,
                breaks = quantile(REL_RET, probs = seq(0, 1, 0.1), na.rm = TRUE),
                 include.lowest = TRUE, labels = FALSE)),
  NA_integer_), by = DATE]


# Replace values based on inactivity for 3 months - subsequent value = NA after first month if 3 months consecutive same price
FSE_ceiling[, Price := {
  unchanged_count <- rleid(Price)
  repeated_lengths <- rle(Price)$lengths
  repeated <- repeated_lengths[unchanged_count] >= 3
  if (any(repeated)) {
    first_inactive <- which(repeated)[1]  # Find the start of the first inactive period
    Price[seq(first_inactive + 1, .N)] <- NA  # Replace all subsequent prices
  }
  Price
}, by = Stock]

FSE_daily_p_l[, .(Unique_Stocks = uniqueN(Stock)), by = DATE][Unique_Stocks < 11, DATE]
FSE_ceiling[!is.na(Price), .(Stocks_Non_NA = uniqueN(Stock)), by = DATE][Stocks_Non_NA < 11, DATE]
FSE_ceiling[DATE == "2020-05" & !is.na(Price)]

# Weighting the stocks within their deciles according to their market cap - as it is a value weighted portfolio
FSE_ceiling[, WEIGHT := REL_MV/sum(REL_MV), by = .(DATE, Decile)]
FSE_ceiling[, WRET := Return_1M*WEIGHT]

FSE_ceiling[ DATE == "2024-01" & Decile  == 10]
FSE_ceiling[ DATE == "2024-02" & Decile  == 10, sum(WRET)]
FSE_ceiling[ DATE == "2024-03" & Decile  == 1, sum(WRET)]

MOM_Data[DATE == "2010-07", WML]

FSE_ceiling[ DATE == "2024-03-01" & Decile  == 10, sum(WEIGHT)]
FSE_ceiling[ DATE == "2024-03-01" & Decile  == 10, sum(WRET)]
FSE_ceiling[ DATE == "2024-04-02" & Decile  == 9, sum(WRET, na.rm = T)]
FSE_ceiling[ DATE == "2024-05-01" & Decile  == 10, sum(WRET)]
FSE_ceiling[ DATE == "2024-01-01" & Decile  == 10, sum(WRET)]
FSE_ceiling[ DATE == "2024-04-04" & Decile  == 2]

FSE_ceiling[ Decile  == 10, mean(WRET)]

FSE_ceiling[ DATE == "2024-01" & Decile  == 1, sum(WEIGHT)]

# Creation of Returns per decile
MOM_Data = FSE_ceiling[Decile==10 , sum(WRET, na.rm = T), by = DATE]
MOM_Data1= FSE_ceiling[Decile==9 , sum(WRET, na.rm = T), by = DATE]
MOM_Data2= FSE_ceiling[Decile==8 , sum(WRET, na.rm = T), by = DATE]
MOM_Data3= FSE_ceiling[Decile==7 , sum(WRET, na.rm = T), by = DATE]
MOM_Data4= FSE_ceiling[Decile==6 , sum(WRET, na.rm = T), by = DATE]
MOM_Data5= FSE_ceiling[Decile==5 , sum(WRET, na.rm = T), by = DATE]
MOM_Data6= FSE_ceiling[Decile==4 , sum(WRET, na.rm = T), by = DATE]
MOM_Data7= FSE_ceiling[Decile==3 , sum(WRET, na.rm = T), by = DATE]
MOM_Data8= FSE_ceiling[Decile==2 , sum(WRET, na.rm = T), by = DATE]
MOM_Data9= FSE_ceiling[Decile==1 , sum(WRET, na.rm = T), by = DATE]

setnames(MOM_Data, c("DATE", "WINNER"))
setnames(MOM_Data1, c("DATE", "NINE"))
setnames(MOM_Data2, c("DATE", "EIGHT"))
setnames(MOM_Data3, c("DATE", "SEVEN"))
setnames(MOM_Data4, c("DATE", "SIX"))
setnames(MOM_Data5, c("DATE", "FIVE"))
setnames(MOM_Data6, c("DATE", "FOUR"))
setnames(MOM_Data7, c("DATE", "THREE"))
setnames(MOM_Data8, c("DATE", "TWO"))
setnames(MOM_Data9, c("DATE", "LOSER"))

setorder(MOM_Data, DATE)

#Merging all the created portfolios into one data.table
MOM_Data = merge(MOM_Data, MOM_Data1, by = c("DATE"))
MOM_Data = merge(MOM_Data, MOM_Data2, by = c("DATE"))
MOM_Data = merge(MOM_Data, MOM_Data3, by = c("DATE"))
MOM_Data = merge(MOM_Data, MOM_Data4, by = c("DATE"))
MOM_Data = merge(MOM_Data, MOM_Data5, by = c("DATE"))
MOM_Data = merge(MOM_Data, MOM_Data6, by = c("DATE"))
MOM_Data = merge(MOM_Data, MOM_Data7, by = c("DATE"))
MOM_Data = merge(MOM_Data, MOM_Data8, by = c("DATE"))
MOM_Data = merge(MOM_Data, MOM_Data9, by = c("DATE"))
CDAX_IND[, DATE := ym(DATE)]
MOM_Data = merge(MOM_Data, CDAX_M, by = c("DATE"), all.x = T)
MOM_Data[, WML := WINNER-LOSER]

## Add the Risk-free data (FIBOR & EURIBOR one-month interest rates)
MOM_Data = merge(MOM_Data, RF_Interest, by = c("DATE"), all.x = T )

MOM_Data[, RF_1M:= NULL]

MOM_Data[!is.na(WINNER) & DATE > "1990-06" , value_of_a_euro_winners := cumprod(1+WINNER)]
MOM_Data[!is.na(LOSER) & DATE > "1990-06" , value_of_a_euro_losers := cumprod(1+LOSER)]
MOM_Data[!is.na(WML) & DATE > "1990-06"    , value_of_a_euro_wml := cumprod(1+WML)]
MOM_Data[ !is.na(RF_1M) & DATE > "1990-06"    , value_of_a_euro_rf := cumprod(1+RF_1M)]
MOM_Data[!is.na(RM_1M) & DATE > "1990-06" , value_of_a_euro_rm := cumprod(1+RM_1M)]

WML_Data_l = melt(WML_Data, id.vars = c("DATE"), measure.vars = c("value_of_a_euro_winners", "value_of_a_euro_losers", "value_of_a_euro_wml", "value_of_a_euro_rf"),  na.rm = T)
setnames(WML_Data_l, c("DATE", "PORTFOLIO", "VALUE"))


# Calculate the cumulated return of month t-12 to t-2  ###### can be removed!!! 

#FSE_monthly_p_l[, Return_2L := as.numeric(shift(Return, n = 2, type = "lead")), by = variable]
FSE_monthly_p_l[, Return_cs := frollmean(Return, 11)] 


## Calculate the monthly returns based on the daily data
FSE_daily_p_l = melt(FSE_daily_p, id.vars = c("DATE"),,  variable.name = "Stock", value.name = c("Price", "MarketValue"))
FSE_daily_p_l[, value := as.numeric(value)]
FSE_daily_p_l[, Return := as.numeric(log(1+(Price - shift(Price, n = 1)) / shift(Price, n = 1))), by = Stock]
FSE_daily_p_l[, Date := ymd(Date)]


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

# Seems to work
## Now calculate the return of the winners minus the return of the losers

# Formation one-month after evaluating the deciles of the returns
FSE_monthly_p_l[, PF_Formation := shift(Decile, n=1)]

WML_Data = FSE_monthly_p_l[PF_Formation == 10, .(WINNER = mean(Return_cs, na.rm = T) ), by = Date]
setorder(WML_Data, Date)
LOSERS = FSE_monthly_p_l[PF_Formation == 1, .(LOSER = mean(Return_cs, na.rm = T)), by = Date]
WML_Data = merge(WML_Data, LOSERS, by = "Date")
WML_Data[, WML := WINNER - LOSER]

### Create WML DATA from the current data-set based on daily data


## Add the RF_Interest to the data.table for comparison

setnames(WML_Data, c("DATE", "WINNER", "LOSER", "WML"))
WML_Data[, DATE := ceiling_date(DATE, "month")]
RF_Interest[, DATE := ceiling_date(DATE, "month")]

WML_Data = merge(WML_Data, RF_Interest, by = c("DATE"), all.x = T)
WML_Data[, RF_1M.y := NULL]
setorder(WML_Data, Date)

### Convert these returns into prices as if invested into portfolio for comparison of return development

WML_Data[!is.na(WINNER), value_of_a_euro_winners := cumprod(1+WINNER)]
WML_Data[!is.na(LOSER), value_of_a_euro_losers := cumprod(1+LOSER)]
WML_Data[!is.na(WML), value_of_a_euro_wml := cumprod(1+WML)]
WML_Data[ !is.na(RF_1M), value_of_a_euro_rf := cumprod(1+RF_1M)]



WML_Data_l = melt(WML_Data, id.vars = c("DATE"), measure.vars = c("value_of_a_euro_winners", "value_of_a_euro_losers", "value_of_a_euro_wml", "value_of_a_euro_rf"),  na.rm = T)
setnames(WML_Data_l, c("DATE", "PORTFOLIO", "VALUE"))


## Plotting of results
ggplot(WML_Data_l, aes(x = DATE, y = VALUE, group = PORTFOLIO, color = PORTFOLIO))+
  geom_line() +
  xlab("Date") + ylab("Portfolio Value") +
  theme(legend.position = c(0.09,0.88), legend.title = element_blank()) +
  scale_colour_discrete(labels = c("Winners", "Losers", "WML", "Risk-Free")) +
  scale_y_continuous(trans="log10") +
  scale_x_date(labels = function(date) {year(date)}, date_breaks = "5 years")
  

## Investigation of results
## Calculation of relevant metrics 
# eg sharpe ratio of the porftolio compared to just winners portfolio or risk free
## Adjust risk free rate to daily rate

RF_Interest[, DATE := ym(DATE)]


daily_dates <- data.table(DATE = seq(min(as.Date(FSE_daily_p_l$DATE)), 
                                     max(as.Date(FSE_daily_p_l$DATE)) + months(1) - 1, 
                                     by = "day"))

daily_dates[, DATE := as.Date(DATE)]
RF_Interest[, DATE := as.Date(DATE)]
RF_Interest[, DATE := ymd(DATE)]
RF_Interest[, YearMonth := format(DATE, "%Y-%m")]
daily_dates[, YearMonth := format(DATE, "%Y-%m")]

daily_rf = merge(daily_dates, RF_Interest[, .(YearMonth, RF_1M_D)], by = "YearMonth", all.x = TRUE, allow.cartesian = TRUE)
daily_rf[, YearMonth := NULL]
daily_rf[, Stock := "Risk-free"]
setnames(daily_rf, "RF_1M_D", "Return")
daily_rf[, DATE := as.Date(DATE)]

FSE_combined = rbind(FSE_daily_p_l, daily_rf[, .(DATE, Stock, Return)], use.names = TRUE, fill = TRUE)
setorder(FSE_combined, Stock, DATE)
FSE_combined = FSE_combined[DATE < "2024-12-31"]



## Sharpe Ratio based on MOM Data
#Create the values for the excess return over the risk-free rate
MOM_Data[, EX_RET_WML :=  WML-RF_1M]
MOM_Data[, EX_RET_W := WINNER-RF_1M]
MOM_Data[, EX_RET_L := LOSER-RF_1M]
MOM_Data[, EX_RET_MKT := RM_1M-RF_1M]

MOM_Data[, sd(EX_RET_WML, na.rm=T)]

## Calculate the Sharpe Ratios for the Portfolios (from July 1990 as this is the start of the FIBOR time series)
Sharpe_Ratio_WML_ALL = MOM_Data[DATE > "1990-06"& !is.na(RF_1M), (mean(EX_RET_WML, na.rm = T)/sd(EX_RET_WML, na.rm = T))*sqrt(12)]
Sharpe_Ratio_W_ALL = MOM_Data[DATE > "1990-06"& !is.na(RF_1M), (mean(EX_RET_W, na.rm = T)/sd(EX_RET_W, na.rm = T))*sqrt(12)]
Sharpe_Ratio_L_ALL = MOM_Data[DATE > "1990-06" & !is.na(RF_1M), (mean(EX_RET_L, na.rm = T)/sd(EX_RET_L, na.rm = T))*sqrt(12)]
Sharpe_Ratio_NINE_ALL = MOM_Data[DATE > "1990-06" & !is.na(RF_1M), (mean(NINE-RF_1M, na.rm = T)/sd(NINE-RF_1M, na.rm = T))*sqrt(12)]
Sharpe_Ratio_EIGHT_ALL= MOM_Data[DATE > "1990-06" & !is.na(RF_1M), (mean(EIGHT-RF_1M, na.rm = T)/sd(EIGHT-RF_1M, na.rm = T))*sqrt(12)]
Sharpe_Ratio_SEVEN_ALL= MOM_Data[DATE > "1990-06" & !is.na(RF_1M), (mean(SEVEN-RF_1M, na.rm = T)/sd(SEVEN-RF_1M, na.rm = T))*sqrt(12)]
Sharpe_Ratio_SIX_ALL= MOM_Data[DATE > "1990-06" & !is.na(RF_1M), (mean(SIX-RF_1M, na.rm = T)/sd(SIX-RF_1M, na.rm = T))*sqrt(12)]
Sharpe_Ratio_FIVE_ALL= MOM_Data[DATE > "1990-06" & !is.na(RF_1M), (mean(FIVE-RF_1M, na.rm = T)/sd(FIVE-RF_1M, na.rm = T))*sqrt(12)]
Sharpe_Ratio_FOUR_ALL= MOM_Data[DATE > "1990-06" & !is.na(RF_1M), (mean(FOUR-RF_1M, na.rm = T)/sd(FOUR-RF_1M, na.rm = T))*sqrt(12)]
Sharpe_Ratio_THREE_ALL= MOM_Data[DATE > "1990-06" & !is.na(RF_1M), (mean(THREE-RF_1M, na.rm = T)/sd(THREE-RF_1M, na.rm = T))*sqrt(12)]
Sharpe_Ratio_TWO_ALL= MOM_Data[DATE > "1990-06" & !is.na(RF_1M), (mean(TWO-RF_1M, na.rm = T)/sd(TWO-RF_1M, na.rm = T))*sqrt(12)]
Sharpe_Ratio_RM_ALL = MOM_Data[DATE > "1990-06" & !is.na(RF_1M), (mean(RM_1M-RF_1M, na.rm = T)/sd(RM_1M-RF_1M, na.rm = T))*sqrt(12)]


MOM_Data[DATE > "1990-07" & !is.na(RF_1M), sd(RM_1M-RF_1M, na.rm = T)*sqrt(12)]
MOM_Data[DATE > "1990-06"& !is.na(RF_1M), mean(RM_1M-RF_1M, na.rm = T)]
MOM_Data[DATE > "1990-06"& !is.na(RF_1M), sd(RM_1M-RF_1M, na.rm = T)]
MOM_Data[DATE > "1990-06"& !is.na(RF_1M), mean(EX_RET_WML, na.rm = T)]
MOM_Data[DATE > "1990-06"& !is.na(RF_1M), sd(EX_RET_WML, na.rm = T)]

### Calculation of Portfolio Beta's
#Load CDAX
# Calc Return of Market
# Regress excess return of portfolio against excess return of the market

WML_reg = lm(EX_RET_WML~EX_RET_MKT, data = MOM_Data)
W_reg = lm(EX_RET_W~EX_RET_MKT, data = MOM_Data)
L_reg = lm(EX_RET_L~EX_RET_MKT, data = MOM_Data)
NINE_reg = lm((NINE-RF_1M)~EX_RET_MKT, data = MOM_Data)
EIGHT_reg = lm((EIGHT-RF_1M)~EX_RET_MKT, data = MOM_Data)
SEVEN_reg = lm((SEVEN-RF_1M)~EX_RET_MKT, data = MOM_Data)
SIX_reg = lm((SIX-RF_1M)~EX_RET_MKT, data = MOM_Data)
FIVE_reg = lm((FIVE-RF_1M)~EX_RET_MKT, data = MOM_Data)
FOUR_reg = lm((FOUR-RF_1M)~EX_RET_MKT, data = MOM_Data)
THREE_reg = lm((THREE-RF_1M)~EX_RET_MKT, data = MOM_Data)
TWO_reg = lm((TWO-RF_1M)~EX_RET_MKT, data = MOM_Data)

WML_alpha = WML_reg$coefficients["(Intercept)"]
WML_beta = WML_reg$coefficients["EX_RET_MKT"]
W_alpha= W_reg$coefficients["(Intercept)"]
W_beta= W_reg$coefficients["EX_RET_MKT"]
L_alpha= L_reg$coefficients["(Intercept)"]
L_beta= L_reg$coefficients["EX_RET_MKT"]

summary(WML_reg)
tab_model(WML_reg)
tab_model(W_reg)
tab_model(L_reg)

stargazer(WML_reg)

skewness_wml_m = skewness(MOM_Data$WML)
skewness_w_m = skewness(MOM_Data$WINNER)
skewness_l_m = skewness(MOM_Data$LOSER)

## Generate the table in the style of Daniel & Moskowitz
## Generate empty data.table
summarycols = c("LOSERS", "TWO", "THREE", "FOUR", "FIVE", "SIX", "SEVEN", "EIGHT", "NINE", "WINNERS", "WML", "RF_1M")

mean_excess_return_expr = expression(paste(bar(r), " - ", bar(r[f])))
mean_excess_return_expr
summaryrows = c("")
eval(mean_excess_return_expr)


## Create daily data.table with the decile column for more in-depth investigation
FSE_daily_p_l[, YearMonth := format(DATE, "%Y-%m")]
FSE_ceiling[, YearMonth := format(ym(DATE), "%Y-%m")]

# Perform a left join of daily_data with monthly_data to get the Decile column
MOM_daily = merge(FSE_daily_p_l, FSE_ceiling[, .(Stock, YearMonth, Decile)], by = c("Stock", "YearMonth"), all.x = TRUE)

# Remove the YearMonth column if not needed
MOM_daily[, YearMonth := NULL]

MOM_daily_wml = MOM_daily[ DATE >= "1990-07-01",   ]

MOM_daily[, REL_MV := shift(MV, n = 1, type = "shift")]

MOM_daily[, WEIGHT := REL_MV/sum(REL_MV), by = .(DATE, Decile)]
MOM_daily[, WRET := Return*WEIGHT]
MOM_daily[ DATE == "2020-10-1" & Decile == 10, sum(WEIGHT)]

Merger = MOM_daily[ DATE >= "1990-07-01", sum(WRET), by = .(DATE, Decile)]
setorder(Merger, DATE, Decile)
setnames(Merger, c("DATE", "DECILE", "RETURN"))
Merger = Merger[ !is.na(DECILE)]
setnames(daily_rf, c("DATE", "RETURN", "DECILE"))
setcolorder(daily_rf, names(Merger))
daily_rf = daily_rf[!is.na(RETURN)]
Merger = rbind(Merger, daily_rf[DATE >= "1990-07-01" & DATE %in% Merger[, unique(DATE)]], fill = T)
setorder(Merger, DATE, DECILE)

CDAX_IND[, RETURN := as.numeric(log(1+((M_IND - shift(M_IND, n = 1)) / shift(M_IND, n = 1))))]
CDAX_IND[, DECILE := "MARKET"]
CDAX_Merge = CDAX_IND[, .(DATE, DECILE, RETURN)]
setcolorder(CDAX_Merge, names(Merger))
Merger = rbind(Merger, CDAX_Merge[DATE >= "1990-07-01" & DATE %in% Merger[, unique(DATE)]], fill = T )
setorder(Merger, DATE, DECILE)
Mergerwml = Merger[,.( DATE = unique(DATE),RETURN = (as.numeric(Merger[DECILE == 10, RETURN]) - as.numeric(Merger[DECILE == 1, RETURN]))) ]
Mergerwml[, DECILE := "WML"] ; setcolorder(Mergerwml, names(Merger))
Merger= rbind(Merger, Mergerwml, fill = T)
setorder(Merger, DATE, DECILE)
Merger[, value_of_a_euro := cumprod(as.numeric(RETURN)+1), by = .(DECILE)]

## calculate the daily skewness
skewness_wml_d = skewness(Merger[DECILE == "WML", RETURN])
skewness_w_d = skewness(Merger[DECILE == 10, RETURN])
skewness_l_d = skewness(Merger[DECILE == 1, RETURN])

sd(Merger[DECILE == 10, RETURN])


### Generate the plot comparing the cumulative monthly return of past winner, loser, rf, and market portfolio

MOM_Data_l = melt(MOM_Data, id.vars = c("DATE", measure.vars = x()) )

MOM_Data_l = melt(MOM_Data[DATE > "1990-07"], id.vars = c("DATE"), measure.vars = c("value_of_a_euro_winners", "value_of_a_euro_losers", "value_of_a_euro_rf", "value_of_a_euro_rm"),  na.rm = T)
setnames(MOM_Data_l, c("DATE", "PORTFOLIO", "VALUE"))
MOM_Data_l[, DATE := ym(DATE)]



ggplot(MOM_Data_l, aes(x = DATE, y = VALUE, group = PORTFOLIO, color = PORTFOLIO))+
  geom_line() +
  xlab("Date") + ylab("Portfolio Value") +
  theme(legend.position = c(0.09,0.88), legend.title = element_blank()) +
  scale_colour_discrete(labels = c("Winners", "Losers", "Risk-Free", "Market")) +
  scale_y_continuous(trans="log10") +
  scale_x_date(labels = function(date) {year(date)}, date_breaks = "5 years") + theme_classic()


### Investigate the corona crisis
## Let the cum returns start at the start of 2020
MOM_COV = MOM_Data[DATE >= "2019-01"]
MOM_COV[, value_of_a_euro_winners := cumprod(1+WINNER)]
MOM_COV[, value_of_a_euro_losers := cumprod(1+LOSER )]
MOM_COV[, value_of_a_euro_rf := cumprod(1+RF_1M )]
MOM_COV[, value_of_a_euro_rm := cumprod(1+RM_1M )]
MOM_COV_l = melt(MOM_COV, id.vars = c("DATE"), measure.vars = c("value_of_a_euro_winners", "value_of_a_euro_losers", "value_of_a_euro_rf", "value_of_a_euro_rm"),  na.rm = T)
setnames(MOM_COV_l, c("DATE", "PORTFOLIO", "VALUE"))
MOM_COV_l[, DATE := ym(DATE)]

ggplot(MOM_COV_l, aes(x = DATE, y = VALUE, group = PORTFOLIO, color = PORTFOLIO)) +
  geom_line() +
  xlab("Date") + ylab("Portfolio Value") +
  theme(legend.position = c(0.09,0.88), legend.title = element_blank()) +
  scale_colour_discrete(labels = c("Winners", "Losers", "Risk-Free", "Market")) +
  scale_y_continuous(trans="log10") +
  scale_x_date(date_labels = "%m-%y", date_breaks = "6 months") +
  theme_classic()

### Investigate with daily data
ggplot(Merger[DECILE == 10 | DECILE == 1 | DECILE == "Risk-free"| DECILE == "MARKET", .(DATE,DECILE, value_of_a_euro)], aes(x = DATE, y = value_of_a_euro, group = DECILE, color = DECILE))+
  geom_line() +
  xlab("Date") + ylab("Portfolio Value") +
  theme(legend.position = c(0.09,0.88), legend.title = element_blank()) +
  scale_colour_discrete(labels = c("Losers","Winners", "Market", "Risk-Free")) +
  scale_y_continuous(trans="log10") +
  scale_x_date(labels = function(date) {year(date)}, date_breaks = "5 years") + theme_classic()

### investigate Euro area crisis drop with a graph
MergerEuro = Merger[DECILE != value_of_a_euro]
MergerEuro = MergerEuro[DATE >= "2007-01-01"& DATE <= "2012-12-31"]
MergerEuro[DATE >= "2007-01-01" & DATE <= "2012-12-31", value_of_a_euro := cumprod(as.numeric(RETURN)+1), by = .(DECILE)]

ggplot(MergerEuro[DECILE == 10 | DECILE == 1 | DECILE == "Risk-free"| DECILE == "MARKET", .(DATE,DECILE, value_of_a_euro)], aes(x = DATE, y = value_of_a_euro, group = DECILE, color = DECILE))+
  geom_line() +
  xlab("Date") + ylab("Portfolio Value") +
  theme(legend.position = c(0.09,0.88), legend.title = element_blank()) +
  scale_colour_discrete(labels = c("Losers","Winners", "Market", "Risk-Free")) +
  scale_y_continuous(trans="log10") +
  scale_x_date(labels = function(date) {year(date)}, date_breaks = "1 years") + theme_classic()

## investigate 2001 drop with a graph (optional)

Mergerdotcom = Merger[DATE >= "1995-01-01" & DATE <= "2004-12-31"]
Mergerdotcom = Mergerdotcom[DECILE != value_of_a_euro]
Mergerdotcom[ , value_of_a_euro := cumprod(as.numeric(RETURN)+1), by = .(DECILE)]

ggplot(Mergerdotcom[DECILE == 10 | DECILE == 1 | DECILE == "Risk-free"| DECILE == "MARKET", .(DATE,DECILE, value_of_a_euro)], aes(x = DATE, y = value_of_a_euro, group = DECILE, color = DECILE))+
  geom_line() +
  xlab("Date") + ylab("Portfolio Value") +
  theme(legend.position = c(0.09,0.88), legend.title = element_blank()) +
  scale_colour_discrete(labels = c("Losers","Winners", "Market", "Risk-Free")) +
  scale_y_continuous(trans="log10") +
  scale_x_date(labels = function(date) {year(date)}, date_breaks = "1 years") + theme_classic()

### Investigate the Covid-19 Period and subsequent Ukraine - Russia conflict
Mergercovid =  Merger[DATE >= "2019-01-01" & DATE <= "2024-11-30"]
Mergercovid = Mergercovid[DECILE != value_of_a_euro]
Mergercovid[ , value_of_a_euro := cumprod(as.numeric(RETURN)+1), by = .(DECILE)]

#Generate plot of daily log returns during covid period and Ukraine russia conflict
ggplot(Mergercovid[DECILE == 10 | DECILE == 1 | DECILE == "Risk-free"| DECILE == "MARKET", .(DATE,DECILE, value_of_a_euro)], aes(x = DATE, y = value_of_a_euro, group = DECILE, color = DECILE))+
  geom_line() +
  xlab("Date") + ylab("Portfolio Value") +
  theme(legend.position = c(0.09,0.88), legend.title = element_blank()) +
  scale_colour_discrete(labels = c("Losers","Winners", "Market", "Risk-Free")) +
  scale_y_continuous(trans="log10") +
  scale_x_date(labels = function(date) {year(date)}, date_breaks = "1 years") + theme_classic()

### Generate table with worst 15 momentum returns and 2 year return
## Generate the 2 Year market return
setorder(MOM_Data, DATE)
MOM_Data[, RM_2Y := as.numeric(log(1+((M_IND - shift(M_IND, n = 24)) / shift(M_IND, n = 24))))]
setorder(MOM_Data, WML)
Worst_15WML = head(MOM_Data[, .(DATE, WML, RM_2Y, RM_1M)], n = 15)

# Create the table
Worst_15WML[, Rank := .I]
header_worst_wml = c("Date", "WML", "Mkt-2y", "Mkt")
setnames(Worst_15WML, header_worst_wml)
header_worst_wml = c("Date", "WML", "Mkt-2y", "Mkt")
setcolorder(Worst_15WML,header_worst_wml)

Worst_15WML[, (names(Worst_15WML)[2:4]) := lapply(.SD, function(x) round(as.numeric(x) * 100, 1)), .SDcols = 2:4]

stargazer(Worst_15WML, type = "latex", title = "Worst Monthly Momentum Returns", notes = "Note: All numbers are in percent. Considered period ranging from June 1990 until November 2024.", notes.align = "l", out = paste0(path_to_output,"worst_monthly.tex"), summary = FALSE, digits = 2, single.row = T, column.sep.width = "-15pt")


############################################ 126-day rolling regression for estimation of beta's ########################

Mergerbetas = Merger[, .(DATE,DECILE, RETURN)]
Mergerbetas_10 = Mergerbetas[DECILE == 10]
Mergerbetas_1 = Mergerbetas[DECILE == 1]
Mergerbetas_riskfree = Mergerbetas[DECILE == "Risk-free"]
Mergerbetas_market = Mergerbetas[DECILE == "MARKET"]
merged_data = merge(Mergerbetas_10[, .(DATE, WINNER = RETURN)], Mergerbetas_riskfree[, .(DATE, RF = RETURN)], by = "DATE")
merged_data = merge(merged_data, Mergerbetas_1[, .(DATE, LOSER = RETURN)], by = "DATE")
merged_data = merge(merged_data, Mergerbetas_market[,.(DATE, MKT = RETURN)], by = "DATE")
merged_data[, excess_10 := WINNER - RF]
merged_data[, excess_1 := LOSER - RF]
merged_data[, excess_mkt := MKT - RF]


## Create lagged values of excess market return for the rolling regression
for (i in 1:10) {
  merged_data[, paste0("lag_", i) := shift(excess_mkt, n = i, type = "lag")]
}

## Calculate the 126 days rolling regression for Winner portfolio

rolling_regression10 = rollapplyr(
  data = merged_data[, .(excess_10, excess_mkt, lag_1, lag_2, lag_3, lag_4, lag_5, lag_6, lag_7, lag_8, lag_9, lag_10)],
  width = 126,
  FUN = function(sub_data) {
    model = lm(excess_10 ~ ., data = as.data.frame(sub_data))
    return(coef(model))
  },
  by.column = FALSE,
  align = "right", fill = NA
)

coefficient_names = c("Alpha_10", "Beta_10_0", paste0("Beta_10_", 1:10))
regression_results10 = as.data.table(rolling_regression10)
setnames(regression_results10, coefficient_names)
merged_data = cbind(merged_data, regression_results10)

## Calculate the 126 days rolling regression for Loser portfolio

rolling_regression1 = rollapplyr(
  data = merged_data[, .(excess_1, excess_mkt, lag_1, lag_2, lag_3, lag_4, lag_5, lag_6, lag_7, lag_8, lag_9, lag_10)],
  width = 126,
  FUN = function(sub_data) {
    model = lm(excess_1 ~ ., data = as.data.frame(sub_data))
    return(coef(model))
  },
  by.column = FALSE,
  align = "right", fill = NA
)

coefficient_names = c("Alpha_1", "Beta_1_0", paste0("Beta_1_", 1:10))
regression_results1 = as.data.table(rolling_regression1)
setnames(regression_results1, coefficient_names)
merged_data = cbind(merged_data, regression_results1)

### Calculate the betas per date per portfolio
merged_data[, Beta_Winner := rowSums(.SD), by = DATE, .SDcols = grep("^Beta_10_", names(merged_data), value = TRUE)]
merged_data[, Beta_Loser := rowSums(.SD), by = DATE, .SDcols = grep("^Beta_1_", names(merged_data), value = TRUE)]

## Melt the data.table to plot the graph

Beta_plot = melt(merged_data, id.vars = c("DATE"), measure.vars = c("Beta_Winner", "Beta_Loser"), variable.name = c("Portfolio"), value.name =  ("Beta"), na.rm = T)
Beta_plot = as.data.table(Beta_plot)
setorder(Beta_plot,DATE, Portfolio)


## Overall from 1990 until now
ggplot(Beta_plot, aes(x = DATE, y = Beta, group = Portfolio, color = Portfolio))+
  geom_line() +
  xlab("Date") + ylab("Beta") +
  theme(legend.position = c(0.09,0.88), legend.title = element_blank()) +
  scale_colour_discrete(labels = c("Winners","Losers")) +
  scale_x_date(labels = function(date) {year(date)}, date_breaks = "3 years") +
  scale_y_continuous(breaks = c(seq(-2,6,1)))+
  theme_bw()

## Beta development covid crisis and russia ukraine conflict
ggplot(Beta_plot[DATE >= "2019-01-01" & DATE <= "2024-11-30"], aes(x = DATE, y = Beta, group = Portfolio, color = Portfolio))+
  geom_line() +
  xlab("Date") + ylab("Beta") +
  theme(legend.position = c(0.09,0.88), legend.title = element_blank()) +
  scale_colour_discrete(labels = c("Winners","Losers")) +
  scale_x_date(labels = function(date) {year(date)}, date_breaks = "1 years") +
  scale_y_continuous(breaks = c(seq(-2,6,1)))+
  theme_bw()

## Beta development Euro-area crisis
ggplot(Beta_plot[DATE >= "2007-01-01"& DATE <= "2012-12-31"], aes(x = DATE, y = Beta, group = Portfolio, color = Portfolio))+
  geom_line() +
  xlab("Date") + ylab("Beta") +
  theme(legend.position = c(0.09,0.88), legend.title = element_blank()) +
  scale_colour_discrete(labels = c("Winners","Losers")) +
  scale_x_date(labels = function(date) {year(date)}, date_breaks = "1 years") +
  scale_y_continuous(breaks = c(seq(-2,6,1)))+
  theme_bw()

## Beta development dot.com bubble
ggplot(Beta_plot[DATE >= "1995-01-01" & DATE <= "2004-12-31"], aes(x = DATE, y = Beta, group = Portfolio, color = Portfolio))+
  geom_line() +
  xlab("Date") + ylab("Beta") +
  theme(legend.position = c(0.09,0.88), legend.title = element_blank()) +
  scale_colour_discrete(labels = c("Winners","Losers")) +
  scale_x_date(labels = function(date) {year(date)}, date_breaks = "1 years") +
  scale_y_continuous(breaks = c(seq(-2,6,1)))+
  theme_bw()

################# Option-like behavior of the WML portfolio #########################
## Create the Bear market indicator
setorder(MOM_Data, DATE)
MOM_Data[, IB := ifelse(RM_2Y < 0, 1, 0)]

## Create the Up-Market indicator
MOM_Data[, IU := ifelse(EX_RET_MKT > 0, 1, 0)]

## Create the four regressions with the indicators
WML_uncond_mm = lm(data = MOM_Data[DATE >= "1990-07"], WML ~ 1+ EX_RET_MKT )
WML_cond_capm = lm(data = MOM_Data[DATE >= "1990-07"], WML ~ 1+ IB*EX_RET_MKT)
WML_option_beh = lm(data = MOM_Data[DATE >= "1990-07"], WML ~ 1+ IB * EX_RET_MKT+IB:IU:EX_RET_MKT)
WML_option_beh_s = lm(data = MOM_Data[DATE >= "1990-07"], WML ~ 1+ EX_RET_MKT + IB:EX_RET_MKT+IB:IU:EX_RET_MKT)

stargazer(WML_uncond_mm, WML_cond_capm, WML_option_beh, WML_option_beh_s, type="latex", intercept.top = T,intercept.bottom = F,
           out=paste0(path_to_output,"Option_like_behavior.tex"))


summary(WML_option_beh)
summary(WML_option_beh_s)
tab_model(WML_uncond_mm, WML_cond_capm, WML_option_beh, WML_option_beh_s, show.ci = F, show.obs = F,title = "Market Timing Regression Results", p.style = "stars" , dv.labels = "WML", show.r2 = T )

###### Dynamic Weighting of Momentum Portfolio ########
Mergerwml = Mergerwml[!is.na(RETURN)]

### fit the GJR-Garch model on the daily returns of the WML 
gjrgarchspec = ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder= c(1,1)), mean.model = list(armaOder = c(1,1)), distribution.model = "std")
gjrfit  = ugarchfit(data = Mergerwml$RETURN, spec = gjrgarchspec)

summary(gjrfit)
gjrfit
coef(gjrfit)
length(gjrfit@fit$sigma)

## Estimate the 22 day volatility to be put into the optimal sharpe ratio weighting
rolling_gjr = ugarchroll(data = Mergerwml$RETURN, spec = gjrgarchspec, refit.every = 22, forecast.length = MOM_Data[DATE >= "1990-07", .N], refit.window = "moving" )
GJRFORE = rolling_gjr@forecast$density$Sigma
GJRFORE = as.data.table(GJRFORE)
GJRFORE = GJRFORE[-1]
### Create empty data.table to rbind() GJR-Garch estimates onto it to cbind() forecasts to monthly Momentum data in MOM_Data
#empty_rows = data.table(matrix(NA, nrow = MOM_Data[DATE < "1990-07", .N], ncol = ncol(GJRFORE)))
#setnames(empty_rows, names(GJRFORE))
#GJRFORE = rbind(empty_rows,GJRFORE)

### Attach GJR Garch estimates as another column
MOM_Data = cbind(MOM_Data,GJRFORE)

###Estimate the 126 day rolling estimate of the variance of the market returns
merged_data[, MKT_R_VAR := rollapply(excess_mkt,width = 126,FUN = var ,align = "right", fill = NA)]

### Estimate Regression for Expected WML Return forecast

# retrieve the last value before relevant month for rolling var of market returns
merged_data[, Month_Year := ym(format(as.Date(DATE), "%Y-%m"))]
last_dates = merged_data[, .(Last_Date = max(DATE)), by = Month_Year]


# Step 3: Join the last dates with the originDATE# Step 3: Join the last dates with the original data to get the corresponding variance
variance_at_last_dates = merge(merged_data[, .(DATE,MKT_R_VAR)], last_dates, by.x = "DATE", by.y = "Last_Date")
variance_at_last_dates[, DATE := format(as.Date(DATE), "%Y-%m")]
variance_at_last_dates[, MKT_R_VAR:= shift(MKT_R_VAR, n = 1)]
MOM_Data = merge(MOM_Data, variance_at_last_dates, by = "DATE")
MOM_Data[, IB := shift(IB, n = 1)]
#### Forecasting future WML Returns
Exp_WML_Ret_reg = lm(data = MOM_Data, WML ~ IB*MKT_R_VAR )
summary(Exp_WML_Ret_reg)

MOM_Data[, Month_Year := NULL ]
MOM_Data[, MKT_R_VAR := NULL ]

### Determine the forecasts from the linear time-series regression for the weighting parameter
for_coef = coef(Exp_WML_Ret_reg)
MOM_Data[, WML_mu := for_coef[1]+for_coef[2]*IB+for_coef[3]*MKT_R_VAR+for_coef[2]*IB*for_coef[3]*MKT_R_VAR]

### For the weighting of the optimal sharpe ratio portfolio,lambda is chosen so that the in-sample annualized volatility is = 19%
#See derivation in Appendix C of Daniel & Moskowitz (2016)
## Provisionally define Lambda
MOM_Data[, Lambda := 1.52]

## Calc the weighting of WML/(WML+RF)
MOM_Data[, WML_Weight := (1/2*Lambda)*WML_mu/GJRFORE]

### Testing whether the weighting exceeds one or is below 0
MOM_Data[ WML_Weight<0 | WML_Weight > 1]

## Cap the Weighting to 100%
MOM_Data[ WML_Weight >=  1, WML_Weight:= 1]

####  Recalculate the return of the Portfolio with dynamic weighting
MOM_Data[,DYNWML := WML_Weight*RF_1M + (1-WML_Weight)*RF_1M]

MOM_Data[, ((1+var(DYNWML, na.rm = T))**12)-1]

comp_res = MOM_Data[, .(DATE, WML, DYNWML)]
comp_res = comp_res[!is.na(DYNWML)]

comp_res[, value_of_a_euro_wml := cumprod(1+WML) ]
comp_res[, value_of_a_euro_dynwml := cumprod(1+DYNWML)]

comp_res_l = melt(comp_res,id.vars = c("DATE"), measure.vars = c("value_of_a_euro_wml", "value_of_a_euro_dynwml"),  na.rm = T)
comp_res_l[, DATE := ym(DATE)]
setnames(comp_res_l, c("DATE", "PORTFOLIO", "VALUE"))
setorder(comp_res_l, DATE, PORTFOLIO)


ggplot(comp_res_l, aes(x = DATE, y = VALUE, group = PORTFOLIO, color = PORTFOLIO))+
  geom_line() +
  xlab("Date") + ylab("Portfolio Value") +
  theme(legend.position = c(0.09,0.88), legend.title = element_blank()) +
  scale_colour_discrete(labels = c("WML", "DYN WML")) +
  scale_y_continuous(trans="log10") +
  scale_x_date(labels = function(date) {year(date)}, date_breaks = "5 years")

summary(gjrfit )







