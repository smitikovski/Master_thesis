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

### Pathway to data
path_to_data = "/Users/mirkosmit/Documents/CAU/Master_Thesis/Data/"

### Read in the Data ´

raw_Dax_Data = "DAX_All.xlsx"
raw_FSE_monthly = "FSE_Monthly.xlsx"
raw_FSE_daily = "FSE_ALL.xlsx"

EURIBOR_name = "EURIBOR.xlsx"
FIBOR_name = "FIBOR.xlsx"

### Read in relevant data (monthly and daily)
FSE_Stocks = read_xlsx(paste0(path_to_data, raw_FSE_monthly))
FSE_Daily = read_xlsx(paste0(path_to_data, raw_FSE_daily))


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
RF_Interest[, RF_1M_D := (1 + RF_1M_M / 100) ^ (1/30) - 1]
#RF_Interest[, DATE := format(DATE, "%Y-%m")]



###### Data Cleansing ############################################
## Monthly FSE Stock Data
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

## Daily FSE Stock Data
FSE_Daily = as.data.table(FSE_Daily)
setnames(FSE_Daily, c(""),c( "DATE"))
FSE_Daily[, DATE := ymd(DATE)]
col_names <- colnames(FSE_Daily)
columns_to_keep_daily = !grepl("#ERROR", colnames(FSE_Daily))
FSE_Daily = FSE_Daily[, ..columns_to_keep_daily]
setnames(FSE_Daily, gsub("\\.\\.\\..*", "", colnames(FSE_Daily)))

# Drop unneccessary data to make processing of data quicker
search_to_drop_daily = c(" - EARNINGS PER SHR"," - TOT RETURN IND", " - TURNOVER BY VALUE", " - TURNOVER BY VOLUME")
pattern_d = paste(search_to_drop_daily, collapse = "|")
price_columns_to_keep_daily = !grepl(pattern_d, colnames(FSE_Daily))
FSE_Daily = FSE_Daily[, ..price_columns_to_keep_daily]


######## Remove any data that is collected on public holidays (or remnants of it)
# Ensure your start_date and end_date are properly set
start_date <- as.Date(min(FSE_Daily$DATE))  # Assuming DATE is already a Date object
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

### Require a minimum of trade volume 

### Excluding all stocks that have less than 9 months of data

stock_columns <- setdiff(names(FSE_Daily), "DATE")
valid_stocks <- sapply(stock_columns, function(col) {
  sum(!is.na(FSE_Daily[[col]]))
})
stocks_to_keep <- names(valid_stocks[valid_stocks >= 270])
FSE_Daily <- FSE_Daily[, c("DATE", stocks_to_keep), with = FALSE]
colnames(FSE_Daily)

### Excluding all stocks that have a starting value < 1 ### to be reviewed
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

# Example usage
last_dates <- get_last_in_month(FSE_daily_p_l$DATE)
last_dates[, Last_DATE := as.Date(Last_DATE)] # Ensure dates are properly formatted

# Merge with your main data table
FSE_ceiling = FSE_daily_p_l[last_dates, on = .(DATE = Last_DATE)]
FSE_ceiling[, YearMonth := NULL]
FSE_ceiling[, DATE := format(DATE, "%Y-%m")]

#FSE_ceiling[, Daily_Return := NULL]
FSE_ceiling[, Return_11M := as.numeric((Price - shift(Price, n = 11)) / shift(Price, n = 11)), by = Stock]
FSE_ceiling[, REL_RET := shift(Return_11M, n = 1, type = "lag"), by = Stock]
FSE_ceiling[, REL_MV := shift(MV, n = 1, type = "lag"), by = Stock]
FSE_ceiling[, Return_1M := as.numeric((Price - shift(Price, n = 1)) / shift(Price, n = 1)), by = Stock]
 

FSE_daily_p_l[DATE > "2024-03-01" & DATE < "2024-05-01", DATE]

# Assign the stocks of each month to a decile according to their 11 month return
FSE_ceiling[, Decile := fifelse(
  !is.na(REL_RET),  
  as.integer(cut(REL_RET,
                breaks = quantile(REL_RET, probs = seq(0, 1, 0.1), na.rm = TRUE),
                 include.lowest = TRUE, labels = FALSE)),
  NA_integer_), by = DATE]

#FSE_ceiling[, Decile := fifelse(
#  !is.na(REL_RET),  
#  as.integer(cut(REL_RET,
#                 breaks = quantile(REL_RET, probs = c(0, 0.3, 0.7, 1), na.rm = TRUE),
#                 include.lowest = TRUE)),
#  NA_integer_), by = DATE]

#FSE_ceiling[, Decile := fifelse(
#  !is.na(REL_RET),  
#  as.integer(cut(REL_RET,
#                 breaks = quantile(REL_RET, probs = seq(0, 1, by = 0.2), na.rm = TRUE),
#                 include.lowest = TRUE, labels = FALSE)),
#  NA_integer_), by = DATE]



setorder(FSE_ceiling, Stock, DATE)

FSE_ceiling[ DATE == "2010-06" & Decile == 10,  mean(Return_1M)]
FSE_ceiling[ DATE == "2010-06" & Decile == 1, mean(Return_1M)]

MOM_Data[DATE == "2010-06", WML]

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


MOM_Data = FSE_ceiling[Decile==5 , sum(WRET, na.rm = T), by = DATE]
MOM_Data1 = FSE_ceiling[Decile==1 , sum(WRET, na.rm = T), by = DATE]
MOM_Data2 = FSE_ceiling[Decile==3 , sum(WRET, na.rm = T), by = DATE]
MOM_Data2 = FSE_ceiling[Decile==2 , sum(WRET, na.rm = T), by = DATE]
MOM_Data2 = FSE_ceiling[Decile==1 , sum(WRET, na.rm = T), by = DATE]




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
setnames(MOM_Data2, c("DATE", "LOSER"))

MOM_Data = merge(MOM_Data, MOM_Data1, by = c("DATE"))
MOM_Data = merge(MOM_Data, MOM_Data2, by = c("DATE"))
MOM_Data = merge(MOM_Data, MOM_Data3, by = c("DATE"))
MOM_Data = merge(MOM_Data, MOM_Data4, by = c("DATE"))
MOM_Data = merge(MOM_Data, MOM_Data5, by = c("DATE"))
MOM_Data = merge(MOM_Data, MOM_Data6, by = c("DATE"))
MOM_Data = merge(MOM_Data, MOM_Data7, by = c("DATE"))
MOM_Data = merge(MOM_Data, MOM_Data8, by = c("DATE"))
MOM_Data = merge(MOM_Data, MOM_Data9, by = c("DATE"))
MOM_Data[, WML := WINNER-LOSER]

## Add the Risk-free data (FIBOR & EURIBOR one-month interest rates)
MOM_Data = merge(MOM_Data, RF_Interest, by = c("DATE"), all.x = T )

MOM_Data[, RF_1M:= NULL]

MOM_Data[!is.na(WINNER) , value_of_a_euro_winners := cumprod(1+WINNER)]
MOM_Data[!is.na(LOSER), value_of_a_euro_losers := cumprod(1+LOSER)]
MOM_Data[!is.na(WML)   , value_of_a_euro_wml := cumprod(1+WML)]
MOM_Data[ !is.na(RF_1M)   , value_of_a_euro_rf := cumprod(1+RF_1M)]







MOM_Data[, RF_1M:= NULL]

MOM_Data[DATE == "2001-07", WML]
MOM_Data[DATE == "2001-08", WML]
MOM_Data[DATE == "2001-09", WML]
MOM_Data[DATE == "2001-10", WML]
MOM_Data[DATE == "2001-11", WML]
MOM_Data[DATE == "2001-12", WML]
MOM_Data[DATE == "2002-01", WML]


WML_Data_l = melt(WML_Data, id.vars = c("DATE"), measure.vars = c("value_of_a_euro_winners", "value_of_a_euro_losers", "value_of_a_euro_wml", "value_of_a_euro_rf"),  na.rm = T)
setnames(WML_Data_l, c("DATE", "PORTFOLIO", "VALUE"))


















# Calculate the cumulated return of month t-12 to t-2  ###### can be removed!!! 

#FSE_monthly_p_l[, Return_2L := as.numeric(shift(Return, n = 2, type = "lead")), by = variable]
FSE_monthly_p_l[, Return_cs := frollmean(Return, 11)] 


## Calculate the monthly returns based on the daily data
FSE_daily_p_l = melt(FSE_daily_p, id.vars = c("DATE"),,  variable.name = "Stock", value.name = c("Price", "MarketValue"))
FSE_daily_p_l[, value := as.numeric(value)]
FSE_daily_p_l[, Return := as.numeric((value - shift(value, n = 1)) / shift(value, n = 1)), by = variable]
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

WML_Data[!is.na(WINNER) , value_of_a_euro_winners := cumprod(1+WINNER/1)]
WML_Data[!is.na(LOSER), value_of_a_euro_losers := cumprod(1+LOSER)]
WML_Data[!is.na(WML)   , value_of_a_euro_wml := cumprod(1+WML)]
WML_Data[ !is.na(RF_1M)   , value_of_a_euro_rf := cumprod(1+RF_1M)]



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






















## Sharpe Ratio
risk_free_daily <- MOM_Data / 252  # Annual risk-free rate converted to daily

# Step 1: Calculate excess returns
FSE_daily_p_l[, Excess_Return := Return - risk_free_daily]

# Step 2: Compute mean and standard deviation of excess returns
mean_excess_return <- FSE_daily_p_l[, mean(Excess_Return, na.rm = TRUE)]
sd_excess_return <- FSE_daily_p_l[, sd(Excess_Return, na.rm = TRUE)]

# Step 3: Annualize returns and volatility
annualized_mean_excess_return <- mean_excess_return * 252
annualized_sd_excess_return <- sd_excess_return * sqrt(252)

# Step 4: Calculate Sharpe Ratio
sharpe_ratio <- annualized_mean_excess_return / annualized_sd_excess_return









# Create wide table again visual comparability








### Read in the data used in Daniel & Moskowitz




