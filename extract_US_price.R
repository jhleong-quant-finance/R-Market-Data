##############################################################################################
# Date: 22 AUG 2012
#
# Read the downloaded companylist from http://www.nasdaq.com/screening/company-list.aspx
#
# Create a company listed in USA
#
#############################################################################################
Sys.setenv(TZ='EST')
source('/home/jhleong/dev/R/lib/libtrade.R');
library(TTR)

US_listed_company_list <- stockSymbols()
#lc <- US_listed_company_list

##########################################################################################



# sym <-data.frame()
# for(i in 1:26){
#   sym_tmp <- lc[substring(lc$Symbol, 1,1) == toupper(letters[i]),]
#   sym <- rbind(sym, sym_tmp)
# }

sym <- US_listed_company_list;

#remove symbol with less then 500 Million market cap.
sym <- sym[sym$MarketCap >= 500000000,'Symbol']

# add in the stock in SP600 small cap which not in the list (<500mil cap)
sp600_sym <- sp600small.components();
sym <- unique(append(sym, sp600_sym));

# add in the stock in Russell 2000
r2k_sym <- russell2000.components();
sym <- unique(append(sym, r2k_sym));

# add in the stock in top 100 etf
etf100_sym = top100_etf.components();
sym <- unique(append(sym, etf100_sym));

#find the symbols of the list of historical price file in drive.
downloaded_sym <- list.files(path="/data/finance/US/price")
downloaded_sym <- toupper(gsub("\\.csv","",downloaded_sym))

# remove Symbol already downloaded
sym <- sym[! sym %in% downloaded_sym]
# remove Symbol with character "^", yahoo don't seem to have this data, google use "-"
sym <- sym[ !grepl("\\^", sym)]

cat('Number of stock need to download:', length(sym),'\n');

library(quantmod)
require(doMC);

registerDoMC(4);

sym_data <- foreach(c_symbol=sym) %dopar%
{
  downloaded_ts <- NULL;
  try(downloaded_ts <- getSymbols(c_symbol, from="1900-01-01", src="yahoo", auto.assign=F))
  filename <- paste("/data/finance/US/price/", c_symbol, ".csv", sep = "")
  if(!is.null(downloaded_ts)){
     try(write.zoo(downloaded_ts, file=filename, sep=","))
     try(remove(downloaded_ts))
  }
}

# Pass 2
#find the symbols of the list of historical price file in drive.
downloaded_sym <- list.files(path="/data/finance/US/price")
downloaded_sym <- toupper(gsub("\\.csv","",downloaded_sym))

# remove Symbol already downloaded
sym <- sym[! sym %in% downloaded_sym]

Sys.sleep(5);
cat('Pass 2 - Number of stock need to download:', length(sym),'\n');

sym_data <- foreach(c_symbol=sym) %dopar%
{
  downloaded_ts <- NULL;
  try(downloaded_ts <- getSymbols(c_symbol, from="1900-01-01", src="yahoo", auto.assign=F))
  filename <- paste("/data/finance/US/price/", c_symbol, ".csv", sep = "")
  if(!is.null(downloaded_ts)){
     try(write.zoo(downloaded_ts, file=filename, sep=","))
     try(remove(downloaded_ts))
  }

}


# remove Symbol already downloaded
sym <- sym[! sym %in% downloaded_sym]

Sys.sleep(5);
cat('Pass 3 - Number of stock need to download:', length(sym),'\n');

registerDoMC(1);

sym_data <- foreach(c_symbol=sym) %dopar%
{
  Sys.sleep(1);
  downloaded_ts <- NULL;
  try(downloaded_ts <- getSymbols(c_symbol, from="1900-01-01", src="yahoo", auto.assign=F))
  filename <- paste("/data/finance/US/price/", c_symbol, ".csv", sep = "")
  if(!is.null(downloaded_ts)){
     try(write.zoo(downloaded_ts, file=filename, sep=","))
     try(remove(downloaded_ts))
  }

}



# library(quantmod)
# for(c_symbol in sym){
#   print(c_symbol)
#   try(Sys.sleep(abs(rnorm(1, mean = 1.0, sd =0.5))))
#   try(downloaded_ts <- getSymbols(c_symbol, from="1900-01-01", src="yahoo", auto.assign=F))
#   filename <- paste("/data/finance/US/price/", c_symbol, ".csv", sep = "")
#   try(write.zoo(downloaded_ts, file=filename, sep=","))
#   try(remove(downloaded_ts))
# }

registerDoMC();



# Create the local lookup table
create_local_symbol_lookup();


# Download fundamental data from Yahoo
#find the symbols of the list of historical price file in drive.
downloaded_sym <- list.files(path="/data/finance/US/price")
downloaded_sym <- toupper(gsub("\\.csv","",downloaded_sym))

max <- 100
x <- seq_along(downloaded_sym)
sym_grp <- split(downloaded_sym, ceiling(x/max))
cl_funddata <- data.frame()

for(c_symbol_grp in sym_grp){
  try(temp <- getQuote(paste(c_symbol_grp, sep=";"), 
                   what = yahooQF(c("Average Daily Volume", "Book Value",                                   
                                    "Dividend/Share", "Earnings/Share",                                   
                                    "EPS Estimate Current Year", "EPS Estimate Next Year", 
                                    "EPS Estimate Next Quarter",
                                    "Market Capitalization", "EBITDA",                                  
                                    "Name",                                 
                                    "Price/Sales", "Price/Book", "P/E Ratio", 
                                    "P/E Ratio (Real-time)", "PEG Ratio", 
                                    "Price/EPS Estimate Current Year", 
                                    "Price/EPS Estimate Next Year", 
                                    "Symbol", "Shares Owned", "Short Ratio", 
                                    "1 yr Target Price", "Volume","Dividend Yield"))))
  cl_funddata <- rbind(cl_funddata, temp)
}

save( cl_funddata, file="/data/finance/US/US_stock_fundmental_data.Rdata")

make_sp500_stockData();
make_sp400_stockData();
make_sp600_stockData();
make_russell2000_stockData();
make_top100_etf_stockData();




