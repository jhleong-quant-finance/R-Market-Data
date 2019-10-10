library(quantmod)
source('/home/jhleong/dev/R/lib/libtrade.R');
source('/home/jhleong/dev/R/lib/libMYStock.R');
# important for correct time zone.
Sys.setenv(TZ='EST');

# The saved workspace with all the web page can be found at /data/finance/MY/info/bursa_stock.work_space

# extract from webpage source www.bursamalaysia.com/market/listed-companies/
# the counter number and company name
KLSE_stock <- read.csv("/data/finance/MY/company_list/bursa_stock_name_list.cvs", header = TRUE,sep="|", as.is=TRUE);

# extract from chartnexus downloaded list of company listed in bursa.
# Catogory :
# 30#Call Warrants
# 27#Closed-End Fund
# 17#Construction
# 15#Consumer Products
# 28#Exchange Traded Funds
# 21#Finance
# 22#Hotel
# 16#Industrial Products
# 20#Infrastructure
# 29#Loans
# 25#Mining
# 24#Plantation
# 23#Properties
# 26#Reits
# 19#Technology
# 18#Trading & Services

chartnexus_list <- read.csv("/data/finance/MY/company_list/list_KLSE.csv", 
                            header = TRUE,sep="#", as.is=TRUE);
chartnexus_list <- chartnexus_list[c("stock_code","ticker_code","category")]
#remove white space in chartnexus_list$stock_code
chartnexus_list$stock_code <- gsub("\\s","", chartnexus_list$stock_code);

# Merge the company name from Bursa website into the chartnexus list.
chartnexus_list$company_name <- NA


for(i in 1:NROW(chartnexus_list)) {
  
  tmp=KLSE_stock[KLSE_stock$stock_code==chartnexus_list[i,'stock_code'], 'company_name']
  if(length(tmp) > 0){
    chartnexus_list[i, 'company_name']=tmp;
  }
}

#write.csv(chartnexus_list, file = "/data/finance/MY/company_list/bursa_stock.csv")


################################################################################################
# Dowlonad price from yahoo ichart
#################################################################################################


sym <-data.frame()
for(i in 1:26){
  sym_tmp <- chartnexus_list[substring(chartnexus_list$ticker_code, 1,1) == toupper(letters[i]),]
  sym <- rbind(sym, sym_tmp)
}

# any ticker code not start with alphabet
sym <- rbind(sym, chartnexus_list[!grepl('[A-Z]', substring(chartnexus_list$ticker_code, 1,1)),])


#find the symbols of the list of historical price file in drive.
downloaded_sym <- list.files(path="/data/finance/MY/price")
downloaded_sym <- toupper(gsub("\\_KL.csv","",downloaded_sym))

# remove Symbol already downloaded
sym <- sym[! sym$ticker_code %in% downloaded_sym, ]
# remove Symbol with character "^", yahoo don't seem to have this data, google use "-"
sym <- sym[ !grepl("\\^", sym$ticker_code),]

# only take stock with numberic counter, exclude Warrent etc.
sym <- sym[grepl('^[0-9]+$',sym$stock_code),]

#only take stock with 4 digit counter
sym <- sym[nchar(sym$stock_code)==4,]



for(c_symbol in sym$ticker_code){
  print(c_symbol)
  try(downloaded_ts <- getSymbols_KLSE(c_symbol, from="1900-01-01", src="yahoo_ichart", auto.assign=F))
  filename <- paste("/data/finance/MY/price/", c_symbol, "_KL.csv", sep = "")
  try(write.zoo(downloaded_ts, file=filename, sep=","))
  try(remove(downloaded_ts))
  try(Sys.sleep(abs(rnorm(1, mean = 1, sd =1))))
}

create_local_symbol_lookup();





# ###################################################################################################
# 
# 
# 
# library(RCurl)
# library(XML)
# 
# #bursa_stock_list <- read.csv("/data/finance/MY/company_list/bursa_stock.csv", header = TRUE, as.is=TRUE);
# bursa_stock_list <- chartnexus_list
# 
# bursa_stock_list$ric <- NA
# bursa_stock_list$reuter_web <- NA
# 
# options(RCurlOptions = list(verbose = TRUE, followlocation = TRUE, timeout = 100))
# 
# # use www.reuters.com to search for reuter RIC. Search with stock ticker name,
# # return the RIC in the header H1 most of the time.
# for(i in 1:nrow(bursa_stock_list)) {
#   if(grepl("\\.KL$", bursa_stock_list[i,'ric']) == FALSE && grepl("[^0-9]$", bursa_stock_list[i,'stock_code']) == FALSE)
#   {
#     bursa_stock_list[i,'ric'] = NA;
#     
#     web <- getForm("http://www.reuters.com/search", blob=bursa_stock_list[i,'ticker_code']);
#     yy <- htmlTreeParse(web, asText=TRUE, useInternalNodes=TRUE);
#     # reuters web source code
#     bursa_stock_list[i,'reuter_web'] = web;
#     nArticles <- getNodeSet ( yy , "//*/h1");
#     
#     if(length(nArticles) > 0) {
#       # remove the bracket ( )
#       m <- regmatches(xmlValue(nArticles[[1]]), regexpr("\\(([^(\\)]*)[^(]*(?:\\([^\\)]*)?$",  xmlValue(nArticles[[1]])));
#       n <- gsub(".*\\(([^()]*)\\).*", "\\1",m); 
#       if(length(n)>0){
#         bursa_stock_list[i,'ric'] = n;
#       }
#     }
#     
#     Sys.sleep(1);
#     
#     # if the RIC is correctly identified, it end with .KL .
#     # if the previous search doesn't get correct RIC, search by stock company name
#     if(grepl("\\.KL$", bursa_stock_list[i,'ric']) == FALSE)
#     {
#       web <- getForm("http://www.reuters.com/search", blob=bursa_stock_list[i,'company_name']);
#       yy <- htmlTreeParse(web, asText=TRUE, useInternalNodes=TRUE);
#       bursa_stock_list[i,'reuter_web'] = web;
#       nArticles <- getNodeSet ( yy , "//*/div[@class='stockName']")
#       
#       if(length(nArticles) > 0) {
#         m <- regmatches(xmlValue(nArticles[[1]]), regexpr("\\(([^(\\)]*)[^(]*(?:\\([^\\)]*)?$",  xmlValue(nArticles[[1]])));
#         n <- gsub(".*\\(([^()]*)\\).*", "\\1",m);
#         if(length(n)>0){
#           bursa_stock_list[i,'ric'] = n;
#         }
#       }
#       
#       Sys.sleep(1);
#     }
#   }
# }
# 
# ################################################################################################
# # Manual update of RIC
# #
# 
# bursa_stock_list[bursa_stock_list$stock_code=='4162','ric']="BATO.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='8044','ric']="CFMS.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='1023','ric']="CIMB.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='9172','ric']="FPIB.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='3255','ric']="GUMS.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='0003','ric']="INEH.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='1597','ric']="IGBS.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='0094','ric']="INIX.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='5097','ric']="KSKG.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='0110','ric']=NA
# bursa_stock_list[bursa_stock_list$stock_code=='3786','ric']="MASM.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='7079','ric']=NA
# bursa_stock_list[bursa_stock_list$stock_code=='0103','ric']=NA
# bursa_stock_list[bursa_stock_list$stock_code=='5916','ric']="MSCB.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='4707','ric']="NESM.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='7107','ric']="OFIH.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='4715','ric']="GENM.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='1783','ric']="SELS.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='1813','ric']="SPKS.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='9369','ric']="TGLB.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='4863','ric']="TLMM.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='0060','ric']="TMDS.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='2488','ric']="ALFG.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='5081','ric']="ESTH.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='5149','ric']="TSOF.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='5162','ric']="ECSI.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='9822','ric']="SAEG.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='0155','ric']="MGRC.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='5182','ric']=NA
# bursa_stock_list[bursa_stock_list$stock_code=='5189','ric']=NA
# bursa_stock_list[bursa_stock_list$stock_code=='0159','ric']=NA
# bursa_stock_list[bursa_stock_list$stock_code=='5192','ric']=NA
# bursa_stock_list[bursa_stock_list$stock_code=='5214','ric']=NA
# bursa_stock_list[bursa_stock_list$stock_code=='1295F','ric']="PUBMe.KL"
# 
# # reuter return PETGAS as result for all this counter, manually rewrite.
# bursa_stock_list[bursa_stock_list$stock_code=='7579','ric']="AWCF.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='1007','ric']="AMCO.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='0041','ric']="CBSA.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='7029','ric']="MSTP.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='1198','ric']=NA
# bursa_stock_list[bursa_stock_list$stock_code=='9458','ric']=NA
# bursa_stock_list[bursa_stock_list$stock_code=='5152','ric']=NA
# bursa_stock_list[bursa_stock_list$stock_code=='0151','ric']="KELG.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='5167','ric']=NA
# bursa_stock_list[bursa_stock_list$stock_code=='8419','ric']="PANS.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='5163','ric']=NA
# bursa_stock_list[bursa_stock_list$stock_code=='5175','ric']="IVRY.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='0158','ric']="SCCH.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='0163','ric']=NA
# bursa_stock_list[bursa_stock_list$stock_code=='9075WA','ric']=NA
# bursa_stock_list[bursa_stock_list$stock_code=='0173','ric']="CATC.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='5213','ric']=NA
# bursa_stock_list[bursa_stock_list$stock_code=='9377WA','ric']=NA
# bursa_stock_list[bursa_stock_list$stock_code=='5219','ric']="PEIN.KL"
# bursa_stock_list[bursa_stock_list$stock_code=='5220','ric']=NA
# 
# ################################################################################################
# 
# 
# 
# # save the company list without the web source
# write.csv(bursa_stock_list[-c(1,7)], file = "/data/finance/MY/company_list/bursa_stock_list.csv")
# # save all the downloaded web source.
# save(bursa_stock_list, file="/data/finance/MY/info/bursa_stock.dat");
# 
# 
# #stock that don't get populated correctly
# #not_na_list <- bursa_stock_list[!is.na(bursa_stock_list$ric), c('stock_code', 'company_name', 'ticker_code', 'ric','category')]
# #nn <- not_na_list[!grepl("\\.KL$", not_na_list$ric), ]
# #nn[!grepl("[^0-9]$", nn$stock_code),]
# 
# 
# # extract the financial information from the downloaded webpage, if not available it will 
# # download from the internet.
# for(i in 1:nrow(bursa_stock_list)) {
#   # only stock, not warrent
#   if(grepl("[^0-9]$", bursa_stock_list[i,'stock_code']) == FALSE){
#     yy <-htmlTreeParse(bursa_stock_list[i,'reuter_web'], asText=TRUE, useInternalNodes=TRUE);
#     dd <-xpathApply( yy, "//*/div[@id='overallRatios']/*/div[@class='moduleBody']/table/tbody/tr/td", function(x) xmlValue(x))
#     
#     if(length(dd)== 0)
#     {
#       web <- getForm("http://www.reuters.com/finance/stocks/overview", symbol=bursa_stock_list[i, 'ric']);
#       Sys.sleep(1);
#       yy <-htmlTreeParse(web, asText=TRUE, useInternalNodes=TRUE);
#       bursa_stock_list[i,'reuter_web'] = web;
#       dd <-xpathApply( yy, "//*/div[@id='overallRatios']/*/div[@class='moduleBody']/table/tbody/tr/td", function(x) xmlValue(x))
#       
#     }
#     
#     if(length(dd)> 0)
#     {
#       print(i);
#       #   beta
#       bursa_stock_list[i,'rt_beta'] = as.numeric(dd[2]);
#       #  Market Cap
#       n_tmp_mk_cap <- gsub(",","",dd[4]); 
#       n_mk_cap <- as.numeric(regmatches(n_tmp_mk_cap, regexpr("[0-9\\.].*",  n_tmp_mk_cap)));
#       if(length(n_mk_cap)>0){
#         bursa_stock_list[i, 'rt_marketcap'] = n_mk_cap;}
#       else{
#         bursa_stock_list[i, 'rt_marketcap'] = NA;}
#       
#       # Share outstanding
#       bursa_stock_list[i, 'rt_share_outstanding'] = as.numeric(gsub(",","",dd[6]));
#       #dividend
#       bursa_stock_list[i, 'rt_dividend'] = as.numeric(dd[8]);
#       #Yield
#       bursa_stock_list[i, 'rt_yield'] = as.numeric(dd[10]);
#     }
#   }
# }
# 
# 
# #Financial
# for(i in 1:nrow(bursa_stock_list)) {
#   
#   if(grepl("[^0-9]$", bursa_stock_list[i,'stock_code']) == FALSE){
#     yy <-htmlTreeParse(bursa_stock_list[i,'reuter_web'], asText=TRUE, useInternalNodes=TRUE);
#     dd <-xpathApply( yy, "//*/div[@id='companyVsIndustry']/*/div[@class='moduleBody']/table/tbody/tr/td", function(x) xmlValue(x));
#     
#     if(length(dd)== 0)
#     {
#       web <- getForm("http://www.reuters.com/finance/stocks/overview", symbol=bursa_stock_list[i, 'ric']);
#       Sys.sleep(1);
#       yy <-htmlTreeParse(web, asText=TRUE, useInternalNodes=TRUE);
#       bursa_stock_list[i,'reuter_web'] = web;
#       dd <-xpathApply( yy, "//*/div[@id='companyVsIndustry']/*/div[@class='moduleBody']/table/tbody/tr/td", function(x) xmlValue(x));
#       
#     }
#     
#     if(length(dd)> 0)
#     {
#       print(i);
#       #   P/E
#       bursa_stock_list[i,'rt_pe'] = as.numeric(regmatches(as.character(dd[2]), regexpr("[0-9\\.-].*",  as.character(dd[2]))));
#       #  EPS
#       bursa_stock_list[i,'rt_eps'] = as.numeric(regmatches(as.character(dd[6]), regexpr("[0-9\\.-].*",  as.character(dd[6]))));
#       # ROI
#       bursa_stock_list[i,'rt_roi'] = as.numeric(regmatches(as.character(dd[10]), regexpr("[0-9\\.-].*",  as.character(dd[10]))));
#       # ROE
#       bursa_stock_list[i,'rt_roe'] = as.numeric(regmatches(as.character(dd[14]), regexpr("[0-9\\.-].*",  as.character(dd[14]))));
#     }
#   }
# }
# 
# bursa_stock_info <- bursa_stock_list[-c(1, 7)];
# write.csv(bursa_stock_info, file = "/data/finance/MY/info/bursa_stock_info.csv");