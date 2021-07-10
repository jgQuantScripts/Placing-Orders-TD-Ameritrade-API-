# https://developer.tdameritrade.com/content/place-order-samples
require("reticulate");require("lubridate");require("httr");require("jsonlite")
# save API keys and account number 
PASS <- new.env()
assign("apikey","**************",envir = PASS)
assign("acctNum","*************",envir = PASS)
# call back URL from TD App
callback = "https://127.0.0.1"
# source python script
source_python("~/Desktop/R/authTD.py")
# *********************************************************************************************************
#                                          helper functions
# *********************************************************************************************************
# get Bearer token
getBearertoken= function()
{
  # How to get token ->  https://www.youtube.com/watch?v=mXCEV3NhPoM
  # follow directions to get Refresher Token and save it as an RDS file 
  # ex: saveRDS(token,"token.90.rds")
  token = readRDS("~/Desktop/R/token90.rds")
  # get a new token
  token = access_token(refresh_token=token$refresh_token, client_id=PASS$apikey)
  # format token for request
  btoken = paste0("Bearer ",token$access_token)
  # return updated token
  btoken
}
# cancel orders by order ID
cancel_order = function(orderID)
{
  # URL
  url = paste0("https://api.tdameritrade.com/v1/accounts/",PASS$acctNum,"/orders/",orderID)
  # get bearer token
  btoken = getBearertoken()
  # DELETE request 
  pg <- httr::DELETE(url,httr::add_headers(`Authorization` = btoken), encode = "json")
}
# *********************************************************************************************************
# from : Date must be within 60 days from today's date
# status : AWAITING_PARENT_ORDER | AWAITING_CONDITION | AWAITING_MANUAL_REVIEW | ACCEPTED | AWAITING_UR_OUT
#          PENDING_ACTIVATION | QUEUED | WORKING | REJECTED | PENDING_CANCEL | CANCELED | PENDING_REPLACE
#          REPLACED | FILLED | EXPIRED
getOrderHistory = function(maxResults,from, to, status)
{
  # format the dates 
  from = format(as.Date(from), "%Y-%m-%d")
  to   = format(as.Date(to), "%Y-%m-%d")
  # build URL
  url = paste0("https://api.tdameritrade.com/v1/accounts/",PASS$acctNum,"/orders?maxResults=",maxResults,
               "&fromEnteredTime=",from,"&toEnteredTime=",to,"&status=",status)
  # get bearer token
  btoken = getBearertoken()
  # GET request
  pg <- httr::GET(url,httr::add_headers(`Authorization` = btoken))
  # read in content
  dt<- fromJSON(rawToChar(pg$content))
  # return data
  dt  
}
# example
OH <- getOrderHistory(maxResults = 100, from=Sys.Date()-days(60), to=Sys.Date(),status = "CANCELED")
#OH[,ncol(OH)] <- NA
#OH <- getOrderHistory(maxResults = 100, from=Sys.Date()-days(60), to=Sys.Date(),status = "FILLED")
# *********************************************************************************************************
# from : Date ....Note: The maximum date range is one year.
# type : ALL | TRADE | BUY_ONLY | SELL_ONLY | CASH_IN_OR_CASH_OUT | CHECKING | DIVIDEND | INTEREST | OTHER | ADVISOR_FEES
getTransactions = function(type, symbol,from, to)
{
  # format the dates 
  from = format(as.Date(from), "%Y-%m-%d")
  to   = format(as.Date(to), "%Y-%m-%d")
  # build URL
  if(is.null(symbol)){
    url = paste0("https://api.tdameritrade.com/v1/accounts/",PASS$acctNum,"/transactions?type=",type,
                 "&startDate=",from,"&endDate=",to)
  }
  if(!is.null(symbol)){
    url = paste0("https://api.tdameritrade.com/v1/accounts/",PASS$acctNum,"/transactions?type=",type,
                 "&startDate=",from,"&endDate=",to,"&symbol=",symbol)
  }
  # get bearer token
  btoken = getBearertoken()
  # GET request
  pg <- httr::GET(url,httr::add_headers(`Authorization` = btoken))
  # read in content
  dt<- fromJSON(rawToChar(pg$content))
  # return data
  dt  
}
# example
df <- getTransactions(type="ALL",symbol="VUG",from="2021-01-01",to=Sys.Date())
# df$transactionItem$accountId <- NA
df <- getTransactions(type="ALL",symbol=NULL,from="2021-01-01",to=Sys.Date())
# df$transactionItem$accountId <- NA
# *********************************************************************************************************
# orderType        : MARKET | LIMIT | STOP | STOP_LIMIT | TRAILING_STOP | MARKET_ON_CLOSE | EXERCISE | 
#                    TRAILING_STOP_LIMIT | NET_DEBIT | NET_CREDIT | NET_ZERO
# session          : AM | PM | NORMAL | SEAMLESS (extended hours)
# duration         : DAY | GOOD_TILL_CANCEL | FILL_OR_KILL
# orderStrategyType: SINGLE | OCO | TRIGGER
# instruction      : BUY | SELL | BUY_TO_COVER | SELL_SHORT | BUY_TO_OPEN | BUY_TO_CLOSE | SELL_TO_OPEN
#                    SELL_TO_CLOSE | EXCHANGE
# assetType        :EQUITY | OPTION | INDEX | MUTUAL_FUND | CASH_EQUIVALENT | FIXED_INCOME | CURRENCY
# *********************************************************************************************************
#                                     SINGLE EQUITY/ETF ORDERS
# *********************************************************************************************************
eqt_order = function(orderType, price,session,duration,orderStrategyType,instruction,quantity,symbol,assetType)
{
  # build URL
  url = paste0("https://api.tdameritrade.com/v1/accounts/",PASS$acctNum,"/orders")
  # get bearer token
  btoken = getBearertoken()
  # POST request
  pg <- httr::POST(url,httr::add_headers(`Authorization` = btoken),
                   body = list(orderType = orderType,
                               price = price,
                               session = session,
                               duration = duration,
                               orderStrategyType = orderStrategyType,
                               orderLegCollection = list(list(
                                 instruction = instruction,
                                 quantity = quantity,
                                 instrument = list(
                                   symbol = symbol,
                                   assetType = assetType)))), encode = "json")
  # extract Order ID from URL
  orderNum = gsub('.*orders/','',pg$headers$location)
  # return order ID
  orderNum
}
# *********************************************************************************************************
# example
eqtORDER = eqt_order(orderType = "LIMIT", price=115, session = "NORMAL", duration = "DAY", 
                     orderStrategyType = "SINGLE",instruction = "BUY",quantity = 1, 
                     symbol = "ARKK",assetType = "EQUITY")
# cancel order
cancel_order(orderID = eqtORDER)
# *********************************************************************************************************
#                                     SINGLE OPTION ORDERS        
# *********************************************************************************************************
# EXPIRATION FORMAT: 2021-06-18
# type == C (call) | P (put)
placeOrder_option = function(symbol,expiration,type,strike,orderType,session,price,duration,instruction,quantity)
{
  # build option Symbol
  expiry = format(as.Date(expiration),"%m%d%y")
  opt_Symbol = paste0(symbol,"_",expiry,toupper(type),strike)
  # build URL
  url = paste0("https://api.tdameritrade.com/v1/accounts/",PASS$acctNum,"/orders")
  # get bearer token
  btoken = getBearertoken()
  # POST order
  pg <- httr::POST(url,httr::add_headers(`Authorization` = btoken),
                   body = list(orderType = orderType,
                               price = price,
                               session = session,
                               duration = duration,
                               orderStrategyType = "SINGLE",
                               orderLegCollection = list(list(
                                 instruction = instruction,
                                 quantity = quantity,
                                 instrument = list(
                                   symbol = opt_Symbol,
                                   assetType = "OPTION")))), encode = "json")
  # extract order ID from URL
  orderNum = gsub('.*orders/','',pg$headers$location)
  # return order ID
  orderNum
}
# example:
optORDER = placeOrder_option(symbol="AAPL",expiration = "2021-07-16",type="C",strike = 120,orderType = "LIMIT",
                             session = "NORMAL",price=1.00,duration="DAY",instruction = "BUY_TO_OPEN",quantity = 1)
# cancel order
cancel_order(orderID = optORDER)
# *********************************************************************************************************
#                                     VERTICAL OPTION ORDERS    
# *********************************************************************************************************
# ORDER TYPE = NET_DEBIT | NET_CREDIT
# EXPIRATION FORMAT: 2021-06-18
# type == C (call) | P (put)
open_vertical = function(symbol,expiration,type,strike2buy,strike2sell,orderType,session,price,duration,quantity)
{
  # build option symbols
  expiry = format(as.Date(expiration),"%m%d%y")
  opt2buy  = paste0(symbol,"_",expiry,toupper(type),strike2buy)
  opt2sell = paste0(symbol,"_",expiry,toupper(type),strike2sell)
  # build url
  url = paste0("https://api.tdameritrade.com/v1/accounts/",PASS$acctNum,"/orders")
  # get bearer token
  btoken = getBearertoken()
  # POST order
  pg <- httr::POST(url,httr::add_headers(`Authorization` = btoken),
                   body = list(orderType = orderType,
                               price = price,
                               session = session,
                               duration = duration,
                               orderStrategyType = "SINGLE",
                               orderLegCollection = list(
                                 list(
                                   instruction = "BUY_TO_OPEN",
                                   quantity = quantity,
                                   instrument = list(
                                     symbol = opt2buy,
                                     assetType = "OPTION")),
                                 list(
                                   instruction = "SELL_TO_OPEN",
                                   quantity = quantity,
                                   instrument = list(
                                     symbol = opt2sell,
                                     assetType = "OPTION"))
                               )), encode = "json")
  
  # extract the order ID from URL
  orderNum = gsub('.*orders/','',pg$headers$location)
  # return Order ID
  orderNum
}
close_vertical = function(symbol,expiration,type,strike2buy,strike2sell,orderType,session,price,duration,quantity)
{
  # build option Symbols
  expiry = format(as.Date(expiration),"%m%d%y")
  opt2buy  = paste0(symbol,"_",expiry,toupper(type),strike2buy)
  opt2sell = paste0(symbol,"_",expiry,toupper(type),strike2sell)
  # build URL
  url = paste0("https://api.tdameritrade.com/v1/accounts/",PASS$acctNum,"/orders")
  # get bearer token
  btoken = getBearertoken()
  # POST order 
  pg <- httr::POST(url,httr::add_headers(`Authorization` = btoken),
                   body = list(orderType = orderType,
                               price = price,
                               session = session,
                               duration = duration,
                               orderStrategyType = "SINGLE",
                               orderLegCollection = list(
                                 list(
                                   instruction = "BUY_TO_CLOSE",
                                   quantity = quantity,
                                   instrument = list(
                                     symbol = opt2buy,
                                     assetType = "OPTION")),
                                 list(
                                   instruction = "SELL_TO_CLOSE",
                                   quantity = quantity,
                                   instrument = list(
                                     symbol = opt2sell,
                                     assetType = "OPTION"))
                               )), encode = "json")
  
  # extract order ID from URL
  orderNum = gsub('.*orders/','',pg$headers$location)
  # return order ID
  orderNum
}

# DEBIT SPREAD
optORDER = open_vertical(symbol="SPY",expiration="2021-07-16",type="C",strike2buy=425,strike2sell=426,
                         orderType="NET_DEBIT",session="NORMAL",price=0.20,duration="DAY",quantity=1)
cancel_order(orderID = optORDER)

# CREDIT SPREAD
optORDER = open_vertical(symbol="SPY",expiration="2021-07-16",type="C",strike2buy=431,strike2sell=430,
                         orderType="NET_CREDIT",session="NORMAL",price=0.70,duration="DAY",quantity=1)
cancel_order(orderID = optORDER)
# *********************************************************************************************************
#                                     CONDITIONAL EQT ORDER
# *********************************************************************************************************
# sell at a profit OR sell-stop

oco_order = function(orderType1,orderType2,session,price1,price2,stopPRC,duration,instruction,quantity,symbol,assetType)
{
  # build URL
  url = paste0("https://api.tdameritrade.com/v1/accounts/",PASS$acctNum,"/orders")
  # get bearer token
  btoken = getBearertoken()
  # POST request
  pg <- httr::POST(url,httr::add_headers(`Authorization` = btoken),
                   body = list(orderStrategyType = "OCO", 
                               childOrderStrategies = list(
                                 list(
                                   orderType = orderType1,
                                   session = session,
                                   price = price1,
                                   duration = duration,
                                   orderStrategyType = "SINGLE",
                                   orderLegCollection = list(
                                     instruction = instruction,
                                     quantity = quantity,
                                     instrument = list(
                                       symbol = symbol,
                                       assetType = assetType))),
                                 list(
                                   orderType = orderType2,
                                   session = session,
                                   price = price2,
                                   stopPrice = stopPRC,
                                   duration = duration,
                                   orderStrategyType = "SINGLE",
                                   orderLegCollection = list(
                                     instruction = instruction,
                                     quantity = quantity,
                                     instrument = list(
                                       symbol = symbol,
                                       assetType = assetType))
                                 ))
                   ), encode = "json")
  
  # rawToChar(pg$content)
  # extract Order ID from URL
  orderNum = gsub('.*orders/','',pg$headers$location)
  # return order ID
  orderNum
}

ocoID = oco_order(orderType1 = "LIMIT", orderType2 = "STOP_LIMIT",session = "NORMAL",price1 = 300, price2 = 200,
                  stopPRC=200.25,duration = "DAY",instruction = "SELL",quantity = 1,symbol = "VUG",
                  assetType = "EQUITY")


# "error": "Conditional Orders are not permitted for accounts in this segment."
# https://www.reddit.com/r/algotrading/comments/ewfhln/conditional_orders_with_tda_api/
# Solution: Contact TDA support and ask them to disable Advanced Features of ThinkOrSwim for you
# Here is what you loose:
# 1. Futures trading
# 2. can't see working orders on charts
# 3. account moves to web
# 4. incorrect day trade and margin balances
# 5. can't do complex orders in Think or Swim(OCO etc.)

