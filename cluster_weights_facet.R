#thanks Systematic Investor, Michael Kapler
#for this post http://systematicinvestor.wordpress.com/2013/03/05/cluster-risk-parity-back-test/

###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
setInternet2(TRUE)
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

#*****************************************************************
# Load historical data for ETFs
#****************************************************************** 
load.packages('quantmod')

#tickers = spl('GLD,UUP,SPY,QQQ,IWM,EEM,EFA,IYR,USO,TLT')
#to reduce calls to Yahoo I saved the data in .Rdata
data <- new.env()
load("data.Rdata",envir=data)
#getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
#for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)

bt.prep(data, align='remove.na')

#*****************************************************************
# Code Strategies
#******************************************************************   
periodicity = 'months'
lookback.len = 250
cluster.group = cluster.group.kmeans.90

obj = portfolio.allocation.helper(
  data$prices, 
  periodicity = periodicity, lookback.len = lookback.len,
  min.risk.fns = list(
    EW=equal.weight.portfolio,
    RP=risk.parity.portfolio,
    C.EW = distribute.weights(equal.weight.portfolio, cluster.group),
    C.RP=distribute.weights(risk.parity.portfolio, cluster.group)
  )
) 		

models = create.strategies(obj, data)$models

#*****************************************************************
# Create Report
#****************************************************************** 	
#strategy.performance.snapshoot(models, T)

#use rCharts to get some interactive plots
require(rCharts)
require(reshape2)

weights.df <- data.frame(index(models$C.EW$weight),models$C.EW$weight)[which(models$C.EW$weight[,1]!=0),]
colnames(weights.df)[1] <- "date"

weights.melt <- melt(weights.df, id.vars = 1)
colnames(weights.melt) = c("date","symbol","value")

nWeights <- nPlot(
  value ~ date,
  group = "symbol",
  data = weights.melt,  
  type = "stackedAreaChart"
)
nWeights$chart(clipEdge = TRUE)
nWeights$xAxis(tickFormat =
  "#!function(d) {return d3.time.format('%b %Y')(new Date( d * 1000 ));}!#"
)
nWeights

#I don't think daily is necessary, so let's try some aggregation
require(plyr)
#use plyr to get average weight by some group of dates; I chose %Y for year
weights.avg <- ddply(weights.melt, .(format(date,"%Y"),symbol), summarise, mean = mean(value))
colnames(weights.avg) [1] <- "date"

nAvgBar <- nPlot(
  mean ~ date, 
  group = "symbol",
  data = weights.avg,
  type= "multiBarChart"
)
nAvgBar

nAvgArea <- nPlot(
  mean ~ date, 
  group = "symbol",
  data = weights.avg,
  type= "stackedAreaChart"
)
nAvgArea$chart( useInteractiveGuideline = TRUE )
nAvgArea$yAxis(tickFormat = 
  "#!function(d) { return d3.format('.2%')(d) }!#"
)
nAvgArea

nAvg <- nPlot(
 mean ~ symbol,
 #group = "symbol",
 data = weights.avg,
 type = "pieChart",
 height = 1500
)
nAvg$chart( donut = TRUE, showLegend = FALSE)
nAvg$params$facet="date"
nAvg$templates$script = system.file(
  "/libraries/nvd3/layouts/nvd3FacetPlot.html",
  package = "rCharts"
)
#nAvg



#explore with strategy
#do median weight by year
strategy.melt <- do.call(rbind, lapply(names(models),function(x){
  x.melt <- melt(
    data.frame(
      index(models[[x]]$weight[-(1:254),]),    ###do 254 to eliminate 0s at beginning
      rep(x,NROW(models[[x]]$weight[-(1:254),])),
      models[[x]]$weight[-(1:254),]
    ),
    id.vars = 1:2
  )
  colnames(x.melt) <- c("date","strategy","symbol","weight")
  #get median by year
  x.melt <- ddply(x.melt, .(strategy,format(date,"%Y"),symbol), summarise, median = median(weight))
  colnames(x.melt)[2] <- "date"  
  return(x.melt)
}))

nStrat <- nPlot(
  median ~ date,
  group = "symbol",
  data = strategy.melt,
  type = "multiBarChart",
  height = 800
)
nStrat$chart(showControls = FALSE, showLegend = FALSE) 
nStrat$params$facet = list (y = "date", x = "strategy")
nStrat$templates$script = paste0(getwd(),"/assets/layouts/nvd3Facet_d3grid.html")
nStrat


nStratOld <- nPlot(
  median ~ strategy,
  group = "symbol",
  data = strategy.melt,
  type = "multiBarChart",
  height = 1500
)
nStratOld$params$facet="date"
nStratOld$templates$script = system.file(
  "/libraries/nvd3/layouts/nvd3FacetPlot.html",
  package = "rCharts"
)
nStratOld
