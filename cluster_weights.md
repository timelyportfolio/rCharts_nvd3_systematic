---
title: rCharts + nvd3 | Systematic Investor Methods
author: Timely Portfolio
github: {user: timelyportfolio, repo: rCharts_nvd3_systematic, branch: "gh-pages"}
framework: bootstrap
mode: selfcontained
highlighter: prettify
hitheme: twitter-bootstrap
assets:
  css:
  - "http://fonts.googleapis.com/css?family=Raleway:300"
  - "http://fonts.googleapis.com/css?family=Oxygen"
---

<style>
iframe{
  height:600px;
  width:900px;
  margin:auto auto;
}

body{
  font-family: 'Oxygen', sans-serif;
  font-size: 16px;
  line-height: 24px;
}

h1,h2,h3,h4 {
font-family: 'Raleway', sans-serif;
}

.container { width: 900px; }

h3 {
background-color: #D4DAEC;
  text-indent: 100px; 
}

h4 {
text-indent: 100px;
}
</style>
  
<a href="https://github.com/timelyportfolio/rCharts_nvd3_perf"><img style="position: absolute; top: 0; right: 0; border: 0;" src="https://s3.amazonaws.com/github/ribbons/forkme_right_darkblue_121621.png" alt="Fork me on GitHub"></a>

# Interactive Analysis of Systematic Investor

The [Systematic Investor blog](http://systematicinvestor.wordpress.com) is an incredible resource for the R finance community.  I know it is possible to replicate some of his work in Javascript, but I find it very unlikely that anyone will do it any time soon.  Fortunately though this does not mean R users can't explore his work using Javascript interactive charting.  With [rCharts](http://rcharts.io/site), we R users can use R to do the math and Javascript to do the plots.  I thought it would be good fun to take one of the [posts from Systematic Investor](http://systematicinvestor.wordpress.com/2013/03/05/cluster-risk-parity-back-test/) and d3-ify it.

### Copy/Paste Systematic Investor Brilliance
Let's start by getting the data and performing the calculations in R.  This is a direct copy and paste from the Systematic Investor post.  Thanks again Systematic Investor.






```r
#thanks Systematic Investor http://systematicinvestor.wordpress.com
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

tickers = spl('GLD,UUP,SPY,QQQ,IWM,EEM,EFA,IYR,USO,TLT')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)

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
```


### Plot with [rCharts](http://rcharts.io/site) and [nvd3](http://nvd3.org)
With this really useful set of data, let's explore the weights assigned each asset by the Cluster Equal Weight (C.EW) method.  I will start by using a direct dump of the weight data, but as you might notice, doing the chart with daily data slows things down a bit.


```r
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
nWeights$setLib(lib="libraries/widgets/nvd3")
nWeights
```

<iframe src=assets/fig/unnamed-chunk-3.html seamless></iframe>


### Aggregate Data for Better Performance
With `plyr` we can do summarize our data by year.  I will use `mean` in the next 3 charts.


```r
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
nAvgBar$setLib(lib="libraries/widgets/nvd3")
nAvgBar
```

<iframe src=assets/fig/unnamed-chunk-4.html seamless></iframe>


Here is the same data as a stacked area plot instead of bar plot.


```r
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
nAvgArea$setLib(lib="libraries/widgets/nvd3")
nAvgArea
```

<iframe src=assets/fig/unnamed-chunk-5.html seamless></iframe>


### Facet Like a Very Crude `ggplot2`
In this [test branch](https://github.com/timelyportfolio/rCharts/tree/test-speedimprove) of `rCharts`, I have started experimenting with facets for `dimplejs` and `nvd3`.  Here are some ugly pie charts facetted by `year`.  Of course, if we had facet wrap like `ggplot2` these would be much better.


```r
nAvg <- nPlot(
 mean ~ symbol,
 #group = "symbol",
 data = weights.avg,
 type = "pieChart",
 height = 1500
)
nAvg$setLib(lib="libraries/widgets/nvd3")
nAvg$chart( donut = TRUE, showLegend = FALSE)
nAvg$params$facet="date"
nAvg$templates$script = system.file(
  "/libraries/nvd3/layouts/nvd3FacetPlot.html",
  package = "rCharts"
)
nAvg
```

<iframe src=assets/fig/unnamed-chunk-6.html seamless></iframe>


### More Comprehensive View
In the charts above, we limited our analysis to just one asset allocation method.  Let's expand our analysis to all the asset allocation methods that were calculated and demonstrate another chart facetted by `year`.


```r
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
  median ~ strategy,
  group = "symbol",
  data = strategy.melt,
  type = "multiBarChart",
  height = 1500
)
nStrat$params$facet="date"
nStrat$setLib(lib="libraries/widgets/nvd3")
nStrat$templates$script = system.file(
  "/libraries/nvd3/layouts/nvd3FacetPlot.html",
  package = "rCharts"
)
nStrat
```

<iframe src=assets/fig/unnamed-chunk-7.html seamless></iframe>


Another way to facet would be by strategy.


```r
nStrat2 <- nPlot(
  median ~ date,
  group = "symbol",
  data = strategy.melt,
  type = "multiBarChart",
  height = 1500
)
nStrat2$params$facet="strategy"
nStrat2$setLib(lib="libraries/widgets/nvd3")
nStrat2$templates$script = system.file(
  "/libraries/nvd3/layouts/nvd3FacetPlot.html",
  package = "rCharts"
)
nStrat2
```

<iframe src=assets/fig/unnamed-chunk-8.html seamless></iframe>


### Thanks
Thanks again:
- [Systematic Investor](http://systematicinvestor.wordpress.com) for all the code
- [Ramnath Vaidyanathan](http://github.com/ramnathv) for [rCharts](http://rcharts.io/site) and [slidify](http://slidify.org)
- the team behind [nvd3](http://nvd3.org)
- and [Mike Bostock](http://bost.ocks.org/mike/)
