Activision Blizzard Stock Time Series Analysis
================
Jeffrey Li
November 20, 2018

``` r
rm(list = ls())
library(quantmod)
library(forecast)
library(ggplot2)
library(scales)
theme_set(theme_minimal())
library(gridExtra)
library(dygraphs)
library(tseries)
library(ggfortify)
library(TTR)
library(kableExtra)

start <- as.Date("2017-05-22")
end <- as.Date("2018-11-30")
suppressWarnings(getSymbols("ATVI", src = "yahoo", from = start, to = end))
```

    ## [1] "ATVI"

``` r
ATVI_df <- as.data.frame(ATVI)
ATVI_df<- cbind(Date=as.Date(rownames(ATVI_df)),ATVI_df)

nrow(ATVI)
```

    ## [1] 386

``` r
acc_table <- data.frame()

forecast_len <- floor(0.1*(nrow(ATVI)))
```

A summary of the Activision Blizzard stock.

``` r
summary(ATVI)
```

    ##      Index              ATVI.Open       ATVI.High        ATVI.Low    
    ##  Min.   :2017-05-22   Min.   :47.77   Min.   :49.91   Min.   :46.83  
    ##  1st Qu.:2017-10-06   1st Qu.:62.41   1st Qu.:63.15   1st Qu.:61.74  
    ##  Median :2018-02-26   Median :66.61   Median :67.44   Median :65.53  
    ##  Mean   :2018-02-24   Mean   :67.58   Mean   :68.40   Mean   :66.62  
    ##  3rd Qu.:2018-07-15   3rd Qu.:72.50   3rd Qu.:73.31   3rd Qu.:71.31  
    ##  Max.   :2018-11-29   Max.   :84.18   Max.   :84.68   Max.   :82.74  
    ##    ATVI.Close     ATVI.Volume       ATVI.Adjusted  
    ##  Min.   :49.14   Min.   : 1644700   Min.   :49.14  
    ##  1st Qu.:62.41   1st Qu.: 4688600   1st Qu.:62.09  
    ##  Median :66.34   Median : 5723350   Median :66.29  
    ##  Mean   :67.54   Mean   : 6603749   Mean   :67.35  
    ##  3rd Qu.:72.36   3rd Qu.: 7589275   3rd Qu.:72.09  
    ##  Max.   :83.39   Max.   :34543400   Max.   :83.39

There are 386 data entries.

Check for anyNA values.

``` r
anyNA(ATVI)
```

    ## [1] FALSE

Transforming the xts to a timeseries (ts).

``` r
ATVI_ts <- ts(ATVI, frequency = 5)
plot(ATVI_ts)
```

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-4-1.png)

Looking at univariate distributions of the stocks.

``` r
par(mfrow = c(2,2))
uni_open <- ggplot(ATVI, aes(ATVI.Open)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()

uni_high <- ggplot(ATVI, aes(ATVI.High)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()

uni_low <- ggplot(ATVI, aes(ATVI.Low)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()

uni_close <- ggplot(ATVI, aes(ATVI.Close)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()

grid.arrange(uni_open, uni_high, uni_low, uni_close, nrow = 2, ncol = 2)
```

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
average_price = sum(ATVI_df$ATVI.Open + ATVI_df$ATVI.Close)/(2*nrow(ATVI))
```

It seems historically the majority of prices for the ATVI stock has been around 67.56 USD. The density plots are left skewed and have a bimodal distribution.

Visualizing the closing stock prices.

``` r
dygraph(ATVI_df[,c("ATVI.Open","ATVI.Close")], xlab = "Time", ylab = "Price in USD", main = "Activision Blizzard, Inc. Opening and Closing Stock Price") %>% dyOptions(colors = c(rgb(57,106,177, maxColorValue = 255), rgb(218,124,48, maxColorValue = 255)))
```

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-53286855ccb35cb35999">{"x":{"attrs":{"title":"Activision Blizzard, Inc. Opening and Closing Stock Price","xlabel":"Time","ylabel":"Price in USD","labels":["day","ATVI.Open","ATVI.Close"],"legend":"auto","retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60,"drawAxis":true},"y":{"drawAxis":true}},"stackedGraph":false,"fillGraph":false,"fillAlpha":0.15,"stepPlot":false,"drawPoints":false,"pointSize":1,"drawGapEdgePoints":false,"connectSeparatedPoints":false,"strokeWidth":1,"strokeBorderColor":"white","colors":["#396AB1","#DA7C30"],"colorValue":0.5,"colorSaturation":1,"includeZero":false,"drawAxesAtZero":false,"logscale":false,"axisTickSize":3,"axisLineColor":"black","axisLineWidth":0.3,"axisLabelColor":"black","axisLabelFontSize":14,"axisLabelWidth":60,"drawGrid":true,"gridLineWidth":0.3,"rightGap":5,"digitsAfterDecimal":2,"labelsKMB":false,"labelsKMG2":false,"labelsUTC":false,"maxNumberWidth":6,"animatedZooms":false,"mobileDisableYTouch":true,"disableZoom":false},"scale":"daily","annotations":[],"shadings":[],"events":[],"format":"date","data":[["2017-05-22T04:00:00.000Z","2017-05-23T04:00:00.000Z","2017-05-24T04:00:00.000Z","2017-05-25T04:00:00.000Z","2017-05-26T04:00:00.000Z","2017-05-30T04:00:00.000Z","2017-05-31T04:00:00.000Z","2017-06-01T04:00:00.000Z","2017-06-02T04:00:00.000Z","2017-06-05T04:00:00.000Z","2017-06-06T04:00:00.000Z","2017-06-07T04:00:00.000Z","2017-06-08T04:00:00.000Z","2017-06-09T04:00:00.000Z","2017-06-12T04:00:00.000Z","2017-06-13T04:00:00.000Z","2017-06-14T04:00:00.000Z","2017-06-15T04:00:00.000Z","2017-06-16T04:00:00.000Z","2017-06-19T04:00:00.000Z","2017-06-20T04:00:00.000Z","2017-06-21T04:00:00.000Z","2017-06-22T04:00:00.000Z","2017-06-23T04:00:00.000Z","2017-06-26T04:00:00.000Z","2017-06-27T04:00:00.000Z","2017-06-28T04:00:00.000Z","2017-06-29T04:00:00.000Z","2017-06-30T04:00:00.000Z","2017-07-03T04:00:00.000Z","2017-07-05T04:00:00.000Z","2017-07-06T04:00:00.000Z","2017-07-07T04:00:00.000Z","2017-07-10T04:00:00.000Z","2017-07-11T04:00:00.000Z","2017-07-12T04:00:00.000Z","2017-07-13T04:00:00.000Z","2017-07-14T04:00:00.000Z","2017-07-17T04:00:00.000Z","2017-07-18T04:00:00.000Z","2017-07-19T04:00:00.000Z","2017-07-20T04:00:00.000Z","2017-07-21T04:00:00.000Z","2017-07-24T04:00:00.000Z","2017-07-25T04:00:00.000Z","2017-07-26T04:00:00.000Z","2017-07-27T04:00:00.000Z","2017-07-28T04:00:00.000Z","2017-07-31T04:00:00.000Z","2017-08-01T04:00:00.000Z","2017-08-02T04:00:00.000Z","2017-08-03T04:00:00.000Z","2017-08-04T04:00:00.000Z","2017-08-07T04:00:00.000Z","2017-08-08T04:00:00.000Z","2017-08-09T04:00:00.000Z","2017-08-10T04:00:00.000Z","2017-08-11T04:00:00.000Z","2017-08-14T04:00:00.000Z","2017-08-15T04:00:00.000Z","2017-08-16T04:00:00.000Z","2017-08-17T04:00:00.000Z","2017-08-18T04:00:00.000Z","2017-08-21T04:00:00.000Z","2017-08-22T04:00:00.000Z","2017-08-23T04:00:00.000Z","2017-08-24T04:00:00.000Z","2017-08-25T04:00:00.000Z","2017-08-28T04:00:00.000Z","2017-08-29T04:00:00.000Z","2017-08-30T04:00:00.000Z","2017-08-31T04:00:00.000Z","2017-09-01T04:00:00.000Z","2017-09-05T04:00:00.000Z","2017-09-06T04:00:00.000Z","2017-09-07T04:00:00.000Z","2017-09-08T04:00:00.000Z","2017-09-11T04:00:00.000Z","2017-09-12T04:00:00.000Z","2017-09-13T04:00:00.000Z","2017-09-14T04:00:00.000Z","2017-09-15T04:00:00.000Z","2017-09-18T04:00:00.000Z","2017-09-19T04:00:00.000Z","2017-09-20T04:00:00.000Z","2017-09-21T04:00:00.000Z","2017-09-22T04:00:00.000Z","2017-09-25T04:00:00.000Z","2017-09-26T04:00:00.000Z","2017-09-27T04:00:00.000Z","2017-09-28T04:00:00.000Z","2017-09-29T04:00:00.000Z","2017-10-02T04:00:00.000Z","2017-10-03T04:00:00.000Z","2017-10-04T04:00:00.000Z","2017-10-05T04:00:00.000Z","2017-10-06T04:00:00.000Z","2017-10-09T04:00:00.000Z","2017-10-10T04:00:00.000Z","2017-10-11T04:00:00.000Z","2017-10-12T04:00:00.000Z","2017-10-13T04:00:00.000Z","2017-10-16T04:00:00.000Z","2017-10-17T04:00:00.000Z","2017-10-18T04:00:00.000Z","2017-10-19T04:00:00.000Z","2017-10-20T04:00:00.000Z","2017-10-23T04:00:00.000Z","2017-10-24T04:00:00.000Z","2017-10-25T04:00:00.000Z","2017-10-26T04:00:00.000Z","2017-10-27T04:00:00.000Z","2017-10-30T04:00:00.000Z","2017-10-31T04:00:00.000Z","2017-11-01T04:00:00.000Z","2017-11-02T04:00:00.000Z","2017-11-03T04:00:00.000Z","2017-11-06T05:00:00.000Z","2017-11-07T05:00:00.000Z","2017-11-08T05:00:00.000Z","2017-11-09T05:00:00.000Z","2017-11-10T05:00:00.000Z","2017-11-13T05:00:00.000Z","2017-11-14T05:00:00.000Z","2017-11-15T05:00:00.000Z","2017-11-16T05:00:00.000Z","2017-11-17T05:00:00.000Z","2017-11-20T05:00:00.000Z","2017-11-21T05:00:00.000Z","2017-11-22T05:00:00.000Z","2017-11-24T05:00:00.000Z","2017-11-27T05:00:00.000Z","2017-11-28T05:00:00.000Z","2017-11-29T05:00:00.000Z","2017-11-30T05:00:00.000Z","2017-12-01T05:00:00.000Z","2017-12-04T05:00:00.000Z","2017-12-05T05:00:00.000Z","2017-12-06T05:00:00.000Z","2017-12-07T05:00:00.000Z","2017-12-08T05:00:00.000Z","2017-12-11T05:00:00.000Z","2017-12-12T05:00:00.000Z","2017-12-13T05:00:00.000Z","2017-12-14T05:00:00.000Z","2017-12-15T05:00:00.000Z","2017-12-18T05:00:00.000Z","2017-12-19T05:00:00.000Z","2017-12-20T05:00:00.000Z","2017-12-21T05:00:00.000Z","2017-12-22T05:00:00.000Z","2017-12-26T05:00:00.000Z","2017-12-27T05:00:00.000Z","2017-12-28T05:00:00.000Z","2017-12-29T05:00:00.000Z","2018-01-02T05:00:00.000Z","2018-01-03T05:00:00.000Z","2018-01-04T05:00:00.000Z","2018-01-05T05:00:00.000Z","2018-01-08T05:00:00.000Z","2018-01-09T05:00:00.000Z","2018-01-10T05:00:00.000Z","2018-01-11T05:00:00.000Z","2018-01-12T05:00:00.000Z","2018-01-16T05:00:00.000Z","2018-01-17T05:00:00.000Z","2018-01-18T05:00:00.000Z","2018-01-19T05:00:00.000Z","2018-01-22T05:00:00.000Z","2018-01-23T05:00:00.000Z","2018-01-24T05:00:00.000Z","2018-01-25T05:00:00.000Z","2018-01-26T05:00:00.000Z","2018-01-29T05:00:00.000Z","2018-01-30T05:00:00.000Z","2018-01-31T05:00:00.000Z","2018-02-01T05:00:00.000Z","2018-02-02T05:00:00.000Z","2018-02-05T05:00:00.000Z","2018-02-06T05:00:00.000Z","2018-02-07T05:00:00.000Z","2018-02-08T05:00:00.000Z","2018-02-09T05:00:00.000Z","2018-02-12T05:00:00.000Z","2018-02-13T05:00:00.000Z","2018-02-14T05:00:00.000Z","2018-02-15T05:00:00.000Z","2018-02-16T05:00:00.000Z","2018-02-20T05:00:00.000Z","2018-02-21T05:00:00.000Z","2018-02-22T05:00:00.000Z","2018-02-23T05:00:00.000Z","2018-02-26T05:00:00.000Z","2018-02-27T05:00:00.000Z","2018-02-28T05:00:00.000Z","2018-03-01T05:00:00.000Z","2018-03-02T05:00:00.000Z","2018-03-05T05:00:00.000Z","2018-03-06T05:00:00.000Z","2018-03-07T05:00:00.000Z","2018-03-08T05:00:00.000Z","2018-03-09T05:00:00.000Z","2018-03-12T04:00:00.000Z","2018-03-13T04:00:00.000Z","2018-03-14T04:00:00.000Z","2018-03-15T04:00:00.000Z","2018-03-16T04:00:00.000Z","2018-03-19T04:00:00.000Z","2018-03-20T04:00:00.000Z","2018-03-21T04:00:00.000Z","2018-03-22T04:00:00.000Z","2018-03-23T04:00:00.000Z","2018-03-26T04:00:00.000Z","2018-03-27T04:00:00.000Z","2018-03-28T04:00:00.000Z","2018-03-29T04:00:00.000Z","2018-04-02T04:00:00.000Z","2018-04-03T04:00:00.000Z","2018-04-04T04:00:00.000Z","2018-04-05T04:00:00.000Z","2018-04-06T04:00:00.000Z","2018-04-09T04:00:00.000Z","2018-04-10T04:00:00.000Z","2018-04-11T04:00:00.000Z","2018-04-12T04:00:00.000Z","2018-04-13T04:00:00.000Z","2018-04-16T04:00:00.000Z","2018-04-17T04:00:00.000Z","2018-04-18T04:00:00.000Z","2018-04-19T04:00:00.000Z","2018-04-20T04:00:00.000Z","2018-04-23T04:00:00.000Z","2018-04-24T04:00:00.000Z","2018-04-25T04:00:00.000Z","2018-04-26T04:00:00.000Z","2018-04-27T04:00:00.000Z","2018-04-30T04:00:00.000Z","2018-05-01T04:00:00.000Z","2018-05-02T04:00:00.000Z","2018-05-03T04:00:00.000Z","2018-05-04T04:00:00.000Z","2018-05-07T04:00:00.000Z","2018-05-08T04:00:00.000Z","2018-05-09T04:00:00.000Z","2018-05-10T04:00:00.000Z","2018-05-11T04:00:00.000Z","2018-05-14T04:00:00.000Z","2018-05-15T04:00:00.000Z","2018-05-16T04:00:00.000Z","2018-05-17T04:00:00.000Z","2018-05-18T04:00:00.000Z","2018-05-21T04:00:00.000Z","2018-05-22T04:00:00.000Z","2018-05-23T04:00:00.000Z","2018-05-24T04:00:00.000Z","2018-05-25T04:00:00.000Z","2018-05-29T04:00:00.000Z","2018-05-30T04:00:00.000Z","2018-05-31T04:00:00.000Z","2018-06-01T04:00:00.000Z","2018-06-04T04:00:00.000Z","2018-06-05T04:00:00.000Z","2018-06-06T04:00:00.000Z","2018-06-07T04:00:00.000Z","2018-06-08T04:00:00.000Z","2018-06-11T04:00:00.000Z","2018-06-12T04:00:00.000Z","2018-06-13T04:00:00.000Z","2018-06-14T04:00:00.000Z","2018-06-15T04:00:00.000Z","2018-06-18T04:00:00.000Z","2018-06-19T04:00:00.000Z","2018-06-20T04:00:00.000Z","2018-06-21T04:00:00.000Z","2018-06-22T04:00:00.000Z","2018-06-25T04:00:00.000Z","2018-06-26T04:00:00.000Z","2018-06-27T04:00:00.000Z","2018-06-28T04:00:00.000Z","2018-06-29T04:00:00.000Z","2018-07-02T04:00:00.000Z","2018-07-03T04:00:00.000Z","2018-07-05T04:00:00.000Z","2018-07-06T04:00:00.000Z","2018-07-09T04:00:00.000Z","2018-07-10T04:00:00.000Z","2018-07-11T04:00:00.000Z","2018-07-12T04:00:00.000Z","2018-07-13T04:00:00.000Z","2018-07-16T04:00:00.000Z","2018-07-17T04:00:00.000Z","2018-07-18T04:00:00.000Z","2018-07-19T04:00:00.000Z","2018-07-20T04:00:00.000Z","2018-07-23T04:00:00.000Z","2018-07-24T04:00:00.000Z","2018-07-25T04:00:00.000Z","2018-07-26T04:00:00.000Z","2018-07-27T04:00:00.000Z","2018-07-30T04:00:00.000Z","2018-07-31T04:00:00.000Z","2018-08-01T04:00:00.000Z","2018-08-02T04:00:00.000Z","2018-08-03T04:00:00.000Z","2018-08-06T04:00:00.000Z","2018-08-07T04:00:00.000Z","2018-08-08T04:00:00.000Z","2018-08-09T04:00:00.000Z","2018-08-10T04:00:00.000Z","2018-08-13T04:00:00.000Z","2018-08-14T04:00:00.000Z","2018-08-15T04:00:00.000Z","2018-08-16T04:00:00.000Z","2018-08-17T04:00:00.000Z","2018-08-20T04:00:00.000Z","2018-08-21T04:00:00.000Z","2018-08-22T04:00:00.000Z","2018-08-23T04:00:00.000Z","2018-08-24T04:00:00.000Z","2018-08-27T04:00:00.000Z","2018-08-28T04:00:00.000Z","2018-08-29T04:00:00.000Z","2018-08-30T04:00:00.000Z","2018-08-31T04:00:00.000Z","2018-09-04T04:00:00.000Z","2018-09-05T04:00:00.000Z","2018-09-06T04:00:00.000Z","2018-09-07T04:00:00.000Z","2018-09-10T04:00:00.000Z","2018-09-11T04:00:00.000Z","2018-09-12T04:00:00.000Z","2018-09-13T04:00:00.000Z","2018-09-14T04:00:00.000Z","2018-09-17T04:00:00.000Z","2018-09-18T04:00:00.000Z","2018-09-19T04:00:00.000Z","2018-09-20T04:00:00.000Z","2018-09-21T04:00:00.000Z","2018-09-24T04:00:00.000Z","2018-09-25T04:00:00.000Z","2018-09-26T04:00:00.000Z","2018-09-27T04:00:00.000Z","2018-09-28T04:00:00.000Z","2018-10-01T04:00:00.000Z","2018-10-02T04:00:00.000Z","2018-10-03T04:00:00.000Z","2018-10-04T04:00:00.000Z","2018-10-05T04:00:00.000Z","2018-10-08T04:00:00.000Z","2018-10-09T04:00:00.000Z","2018-10-10T04:00:00.000Z","2018-10-11T04:00:00.000Z","2018-10-12T04:00:00.000Z","2018-10-15T04:00:00.000Z","2018-10-16T04:00:00.000Z","2018-10-17T04:00:00.000Z","2018-10-18T04:00:00.000Z","2018-10-19T04:00:00.000Z","2018-10-22T04:00:00.000Z","2018-10-23T04:00:00.000Z","2018-10-24T04:00:00.000Z","2018-10-25T04:00:00.000Z","2018-10-26T04:00:00.000Z","2018-10-29T04:00:00.000Z","2018-10-30T04:00:00.000Z","2018-10-31T04:00:00.000Z","2018-11-01T04:00:00.000Z","2018-11-02T04:00:00.000Z","2018-11-05T05:00:00.000Z","2018-11-06T05:00:00.000Z","2018-11-07T05:00:00.000Z","2018-11-08T05:00:00.000Z","2018-11-09T05:00:00.000Z","2018-11-12T05:00:00.000Z","2018-11-13T05:00:00.000Z","2018-11-14T05:00:00.000Z","2018-11-15T05:00:00.000Z","2018-11-16T05:00:00.000Z","2018-11-19T05:00:00.000Z","2018-11-20T05:00:00.000Z","2018-11-21T05:00:00.000Z","2018-11-23T05:00:00.000Z","2018-11-26T05:00:00.000Z","2018-11-27T05:00:00.000Z","2018-11-28T05:00:00.000Z","2018-11-29T05:00:00.000Z"],[56.200001,59.369999,57.939999,58.970001,59.16,58.48,58.599998,58.810001,59.650002,59.759998,59.93,60.060001,60.650002,60.630001,57.060001,57.459999,58.029999,57.400002,58.759998,59.16,60.490002,60.5,60.73,61.029999,60.68,60.110001,58.549999,58.400002,57.93,57.77,56.66,56.57,56.75,58.279999,58.43,58.59,61.400002,60.849998,60.82,60.610001,61.57,61.419998,60.740002,60.950001,61.610001,61.150002,63.169998,60.759998,61.549999,62.259998,62.599998,63.009998,63.02,62.139999,62.310001,60.630001,60.84,60.259998,61.5,62.139999,62.130001,62.57,61.27,62,62.09,63.68,64.309998,63.630001,63.09,62.040001,63.529999,66,65.660004,64.839996,65.089996,64.300003,65.540001,65.540001,66.480003,65.910004,65.029999,65.32,65,64.339996,64.809998,64.779999,63.740002,63.619999,62.529999,62,63.049999,63.490002,64.419998,63.610001,63.07,62.650002,62.73,61.5,61.150002,61.27,61.450001,62.380001,61.119999,61.529999,61.720001,60.950001,62.48,62.700001,61.939999,62.279999,62.27,62.360001,64.330002,65.339996,65.690002,65.410004,66.870003,63.41,61.09,62.5,63.490002,62.869999,62.720001,63.369999,63.43,63.130001,64,63.93,64.489998,64.910004,65.18,66.18,66.019997,65.610001,62.549999,61.959999,62.389999,57.639999,59.48,60.799999,62.259998,62.139999,64.110001,64.849998,64.699997,65.470001,67.080002,66.099998,65.309998,64.510002,64.860001,64.239998,63.669998,63.34,63.59,63.540001,64.550003,65.75,64.879997,66.489998,66.75,65.93,68.290001,69.699997,70.919998,69.970001,70.110001,70.220001,70.449997,71.32,71.660004,70.599998,71.339996,72.5,71.440002,72.959999,73.480003,72.660004,70.540001,66,69.620003,69.629997,66.989998,67.160004,67.959999,67.209999,70,71.959999,69.970001,70.900002,70.059998,69.949997,72.68,73.050003,73.150002,73.160004,71.220001,73.660004,75.400002,73.379997,74.510002,76.860001,79.239998,77.489998,76.349998,72.989998,73.870003,71,71.339996,71.199997,70.629997,69.860001,69.43,70.669998,65.93,66.339996,66.800003,65.389999,63.959999,67.199997,65.849998,65.160004,66.959999,65.800003,66.25,67.459999,66.529999,67.25,68.75,68.25,67.440002,66.599998,66.620003,65.379997,65.260002,66.690002,65.860001,66.150002,67.940002,68.489998,66.300003,70.269997,69.660004,69.980003,71.779999,72.769997,71.870003,70.980003,71.129997,70.300003,71.519997,72.5,71.949997,70.730003,71.75,70.660004,70.800003,70.919998,70.830002,71.419998,73.25,72.790001,73.260002,74.790001,73.449997,74.25,75.230003,76.040001,76.830002,77.660004,77.019997,76.639999,77.25,77.169998,76.459999,75.190002,75.32,75.769997,74,76.440002,75.360001,77.68,76.510002,76.199997,77.699997,77,75.349998,79.290001,81.400002,81.610001,79.160004,80.75,81.019997,80.940002,79.5,80.099998,78.599998,79.389999,77.080002,75.370003,72.910004,73.769997,73.650002,74.5,71.160004,70.57,70.07,70.019997,70.099998,70.610001,71.050003,70.389999,69.68,69.510002,69.209999,69.269997,69.580002,71.559998,71.709999,74.870003,74.25,73.940002,74.400002,71.519997,72.129997,72.93,71.629997,71.830002,73.879997,74.879997,79.699997,80.589996,80.5,81.489998,78.82,79.93,79.910004,81.330002,79.699997,80.919998,81.110001,81.940002,82.760002,84.18,83.019997,83.82,82.5,80.25,79.019997,76.879997,77.470001,73.800003,78.400002,75.540001,76.480003,77.349998,73.970001,72.199997,70.099998,67.790001,69.129997,68.019997,68.25,70.339996,65.690002,68.669998,69.480003,69.949997,68,64.32,65.260002,65.370003,55,55,54.200001,53.189999,51.41,53.060001,50.810001,47.77,50.040001,50.459999,50.689999,50.610001,51.099998,50.639999],[57.119999,57.799999,58.509998,59.220001,58.279999,58.580002,58.580002,59.560001,59.650002,59.799999,59.84,60.529999,60.549999,58.029999,56.759998,57.900002,58.450001,58.91,58.799999,60.32,60.150002,60.68,61.16,60.439999,60.220001,58.290001,58.810001,57.580002,57.57,56.259998,57.16,56.619999,58.09,58.509998,57.98,61.02,60.5,60.75,60.5,61.330002,61.330002,61.080002,60.93,61.439999,60.950001,62.639999,61.169998,61.450001,61.779999,62.400002,61.450001,63.970001,62.009998,62.509998,61.330002,61.380001,60.209999,60.740002,61.889999,61.970001,62.889999,61.369999,62.009998,61.779999,64.260002,64.010002,63.43,62.580002,62.75,63.48,65.68,65.559998,65.120003,65.050003,64,65.830002,64.93,66.160004,66.089996,65.400002,65.169998,65,64.199997,64.309998,65,64.110001,63.900002,62.139999,61.759998,63.09,63.540001,64.510002,63.389999,63.450001,62.630001,63.09,63.25,61.18,61.450001,61.360001,62.049999,61.080002,61.48,61.66,61.5,61.990002,62.630001,61.459999,62.73,61.810001,62.060001,64.139999,65.300003,65.489998,65.330002,65.449997,63.200001,60.869999,60.959999,64.550003,63.310001,63,63.619999,64.029999,62.889999,64.129997,64.099998,63.93,64.5,65.139999,66.029999,65.739998,65.970001,62.009998,62.400002,61.959999,58.599998,59.549999,60.75,61.57,62.43,63.169998,64.400002,64.599998,65.029999,66.730003,66.010002,64.900002,64.379997,65.080002,64.510002,63.450001,63.34,63.400002,63.32,64.309998,65.309998,64.660004,66.370003,66.629997,66.190002,66.830002,69.239998,70.360001,68.709999,70.360001,69.800003,70.57,71.300003,71.550003,70.080002,70.82,72.519997,72.449997,71.610001,74.129997,73.349998,71.419998,67.980003,69.699997,69.459999,65.830002,67.080002,68.32,68.029999,69.650002,71.989998,70.190002,70.839996,70.220001,69.489998,72.07,73.160004,72.610001,73.129997,72.010002,74.010002,75.099998,73.919998,74.07,76.809998,78.220001,76.860001,75.93,74.220001,73.379997,71.68,71.050003,71.370003,71.610001,69.760002,68.040001,70.080002,66.290001,65.970001,67.459999,65.089996,65.480003,66.269997,66.529999,64.559998,65.769997,66.519997,65.849998,67.220001,65.879997,66.559998,68.279999,68.690002,67.68,66.300003,66.230003,65.339996,64.82,66.339996,65.790001,66.349998,67.800003,68.389999,66.82,69.839996,70.370003,69.610001,71.309998,72.900002,71.699997,71.699997,70.169998,70.330002,71.660004,71.989998,71.449997,71.349998,71.739998,70.800003,71.459999,70.639999,70.690002,70.910004,73.029999,72.790001,72.769997,74.790001,73.949997,74.290001,75,75.940002,76.669998,77.709999,77.410004,77.870003,77.239998,76.839996,76.050003,75.980003,74.699997,75.269997,73.419998,76.010002,76.32,77.279999,75.879997,76.190002,77.190002,76.839996,76.190002,78.610001,81.370003,81.5,80.230003,80.949997,81.269997,80.610001,79.650002,79.75,78.410004,79.449997,77.650002,75.360001,72.75,73.419998,73.080002,74.059998,71.32,70.57,70.230003,70.010002,70.5,70.610001,70.519997,71.290001,69.309998,69.690002,69.150002,68.93,69.660004,71.43,71.169998,74.089996,74.089996,74,74.910004,74.099998,72.099998,72.980003,71.650002,71.959999,73.57,74.260002,79.5,80.519997,80.43,81.269997,78.879997,79.629997,79.580002,80.650002,80.290001,80.599998,80.529999,81.57,82.870003,83.190002,83.279999,83.389999,83.019997,80.360001,79.589996,77.629997,77.639999,73.580002,73.849998,77.919998,75.669998,78.730003,78.300003,71.809998,69.75,69.779999,68.980003,67.25,70.32,68.839996,65.220001,67.480003,69.050003,69.32,68.989998,64.339996,64.25,65.089996,62.790001,55.009998,53.889999,52.549999,51.599998,53.560001,50.939999,49.139999,49.259998,50.849998,50.049999,50.91,50.75,51.02,52.560001]],"fixedtz":false,"tzone":""},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->
The peak closing price of 83.389999 USDand the lowest recorded closing price of 49.139999.

Visualizing the daily stock returns.

``` r
ATVI_return <- dailyReturn(ATVI)
ATVI_return <- as.data.frame(ATVI_return)
ATVI_return <- cbind(Date=as.Date(rownames(ATVI_return)),ATVI_return)
ggplot(ATVI_return, aes(x = Date, y = daily.returns)) + geom_path() + geom_hline(yintercept = 0) + ggtitle("Daily Returns of ATVI") + ylab("Daily Returns in USD")
```

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
mean_return <- mean(ATVI_return[,2])
```

The average daily return is 0.00004917699.

Plotting the stock using a candlestick chart. The chart represents days where the opening price was greater than the closing price with a black candlestick and red candlesticks where the opening price is lower than the closing price.

``` r
candleChart(ATVI, up.col = "black", dn.col = "red", theme = "white")
```

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
addSMA(n = c(20, 50, 100))
```

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-8-2.png) Looking at the 20-day moving average (dark blue) the stock needs to be above or below for the slope of the line to change directions. This can possibly be used to identify a change in trend and can be seen as trading signals. The 50-day moving average (light blue) indicates that the stock switches between a bearish and bullish trend but overall was rising. The 100-day moving average (pink) indicated that the stock in the long run was increasing but near November the stock starts to trend downwards.

Decomposing the data
====================

Decomposing the ATVI time series using both a additive and multiplicative model.

``` r
ATVI_add <- decompose(ATVI_ts[,"ATVI.Close"], type = "additive")
ATVI_mul <- decompose(ATVI_ts[,"ATVI.Close"], type = "multiplicative")

plot(ATVI_add, col = "red")
```

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
plot(ATVI_mul, col = "blue")
```

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-9-2.png) The additive and multiplicative models are very similary with slight differences in the seasonal and random elements.

Stationarity
============

Checking for stationarity is important as it is an assumption for many underlying tools for time series analysis.

``` r
adf.test(ATVI$ATVI.Close, k = 11)
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  ATVI$ATVI.Close
    ## Dickey-Fuller = -1.2102, Lag order = 11, p-value = 0.9041
    ## alternative hypothesis: stationary

``` r
kpss.test(ATVI$ATVI.Close, lshort = FALSE, null = "Trend")
```

    ## 
    ##  KPSS Test for Trend Stationarity
    ## 
    ## data:  ATVI$ATVI.Close
    ## KPSS Trend = 0.19081, Truncation lag parameter = 14, p-value =
    ## 0.01945

A stationary process has mean and variance that do not change with time and the process does not have trend. The Augmented Dickey-Fuller Test tests the null hypothesis that a unit root is present in the time series and the alternative hypothesis is that the time series is stationary or trend-stationary.
The Kwiatkowski–Phillips–Schmidt–Shin (KPSS) test is used for testing the null hypothesis that the time series is stationary and has a deterministic trend; the alternative hypothesis is that there is a unit root.
Both tests point to the time series not being stationary; we fail to reject the null hypothesis for the ADF test and we reject the null hypothesis of the KPSS test, both at 5% level at significance.

Differencing
============

``` r
diff_ATVI_close <- diff(ATVI_ts[,"ATVI.Close"], differences = 1)
suppressWarnings(adf.test(diff_ATVI_close, k = 11))
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  diff_ATVI_close
    ## Dickey-Fuller = -5.1559, Lag order = 11, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
suppressWarnings(kpss.test(diff_ATVI_close, lshort = FALSE, null = "Trend"))
```

    ## 
    ##  KPSS Test for Trend Stationarity
    ## 
    ## data:  diff_ATVI_close
    ## KPSS Trend = 0.087797, Truncation lag parameter = 14, p-value =
    ## 0.1

Differencing once does not allow us to reject the null of either the ADF or KPSS test, but the nulls of these tests are contradictory as a time series with a unit root cannot be stationary. Therefore, lets difference once more.

``` r
diff_ATVI_close2 <- diff(ATVI_ts[,"ATVI.Close"], differences = 2)

diff_adf <- (adf.test(diff_ATVI_close2, k = 11))
```

    ## Warning in adf.test(diff_ATVI_close2, k = 11): p-value smaller than printed
    ## p-value

``` r
diff_adf
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  diff_ATVI_close2
    ## Dickey-Fuller = -9.663, Lag order = 11, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
(kpss.test(diff_ATVI_close2, lshort = FALSE, null = "Trend"))
```

    ## Warning in kpss.test(diff_ATVI_close2, lshort = FALSE, null = "Trend"): p-
    ## value greater than printed p-value

    ## 
    ##  KPSS Test for Trend Stationarity
    ## 
    ## data:  diff_ATVI_close2
    ## KPSS Trend = 0.020954, Truncation lag parameter = 13, p-value =
    ## 0.1

We have chosen the significance level *a**l**p**h**a* = 5% or 0.05. Here we reject the the null hypothesis for the ADF test since the p-value &lt; 0.01 which is less than 0.05. In addition we can look at the Dicker-Fuller statistic of -9.6629905 and we compare this to the critical values for Dickey–Fuller t-distribution table\[^1\], we observe that the ADF test statistic is less than -3.43 and the null hypothesis of a unit root is rejected. We accept the null hypothesis of the KPSS test since the p-value is larger than 0.1. Therefore we can say the timeseries is stationary when differenced twice.

``` r
autoplot(diff_ATVI_close2, ts.colour = "blue", main = "Closing Prices Differenced Twice")
```

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-13-1.png)

Simple Exponential Smoothing (SES)
==================================

Simple exponential smoothing utilizes weights to forecast future values by assigning weights to observations. The more recent the observation the higher the weight.

Creating a training and data subset of the timeseries to see how well the SES model is at forecasting.

``` r
start <- floor(0.8*(nrow(ATVI)))
test_ahead <- (nrow(ATVI) - start)
ATVI_window <- window(ts(ATVI_ts[,"ATVI.Close"]), end = floor(0.8*(nrow(ATVI))))

ATVI_SES <- HoltWinters(ATVI_window, beta = FALSE, gamma = FALSE)
ATVI_SES
```

    ## Holt-Winters exponential smoothing without trend and without seasonal component.
    ## 
    ## Call:
    ## HoltWinters(x = ATVI_window, beta = FALSE, gamma = FALSE)
    ## 
    ## Smoothing parameters:
    ##  alpha: 0.885945
    ##  beta : FALSE
    ##  gamma: FALSE
    ## 
    ## Coefficients:
    ##       [,1]
    ## a 70.44766

``` r
ATVI_forecast <- forecast(ATVI_SES, h = test_ahead)
#ATVI_forecast

plot(ATVI_forecast)
lines(ts(ATVI_ts[,"ATVI.Close"]))
```

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
acc_table <- rbind(accuracy(ATVI_forecast$fitted, ATVI_ts[,"ATVI.Close"][191:255]), acc_table)
```

Forecasting values in the future using Simple Exponential Smoothing.

``` r
ATVI_SES1 <- HoltWinters(ATVI_ts[,"ATVI.Close"], beta = FALSE, gamma = FALSE)
ATVI_SES1
```

    ## Holt-Winters exponential smoothing without trend and without seasonal component.
    ## 
    ## Call:
    ## HoltWinters(x = ATVI_ts[, "ATVI.Close"], beta = FALSE, gamma = FALSE)
    ## 
    ## Smoothing parameters:
    ##  alpha: 0.9678723
    ##  beta : FALSE
    ##  gamma: FALSE
    ## 
    ## Coefficients:
    ##       [,1]
    ## a 52.51025

``` r
plot(ATVI_SES1)
```

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-15-1.png)

``` r
ATVI_forecast1 <- forecast(ATVI_SES1, h = forecast_len)
#ATVI_forecast
plot(ATVI_forecast1)
```

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-15-2.png)

Looking at the residuals on the forecast.

``` r
ATVI_fc1_acf <- Acf(ATVI_forecast$residuals, lag.max=20)
```

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-16-1.png)

``` r
ATVI_boxtest1 <- Box.test(ATVI_forecast$residuals, lag=20, type="Ljung-Box")
ATVI_boxtest1 
```

    ## 
    ##  Box-Ljung test
    ## 
    ## data:  ATVI_forecast$residuals
    ## X-squared = 16.157, df = 20, p-value = 0.7068

``` r
plot.ts(ATVI_forecast$residuals, main = "SES Forecast Residuals")
```

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-16-2.png) From the autocorrelcation function plot only 1 out of the 20 lags are out of significance bounds. The p-value from the BoxLjung test is 0.7068, this indicates that there is little evidence of non-zero autocorrelations in the in-sample forecast errors at lags 1-20.

Holt-Winters Exponential Smoothing (HWES)
=========================================

Holt-Winters Exponential smoothing is an exponentially weighted moving average filter of the level, trend, and seasonal components of a time series. The parameters used for smoothing are chosen by minimizing the sum of the squared one-step-ahead-prediction errors.

Creating a training and data subset of the timeseries to see how well the Holt-Winters Exponential Smoothing model is at forecasting.

``` r
suppressWarnings(ATVI_hw <- HoltWinters(ATVI_window, gamma = FALSE))
ATVI_hw
```

    ## Holt-Winters exponential smoothing with trend and without seasonal component.
    ## 
    ## Call:
    ## HoltWinters(x = ATVI_window, gamma = FALSE)
    ## 
    ## Smoothing parameters:
    ##  alpha: 0.8965743
    ##  beta : 0.03041517
    ##  gamma: FALSE
    ## 
    ## Coefficients:
    ##         [,1]
    ## a 70.4360972
    ## b -0.1229162

``` r
ATVI_forecast2 <- forecast(ATVI_hw, h = test_ahead)
plot(ATVI_forecast2)
lines(ts(ATVI_ts[,"ATVI.Close"]))
```

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-17-1.png)

``` r
acc_table <- rbind(accuracy(ATVI_forecast2$fitted, ATVI_ts[,"ATVI.Close"][191:255]), acc_table)
```

``` r
ATVI_hw2 <- HoltWinters(ATVI_ts[,"ATVI.Close"], gamma = FALSE)
ATVI_hw2
```

    ## Holt-Winters exponential smoothing with trend and without seasonal component.
    ## 
    ## Call:
    ## HoltWinters(x = ATVI_ts[, "ATVI.Close"], gamma = FALSE)
    ## 
    ## Smoothing parameters:
    ##  alpha: 0.9590403
    ##  beta : 0.02620724
    ##  gamma: FALSE
    ## 
    ## Coefficients:
    ##         [,1]
    ## a 52.4781388
    ## b -0.3784137

``` r
plot(ATVI_hw2)
```

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-18-1.png)

``` r
ATVI_forecast2 <- forecast(ATVI_hw2, h =  forecast_len)
#ATVI_forecast2
plot(ATVI_forecast2)
```

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-18-2.png) The estimated values of alpha and beta are 0.959 and 0.0262. The value of alpha is high indicating that the estimate of the level at the time T is based upon more recent observations rather than observations further in the past. The value of beta is rather small and points towards the older values being weighted more heavily than the recent ones when determining the slope. There is no gamma component due to the lack of seasonality within the data.

``` r
ATVI_fc2_acf <- Acf(ATVI_forecast2$residuals, lag.max=20)
```

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-19-1.png)

``` r
ATVI_boxtest2 <- Box.test(ATVI_forecast2$residuals, lag=20, type="Ljung-Box")
ATVI_boxtest2
```

    ## 
    ##  Box-Ljung test
    ## 
    ## data:  ATVI_forecast2$residuals
    ## X-squared = 24.979, df = 20, p-value = 0.2022

``` r
plot.ts(ATVI_forecast2$residuals, main = "HoltWinters Forecast Residuals")
```

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-19-2.png) The autocorrelation plot has two and possibly three lags that are out of the significance bounds. The p-value from the Box-Ljung test is 0.2022, this indicates that there is little evidence of non-zero autocorrelations in the in-sample forecast errors at lags 1-20.

Autoregressive integrated moving average (ARIMA) model
======================================================

We must now see if the model is an AR or MA process. This can be done using by looking at the plots of acf() and pacf() .

``` r
close_acf <- Acf(diff_ATVI_close2, lag = 20)
```

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-20-1.png)

``` r
close_acf
```

    ## 
    ## Autocorrelations of series 'diff_ATVI_close2', by lag
    ## 
    ##      0      1      2      3      4      5      6      7      8      9 
    ##  1.000 -0.486 -0.103  0.147 -0.020 -0.110  0.114  0.013 -0.148  0.111 
    ##     10     11     12     13     14     15     16     17     18     19 
    ##  0.021 -0.081  0.068  0.007 -0.057  0.020  0.035 -0.027 -0.004  0.032 
    ##     20 
    ## -0.069

``` r
close_pacf <- Pacf(diff_ATVI_close2, lag = 20)
```

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-20-2.png)

``` r
close_pacf
```

    ## 
    ## Partial autocorrelations of series 'diff_ATVI_close2', by lag
    ## 
    ##      1      2      3      4      5      6      7      8      9     10 
    ## -0.486 -0.444 -0.230 -0.131 -0.217 -0.115 -0.026 -0.169 -0.121 -0.081 
    ##     11     12     13     14     15     16     17     18     19     20 
    ## -0.107 -0.043 -0.025 -0.035 -0.043 -0.032  0.000  0.005  0.031 -0.040

Looking at the ACF and PACF gives us p = 5,6,8 and q = 3.

Using the auto.arima() function with d=2 for the number of times the data needed to be differenced to obtain stationarity.

``` r
auto.arima(ATVI_ts[,4], d=2, seasonal = FALSE)
```

    ## Series: ATVI_ts[, 4] 
    ## ARIMA(0,2,2) 
    ## 
    ## Coefficients:
    ##           ma1     ma2
    ##       -1.0267  0.0372
    ## s.e.   0.0538  0.0543
    ## 
    ## sigma^2 estimated as 2.044:  log likelihood=-683.11
    ## AIC=1372.22   AICc=1372.28   BIC=1384.07

Testing the accuracy of the arima(3,2,1)

``` r
fit_no_holdout = arima(ATVI_window, order=c(3,2,1))

fcast_no_holdout <- forecast(fit_no_holdout, h = test_ahead)
plot(fcast_no_holdout, main=" ")
lines(ts(ATVI_ts[,"ATVI.Close"]))
```

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-22-1.png)

``` r
acc_table <- rbind(accuracy(fcast_no_holdout$fitted, ATVI_ts[,"ATVI.Close"][191:255]), acc_table)
```

Fitting an arima(3,2,1) to forecast.

``` r
fit <- arima(ATVI_ts[,4], order = c(3,2,1))
fit
```

    ## 
    ## Call:
    ## arima(x = ATVI_ts[, 4], order = c(3, 2, 1))
    ## 
    ## Coefficients:
    ##           ar1      ar2     ar3      ma1
    ##       -0.0256  -0.0566  0.1207  -0.9916
    ## s.e.   0.0517   0.0515  0.0515   0.0113
    ## 
    ## sigma^2 estimated as 1.996:  log likelihood = -679.61,  aic = 1369.23

``` r
tsdisplay(residuals(fit), lag.max=20, main = "Arima(3,2,1) Model Residuals")
```

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-23-1.png)

``` r
ATVI_fcast <- forecast(fit, h= forecast_len)
plot(ATVI_fcast, main = "Forecasts from ARIMA(3,2,1)")
```

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-23-2.png)

Testing the ARIMA model using p and q from our visual analysis of the Acf and Pacf.

    ## 
    ## Call:
    ## arima(x = ATVI_ts[, 4], order = c(5, 2, 3))
    ## 
    ## Coefficients:
    ##           ar1      ar2     ar3     ar4     ar5      ma1      ma2      ma3
    ##       -0.8767  -0.8201  0.0463  0.0575  0.0138  -0.1313  -0.0886  -0.7553
    ## s.e.   0.1919   0.1739  0.0848  0.0852  0.0681   0.1843   0.1761   0.1566
    ## 
    ## sigma^2 estimated as 1.961:  log likelihood = -676.23,  aic = 1370.46

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-24-1.png)![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-24-2.png)

``` r
ggplot(data.frame(residuals = residuals(fit_test)), aes(x =residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()
```

    ## Don't know how to automatically pick scale for object of type ts. Defaulting to continuous.

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-25-1.png)

``` r
qqnorm(residuals(fit_test))
qqline(residuals(fit_test))
```

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-25-2.png) The residuals of the arima(3,2,1) model appear to be slightly left skewed but normally distributed.The Q-Q plot supports that most of the residuals are normally distributed however the points at the ends of the range.

    ## 
    ## Call:
    ## arima(x = ATVI_ts[, 4], order = c(6, 2, 3))
    ## 
    ## Coefficients:
    ##           ar1      ar2     ar3     ar4     ar5     ar6      ma1      ma2
    ##       -0.8164  -0.8072  0.0499  0.0627  0.0266  0.0239  -0.1918  -0.0414
    ## s.e.   0.2956   0.1943  0.0835  0.0850  0.0784  0.0756   0.2913   0.2218
    ##           ma3
    ##       -0.7446
    ## s.e.   0.1771
    ## 
    ## sigma^2 estimated as 1.96:  log likelihood = -676.18,  aic = 1372.36

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-26-1.png)![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-26-2.png)

    ## 
    ## Call:
    ## arima(x = ATVI_ts[, 4], order = c(8, 2, 3))
    ## 
    ## Coefficients:
    ##           ar1      ar2     ar3     ar4     ar5     ar6      ar7      ar8
    ##       -0.5579  -0.4295  0.0655  0.0512  0.0005  0.0271  -0.0061  -0.0842
    ## s.e.   0.4799   0.4475  0.0712  0.0765  0.0843  0.0682   0.0720   0.0768
    ##           ma1      ma2      ma3
    ##       -0.4505  -0.1581  -0.3713
    ## s.e.   0.4812   0.3234   0.4393
    ## 
    ## sigma^2 estimated as 1.954:  log likelihood = -675.57,  aic = 1375.15

![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-27-1.png)![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-27-2.png)
<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Arima\_Model
</th>
<th style="text-align:right;">
AIC
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
(3,2,1)
</td>
<td style="text-align:right;">
1369.226
</td>
</tr>
<tr>
<td style="text-align:left;">
(5,2,3)
</td>
<td style="text-align:right;">
1370.461
</td>
</tr>
<tr>
<td style="text-align:left;">
(6,2,3)
</td>
<td style="text-align:right;">
1372.360
</td>
</tr>
<tr>
<td style="text-align:left;">
(8,2,3)
</td>
<td style="text-align:right;">
1375.150
</td>
</tr>
</tbody>
</table>
The Akaike information criterion (AIC) is an estimator of the relative quality of the model for the given data. From the table, the arima(3,2,1) model performed the best in reduction of information loss; however there is no substantial difference between the models as the AIC's are not relatively different from each other.

Conclusion
==========

Our dataset contains 384 entries of Activision Blizzard Inc. (ATVI) daily stock data. We have set the frequency = 5 to match a Monday to Friday week.
During the analysis of the plots for ATVI certain points of interest were observered in the Open/Close and Volume plots.
In both the following plots the colored points represent points of interest. \* Orange + October 2nd, 2018: the highest closing price of Activision Blizzard stock at 83.39. \* Green + November 1st, 2018: the closing price of the stock prior to BlizzCon and the announcement of Diablo Immortals. \* Red + November 9th: the day with the highest volume traded. ![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-29-1.png) The volume traded on 2018-11-09(November 9th, 2018) was more than six times the volume traded one week prior.
![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-30-1.png) On October 2nd 2018 (orange point in the above plot), the highest ATVI stock was at an all time high closing price of 83.39 USD; however in the following days the stock would drop to 73.58 USD on October 10th before starting to rise again. On October 12th, Activision launched *Call of Duty: Black Ops 4*; the highly anticpated sequel to *Call of Duty: Black Ops 3*. Less than a week later on October 18th, Activsion announced that within the first three days of release that they have sold more than $500 million worldwide and that the release set records for digital sales across all platforms\[^2\]. However, investors were disappointed as this only matched the sales of *Black Ops 3* which implies flat sales growth over the year. Investors expected the release of *Black Ops 4* to capitalize on the popularity of "battle royale"" type games and result in bigger sales.

Exactly one week prior to November 9th (red point), Blizzard at their annual gaming convention, BlizzCon, was expected to announce a new release; many were hopeful for the next installment in their Diablo series, *Diablo 4*, but were instead disappointed with a release of a mobile game titled *Diablo: Immortal*. On the day of their announcement, Novemember 2nd, the stock price opened at 69.949997 and closed at 68.989998 very small decrease. However on the following Monday, November 5th, the stock price ended at 64.339996, this is a 5.38% decrease from the opening price of 68. Since the BlizzCon on November 2nd, as of November 29th the stock has dropped 24.9%.

Forecasting
-----------

When training our model we took 80% of the dataset and used the remaining 20% to test the accuracy of the forecasts.
<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
ME
</th>
<th style="text-align:right;">
RMSE
</th>
<th style="text-align:right;">
MAE
</th>
<th style="text-align:right;">
MPE
</th>
<th style="text-align:right;">
MAPE
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Test set
</td>
<td style="text-align:right;">
9.714185
</td>
<td style="text-align:right;">
10.40043
</td>
<td style="text-align:right;">
9.714185
</td>
<td style="text-align:right;">
13.71713
</td>
<td style="text-align:right;">
13.71713
</td>
</tr>
<tr>
<td style="text-align:left;">
Test set2
</td>
<td style="text-align:right;">
9.367425
</td>
<td style="text-align:right;">
10.06119
</td>
<td style="text-align:right;">
9.367425
</td>
<td style="text-align:right;">
13.22296
</td>
<td style="text-align:right;">
13.22296
</td>
</tr>
<tr>
<td style="text-align:left;">
Test set1
</td>
<td style="text-align:right;">
9.834414
</td>
<td style="text-align:right;">
10.54824
</td>
<td style="text-align:right;">
9.834414
</td>
<td style="text-align:right;">
13.87919
</td>
<td style="text-align:right;">
13.87919
</td>
</tr>
</tbody>
</table>
The following measures were used to test the accuracy of the models: mean error (ME), root-mean-square error (RMSE), mean absolute error (MAE), mean percentage error (MPE), and mean absolute percentage error (MAPE). The lower the measure the less error the model had in forecasting the correct values in the test set and thus should be a better model at forecasting future values. From our table, the HoltWinters exponential smoothing model performed the the best in forecasting the test set in every measure.

Looking at the Holtwinters forecast: ![](Activision_Blizzard_Stock_Time_Series_Analysis_files/figure-markdown_github/unnamed-chunk-32-1.png) The model forecasts that the price will continue to drop (blue line) while the lighter grey area (wider area) represents the 80% prediction inteval and the darker grey area represents the 95% prediction interval. This is not unexpected as the trend prior to the forecast was downwards and was a significant drop in stock price compare to years prior.

References
==========

\[^1\] Fuller, W. A. (1976). Introduction to Statistical Time Series. New York: John Wiley and Sons. ISBN 0-471-28715-6.

\[^2\] <https://investor.activision.com/news-releases/news-release-details/call-duty-black-ops-4-delivers-over-half-billion-dollar-opening>

<https://docs.tibco.com/pub/enterprise-runtime-for-R/4.0.0/doc/html/Language_Reference/stats/HoltWinters.html>
