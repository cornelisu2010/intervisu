
date1=as.Date(paste0(2000,"-1-1"))
date2=as.Date(paste0(2010,"-1-1"))
if (interactive()) {
  data=get.hist.quote(instrument= "BMW.DE",start = date1, end = date2 ,
                      quote=c("Open", "High", "Low", "Close",
                              "AdjClose","Volume"), provider = "yahoo",
                      compression = "m",retclass="zoo",quiet = T)
  data=as.data.frame(data)

  Timeseries(data=data)
}
