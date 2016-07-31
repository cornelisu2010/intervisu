
if (interactive()) {
  data=tseries::get.hist.quote(start = as.Date("2000-01-01"),
                               end = as.Date("2005-01-01"),provider = "yahoo",
                               instrument = "^DJI",quiet = T)
  data=as.data.frame(data)
  Timeseries(data=data)
}
