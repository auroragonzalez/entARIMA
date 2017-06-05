#df = read.table("inputs.csv", sep=",", head=T)
entARIMA = function(df){
  df$date = strptime(df$date,"%Y-%m-%dT%H:%M:%S", tz = "GMT")

  # reverse the order
  df  =df[seq(dim(df)[1],2),]

  fit <- arima(x=df$temperature, order=c(1,0,0))
  forecasts <- predict(fit, n.ahead=12)

  # xlim=c(as.POSIXct(min(df$date), format="%Y-%m-%d %H:%M:%S")-2*60*60,
  #        as.POSIXct(max(df$date), format="%Y-%m-%d %H:%M:%S")+8*60*60)
  #
  # plot(x = df$date, y = df$temperature, xlim = xlim)
  #
  # lines(x =   seq(
  #   from=as.POSIXct(max(df$date), format="%Y-%m-%d %H:%M:%S")+1*60*60,
  #   to=as.POSIXct(max(df$date), format="%Y-%m-%d %H:%M:%S")+12*60*60,
  #   by="hour"
  # )        ,
  #       y=forecasts$pred,col="red")

  return(as.numeric(forecasts$pred))

}




