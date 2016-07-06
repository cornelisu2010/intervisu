#' Interactive Seasonal Decomposition of Time Series by Loess example application (Type 1)
#'
#' For this example a working internet connection is needed.
#' The user can type the name of any stock on yahoo finance to be downloaded as time series and then plot a Seasonal Decomposition of Time Series by Loess
#'
#' @param height A two-dimensional numeric value indicating the height of the decomposition and the heigth of the zoom-graph
#' @author Cornelius Fritz <cornelius.fritz@campus.lmu.de>
#' @example demo/demo8.R
#' @details If you brush over any part of the decomposition you see a zoom plot in the sidebar panel of the shaded area.
#' @export
#' @import shiny
#' @importFrom  scatterplot3d scatterplot3d
#' @import car
#' @import mgcv
#' @importFrom DT datatable
#' @import zoo
#' @import splines
#' @importFrom "tseries" "get.hist.quote"
#' @importFrom "grDevices" "heat.colors"
#' @importFrom "graphics" "abline" "barplot" "boxplot" "hist" "legend" "lines" "par" "plot" "plot.new" "plot.window" "points" "polygon" "stripchart" "text"
#' @importFrom "stats" "chisq.test" "density" "fisher.test" "lm" "predict" "quantile" "resid" "stl" "time" "var"
#' @importFrom "utils" "install.packages" "installed.packages"
#' @importFrom hypergea getOddsRatio

Timeseries_Stat1 = function(height=c(800,400)) {

  ui <- fluidPage(
    tags$div( HTML(
      "<h2><center>Zeitreihen</center></h2>")
    ),

    sidebarLayout(
      sidebarPanel(
        textInput("text", label = "What stock should be shown?", value = "^DJI"),
        fluidRow(
          column(6,numericInput("num1", label = "Start", value = 2000)),
          column(6,numericInput("num2", label = "Ende", value = 2010))
          )

      ),
      mainPanel(
        plotOutput("plot1",height = height[1], click = "click1", dblclick = "click2", brush = brushOpts(id = "brush")),
        plotOutput("plot2")
      )
    )
  )



  server <- function(input, output) {

    ts= reactiveValues(
      ts=c(),ts_m=c()
    )

    click= reactiveValues(
      brush=list()
    )

    observeEvent(input$brush, {
      click$brush=list(xmin=input$brush$xmin,xmax=input$brush$xmax,
                       ymin=input$brush$ymin,ymax=input$brush$ymax)
      })

    observeEvent(input$num1, {
      date1=as.Date(paste0(input$num1,"-1-1"))
      date2=as.Date(paste0(input$num2,"-1-1"))
      spy = get.hist.quote(instrument= input$text,start = date1, end = date2 ,quote="Open", provider = "yahoo",compression = "d",retclass="ts",quiet = T)
      spy_m = get.hist.quote(instrument= input$text,start = date1, end = date2 ,quote="Open", provider = "yahoo",compression = "m",retclass="ts",quiet = T)
      spy=ts(as.numeric(spy),start = input$num1,frequency = 365)
      spy_m=ts(as.numeric(spy_m[!is.na(spy_m)]),start = input$num1,frequency = 12)
      ts$ts=spy
      ts$ts_m=spy_m
    })

    observeEvent(input$num2, {
      date1=as.Date(paste0(input$num1,"-1-1"))
      date2=as.Date(paste0(input$num2,"-1-1"))
      spy = get.hist.quote(instrument= input$text,start = date1, end = date2 ,quote="Open", provider = "yahoo",compression = "d",retclass="ts",quiet = T)
      spy_m = get.hist.quote(instrument= input$text,start = date1, end = date2 ,quote="Open", provider = "yahoo",compression = "m",retclass="ts",quiet = T)
      spy=ts(as.numeric(spy),start = input$num1,frequency = 365)
      spy_m=ts(as.numeric(spy_m[!is.na(spy_m)]),start = input$num1,frequency = 12)
      ts$ts=spy
      ts$ts_m=spy_m
    })

    observeEvent(input$text, {
      date1=as.Date(paste0(input$num1,"-1-1"))
      date2=as.Date(paste0(input$num2,"-1-1"))
      spy = get.hist.quote(instrument= input$text,start = date1, end = date2 ,quote="Open", provider = "yahoo",compression = "d",retclass="ts",quiet = T)
      spy_m = get.hist.quote(instrument= input$text,start = date1, end = date2 ,quote="Open", provider = "yahoo",compression = "m",retclass="ts",quiet = T)
      spy=ts(as.numeric(spy),start = input$num1,frequency = 365)
      spy_m=ts(as.numeric(spy_m[!is.na(spy_m)]),start = input$num1,frequency = 12)
      ts$ts=spy
      ts$ts_m=spy_m
    })

    output$plot1 <- renderPlot({

      plot(stl(na.approx(ts$ts),s.window = "periodic"),range.bars = F,set.pars = list(mar =c(2.2,5,1,3), oma = c(0, 0, 0, 0),tck = 0, mfrow = c(4, 1)))

      par(new=T,mar =c(1.5,3.2,0.6,2),oma = c(0, 0, 0, 0))
      plot.window(xlim=c(0.04,0.96),ylim=c(0.04,0.94))
    },height = height[1])


    output$plot2 <- renderPlot({
      if(length(click$brush)>0) {
        ts_w=na.approx(ts$ts)
        stl_w=stl(ts_w,s.window = "periodic")
        stl_w$time.series=cbind(stl_w$time.series,ts_w)
        stl_w$time.series=window_new(beginning = input$num1+position6(xmin = click$brush$xmin, xmax = click$brush$xmax,ending = input$num2, beginning=input$num1)[1],
                   ending = input$num1+position6(xmin = click$brush$xmin, xmax = click$brush$xmax,ending = input$num2, beginning=input$num1)[2],timedata = stl_w$time.series)


        if(position4((click$brush$ymin+click$brush$ymax)/2)==4){
          plot(x=stl_w$time.series[,5], y= stl_w$time.series[,4],type = "l", xlab="time", ylab="data",
               ylim=position5(x = position4((click$brush$ymin+click$brush$ymax)/2),ymin = click$brush$ymin,ymax = click$brush$ymax,ts_w = na.approx(ts$ts)))
        } else if(position4((click$brush$ymin+click$brush$ymax)/2)==3) {
          plot(x=stl_w$time.series[,5], y= stl_w$time.series[,1],type = "l", xlab="time", ylab="seasonal",
               ylim=position5(x = position4((click$brush$ymin+click$brush$ymax)/2),ymin = click$brush$ymin,ymax = click$brush$ymax,ts_w = na.approx(ts$ts)))
        } else if(position4((click$brush$ymin+click$brush$ymax)/2)==2) {
          plot(x=stl_w$time.series[,5], y= stl_w$time.series[,2],type = "l", xlab="time", ylab="trend",
               ylim=position5(x = position4((click$brush$ymin+click$brush$ymax)/2),ymin = click$brush$ymin,ymax = click$brush$ymax,ts_w = na.approx(ts$ts)))
        } else if(position4((click$brush$ymin+click$brush$ymax)/2)==1) {
          plot(x=stl_w$time.series[,5], y= stl_w$time.series[,3],type = "l", xlab="time", ylab="remainder",
               ylim=position5(x = position4((click$brush$ymin+click$brush$ymax)/2),ymin = click$brush$ymin,ymax = click$brush$ymax,ts_w = na.approx(ts$ts)))
          polygon(c(stl_w$time.series[,5][1]-0.0001,stl_w$time.series[,5],stl_w$time.series[,5][length(stl_w$time.series[,5])]+0.0001),
                  c(0,stl_w$time.series[,3],0), col='black')

          }
      }
    })



  }
  shinyApp(ui = ui, server = server)
}

#' Interactive Seasonal Decomposition of Time Series by Loess example application (Type 2)
#'
#' For this example a working internet connection is needed.
#' The user can chose out of a range of stocks and indicators from  on yahoo finance what data from what time (in years) should be downloaded as a time series.
#' Then a Seasonal Decomposition of Time Series by Loess is performed on the given time series.
#'
#' @param height A two-dimensional numeric value indicating the height of the decomposition and the heigth of the zoom-graph
#' @author Cornelius Fritz <cornelius.fritz@campus.lmu.de>
#' @example demo/demo8.R
#' @details If you brush over any part of the decomposition you see a zoom plot in the sidebar panel of the shaded area.
#' @export
#'


Timeseries_Stat2 = function(height=c(800,400)) {

  optindex=list()
  optindex$"Dax"=1
  optindex$"Nasdaq"=2
  optindex$"Dow Jones"=3




  ui <- fluidPage(
    tags$div( HTML(
      "<h2><center>Zeitreihen</center></h2>")
    ),

    sidebarLayout(
      sidebarPanel(
        selectInput("index",label = "What index should be shown?",choices = optindex,selected = 1),
        uiOutput("dynamic"),
        fluidRow(
          column(6,numericInput("num1", label = "Start", value = 2000)),
          column(6,numericInput("num2", label = "Ende", value = 2010))
        ),
        plotOutput("plot2")

      ),
      mainPanel(
        plotOutput("plot1",height = height[1], click = "click1", dblclick = "click2", brush = brushOpts(id = "brush"))
      )
    )
  )



  server <- function(input, output) {

    output$dynamic = renderUI({

      if(!is.na(input$index)){
        optdata=list()

        if(input$index==1) {
          optdata$"Dax"="^GDAXI"
          optdata$"Deutsche Bank"="DBK.DE"
          optdata$"Lufthansa"="LHA.DE"
          optdata$"Thysen Krupp"="TKA.DE"
          optdata$"Infineon"="IFX.DE"
          optdata$"Deutsche Post"="DPW.DE"
          optdata$"Siemens"="SIE.DE"
          optdata$"Fresenius"="FME.DE"
          optdata$"RWE AG"="RWE.DE"
          optdata$"BMW"="BMW.DE"
          a= selectInput(inputId = "text", choices = optdata,label = "What stock should be shown?",selected = "^GDax")

        } else if(input$index==2) {
          optdata$"Nasdaq Composit"="^IXIC"
          optdata$"Centrue Financial Corporation"="CFCB"
          optdata$"E*TRADE Financial Corporation"="ETFC"
          optdata$"First Internet Bancorp"="INBK"
          optdata$" Medivation, Inc. "="MDVN"
          optdata$"Mimecast Limited "="MIME"
          optdata$"Siliconware Precision Industries Co. Ltd. "="SPIL"
          optdata$"GW Pharmaceuticals plc"="GWPH"
          optdata$"IZEA, Inc."="IZEA"
          a= selectInput(inputId = "text", choices = optdata,label = "What stock should be shown?",selected = "^IXIC")

        } else if(input$index==3) {
          optdata$"Dow Jones Industrial"="^DJI"
          optdata$"Exxon Mobil Corporation"="XOM"
          optdata$"Wal-Mart Stores Inc."="WMT"
          optdata$"Verizon Communications Inc."="VZ"
          optdata$"Visa Inc."="V"
          optdata$"United Technologies Corporation"="UTX"
          optdata$"UnitedHealth Group Incorporated"="UNH"
          optdata$"The Travelers Companies, Inc."="TRV"
          optdata$"The Procter & Gamble Company"="PG"
          optdata$"NIKE, Inc."="NKE"
          a= selectInput(inputId = "text", choices = optdata,label = "What stock should be shown?")

        }

        a
      }

    })

    ts= reactiveValues(
      ts=c(),ts_m=c()
    )

    click= reactiveValues(
      brush=list()
    )

    observeEvent(input$brush, {
      click$brush=list(xmin=input$brush$xmin,xmax=input$brush$xmax,
                       ymin=input$brush$ymin,ymax=input$brush$ymax)
    })

    observeEvent(input$num1, {
      if(!is.null(input$text)) {
        date1=as.Date(paste0(input$num1,"-1-1"))
        date2=as.Date(paste0(input$num2,"-1-1"))
        spy = get.hist.quote(instrument= input$text,start = date1, end = date2 ,quote="Open", provider = "yahoo",compression = "d",retclass="ts",quiet = T)
        spy_m = get.hist.quote(instrument= input$text,start = date1, end = date2 ,quote="Open", provider = "yahoo",compression = "m",retclass="ts",quiet = T)
        spy=ts(as.numeric(spy),start = input$num1,frequency = 365)
        spy_m=ts(as.numeric(spy_m[!is.na(spy_m)]),start = input$num1,frequency = 12)
        ts$ts=spy
        ts$ts_m=spy_m
      }
    })

    observeEvent(input$num2, {
      if(!is.null(input$text)) {
        date1=as.Date(paste0(input$num1,"-1-1"))
        date2=as.Date(paste0(input$num2,"-1-1"))
        spy = get.hist.quote(instrument= input$text,start = date1, end = date2 ,quote="Open", provider = "yahoo",compression = "d",retclass="ts",quiet = T)
        spy_m = get.hist.quote(instrument= input$text,start = date1, end = date2 ,quote="Open", provider = "yahoo",compression = "m",retclass="ts",quiet = T)
        spy=ts(as.numeric(spy),start = input$num1,frequency = 365)
        spy_m=ts(as.numeric(spy_m[!is.na(spy_m)]),start = input$num1,frequency = 12)
        ts$ts=spy
        ts$ts_m=spy_m
      }
    })

    observeEvent(input$text, {
      if(!is.null(input$text)) {
        date1=as.Date(paste0(input$num1,"-1-1"))
        date2=as.Date(paste0(input$num2,"-1-1"))
        spy = get.hist.quote(instrument= input$text,start = date1, end = date2 ,quote="Open", provider = "yahoo",compression = "d",retclass="ts",quiet = T)
        spy_m = get.hist.quote(instrument= input$text,start = date1, end = date2 ,quote="Open", provider = "yahoo",compression = "m",retclass="ts",quiet = T)
        spy=ts(as.numeric(spy),start = input$num1,frequency = 365)
        spy_m=ts(as.numeric(spy_m[!is.na(spy_m)]),start = input$num1,frequency = 12)
        ts$ts=spy
        ts$ts_m=spy_m
      }
    })

    output$plot1 <- renderPlot({
      if(!is.null(input$text)){
        plot(stl(na.approx(ts$ts),s.window = "periodic"),range.bars = F,set.pars = list(mar =c(2.2,5,1,3), oma = c(0, 0, 0, 0),tck = 0, mfrow = c(4, 1)))

        par(new=T,mar =c(1.5,3.2,0.6,2),oma = c(0, 0, 0, 0))
        plot.window(xlim=c(0.04,0.96),ylim=c(0.04,0.94))
      }

    },height = height[1])


    output$plot2 <- renderPlot({
      if(length(click$brush)>0 & !is.null(input$text)) {
        ts_w=na.approx(ts$ts)
        stl_w=stl(ts_w,s.window = "periodic")
        stl_w$time.series=cbind(stl_w$time.series,ts_w)
        stl_w$time.series=window_new(beginning = input$num1+position6(xmin = click$brush$xmin, xmax = click$brush$xmax,ending = input$num2, beginning=input$num1)[1],
                                     ending = input$num1+position6(xmin = click$brush$xmin, xmax = click$brush$xmax,ending = input$num2, beginning=input$num1)[2],timedata = stl_w$time.series)


        if(position4((click$brush$ymin+click$brush$ymax)/2)==4){
          plot(x=stl_w$time.series[,5], y= stl_w$time.series[,4],type = "l", xlab="time", ylab="data",
               ylim=position5(x = position4((click$brush$ymin+click$brush$ymax)/2),ymin = click$brush$ymin,ymax = click$brush$ymax,ts_w = na.approx(ts$ts)))
        } else if(position4((click$brush$ymin+click$brush$ymax)/2)==3) {
          plot(x=stl_w$time.series[,5], y= stl_w$time.series[,1],type = "l", xlab="time", ylab="seasonal",
               ylim=position5(x = position4((click$brush$ymin+click$brush$ymax)/2),ymin = click$brush$ymin,ymax = click$brush$ymax,ts_w = na.approx(ts$ts)))
        } else if(position4((click$brush$ymin+click$brush$ymax)/2)==2) {
          plot(x=stl_w$time.series[,5], y= stl_w$time.series[,2],type = "l", xlab="time", ylab="trend",
               ylim=position5(x = position4((click$brush$ymin+click$brush$ymax)/2),ymin = click$brush$ymin,ymax = click$brush$ymax,ts_w = na.approx(ts$ts)))
        } else if(position4((click$brush$ymin+click$brush$ymax)/2)==1) {
          plot(x=stl_w$time.series[,5], y= stl_w$time.series[,3],type = "l", xlab="time", ylab="remainder",
               ylim=position5(x = position4((click$brush$ymin+click$brush$ymax)/2),ymin = click$brush$ymin,ymax = click$brush$ymax,ts_w = na.approx(ts$ts)))
          polygon(c(stl_w$time.series[,5][1]-0.0001,stl_w$time.series[,5],stl_w$time.series[,5][length(stl_w$time.series[,5])]+0.001),
                  c(0,stl_w$time.series[,3],0), col='black')

        }
      }
    })



  }
  shinyApp(ui = ui, server = server)
}

position4 = function(y){
  if(y<0.215){
    x=1
  } else if (y>=0.254 & y< 0.469){
    x=2
  } else if (y>=0.508 & y< 0.723){
    x=3
  } else if (y>=0.723 ){
    x=4
  }
  return(x)

}

position5= function(x,ymin=0.8,ymax=0.9,ts_w) {
  stl_w=stl(ts_w,s.window = "periodic")
  if(x==4){
    dim=c(0.758+0.008,0.973-0.008)
    if(ymin<dim[1]) {
      ymin=dim[1]
    }
    if(ymax>dim[2]) {
      ymax=dim[2]
    }
    s = c(dim[1],ymin,ymax,dim[2])
    s= range01(s)
    ylim=c(range(ts_w)[1]+s[2]*abs(range(ts_w)[1]-range(ts_w)[2]),
           range(ts_w)[2]-(1-s[3])*abs(range(ts_w)[1]-range(ts_w)[2]))

  } else if(x==3) {
    dim=c(0.507+0.008,0.72-0.008)
    if(ymin<dim[1]) {
      ymin=dim[1]
    }
    if(ymax>dim[2]) {
      ymax=dim[2]
    }
    s = c(dim[1],ymin,ymax,dim[2])
    s= range01(s)
    ylim=c(range(stl_w$time.series[,1])[1]+s[2]*abs(range(stl_w$time.series[,1])[1]-range(stl_w$time.series[,1])[2]),
           range(stl_w$time.series[,1])[2]-(1-s[3])*abs(range(stl_w$time.series[,1])[1]-range(stl_w$time.series[,1])[2]))
  } else if(x==2) {
    dim=c(0.255+0.008,0.468-0.008)
    if(ymin<dim[1]) {
      ymin=dim[1]
    }
    if(ymax>dim[2]) {
      ymax=dim[2]
    }
    s = c(dim[1],ymin,ymax,dim[2])
    s= range01(s)
    ylim=c(range(stl_w$time.series[,2])[1]+s[2]*abs(range(stl_w$time.series[,2])[1]-range(stl_w$time.series[,2])[2]),
           range(stl_w$time.series[,2])[2]-(1-s[3])*abs(range(stl_w$time.series[,2])[1]-range(stl_w$time.series[,2])[2]))
  } else if(x==1) {
    dim=c(0+0.008,0.215-0.008)
    if(ymin<dim[1]) {
      ymin=dim[1]
    }
    if(ymax>dim[2]) {
      ymax=dim[2]
    }
    s = c(dim[1],ymin,ymax,dim[2])
    s= range01(s)
    ylim=c(range(stl_w$time.series[,3])[1]+s[2]*abs(range(stl_w$time.series[,3])[1]-range(stl_w$time.series[,3])[2]),
           range(stl_w$time.series[,3])[2]-(1-s[3])*abs(range(stl_w$time.series[,3])[1]-range(stl_w$time.series[,3])[2]))
  }

  return(as.numeric(ylim))
}

position6= function(xmin, xmax, beginning, ending) {
  dim= c(0.04,0.96)
  if(xmin<dim[1]) {
    xmin=dim[1]
  }
  if(xmax>dim[2]) {
    xmax=dim[2]
  }
  xlim=c((xmin-0.04)/((0.96-0.04)/abs(beginning-ending)),
         (xmax-0.04)/((0.96-0.04)/abs(beginning-ending)))

  return(xlim)
}

range01 <- function(x){
  (x-min(x))/(max(x)-min(x))
}

window_new= function(beginning, ending, timedata) {

  new_timedata=timedata[time(timedata)>=beginning& time(timedata)<=ending,]
  new_timedata= cbind(new_timedata,seq(beginning, ending, length.out = nrow(new_timedata)))
  return(new_timedata)

}
