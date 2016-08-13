#' Interactive Seasonal Decomposition of Time Series by Loess example application (Type 2)
#'
#' For this example a working internet connection is needed.
#' The user can chose out of a range of stocks and indicators from  on yahoo finance what data from what time (in years) should be downloaded as a time series.
#' Then a Seasonal Decomposition of Time Series by Loess is performed on the given time series.
#'
#' @param height The height of all plots
#' @param width The width of all plots
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
#' @importFrom plotly ggplotly plotly_empty layout plotlyOutput event_data renderPlotly as.widget
#' @import splines
#' @importFrom "tseries" "get.hist.quote"
#' @importFrom "grDevices" "heat.colors"
#' @importFrom "graphics" "abline" "barplot" "boxplot" "hist" "legend" "lines" "par" "plot" "plot.new" "plot.window" "points" "polygon" "stripchart" "text"
#' @importFrom "stats" "chisq.test" "density" "fisher.test" "lm" "predict" "quantile" "resid" "stl" "time" "var" "complete.cases"
#' @importFrom "utils" "install.packages" "installed.packages"
#' @importFrom hypergea getOddsRatio
#' @import ggplot2
#' @import viridis
#' @import tidyr
#'





Timeseries_Stat = function(height=150,width=800) {

  optindex=list()
  optindex$"Dax"=1
  optindex$"Nasdaq"=2
  optindex$"Dow Jones"=3

  quite_list=list()
  quite_list$"Open"="Open"
  quite_list$"Low"="Low"
  quite_list$"High"="High"
  quite_list$"Adjusted Close"="AdjClose"
  quite_list$"Volume"="Volume"



  ui <- fluidPage(
    tags$div( HTML(
      "<h2><center>Time Series</center></h2>")
    ),

    sidebarLayout(
      sidebarPanel(
        selectInput("index",label = "What index should be shown?",choices = optindex,selected = 1),
        uiOutput("dynamic"),
        selectInput("quote",label = "What quotes should be shown?",choices = quite_list,selected = "open"),

        fluidRow(
          column(6,dateInput("date1",label = "Beginning",value = "2008-01-02")),
          column(6,dateInput("date2",label = "Ending",value = "2012-01-02"))
        ),
        fluidRow(
          column(6,checkboxInput("robust",label = "Robust Fitting",value = F)),
          column(6,checkboxInput("periodic",label = "Cycle-Window",value = T))
        ),conditionalPanel(
          condition = "input.periodic == false",
          numericInput("num4", label = "Span of the loess window", value = 10)
        )
      )

      ,
      mainPanel(
        fluidRow(
          column(12,dygraphOutput("plot1",width = width,height = height))
        ),
        fluidRow(
          column(12,dygraphOutput("plot2",width = width,height = height))
        ),
        fluidRow(
          column(12,dygraphOutput("plot3",width = width,height = height))
        ),
        fluidRow(
          column(12,dygraphOutput("plot4",width = width,height = height))
        )
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
      ts=NULL, stl=NULL
    )



    observeEvent(input$num4, {
      if(!is.null(input$text)) {
        if(input$periodic) {
          if(input$robust) {
            ts$stl=stl(na.approx(ts$ts),s.window = "periodic",robust = T)
          } else {
            ts$stl=stl(na.approx(ts$ts),s.window = "periodic",robust = F)
          }
        } else {
          if(input$robust) {
            ts$stl=stl(na.approx(ts$ts),s.window = input$num4,robust = T)
          } else {
            ts$stl=stl(na.approx(ts$ts),s.window = input$num4,robust = F)
          }
        }
      }
    })

    observeEvent(input$text, {
        spy = get.hist.quote(instrument= input$text,start = input$date1, end = input$date2 ,quote=input$quote, provider = "yahoo",compression = "d",retclass="ts",quiet = T)
        spy=ts(as.numeric(spy),start = input$date1 ,frequency = 365)
        ts$ts=na.approx(spy)

        if(input$periodic) {
          if(input$robust) {
            ts$stl=stl(ts$ts,s.window = "periodic",robust = T)
          } else {
            ts$stl=stl(na.approx(ts$ts),s.window = "periodic",robust = F)
          }
        } else {
          if(input$robust) {
            ts$stl=stl(na.approx(ts$ts),s.window = input$num4,robust = T)
          } else {
            ts$stl=stl(na.approx(ts$ts),s.window = input$num4,robust = F)
          }
        }


    })


    observeEvent(input$quote, {
      if(!is.null(input$text)) {
        spy = get.hist.quote(instrument= input$text,start = input$date1, end = input$date2 ,quote=input$quote, provider = "yahoo",compression = "d",retclass="ts",quiet = T)
        spy=ts(as.numeric(spy),start = input$date1 ,frequency = 365)
        ts$ts=na.approx(spy)

        if(input$periodic) {
          if(input$robust) {
            ts$stl=stl(ts$ts,s.window = "periodic",robust = T)
          } else {
            ts$stl=stl(na.approx(ts$ts),s.window = "periodic",robust = F)
          }
        } else {
          if(input$robust) {
            ts$stl=stl(na.approx(ts$ts),s.window = input$num4,robust = T)
          } else {
            ts$stl=stl(na.approx(ts$ts),s.window = input$num4,robust = F)
          }
        }
      }
    })


    observeEvent(input$date1, {
      if(!is.null(input$text)) {
        spy = get.hist.quote(instrument= input$text,start = input$date1, end = input$date2 ,quote=input$quote, provider = "yahoo",compression = "d",retclass="ts",quiet = T)
        spy=ts(as.numeric(spy),start = input$date1 ,frequency = 365)
        ts$ts=na.approx(spy)

        if(input$periodic) {
          if(input$robust) {
            ts$stl=stl(ts$ts,s.window = "periodic",robust = T)
          } else {
            ts$stl=stl(na.approx(ts$ts),s.window = "periodic",robust = F)
          }
        } else {
          if(input$robust) {
            ts$stl=stl(na.approx(ts$ts),s.window = input$num4,robust = T)
          } else {
            ts$stl=stl(na.approx(ts$ts),s.window = input$num4,robust = F)
          }
        }
      }
    })



    observeEvent(input$date2, {
      if(!is.null(input$text)) {
        spy = get.hist.quote(instrument= input$text,start = input$date1, end = input$date2 ,quote=input$quote, provider = "yahoo",compression = "d",retclass="ts",quiet = T)
        spy=ts(as.numeric(spy),start = input$date1 ,frequency = 365)
        ts$ts=na.approx(spy)

        if(input$periodic) {
          if(input$robust) {
            ts$stl=stl(ts$ts,s.window = "periodic",robust = T)
          } else {
            ts$stl=stl(na.approx(ts$ts),s.window = "periodic",robust = F)
          }
        } else {
          if(input$robust) {
            ts$stl=stl(na.approx(ts$ts),s.window = input$num4,robust = T)
          } else {
            ts$stl=stl(na.approx(ts$ts),s.window = input$num4,robust = F)
          }
        }
      }
    })
    observeEvent(input$robust, {
      if(!is.null(ts$ts)) {
        if(input$periodic) {
          if(input$robust) {
            ts$stl=stl(na.approx(ts$ts),s.window = "periodic",robust = T)
          } else {
            ts$stl=stl(na.approx(ts$ts),s.window = "periodic",robust = F)
          }
        } else {
          if(input$robust) {
            ts$stl=stl(na.approx(ts$ts),s.window = input$num4,robust = T)
          } else {
            ts$stl=stl(na.approx(ts$ts),s.window = input$num4,robust = F)
          }
        }
      }
    })

    observeEvent(input$periodic, {
      if(!is.null(ts$ts)) {
        if(input$periodic) {
          if(input$robust) {
            ts$stl=stl(na.approx(ts$ts),s.window = "periodic",robust = T)
          } else {
            ts$stl=stl(na.approx(ts$ts),s.window = "periodic",robust = F)
          }
        } else {
          if(input$robust) {
            ts$stl=stl(na.approx(ts$ts),s.window = input$num4,robust = T)
          } else {
            ts$stl=stl(na.approx(ts$ts),s.window = input$num4,robust = F)
          }
        }
      }
    })


    output$plot1 <-  renderDygraph({
      if(!is.null(ts$ts)) {
        ts$ts=na.approx(ts$ts)
        z1=zoo(x=ts$ts,seq(input$date1, input$date2, "days"),frequency = 365)
        z1=as.xts(z1)
        names(z1)=input$text
        dygraph(z1, main = "",ylab = "data")
      }
    })

    output$plot2 <- renderDygraph({
      if(!is.null(ts$ts)) {
        ts$ts=na.approx(ts$ts)
        z2=zoo(x=(ts$stl)$time.series[,1],seq(input$date1, input$date2, "days"),frequency =  365)

        z2=as.xts(z2)
        names(z2)=paste(input$text,"Seasonal")
        dygraph(z2, main = "",ylab = "Seasonal")
      }

    })

    output$plot3 <- renderDygraph({

      if(!is.null(ts$ts)) {
        ts$ts=na.approx(ts$ts)
        z2=zoo(x=(ts$stl)$time.series[,2],seq(input$date1, input$date2, "days"),frequency =  365)

        z2=as.xts(z2)
        names(z2)=paste(input$text,"Trend")
        dygraph(z2, main = "",ylab = "Trend")
      }


    })

    output$plot4 <- renderDygraph({
      if(!is.null(ts$ts)) {
        ts$ts=na.approx(ts$ts)
        z2=zoo(x=(ts$stl)$time.series[,3],seq(input$date1, input$date2, "days"),frequency =  365)
        z2=as.xts(z2)
        names(z2)=paste(input$text,"Unobserved")
        dygraph(z2, main = "",ylab = "Unobserved") %>%
          dyOptions(fillGraph = TRUE, fillAlpha = 0.4)
      }


    })



  }
  shinyApp(ui = ui, server = server)
}
