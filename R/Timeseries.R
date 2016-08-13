#' Interactive Seasonal Decomposition of Time Series by Loess and plot it with dygraph
#'
#' Takes any metric data with time series and plots a seasional decomposition by Loess.
#'
#' @param data A data.frame object that is to be analyzed (all categorical variables with be transformed to metrical variables)
#' @param n A numeric value indicating from what number of different values a variable is seen as categorical variables, all variables that have more different values than n are being treated as metric values
#' @param height The height of all plots
#' @param width The width of all plots
#' @author Cornelius Fritz <cornelius.fritz@campus.lmu.de>
#' @example demo/demo7.R
#' @details At first you can chose what metric variable should be composed into a seasonal, trend and remainder part. Then you have to choose from what year to what year the data was observed
#' and the frequency of observations per year. For monthly observed data this parameter should be 12. The STL algorithm is set up in two loops, one inner loop to update the seasonal and trend component and one to updated the robust weights of each oberservation.
#' If you want to update any robust weights you can press the button "Robust Fitting". The bandwidth of the estimation used updating the seasonal component is usually the amount of observations in one period, but can also be set individually, if you don't press the
#' "Periodicity of the Loess window for the Seasional Component".
#' You can brush over any part of the decomposition zoom in on that specific area, perform a double click in order to return to the full data.
#' @references R. B. Cleveland, W. S. Cleveland, J.E. McRae, and I. Terpenning (1990) STL: A Seasonal-Trend Decomposition Procedure Based on Loess. Journal of Official Statistics, 6.
#' @export
#' @import dygraphs
#' @import xts
#'
Timeseries = function(data, height=200,width=1000,n=10) {
  data=faktor2(data,n)[[1]]
  vec=faktor2(data,n)[[2]]
  if(sum(!vec)<=1) {
    stop("Error: In order to use this function you need at least one metric variable")
  }


  choices = list()
  for (i in 1: length(names(data)[!vec])) {
    choices[[i]]=i
  }
  names(choices)=names(data)[!vec]

  freq_select=list()
  freq_select[[1]]="days"
  freq_select[[2]]="months"

  names(freq_select)=c("daily data", "monthly data")

  ui <- fluidPage(
    tags$div( HTML(
      "<h2><center>Time Series</center></h2>")
    ),

    sidebarLayout(
      sidebarPanel(
        selectInput("select", label = "Variable",
                    choices = choices,
                    selected = 1),
        fluidRow(
          column(6,dateInput("date1",label = "Beginning",value = "2010-01-02")),
          column(6,dateInput("date2",label = "Ending",value = "2012-01-02"))
        ),
        selectInput("freq", label = "Frequency",
                    choices = freq_select,
                    selected = "days"),
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
          conditionalPanel(condition = "input.date1!='2010-01-02'",
                           column(12,dygraphOutput("plot1",width = width,height = height)))
        ),
        fluidRow(
          conditionalPanel(condition = "input.date1=='2010-01-02'",
                           column(12,plotOutput("text"))),
          conditionalPanel(condition = "input.date1!='2010-01-02'",
                           column(12,dygraphOutput("plot2",width = width,height = height)))
        ),
        fluidRow(
          conditionalPanel(condition = "input.date1!='2010-01-02'",
                           column(12,dygraphOutput("plot3",width = width,height = height)))
        ),
        fluidRow(
          conditionalPanel(condition = "input.date1!='2010-01-02'",
                           column(12,dygraphOutput("plot4",width = width,height = height)))
        )
      )
    )
  )



  server <- function(input, output) {


    output$text= renderPlot({
      plot.new()
      plot.window(xlim=c(0,1),ylim=c(0,2))
      text("Please set the beginning and end date of your observations and the frequency!",x = 0.5,y = 1.5,cex = 1.5)

    })
    ts= reactiveValues(
      ts=c(), stl=c()
    )

    freq=reactiveValues(
      num3=12
    )

    observeEvent(input$freq, {
      if(input$freq == "days") {
        freq$num3= 365
      } else if(input$freq == "months") {
        freq$num3=12
      }
    })



    observeEvent(input$num4, {
      date3=as.Date(paste0(substr(as.character(input$date1),1,4),"-01-01"))
      date4=as.Date(paste0(substr(as.character(input$date2),1,4),"-01-01"))
      dif1=as.numeric(input$date1-date3+1)
      dif2=as.numeric(input$date2-date4+1)
      if(input$freq=="months") {
        dif1=round(dif1/30,0)+1
        dif2=round(dif1/30,0)+1
      }
      ts$ts=ts(data[,names(data)[as.numeric(input$select)]],start=c(as.numeric(substr(as.character(input$date1),1,4)),dif1),
               end=c(as.numeric(substr(as.character(input$date2),1,4)),dif2),frequency = freq$num3)
      ts$ts=na.approx(ts$ts)
      if(input$periodic) {
        if(input$robust) {
          ts$stl=stl(ts$ts,s.window = "periodic",robust = T)
        } else {
          ts$stl=stl(ts$ts,s.window = "periodic",robust = F)
        }
      } else {
        if(input$robust) {
          ts$stl=stl(ts$ts,s.window = input$num4,robust = T)
        } else {
          ts$stl=stl(ts$ts,s.window = input$num4,robust = F)
        }
      }

    })

    observeEvent(input$robust, {
      date3=as.Date(paste0(substr(as.character(input$date1),1,4),"-01-01"))
      date4=as.Date(paste0(substr(as.character(input$date2),1,4),"-01-01"))
      dif1=as.numeric(input$date1-date3+1)
      dif2=as.numeric(input$date2-date4+1)
      if(input$freq=="months") {
        dif1=round(dif1/30,0)+1
        dif2=round(dif1/30,0)+1
      }
      ts$ts=ts(data[,names(data)[as.numeric(input$select)]],start=c(as.numeric(substr(as.character(input$date1),1,4)),dif1),
               end=c(as.numeric(substr(as.character(input$date2),1,4)),dif2),frequency = freq$num3)
      ts$ts=na.approx(ts$ts)
      if(input$periodic) {
        if(input$robust) {
          ts$stl=stl(ts$ts,s.window = "periodic",robust = T)
        } else {
          ts$stl=stl(ts$ts,s.window = "periodic",robust = F)
        }
      } else {
        if(input$robust) {
          ts$stl=stl(ts$ts,s.window = input$num4,robust = T)
        } else {
          ts$stl=stl(ts$ts,s.window = input$num4,robust = F)
        }
      }

    })

    observeEvent(input$date1, {
      date3=as.Date(paste0(substr(as.character(input$date1),1,4),"-01-01"))
      date4=as.Date(paste0(substr(as.character(input$date2),1,4),"-01-01"))
      dif1=as.numeric(input$date1-date3+1)
      dif2=as.numeric(input$date2-date4+1)
      if(input$freq=="months") {
        dif1=round(dif1/30,0)+1
        dif2=round(dif1/30,0)+1
      }
      ts$ts=ts(data[,names(data)[as.numeric(input$select)]],start=c(as.numeric(substr(as.character(input$date1),1,4)),dif1),
               end=c(as.numeric(substr(as.character(input$date2),1,4)),dif2),frequency = freq$num3)
      ts$ts=na.approx(ts$ts)
      if(input$periodic) {
        if(input$robust) {
          ts$stl=stl(ts$ts,s.window = "periodic",robust = T)
        } else {
          ts$stl=stl(ts$ts,s.window = "periodic",robust = F)
        }
      } else {
        if(input$robust) {
          ts$stl=stl(ts$ts,s.window = input$num4,robust = T)
        } else {
          ts$stl=stl(ts$ts,s.window = input$num4,robust = F)
        }
      }

    })

    observeEvent(input$date2, {
      date3=as.Date(paste0(substr(as.character(input$date1),1,4),"-01-01"))
      date4=as.Date(paste0(substr(as.character(input$date2),1,4),"-01-01"))
      dif1=as.numeric(input$date1-date3+1)
      dif2=as.numeric(input$date2-date4+1)
      if(input$freq=="months") {
        dif1=round(dif1/30,0)+1
        dif2=round(dif1/30,0)+1
      }
      ts$ts=ts(data[,names(data)[as.numeric(input$select)]],start=c(as.numeric(substr(as.character(input$date1),1,4)),dif1),
               end=c(as.numeric(substr(as.character(input$date2),1,4)),dif2),frequency = freq$num3)
      ts$ts=na.approx(ts$ts)
      if(input$periodic) {
        if(input$robust) {
          ts$stl=stl(ts$ts,s.window = "periodic",robust = T)
        } else {
          ts$stl=stl(ts$ts,s.window = "periodic",robust = F)
        }
      } else {
        if(input$robust) {
          ts$stl=stl(ts$ts,s.window = input$num4,robust = T)
        } else {
          ts$stl=stl(ts$ts,s.window = input$num4,robust = F)
        }
      }

    })

    observeEvent(input$periodic, {
      date3=as.Date(paste0(substr(as.character(input$date1),1,4),"-01-01"))
      date4=as.Date(paste0(substr(as.character(input$date2),1,4),"-01-01"))
      dif1=as.numeric(input$date1-date3+1)
      dif2=as.numeric(input$date2-date4+1)
      if(input$freq=="months") {
        dif1=round(dif1/30,0)+1
        dif2=round(dif1/30,0)+1
      }
      ts$ts=ts(data[,names(data)[as.numeric(input$select)]],start=c(as.numeric(substr(as.character(input$date1),1,4)),dif1),
               end=c(as.numeric(substr(as.character(input$date2),1,4)),dif2),frequency = freq$num3)
      ts$ts=na.approx(ts$ts)
      if(input$periodic) {
        if(input$robust) {
          ts$stl=stl(ts$ts,s.window = "periodic",robust = T)
        } else {
          ts$stl=stl(ts$ts,s.window = "periodic",robust = F)
        }
      } else {
        if(input$robust) {
          ts$stl=stl(ts$ts,s.window = input$num4,robust = T)
        } else {
          ts$stl=stl(ts$ts,s.window = input$num4,robust = F)
        }
      }
    })


    observeEvent(input$select, {
      date3=as.Date(paste0(substr(as.character(input$date1),1,4),"-01-01"))
      date4=as.Date(paste0(substr(as.character(input$date2),1,4),"-01-01"))
      dif1=as.numeric(input$date1-date3+1)
      dif2=as.numeric(input$date2-date4+1)
      if(input$freq=="months") {
        dif1=round(dif1/30,0)+1
        dif2=round(dif1/30,0)+1
      }
      ts$ts=ts(data[,names(data)[as.numeric(input$select)]],start=c(as.numeric(substr(as.character(input$date1),1,4)),dif1),
               end=c(as.numeric(substr(as.character(input$date2),1,4)),dif2),frequency = freq$num3)
      ts$ts=na.approx(ts$ts)
      if(input$periodic) {
        if(input$robust) {
          ts$stl=stl(ts$ts,s.window = "periodic",robust = T)
        } else {
          ts$stl=stl(ts$ts,s.window = "periodic",robust = F)
        }
      } else {
        if(input$robust) {
          ts$stl=stl(ts$ts,s.window = input$num4,robust = T)
        } else {
          ts$stl=stl(ts$ts,s.window = input$num4,robust = F)
        }
      }


    })

    output$plot1 <-  renderDygraph({
      z1=zoo(x=ts$ts,seq(input$date1, input$date2, input$freq),frequency = freq$num3)
      z1=as.xts(z1)
      names(z1)=names(data)[as.numeric(input$select)]
      dygraph(z1, main = "",ylab = "data")
    })

    output$plot2 <- renderDygraph({
      z2=zoo(x=(ts$stl)$time.series[,1],seq(input$date1, input$date2, input$freq),frequency = freq$num3)

      z2=as.xts(z2)
      names(z2)=paste(names(data)[as.numeric(input$select)],"Seasonal")
      dygraph(z2, main = "",ylab = "Seasonal")

    })

    output$plot3 <- renderDygraph({
      z2=zoo(x=(ts$stl)$time.series[,2],seq(input$date1, input$date2, input$freq),frequency = freq$num3)
      z2=as.xts(z2)
      names(z2)=paste(names(data)[as.numeric(input$select)],"Trend")
      dygraph(z2, main = "",ylab = "Trend")
    })

    output$plot4 <- renderDygraph({
      z2=zoo(x=(ts$stl)$time.series[,3],seq(input$date1, input$date2, input$freq),frequency = freq$num3)
      z2=as.xts(z2)
      names(z2)=paste(names(data)[as.numeric(input$select)],"Unobserved")
      dygraph(z2, main = "",ylab = "Unobserved") %>%
        dyOptions(fillGraph = TRUE, fillAlpha = 0.4)

    })



  }
  shinyApp(ui = ui, server = server)
}
