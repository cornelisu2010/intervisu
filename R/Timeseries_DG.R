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

Timeseries = function(data, height=200,width=1000,n=10) {
  data=data[complete.cases(data),]

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
  freq_select[[3]]="years"

  names(freq_select)=c("daily data", "monthly data","yearly data")

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
          column(6,dateInput("date2",label = "Ending",value = "2010-01-02"))
        ),
        selectInput("freq", label = "Frequency",
                    choices = freq_select,
                    selected = "days"),
        fluidRow(
          column(6,checkboxInput("robust",label = "Robust Fitting",value = F)),
          column(6,checkboxInput("periodic",label = "Periodicity of the Loess window for the Seasional Component",value = T))
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
      num3=NULL
    )

    observeEvent(input$freq, {
      if(input$freq == "days") {
        freq$num3= 365
      } else if(input$freq == "months") {
        freq$num3=12
      } else if(input$freq == "years") {
        freq$num3=1
      }
    })



    observeEvent(input$num4, {
      ts$ts=ts(data[,names(data)[as.numeric(input$select)]],frequency = freq$num3)
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

    })

    observeEvent(input$robust, {
      ts$ts=ts(data[,names(data)[as.numeric(input$select)]],frequency = freq$num3)
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

    })

    observeEvent(input$periodic, {
      ts$ts=ts(data[,names(data)[as.numeric(input$select)]],frequency = freq$num3)
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

    })


    observeEvent(input$select, {
      ts$ts=ts(data[,names(data)[as.numeric(input$select)]],frequency = freq$num3)
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

    })

    output$plot1 <-  renderDygraph({
      ts$ts=na.approx(ts$ts)
      z1=zoo(x=ts$ts,seq(input$date1, input$date2, "days"),frequency = freq$num3)
      z1=as.xts(z1)
      names(z1)=names(data)[as.numeric(input$select)]
      dygraph(z1, main = "",ylab = "data")
    })

    output$plot2 <- renderDygraph({
      ts$ts=na.approx(ts$ts)
      z2=zoo(x=(ts$stl)$time.series[,1],seq(input$date1, input$date2, "days"),frequency = freq$num3)

      z2=as.xts(z2)
      names(z2)=paste(names(data)[as.numeric(input$select)],"Seasonal")
      dygraph(z2, main = "",ylab = "Seasonal")

    })

    output$plot3 <- renderDygraph({
      ts$ts=na.approx(ts$ts)
      z2=zoo(x=(ts$stl)$time.series[,2],seq(input$date1, input$date2, "days"),frequency = freq$num3)

      z2=as.xts(z2)
      names(z2)=paste(names(data)[as.numeric(input$select)],"Trend")
      dygraph(z2, main = "",ylab = "Trend")

    })

    output$plot4 <- renderDygraph({
      ts$ts=na.approx(ts$ts)
      z2=zoo(x=(ts$stl)$time.series[,3],seq(input$date1, input$date2, "days"),frequency = freq$num3)
      z2=as.xts(z2)
      names(z2)=paste(names(data)[as.numeric(input$select)],"Unobserved")
      dygraph(z2, main = "",ylab = "Unobserved") %>%
        dyOptions(fillGraph = TRUE, fillAlpha = 0.4)

    })



  }
  shinyApp(ui = ui, server = server)
}
