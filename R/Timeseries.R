#' Interactive Seasonal Decomposition of Time Series by Loess
#'
#' Takes any metric data with time series and plots a seasional decomposition by Loess.
#'
#' @param data A data.frame object that is to be analyzed (all categorical variables with be transformed to metrical variables)
#' @param n A numeric value indicating from what number of different values a variable is seen as categorical variables, all variables that have more different values than n are being treated as metric values
#' @param height A two-dimensional numeric value indicating the height of the decomposition and the heigth of the zoom-graph
#' @author Cornelius Fritz <cornelius.fritz@campus.lmu.de>
#' @example demo/demo7.R
#' @details At first you can chose what metric variable should be composed into a seasonal, trend and remainder part. Then you can choose from what year to what year the data was observed
#' and the frequency of observations per year. For monthly observed data this parameter should be 12. The STL algorithm is set up in two loops, one inner loop to update the seasonal and trend component and one to updated the robust weights of each oberservation.
#' If you want to update any robust weights you can press the button "Robust Fitting". The bandwidth of the estimation used updating the seasonal component is usually the amount of observations in one period, but can also be set individually, if you don't press the
#' "Periodicity of the Loess window for the Seasional Component".
#' You can brush over any part of the decomposition and see a zoom plot in the sidebar panel of the shaded area.
#' @references R. B. Cleveland, W. S. Cleveland, J.E. McRae, and I. Terpenning (1990) STL: A Seasonal-Trend Decomposition Procedure Based on Loess. Journal of Official Statistics, 6.
#' @export
#'



Timeseries = function(data,height=c(800,400),n=10) {

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

  ui <- fluidPage(
    tags$div( HTML(
      "<h2><center>Time Series</center></h2>")
    ),

    sidebarLayout(
      sidebarPanel(
        selectInput("select", label = h3("Variable"),
                    choices = choices,
                    selected = 1),
        fluidRow(
          column(6,numericInput("num1", label = "Start", value = 2000)),
          column(6,numericInput("num2", label = "Ende", value = 2010))
        ),
        numericInput("num3", label = "Frequency", value = 12),
        fluidRow(
          column(6,checkboxInput("robust",label = "Robust Fitting",value = F)),
          column(6,checkboxInput("periodic",label = "Periodicity of the Loess window for the Seasional Component",value = T))
        ),conditionalPanel(
        condition = "input.periodic == false",
        numericInput("num4", label = "Span of the loess window", value = 10)
        ),
        plotOutput("plot2")
      )

      ,
      mainPanel(
        plotOutput("plot1",height = height[1], click = "click1", dblclick = "click2", brush = brushOpts(id = "brush"))
      )
    )
  )



  server <- function(input, output) {

    ts= reactiveValues(
      ts=c(), stl=c()
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
      ts$ts=ts(data[,names(data)[as.numeric(input$select)]],start = input$num1, end = input$num2,frequency = input$num3)
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

    observeEvent(input$num4, {
      date1=as.Date(paste0(input$num1,"-1-1"))
      date2=as.Date(paste0(input$num2,"-1-1"))
      ts$ts=ts(data[,names(data)[as.numeric(input$select)]],start = input$num1, end = input$num2,frequency = input$num3)
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
      date1=as.Date(paste0(input$num1,"-1-1"))
      date2=as.Date(paste0(input$num2,"-1-1"))
      ts$ts=ts(data[,names(data)[as.numeric(input$select)]],start = input$num1, end = input$num2,frequency = input$num3)
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
      date1=as.Date(paste0(input$num1,"-1-1"))
      date2=as.Date(paste0(input$num2,"-1-1"))
      ts$ts=ts(data[,names(data)[as.numeric(input$select)]],start = input$num1, end = input$num2,frequency = input$num3)
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

    observeEvent(input$num3, {
      date1=as.Date(paste0(input$num1,"-1-1"))
      date2=as.Date(paste0(input$num2,"-1-1"))
      ts$ts=ts(data[,names(data)[as.numeric(input$select)]],start = input$num1, end = input$num2,frequency = input$num3)
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
    observeEvent(input$num2, {
      date1=as.Date(paste0(input$num1,"-1-1"))
      date2=as.Date(paste0(input$num2,"-1-1"))
      ts$ts=ts(data[,names(data)[as.numeric(input$select)]],start = input$num1, end = input$num2,frequency = input$num3)
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
      date1=as.Date(paste0(input$num1,"-1-1"))
      date2=as.Date(paste0(input$num2,"-1-1"))
      ts$ts=ts(data[,names(data)[as.numeric(input$select)]],start = input$num1, end = input$num2,frequency = input$num3)
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

    output$plot1 <- renderPlot({
      if(!is.null(input$select)) {
        plot(ts$stl,range.bars = F,set.pars = list(mar =c(2.2,5,1,3), oma = c(0, 0, 0, 0),tck = 0, mfrow = c(4, 1)))
      }

      par(new=T,mar =c(1.5,3.2,0.6,2),oma = c(0, 0, 0, 0))
      plot.window(xlim=c(0.04,0.96),ylim=c(0.04,0.94))
    },height = height[1])


    output$plot2 <- renderPlot({
      if(length(click$brush)>0) {
        ts_w=na.approx(ts$ts)
        stl_w=ts$stl
        plot(stl_w)
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
          plot(x=stl_w$time.series[,5], y= stl_w$time.series[,3],type = "h", xlab="time", ylab="remainder",
               ylim=position5(x = position4((click$brush$ymin+click$brush$ymax)/2),ymin = click$brush$ymin,ymax = click$brush$ymax,ts_w = na.approx(ts$ts)))

        }
      }
    })



  }
  shinyApp(ui = ui, server = server)
}

