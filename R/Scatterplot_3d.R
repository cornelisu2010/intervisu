#' Interactive three-dimensional scatterlpot
#'
#' Takes any metric data and plots a three-dimensional scatterplot and a two-dimensional scatterplot.
#'
#' @param data A data.frame object that is to be analyzed (only metric variables are shown)
#' @param n A numeric value indicating from what number of different values a variable is seen as categorical variables, all variables that have more different values than n are being treated as metric values
#' @param height A two-dimensional numeric vector indicating the height of the shown three-dimensional and two-dimensional scatterplots
#' @author Cornelius Fritz <cornelius.fritz@campus.lmu.de>
#' @example demo/demo4.R
#' @details This function produces an application that lets you plot a three-dimensional scatterplot, you can decide what metric variables should be plotted. Next to the
#' three-dimensional scatterplot there is a two-dimensional scatterplot of the first two dimensions of the three-dimensional scatterplot (1.Variable and 2. Variable). With
#' a slide you can set the angle of the plot, there is also a dynamic dimension to this slider. You can press the little 'Play'-Button and it will automatically rotate the plot once.
#' You can also plot vertical lines for each point in the plot by pressing the button named 'Draw vertical lines'. Another feature of this application is the conditional mode, which lets you
#' condition both plots on a fourth variable. To enter this mode press the button called 'Condition plot only values conditioned on the value of a fourth variable. You set value for the fourth variable with a slider
#' and only 30% of the data around that value is plotted.
#' @export
#'
#'

Scatterplot_3d = function(data,n,height=c(800,500)) {

  data=factor_check(data)
  choices = list()
  for (i in 1: length(names(data))) {
    choices[[i]]=i
  }
  names(choices)=names(data)


  ui <- fluidPage(
    tags$div( HTML(
      "<h2><center>Three-dimensional Scatterplot </center></h2>")
    ),

    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(6,selectInput("check1", label = h3("1. Variable"),
                               choices = choices,
                               selected = 1)),
          column(6,selectInput("check2", label = h3("2. Variable"),
                               choices = choices,
                               selected = 1))
        ),
        fluidRow(
          column(6,selectInput("check3", label = h3("3. Variable"),
                               choices = choices,
                               selected = 1)),
          column(6,conditionalPanel(
            condition = "input.checkbox2 == true",
            selectInput("check4", label = h3("4. Variable"),
                        choices = choices,
                        selected = 1)
          ))
        ),
        fluidRow(
          column(12,conditionalPanel(
            condition = "input.checkbox2 == true",
            uiOutput("ui")
          ))
        ),
        fluidRow(
          column(12,sliderInput("slider", label = h3("Angle \n of the plot"), min = 0,
                                max = 360, value = 100, animate=animationOptions(interval =300)))

        ),
        checkboxInput("checkbox2", label = "Condintion plot on a fourth variable", value = F),
        checkboxInput("checkbox", label = "Draw vertikal lines", value = F)
      ),
      mainPanel(
        splitLayout(cellWidths =c("70%","30%"),plotOutput("plot1",height = height[1], click = "click1", dblclick = "click2"),
                    plotOutput("plot2",height = height[2], click = "click3", brush = brushOpts(id = "brush")))
      )
    )
  )



  server <- function(input, output) {
    output$ui= renderUI({
      a=sliderInput("slider2", label = h3("Value of the 4. Variable"),
                    min = floor(min(data[,names(data)[as.numeric(input$check4)]]))+0.3,
                    max = ceiling(max(data[,names(data)[as.numeric(input$check4)]])),
                    value =sum(range(data[,names(data)[as.numeric(input$check4)]]))/2)
      a$children[[2]]$attribs$`data-keyboard-step`=1.4
      a$children[[2]]$attribs$`data-step`=0.05
      a
    })

    group  <- reactiveValues(
      group=rep(T,nrow(data))
    )

    clicks  <- reactiveValues(
      click3=NULL, brush=NULL
    )

    data1 <- reactiveValues(
      data1=NULL
    )

    observeEvent(input$click3, {
      clicks$click3=list(x=input$click3$x,y=input$click3$y)
    })

    observeEvent(input$brush, {
      clicks$brush=list(xmin=input$brush$xmin,xmax=input$brush$xmax,ymin=input$brush$ymin,ymax=input$brush$ymax)
    })

    observeEvent(input$checkbox2, {
      if(!is.null(input$slider2)) {
        data1$data1=data_cut(data, var = data[,names(data)[as.numeric(input$check4)]], value = input$slider2)
        group$group <- !((data1$data1[,names(data)[as.numeric(input$check1)]]<=clicks$brush$xmax)&
                           (data1$data1[,names(data)[as.numeric(input$check1)]]>=clicks$brush$xmin)&
                           (data1$data1[,names(data)[as.numeric(input$check2)]]>=clicks$brush$ymin)&
                           (data1$data1[,names(data)[as.numeric(input$check2)]]<=clicks$brush$ymax))
      }

    })

    observeEvent(input$slider2, {
      if(!is.null(input$slider2)) {
        data1$data1=data_cut(data, var = data[,names(data)[as.numeric(input$check4)]], value = input$slider2)
        group$group <- !((data1$data1[,names(data)[as.numeric(input$check1)]]<=clicks$brush$xmax)&
                           (data1$data1[,names(data)[as.numeric(input$check1)]]>=clicks$brush$xmin)&
                           (data1$data1[,names(data)[as.numeric(input$check2)]]>=clicks$brush$ymin)&
                           (data1$data1[,names(data)[as.numeric(input$check2)]]<=clicks$brush$ymax))
      }

    }
    )





    observeEvent(input$click3, {
      if(!is.null(input$brush)) {
        if(input$checkbox2) {
          group$group <- !((data1$data1[,names(data)[as.numeric(input$check1)]]<=clicks$brush$xmax)&
                             (data1$data1[,names(data)[as.numeric(input$check1)]]>=clicks$brush$xmin)&
                             (data1$data1[,names(data)[as.numeric(input$check2)]]>=clicks$brush$ymin)&
                             (data1$data1[,names(data)[as.numeric(input$check2)]]<=clicks$brush$ymax))
        } else {
          group$group <- !((data[,names(data)[as.numeric(input$check1)]]<=clicks$brush$xmax)&
                             (data[,names(data)[as.numeric(input$check1)]]>=clicks$brush$xmin)&
                             (data[,names(data)[as.numeric(input$check2)]]>=clicks$brush$ymin)&
                             (data[,names(data)[as.numeric(input$check2)]]<=clicks$brush$ymax))
        }

      }
    })
    output$plot1 <- renderPlot(height = height[1],{
      if(input$checkbox2) {
        if(!is.null(input$slider2)) {
          if(input$checkbox) {
            a=scatterplot3d(data1$data1[,names(data)[as.numeric(input$check1)]], data1$data1[,names(data)[as.numeric(input$check2)]],data1$data1[,names(data)[as.numeric(input$check3)]], highlight.3d=F, col.axis="black",
                            type="h", col.grid="grey", main="", pch=19,angle=input$slider,xlab=paste(names(choices)[as.numeric(input$check1)]),ylab=paste(names(choices)[as.numeric(input$check2)]),
                            zlab=paste(names(choices)[as.numeric(input$check3)]), xlim=c(min(data[,names(data)[as.numeric(input$check1)]]), max(data[,names(data)[as.numeric(input$check1)]])),
                            ylim=c(min(data[,names(data)[as.numeric(input$check2)]]), max(data[,names(data)[as.numeric(input$check2)]])),
                            zlim=c(min(data[,names(data)[as.numeric(input$check3)]]), max(data[,names(data)[as.numeric(input$check3)]])))
            data2=data1$data1[!group$group,]
            a$points3d(data2[,names(data)[as.numeric(input$check1)]], data2[,names(data)[as.numeric(input$check2)]],
                       data2[,names(data)[as.numeric(input$check3)]], pch=19,col="red")


          } else {
            a=scatterplot3d(data1$data1[,names(data)[as.numeric(input$check1)]], data1$data1[,names(data)[as.numeric(input$check2)]],data1$data1[,names(data)[as.numeric(input$check3)]], highlight.3d=F, col.axis="black",
                            col.grid="grey", main="", pch=19,angle=input$slider,xlab=paste(names(choices)[as.numeric(input$check1)]),ylab=paste(names(choices)[as.numeric(input$check2)]),
                            zlab=paste(names(choices)[as.numeric(input$check3)]),xlim=c(min(data[,names(data)[as.numeric(input$check1)]]), max(data[,names(data)[as.numeric(input$check1)]])),
                            ylim=c(min(data[,names(data)[as.numeric(input$check2)]]), max(data[,names(data)[as.numeric(input$check2)]])),
                            zlim=c(min(data[,names(data)[as.numeric(input$check3)]]), max(data[,names(data)[as.numeric(input$check3)]])))
            data2=data1$data1[!group$group,]
            a$points3d(data2[,names(data)[as.numeric(input$check1)]], data2[,names(data)[as.numeric(input$check2)]],
                       data2[,names(data)[as.numeric(input$check3)]], pch=19,col="red")
          }
        }
      } else {
        if(!is.null(input$slider2)) {
          if(input$checkbox) {
            a=scatterplot3d(x=data[,names(data)[as.numeric(input$check1)]], y=data[,names(data)[as.numeric(input$check2)]],z=data[,names(data)[as.numeric(input$check3)]], highlight.3d=F, col.axis="black",
                            type="h", col.grid="grey", main="", pch=19,angle=input$slider,xlab=paste(names(choices)[as.numeric(input$check1)]),ylab=paste(names(choices)[as.numeric(input$check2)]),
                            zlab=paste(names(choices)[as.numeric(input$check3)]), xlim=c(min(data[,names(data)[as.numeric(input$check1)]]), max(data[,names(data)[as.numeric(input$check1)]])),
                            ylim=c(min(data[,names(data)[as.numeric(input$check2)]]), max(data[,names(data)[as.numeric(input$check2)]])),
                            zlim=c(min(data[,names(data)[as.numeric(input$check3)]]), max(data[,names(data)[as.numeric(input$check3)]])))
            data2=data[!group$group,]
            a$points3d(data2[,names(data)[as.numeric(input$check1)]], data2[,names(data)[as.numeric(input$check2)]],data2[,names(data)[as.numeric(input$check3)]], pch=19,col="red")

          } else {
            a=scatterplot3d(x=data[,names(data)[as.numeric(input$check1)]], y=data[,names(data)[as.numeric(input$check2)]],z=data[,names(data)[as.numeric(input$check3)]], highlight.3d=F, col.axis="black",
                            col.grid="grey", main="", pch=19,angle=input$slider,xlab=paste(names(choices)[as.numeric(input$check1)]),ylab=paste(names(choices)[as.numeric(input$check2)]),
                            zlab=paste(names(choices)[as.numeric(input$check3)]),xlim=c(min(data[,names(data)[as.numeric(input$check1)]]), max(data[,names(data)[as.numeric(input$check1)]])),
                            ylim=c(min(data[,names(data)[as.numeric(input$check2)]]), max(data[,names(data)[as.numeric(input$check2)]])),
                            zlim=c(min(data[,names(data)[as.numeric(input$check3)]]), max(data[,names(data)[as.numeric(input$check3)]])))
            data2=data[!group$group,]
            a$points3d(data2[,names(data)[as.numeric(input$check1)]], data2[,names(data)[as.numeric(input$check2)]],data2[,names(data)[as.numeric(input$check3)]], pch=19,col="red")

          }
        }
      }



    })

    output$plot2= renderPlot(height = height[2],{
      if(input$checkbox2) {

        plot(data1$data1[,names(data)[as.numeric(input$check1)]], data1$data1[,names(data)[as.numeric(input$check2)]],
             pch=19,xlab=paste(names(data)[as.numeric(input$check1)]),
             ylab=paste(names(data)[as.numeric(input$check2)]))
        data2=data1$data1[!group$group,]
        points(data2[,names(data)[as.numeric(input$check1)]], data2[,names(data)[as.numeric(input$check2)]],
               pch=19,col="red")
      } else {
        plot(data[,names(data)[as.numeric(input$check1)]], data[,names(data)[as.numeric(input$check2)]],
             pch=19,xlab=paste(names(data)[as.numeric(input$check1)]),
             ylab=paste(names(data)[as.numeric(input$check2)]))
        data2=data[!group$group,]
        points(data2[,names(data)[as.numeric(input$check1)]], data2[,names(data)[as.numeric(input$check2)]],
               pch=19,col="red")
      }
    })
  }
  shinyApp(ui = ui, server = server)
}
