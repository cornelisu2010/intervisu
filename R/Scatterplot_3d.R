#' Interactive three-dimensional scatterlpot
#'
#' Takes any metric data and plots a three-dimensional scatterplot and a two-dimensional scatterplot.
#'
#' @param data A data.frame object that is to be analyzed (only metric variables are shown)
#' @param n A numeric value indicating the limit from what number of different values a variable is seen as categorical variable, all variables that have more than n different values are being treated as metric values
#' @param height A two-dimensional numeric vector indicating the height of the shown three-dimensional and two-dimensional scatterplots
#' @author Cornelius Fritz <cornelius.fritz@campus.lmu.de>
#' @example demo/demo4.R
#' @details This function produces an application that lets you plot a interactive three-dimensional scatterplot, you can decide what metric variables should be plotted. Next to the
#' three-dimensional scatterplot there is a two-dimensional scatterplot of the first two dimensions of the three-dimensional scatterplot (1.Variable and 2. Variable). You can change the plotted angle
#' interactively by clicking and brushing over the shown three-dimensional scatterplot with the cursor. You can press the little 'Play'-Button and it will automatically rotate the plot once.
#' There is also a feature added in this application, which lets you
#' condition both plots on a fourth variable. To enter this mode press the button called 'Condition plot only values conditioned on the value of a fourth variable. You can now set a value for the fourth variable with a slider.
#' Only 30% of the data around that value will now be plotted.
#' @export
#' @import threejs
#'

Scatterplot_3d = function(data,n,height=c(1500,500)) {
  data_cut= function(data, var,value, prob=0.15) {
    b=value
    d=c()
    for(i in seq(0,1,by=0.001)){
      c=quantile(var,probs = i)
      if(abs(b-c)<0.1) {
        d=i
      }
    }
    if(length(d)==0) {
      return(data)
    }
    if(d+prob>=1){
      a=c(quantile(var,probs =d-prob),max(var))
    } else if(d-prob<=0){
      a=c(min(var),quantile(var,probs =d+prob))
    } else {
      a=quantile(var,probs = c(d-prob,d+prob))
    }

    data=data[(var<a[2])&(var>a[1]),]
    return(data)
  }


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
                               selected = 2))
        ),
        fluidRow(
          column(6,selectInput("check3", label = h3("3. Variable"),
                               choices = choices,
                               selected = 3)),
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
        checkboxInput("checkbox2", label = "Condintion plot on a fourth variable", value = F),
        plotOutput("plot2",height = height[2], click = "click3", brush = brushOpts(id = "brush"))
      ),
      mainPanel(
        scatterplotThreeOutput("plot1",width = 800,height = 800)
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
    output$plot1 <- renderScatterplotThree({
      if(input$checkbox2) {
        if(!is.null(input$slider2)) {
          a=scatterplot3js(x = data1$data1[,names(data)[as.numeric(input$check1)]],y =  data1$data1[,names(data)[as.numeric(input$check2)]],z = data1$data1[,names(data)[as.numeric(input$check3)]],
                           size = 0.6,bg="white",axisLabels=c(paste(names(choices)[as.numeric(input$check1)]),paste(names(choices)[as.numeric(input$check3)]),paste(names(choices)[as.numeric(input$check2)])),color = "black",
                           xlim=c(min(data[,names(data)[as.numeric(input$check1)]]), max(data[,names(data)[as.numeric(input$check1)]])),
                           ylim=c(min(data[,names(data)[as.numeric(input$check2)]]), max(data[,names(data)[as.numeric(input$check2)]])),
                           zlim=c(min(data[,names(data)[as.numeric(input$check3)]]), max(data[,names(data)[as.numeric(input$check3)]])),renderer="canvas",height = 1500,width = 1500,axis = F)



          data2=data1$data1[!group$group,]
          a$points3d(data2[,names(data)[as.numeric(input$check1)]], data2[,names(data)[as.numeric(input$check2)]],
                     data2[,names(data)[as.numeric(input$check3)]],size = 0.6,col="red",stroke="red")
        }
      } else {
        if(!is.null(input$slider2)) {
          a=scatterplot3js(x=data[,names(data)[as.numeric(input$check1)]], y=data[,names(data)[as.numeric(input$check2)]],z=data[,names(data)[as.numeric(input$check3)]],
                           size = 0.6,bg="white",axisLabels=c(paste(names(choices)[as.numeric(input$check1)]),paste(names(choices)[as.numeric(input$check3)]),paste(names(choices)[as.numeric(input$check2)])),color = "black",
                           xlim=c(min(data[,names(data)[as.numeric(input$check1)]]), max(data[,names(data)[as.numeric(input$check1)]])),
                           ylim=c(min(data[,names(data)[as.numeric(input$check2)]]), max(data[,names(data)[as.numeric(input$check2)]])),
                           zlim=c(min(data[,names(data)[as.numeric(input$check3)]]), max(data[,names(data)[as.numeric(input$check3)]])),renderer="canvas",height = 1000,width = 1000,axis = F)
          data2=data[!group$group,]
          a$points3d(data2[,names(data)[as.numeric(input$check1)]], data2[,names(data)[as.numeric(input$check2)]],
                     data2[,names(data)[as.numeric(input$check3)]],col="red", size = 0.6,stroke="red")
        }
      }



    })

    output$plot2= renderPlot(height = height[2],{
      if(input$checkbox2) {
        if(!is.null(input$slider2)) {
          plot(data1$data1[,names(data)[as.numeric(input$check1)]], data1$data1[,names(data)[as.numeric(input$check2)]],
               pch=19,xlab=paste(names(data)[as.numeric(input$check1)]),
               ylab=paste(names(data)[as.numeric(input$check2)]))
          data2=data1$data1[!group$group,]
          points(data2[,names(data)[as.numeric(input$check1)]], data2[,names(data)[as.numeric(input$check2)]],
                 pch=19,col="red")
        }
      } else {
        if(!is.null(input$slider2)) {
          plot(x=data[,names(data)[as.numeric(input$check1)]],y= data[,names(data)[as.numeric(input$check2)]],
               pch=19,xlab=paste(names(data)[as.numeric(input$check1)]),
               ylab=paste(names(data)[as.numeric(input$check2)]))
          data2=data[!group$group,]
          points(data2[,names(data)[as.numeric(input$check1)]], data2[,names(data)[as.numeric(input$check2)]],
                 col="red",pch=19)
        }
      }
    })
  }
  shinyApp(ui = ui, server = server)
}
