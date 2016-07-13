#' Interactive Boxplot conditioned on a factorial variable
#'
#' Takes any metric data and plots different boxplots conditioned on a factorial variable.
#'
#' @param data A data.frame object that is to be analyzed (only metric variables are shown)
#' @param n A numeric value indicating the limit from what number of different values a variable is seen as categorical variable, all variables that have more than n different values are being treated as metric values
#' @param height A numeric value indicating the height of the shown boxplot
#' @param width A numeric value indicating the width of the shown boxplot
#' @author Cornelius Fritz <cornelius.fritz@campus.lmu.de>
#' @example demo/demo5.R
#' @details A grouped boxplot is plotted, you can decide what metric variable is plotted and according to what factorial variable the boxplot is grouped. You can also interactively melt levels of the factorial variable together.
#' In order to do this you at first need to select one level of the factorial variable by clicking on it with a single-click. The chosen boxplot will be highlighted. The other level that you want to melt together with the first one
#' is identified by a double-click. Once you clicked on one level with a single-click and on one level with a double-click, those two levels are melted into one level. The
#' labels on the x-axis are changed accordingly. You can always return to the older version of the factorial variable (with one more level than the current) by pressing the return button.
#' You can plot a horizontal line at the global mean of the metric variable by checking the box names 'Show global mean'.
#' @export
#'
#'


Group_Boxplot = function(data, n=10,width=600, height=600) {
  data=data[complete.cases(data),]
  position2=function(x, n) {
    for(i in 1: n) {
      if(abs(x-i)<=0.4) {
        return(i)
      }
    }
    return(NA)
  }
  data=faktor2(data,n)[[1]]
  vec=faktor2(data,n)[[2]]

  choices1 = list()
  for (i in 1: length(names(data)[!vec])) {
    choices1[[i]]=i
  }
  names(choices1)=names(data)[!vec]

  choices2 = list()
  for (i in 1: length(names(data)[vec])) {
    choices2[[i]]=i
  }
  names(choices2)=names(data)[vec]
  ui <- fluidPage(
    tags$div( HTML(
      "<h2><center>Grouped Boxplots</center></h2>")
    ),

    sidebarLayout(
      sidebarPanel(
        radioButtons("check1", label = "Metric variable",
                     choices = choices1,
                     selected = 1),
        radioButtons("check2", label = "Categorical variable",
                     choices = choices2,
                     selected = 1),
        checkboxInput("checkbox", label = "Show global mean", value = F),
        actionButton("back", label = "Return")
      ),
      mainPanel(
        plotOutput("Plot",height = height, width = width,click = "click1", dblclick = "click2")
      )
    )
  )



  server <- function(input, output) {


    clicks  <- reactiveValues(
      click1=NA, click2=NA
    )

    factor <- reactiveValues(
      factor=list()
    )

    count <- reactiveValues(
      count=1
    )

    col <- reactiveValues(
      col=NA
    )


    observeEvent(input$click1, {
      clicks$click1=input$click1
      if(length(factor$factor)<1) {
        factor$factor[[count$count]]=data[,names(choices2)[as.numeric(input$check2)]]

      }
      col$col=rep("white", nlevels(factor$factor[[count$count]]))
      col$col[position2(x=clicks$click1$x, n=nlevels(factor$factor[[count$count]]))]="grey"
    })

    observeEvent(input$check2, {
      factor$factor=list()

    })

    observeEvent(input$back, {
      if(count$count >1) {
        count$count=count$count-1
      }
    })

    observeEvent(input$click2, {
      clicks$click2=input$click2
      if(!is.na(clicks$click1$x)) {
        factor$factor[[count$count+1]]=factor_melt(factor_leveln1=position2(x=clicks$click1$x, n=nlevels(factor$factor[[count$count]])),
                                                 factor_leveln2 =position2(x=clicks$click2$x, n=nlevels(factor$factor[[count$count]])),
                                                 factor1 = factor$factor[[count$count]])
        col$col=rep("white", nlevels(factor$factor[[count$count]]))
        count$count=count$count+1
      }
    })


    output$Plot <- renderPlot(height = height, width = width,{
      if(length(factor$factor)<1) {
        plot(data[,names(choices2)[as.numeric(input$check2)]],data[,names(choices1)[as.numeric(input$check1)]],
             xlab = paste(names(choices2)[as.numeric(input$check2)]),
             ylab = paste(names(choices1)[as.numeric(input$check1)]))
      } else {
        plot(factor$factor[[count$count]],data[,names(choices1)[as.numeric(input$check1)]],
             ylab = paste(names(choices1)[as.numeric(input$check1)]),
             xlab = paste(names(choices2)[as.numeric(input$check2)]),col = col$col)
      }
      if(input$checkbox) {
        abline(h=mean(data[,names(choices1)[as.numeric(input$check1)]]),col="red")
      }
    })
  }
  shinyApp(ui = ui, server = server)
}

