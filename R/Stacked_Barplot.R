#' Interactive stacked Barplot
#'
#' Takes any data and creates an interactive stacked Barplot with multiple features.
#'
#' @param data A data.frame object that is to be analyzed
#' @param n A numeric value indicating from what number of different values a variable is seen as categorical, all variables that have more different values than n are being treated as metric values
#' @param height A two-dimensional numeric value indicating the height of the barplot and boxplot
#' @param m A numeric value telling hiow many observations need to be in each cell to perform the approximative chi^2-Test, rather than the exact Fisher-Test(default at 5)
#' @author Cornelius Fritz <cornelius.fritz@campus.lmu.de>
#' @example demo/demo2.R
#' @details In the sidebar you can choose two categorical variables and plot a stacked Barplot, one bar for every level of the second categorical variable.
#' If you click on one of the shown barplots, a boxplot conditioned onto that specifical level of categorical data will be shown underneath the barplot. You can also decide which
#' metric variable should be plotted in the boxplot. With the frequency table of the two categorical variables ether a approximate chi^2 or exact fisher-test is computed.
#' The test can be interpreted as to weather the hypothesis of homogeneity holds true or not. In case of a 2x2 frequency table the Odds Ratio value is given as well. What
#' test is used depends on the observations in each cell, typically the count should be higher than 5 in each cell to perform the approximate test, but one can set
#' this number when one is calling the function. If one cell in the frequency table has less observations than needed for the approximate test the fisher-test is performed.
#' You can also change the grouping of factor levels of the second factorial variable.
#' One click highlights a plot (the boxplot conditioned on that specific level will be shown as well) and once you double clicked on another bar, these two factorial levels will be melted into one.
#' You can also look at the boxplot conditioned onto the new factor levels and if you want to go back to the original factor levels just press the 'back' button.
#' @export
#'



Stacked_Barplot = function(data, n=10,m=5,height=c(500,300)) {

  data=faktor2(data,n)[[1]]
  vec=faktor2(data,n)[[2]]
  if(sum(vec)<=1) {
    stop("Error: In order to use this function there are at lease two factorial variables needed")
  }
  choices = list()
  for (i in 1: length(names(data)[vec])) {
    choices[[i]]=i
  }
  names(choices)=names(data)[vec]

  choices1 = list()
  for (i in 1: length(names(data)[!vec])) {
    choices1[[i]]=i
  }
  names(choices1)=names(data)[!vec]

  ui <- fluidPage(
    tags$div( HTML(
      "<h2><center>Stacked Barplots</center></h2>")
    ),

    sidebarLayout(
      sidebarPanel(
        selectInput("check2", label = h3("1. Categorical Variable"),
                    choices = choices,
                    selected = 1),
        selectInput("check1", label = h3("2. Categorical Variable"),
                    choices = choices,
                    selected = 2),
        selectInput("select", label = h3("Metric Variable"),
                    choices = choices1,
                    selected = 1),
        actionButton("back", label = "Backwards")
      ),
      mainPanel(
        fluidRow(
          column(12,splitLayout(cellWidths = c("70%", "30%"),plotOutput("Plot1",height = height[1],click = "click1", dblclick = "click2"),plotOutput("Plot2", height = height[1])))
        ),
        fluidRow(
          column(7.6,splitLayout(cellWidths = c("65%", "35%"),plotOutput("Plot3",height = height[2]),htmlOutput("test")))
        )

      )
    )
  )



  server <- function(input, output) {

    prop  <- reactiveValues(
      prop=prop.table(table(data[,names(choices)[as.numeric(1)]],data[,names(choices)[as.numeric(2)]]),margin = 2),
      abs=table(table(data[,names(choices)[as.numeric(1)]],data[,names(choices)[as.numeric(2)]]))

    )

    click  <- reactiveValues(
      click1=NULL, click2=NULL
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
      click$click1=input$click1
      if(length(factor$factor)<=1) {
        factor$factor[[count$count]]=data[,names(choices)[as.numeric(input$check2)]]
      }
      col$col=c(heat.colors(length(rownames(prop$prop))),heat.colors(length(rownames(prop$prop)),alpha = 0.7))
    })

    observeEvent(input$back, {
      if(count$count >1) {
        count$count=count$count-1
        prop$prop=table(data[,names(choices)[as.numeric(input$check1)]],factor$factor[[count$count]])
        prop$abs=prop$prop
        prop$prop=prop.table(prop$prop,margin = 2)
      }
    })


    observeEvent(input$click2, {
      click$click2=input$click2
      if(!is.na(click$click1$x)) {
        factor$factor[[count$count+1]]=factor_melt(factor_leveln1=position3(x=click$click1$x),
                                                   factor_leveln2 =position3(x=click$click2$x),
                                                   factor1 = factor$factor[[count$count]])
        prop$prop=table(data[,names(choices)[as.numeric(input$check1)]],factor$factor[[count$count+1]])
        prop$abs=prop$prop
        prop$prop=prop.table(prop$prop,margin = 2)
        count$count=count$count+1
        col$col=heat.colors(length(rownames(prop$prop)))

      }


    })

    observeEvent(input$check1, {
      if(length(factor$factor)<1) {
        prop$prop=table(data[,names(choices)[as.numeric(input$check1)]],data[,names(choices)[as.numeric(input$check2)]])
        prop$abs=prop$prop
        prop$prop=prop.table(prop$prop,margin = 2)
        col$col=heat.colors(length(rownames(prop$prop)))

      } else {
        prop$prop=table(data[,names(choices)[as.numeric(input$check1)]],factor$factor[[count$count]])
        prop$abs=prop$prop
        prop$prop=prop.table(prop$prop,margin = 2)
        col$col=heat.colors(length(rownames(prop$prop)))

      }

    })
    observeEvent(input$check2, {
      factor$factor=list()
      col$col=heat.colors(length(rownames(prop$prop)))
      prop$prop=table(data[,names(choices)[as.numeric(input$check1)]],data[,names(choices)[as.numeric(input$check2)]])
      prop$abs=prop$prop
      prop$prop=prop.table(prop$prop,margin = 2)

    })

    output$Plot1 <- renderPlot(height = height[1],{
      if(is.null(click$click1)){
        barplot(prop$prop, ylim=c(0,1),col=heat.colors(length(rownames(prop$prop))),xlab=names(choices)[as.numeric(input$check2)])
      } else {
        if(position3(click$click1$x)> ncol(prop$prop)){
          prop1=table_transform(table = prop$prop, click = position3(click$click1$x)-1)
        } else {
          prop1=table_transform(table = prop$prop, click = position3(click$click1$x))
        }
        barplot(prop1, ylim=c(0,1),col=col$col,xlab=names(choices)[as.numeric(input$check2)])
      }
    })
    output$Plot2 <- renderPlot(height = height[1],{
      plot.new()
      legend(x=0,y=0.8, fill=heat.colors(length(rownames(prop$prop))), legend=rownames(prop$prop),title=names(choices)[as.numeric(input$check1)],cex=1.5)
    })
    output$Plot3 <- renderPlot(height = height[2],{
      if(!is.null(click$click1)){
        data=faktor2(data,n)[[1]]
        if(length(factor$factor)==0) {
          plot(data[,names(choices)[as.numeric(input$check1)]],
               data[,names(choices1)[as.numeric(input$select)]], col=heat.colors(length(rownames(prop$prop))),
               xlab=paste(names(choices)[as.numeric(input$check1)]),
               ylab=paste(names(choices1)[as.numeric(input$select)]),
               main=paste(names(choices)[as.numeric(input$check2)]))
        } else if(length(levels(factor$factor[[count$count]]))<position3(click$click1$x)) {
          data3=data
          data3[,names(choices)[as.numeric(input$check2)]]=factor$factor[[count$count]]
          data1=data3[(as.character(data3[,names(choices)[as.numeric(input$check2)]]))==(levels(factor$factor[[count$count]])[position3(click$click1$x)-1]),]
          plot(data1[,names(choices)[as.numeric(input$check1)]],
               data1[,names(choices1)[as.numeric(input$select)]], col=heat.colors(length(rownames(prop$prop))),
               xlab=paste(names(choices)[as.numeric(input$check1)]),
               ylab=paste(names(choices1)[as.numeric(input$select)]),
               main=paste(names(choices)[as.numeric(input$check2)],"=",levels(factor$factor[[count$count]])[position3(click$click1$x)-1]))
        } else if(is.null(levels(factor$factor[[count$count]])[position3(click$click1$x)-1])) {
          data1=data
          plot(data1[,names(choices)[as.numeric(input$check1)]],
               data1[,names(choices1)[as.numeric(input$select)]], col=heat.colors(length(rownames(prop$prop))),
               xlab=paste(names(choices)[as.numeric(input$check1)]),
               ylab=paste(names(choices1)[as.numeric(input$select)]))
        } else if(length(levels(factor$factor[[count$count]]))==1){
          plot(data[,names(choices)[as.numeric(input$check1)]],
               data[,names(choices1)[as.numeric(input$select)]], col=heat.colors(length(rownames(prop$prop))),
               xlab=paste(names(choices)[as.numeric(input$check1)]),
               ylab=paste(names(choices1)[as.numeric(input$select)]),
               main=paste(names(choices)[as.numeric(input$check2)],"=",levels(factor$factor[[count$count]])))
        } else {
          data3=data
          data3[,names(choices)[as.numeric(input$check2)]]=factor$factor[[count$count]]
          data1=data3[(as.character(data3[,names(choices)[as.numeric(input$check2)]]))==(levels(factor$factor[[count$count]])[position3(click$click1$x)]),]
          plot(data1[,names(choices)[as.numeric(input$check1)]],
               data1[,names(choices1)[as.numeric(input$select)]], col=heat.colors(length(rownames(prop$prop))),
               xlab=paste(names(choices)[as.numeric(input$check1)]),
               ylab=paste(names(choices1)[as.numeric(input$select)]),
               main=paste(names(choices)[as.numeric(input$check2)],"=",levels(factor$factor[[count$count]])[position3(click$click1$x)]))

        }

      }
    })

    output$test = renderUI({
      if((ncol(prop$abs)>1)&(nrow(prop$abs)>1)) {
        if(is.element(T,as.logical(prop$abs<5))) {
          warning("Some cells include less than ", m, " observations. Because of that an exact Fisher's test is computed.")
          test=fisher.test(prop$abs)
          if((ncol(prop$abs)==2)&(nrow(prop$abs)==2)){
            Ergebnis=paste0("<p><b>Fisher's Exact Test for Count Data </b></p><br> p-palue= ",round(test$p.value,digits = 8)," <br> alternative hypothesis: true odds ratio <br> is not equal to 1 <br> 95 percent confidence interval:",
                            "[",round(test$conf.int[1],digits = 3),", ",round(test$conf.int[2],digits = 3),"] <br> OR=", round(test$estimate,digits = 8))
          } else {
            Ergebnis=paste0("<p><b>Fisher's Exact Test for Count Data </b></p><br> p-palue= ",round(test$p.value,digits = 8)," <br> alternative hypothesis: two.sided")
          }
        } else {
          test=chisq.test(prop$abs)
          test$p.value
          if((ncol(prop$abs)==2)&(nrow(prop$abs)==2)){
            Ergebnis=paste0("<p><b>Pearson's Chi-squared test <br> with Yates' continuity correction </b></p><br> X-squared = ",round(test$statistic,digits = 8),
                            ",df=",round(test$parameter,digits = 1),"<br>p-value= ",round(test$p.value,digits = 8),",OR=", round(getOddsRatio(prop$abs),digits = 5))
          } else {
            Ergebnis=paste0("<p><b>Pearson's Chi-squared test </b></p><br> X-squared = ",round(test$statistic,digits = 8),
                            ",df=",round(test$parameter,digits = 1),"<br>p-value= ",round(test$p.value,digits = 8),",OR=", round(getOddsRatio(prop$abs),digits = 5))
          }
        }
        HTML(Ergebnis)
      }
    })
  }
  shinyApp(ui = ui, server = server)
}
