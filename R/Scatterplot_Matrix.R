
#' Interactive Scatterplot-Matrix
#'
#' Takes any data and creates an interactive Scatterplot-Matrix with multiple features.
#' @param data A data.frame object that is to be analyzed (all categorical variables with be transformed to metrical variables)
#' @param metr_data A logical value, indicating whether a table showing the metric and categorical analogies of levels should be shown
#' @param width A three-dimensional numeric value indicating the width of the three shown plots
#' @param height A three-dimensional numeric value indicating the height of the three shown plots
#' @author Cornelius Fritz <cornelius.fritz@campus.lmu.de>
#' @example demo/demo1.R
#' @details At first one has to choose the variables to be plotted in the scatterplot matrix. Once the user has choosen at least two variables (the minimum of a scatterplot),
#' those two plots are also shown on each side of the Scatterplot-Matrix.
#' The user though can decide what scatterplots out of the Matrix should be zoom in on each side. A conventional click on one scatterplot from the Scatterplot-Matrix triggers a bigger scatter plot on the left-hand side,
#' while you can control the scatterplot on the right-hand side with a double click on the desired scatterplot in the Scatterplot-Matrix.
#' On each of the small scatterplots one can select certain points with the brush option, implemented in the R-packge Shiny. Once you have chosen a cloud of data points
#' and performed a normal click, the chosen points with be colored red in all available scatterplots. If you want to see the chosen data you have to
#' press the "show data"-Button. A linear regression line and a loess curve can also be plotted by pressing the fitting button. In the case of a pressed "Regression"- or "Smooth"-Button.
#' If groups have been definied by brushing actions, you can also plot the linear and loess regression by group once you pressed the Button "By Group".
#' @export
#'


Scatterplot_Matrix= function(data, metr_data=F,width=c(400,700,400), height=c(500,700,500)) {
  data=data[complete.cases(data),]

  position= function(x,y,data) {
    count=ncol(data)
    Ergebnis=list(c(),c())
    for ( i in 1:count) {
      if(x>(i*(1/count))) {Ergebnis[[1]][i]=TRUE} else {Ergebnis[[1]][i]=FALSE}
      if(y>(i*(1/count))) {Ergebnis[[2]][i]=TRUE} else {Ergebnis[[2]][i]=FALSE}
    }
    return(c(sum(Ergebnis[[1]])+1, count-sum(Ergebnis[[2]])))
  }

  faktor1=faktor(data)
  data1=data
  data=faktor1[[1]]

  ui <- fluidPage(
    tags$div( HTML(
      "<h2><center>Scatterplot-Matrix</center></h2>")
    ),
    splitLayout(cellWidths = width,
                cellArgs = list(style = "padding: 6px"),
                plotOutput("Plot2",width = width[1],height = height[1],click="click2",
                           brush = brushOpts(id = "brush2")),
                plotOutput("Plot",width = width[2],height = height[2],click="click",
                           dblclick = "dblclick"),
                plotOutput("Plot3",width = width[3],height = height[3],click="click3",
                           brush = brushOpts(id = "brush3"))),
    fluidRow(
      column(12, selectInput("select", choices = names(data),"Variables",multiple = TRUE,width = "100%")))
    ,
    fluidRow(
      column(2, checkboxInput("regression", label = "Draw Regressionline", value = F)),
      column(2, checkboxInput("smooth", label = "Draw Loessline", value = F)),
      column(2, checkboxInput("data_show", label = "Show Data", value = F)),
      column(2, conditionalPanel(
        condition = "input.regression == true | input.smooth == true",
        checkboxInput("bygroup", label = "By Group", value = F)
      )),
      column(4,if(metr_data){
        dataTableOutput("metr")
      })
    ),br(),br(),
    fluidRow(
      column(10,offset = 1,dataTableOutput("dataset"))
    )
  )

  server <- function(input, output) {
    group  <- reactiveValues(
      group=rep(T,nrow(data))
    )

    clicks  <- reactiveValues(
      click1=NULL, dblclick=NULL, brush2=NULL, brush3=NULL
    )

    observeEvent(input$click, {
      if(length(input$select)>=2){
        clicks$click1=list(x=input$click$x,y=input$click$y)
      }

    })

    observeEvent(input$dblclick, {
      if(length(input$select)>=2){
        clicks$dblclick=list(x=input$dblclick$x,y=input$dblclick$y)
      }
    })

    observeEvent(input$brush2, {
      if(length(input$select)>=2){
        clicks$brush2=list(xmin=input$brush2$xmin,xmax=input$brush2$xmax,ymin=input$brush2$ymin,ymax=input$brush2$ymax)
      }
    })

    observeEvent(input$brush3, {
      if(length(input$select)>=2){
        clicks$brush3=list(xmin=input$brush3$xmin,xmax=input$brush3$xmax,ymin=input$brush3$ymin,ymax=input$brush3$ymax)
      }
    })

    observeEvent(input$click2, {
      if(length(input$select)>=2){
        if(!is.null(clicks$brush2)) {
          if(!is.null(clicks$click1)) {
            group$group <- !((data[,input$select][,position(clicks$click1$x,clicks$click1$y,data[,input$select])[1]]<=clicks$brush2$xmax)&
                               (data[,input$select][,position(clicks$click1$x,clicks$click1$y,data[,input$select])[1]]>=clicks$brush2$xmin)&
                               (data[,input$select][,position(clicks$click1$x,clicks$click1$y,data[,input$select])[2]]>=clicks$brush2$ymin)&
                               (data[,input$select][,position(clicks$click1$x,clicks$click1$y,data[,input$select])[2]]<=clicks$brush2$ymax))
          } else {
            group$group <- !((data[,input$select][,1]<=clicks$brush2$xmax)&
                               (data[,input$select][,1]>=clicks$brush2$xmin)&
                               (data[,input$select][,2]>=clicks$brush2$ymin)&
                               (data[,input$select][,2]<=clicks$brush2$ymax))
          }
        }
      }
    })

    observeEvent(input$click3, {
      if(length(input$select)>=2){
        if(!is.null(clicks$brush3)) {
          if(!is.null(clicks$dblclick)) {
            group$group <- !((data[,input$select][,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[1]]<=clicks$brush3$xmax)&
                               (data[,input$select][,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[1]]>=clicks$brush3$xmin)&
                               (data[,input$select][,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[2]]>=clicks$brush3$ymin)&
                               (data[,input$select][,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[2]]<=clicks$brush3$ymax))
          } else {
            group$group <- !((data[,input$select][,2]<=clicks$brush3$xmax)&
                               (data[,input$select][,2]>=clicks$brush3$xmin)&
                               (data[,input$select][,1]>=clicks$brush3$ymin)&
                               (data[,input$select][,1]<=clicks$brush3$ymax))

          }
        }
      }

    })
    output$Plot <- renderPlot({
      if(!is.null(input$select)){
        data2=data[,input$select]
      }
      if(length(input$select)==0){
        plot.new()
        plot.window(xlim=c(0,1),ylim=c(0,2))
        text("Please choose at least \n two metric variables \n to be able to plot \n a scatterplot-matrix",x = 0.5,y = 1.5,cex = 2)
      }
      if(length(input$select)==1){
        plot.new()
        plot.window(xlim=c(0,1),ylim=c(0,2))
        text("Please choose at least \n two metric variables \n to be able to plot \n a scatterplot-matrix \n (one more needed) ",x = 0.5,y = 1.5,cex = 2)

      }
      if(length(input$select)>=2){
        if(input$bygroup){
          if(input$regression & input$smooth) {
            if(is.null(input$click)){}
            if(is.null(input$click2)){}
            if(is.null(input$click3)){}
            scatterplotMatrix(data2,pch=c(19,19,19),legend.plot = F,groups = !group$group,smoother=loessLine,by.groups = T)
          }
          if(input$regression & !input$smooth) {
            if(is.null(input$click)){}
            if(is.null(input$click2)){}
            if(is.null(input$click3)){}
            scatterplotMatrix(data2,smoother = F,pch=c(19,19),legend.plot = F,groups = !group$group,by.groups = T)
          }
          if(!input$regression &input$smooth) {
            if(is.null(input$click)){}
            if(is.null(input$click2)){}
            if(is.null(input$click3)){}
            scatterplotMatrix(data2,reg.line = F,pch=c(19,19),legend.plot = F,groups = !group$group,smoother=loessLine,by.groups = T)
          }
          if(!input$regression & !input$smooth) {
            if(is.null(input$click)){}
            if(is.null(input$click2)){}
            if(is.null(input$click3)){}
            scatterplotMatrix(data2,smoother =F,reg.line = F,pch=c(19,19),legend.plot = F,groups = !group$group,by.groups = T)
          }
        } else {
          if(input$regression & input$smooth) {
            if(is.null(input$click)){}
            if(is.null(input$click2)){}
            if(is.null(input$click3)){}
            scatterplotMatrix(data2,pch=c(19,19),legend.plot = F,groups = !group$group,smoother=loessLine)
          }
          if(input$regression & !input$smooth) {
            if(is.null(input$click)){}
            if(is.null(input$click2)){}
            if(is.null(input$click3)){}
            scatterplotMatrix(data2,smoother = F,pch=c(19,19),legend.plot = F,groups = !group$group)
          }
          if(!input$regression &input$smooth) {
            if(is.null(input$click)){}
            if(is.null(input$click2)){}
            if(is.null(input$click3)){}
            scatterplotMatrix(data2,reg.line = F,pch=c(19,19),legend.plot = F,groups = !group$group,smoother=loessLine)
          }
          if(!input$regression & !input$smooth) {
            if(is.null(input$click)){}
            if(is.null(input$click2)){}
            if(is.null(input$click3)){}
            scatterplotMatrix(data2,smoother =F,reg.line = F,pch=c(19,19),legend.plot = F,groups = !group$group)
          }
        }

       }


    }, height = height[2], width = width[2])

    output$Plot2 <- renderPlot({
      if(!is.null(input$select)){
        data2=data[,input$select]
      }

      if(length(input$select)>1 & !is.null(clicks$click1)){
        if(input$bygroup& is.element(T,group$group) & is.element(F,group$group)){
          if(input$smooth) {
            scatterplot(data2[,position(clicks$click1$x,clicks$click1$y,data[,input$select])[1]],
                        data2[,position(clicks$click1$x,clicks$click1$y,data[,input$select])[2]],boxplots=F,legend.plot=F, reg.line=F,
                        xlab=paste(names(data2)[position(clicks$click1$x,clicks$click1$y,data[,input$select])[1]]),
                        ylab=paste(names(data2)[position(clicks$click1$x,clicks$click1$y,data[,input$select])[2]]),pch=c(19,19), groups=!group$group,by.groups=T,grid=F)
          } else {
            scatterplot(data2[,position(clicks$click1$x,clicks$click1$y,data[,input$select])[1]],
                        data2[,position(clicks$click1$x,clicks$click1$y,data[,input$select])[2]],boxplots=F,legend.plot=F, reg.line=F, smooth=F,
                        xlab=paste(names(data2)[position(clicks$click1$x,clicks$click1$y,data[,input$select])[1]]),
                        ylab=paste(names(data2)[position(clicks$click1$x,clicks$click1$y,data[,input$select])[2]]),pch=c(19,19), groups=!group$group,by.groups=T,grid=F)
          }
          if(input$regression) {
            if((var(data2[group$group,position(clicks$click1$x,clicks$click1$y,data[,input$select])[2]])>0) &
               (var(data2[group$group,position(clicks$click1$x,clicks$click1$y,data[,input$select])[1]])>0) &
               (var(data2[!group$group,position(clicks$click1$x,clicks$click1$y,data[,input$select])[2]])>0)&
               (var(data2[!group$group,position(clicks$click1$x,clicks$click1$y,data[,input$select])[1]])>0)) {
              abline(lm(data2[group$group,position(clicks$click1$x,clicks$click1$y,data[,input$select])[2]]~data2[group$group,position(clicks$click1$x,clicks$click1$y,data[,input$select])[1]]), col="green",lwd=2)
              abline(lm(data2[!group$group,position(clicks$click1$x,clicks$click1$y,data[,input$select])[2]]~data2[!group$group,position(clicks$click1$x,clicks$click1$y,data[,input$select])[1]]), col="darkgreen",lwd=2)
            } else {
              abline(lm(data2[,position(clicks$click1$x,clicks$click1$y,data[,input$select])[2]]~data2[,position(clicks$click1$x,clicks$click1$y,data[,input$select])[1]]), col="green",lwd=2)
            }
          }
        } else {
          if(input$smooth) {
            scatterplot(data2[,position(clicks$click1$x,clicks$click1$y,data[,input$select])[1]],
                        data2[,position(clicks$click1$x,clicks$click1$y,data[,input$select])[2]],boxplots=F,legend.plot=F, reg.line=F,
                        xlab=paste(names(data2)[position(clicks$click1$x,clicks$click1$y,data[,input$select])[1]]),
                        ylab=paste(names(data2)[position(clicks$click1$x,clicks$click1$y,data[,input$select])[2]]),pch=c(19,19), groups=!group$group,by.groups=F,grid=F)
          } else {
            scatterplot(data2[,position(clicks$click1$x,clicks$click1$y,data[,input$select])[1]],
                        data2[,position(clicks$click1$x,clicks$click1$y,data[,input$select])[2]],boxplots=F,legend.plot=F, reg.line=F, smooth=F,
                        xlab=paste(names(data2)[position(clicks$click1$x,clicks$click1$y,data[,input$select])[1]]),
                        ylab=paste(names(data2)[position(clicks$click1$x,clicks$click1$y,data[,input$select])[2]]),pch=c(19,19), groups=!group$group,by.groups=F,grid=F)
          }
          if(input$regression) {
            abline(lm(data2[,position(clicks$click1$x,clicks$click1$y,data[,input$select])[2]]~data2[,position(clicks$click1$x,clicks$click1$y,data[,input$select])[1]]), col="green",lwd=2)
          }
        }
      } else if (length(input$select)>1 & is.null(clicks$click1)){
        if(input$bygroup){
          if(!input$smooth) {
            scatterplot(data2[,1],data2[,2],boxplots=F,legend.plot=F, reg.line=F, smooth=F,
                        xlab=paste(names(data2)[1]),
                        ylab=paste(names(data2)[2]),pch=c(19,19), by.groups=T,grid=F, groups=!group$group)
          } else {
            scatterplot(data2[,1],data2[,2],boxplots=F,legend.plot=F, reg.line=F,
                        xlab=paste(names(data2)[1]),
                        ylab=paste(names(data2)[2]),pch=c(19,19), by.groups=T,grid=F, groups=!group$group)
          }
          if(input$regression) {
            abline(lm(data2[group$group,2]~data2[group$group,1]), col="green",lwd=2)
            abline(lm(data2[!group$group,2]~data2[!group$group,1]), col="darkgreen",lwd=2)
          }

        } else {
          if(!input$smooth) {
            scatterplot(data2[,1],data2[,2],boxplots=F,legend.plot=F, reg.line=F, smooth=F,
                        xlab=paste(names(data2)[1]),
                        ylab=paste(names(data2)[2]),pch=c(19,19), by.groups=F,grid=F, groups=!group$group)
          } else {
            scatterplot(data2[,1],data2[,2],boxplots=F,legend.plot=F, reg.line=F,
                        xlab=paste(names(data2)[1]),
                        ylab=paste(names(data2)[2]),pch=c(19,19), by.groups=F,grid=F, groups=!group$group)
          }
          if(input$regression) {
            abline(lm(data2[,2]~data2[,1]), col="green",lwd=2)
          }
        }



      }

    }, height = height[1], width = width[1])

    output$Plot3 <- renderPlot({
      if(!is.null(input$select)){
        data2=data[,input$select]
      }


      if(length(input$select)>1 & !is.null(clicks$dblclick)){
        if(input$bygroup){
          if(input$smooth) {
            scatterplot(data2[,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[1]],
                        data2[,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[2]],boxplots=F,legend.plot=F, reg.line=F,
                        xlab=paste(names(data2)[position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[1]]),
                        ylab=paste(names(data2)[position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[2]]),pch=c(19,19), groups=!group$group,by.groups=T,grid=F)
          } else {
            scatterplot(data2[,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[1]],
                        data2[,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[2]],boxplots=F,legend.plot=F, reg.line=F, smooth=F,
                        xlab=paste(names(data2)[position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[1]]),
                        ylab=paste(names(data2)[position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[2]]),pch=c(19,19), groups=!group$group,by.groups=T,grid=F)
          }
          if(input$regression) {
            if((var(data2[group$group,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[2]])>0) &
               (var(data2[group$group,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[1]])>0) &
               (var(data2[!group$group,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[2]])>0)&
               (var(data2[!group$group,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[1]])>0)) {
              abline(lm(data2[group$group,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[2]]~data2[group$group,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[1]]), col="green",lwd=2)
              abline(lm(data2[!group$group,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[2]]~data2[!group$group,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[1]]), col="darkgreen",lwd=2)
            } else {
              abline(lm(data2[,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[2]]~data2[,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[1]]), col="green",lwd=2)
              }
          }
        } else {
          if(input$smooth) {
            scatterplot(data2[,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[1]],
                        data2[,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[2]],boxplots=F,legend.plot=F, reg.line=F,
                        xlab=paste(names(data2)[position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[1]]),
                        ylab=paste(names(data2)[position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[2]]),pch=c(19,19), groups=!group$group,by.groups=F,grid=F)
          } else {
            scatterplot(data2[,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[1]],
                        data2[,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[2]],boxplots=F,legend.plot=F, reg.line=F, smooth=F,
                        xlab=paste(names(data2)[position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[1]]),
                        ylab=paste(names(data2)[position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[2]]),pch=c(19,19), groups=!group$group,by.groups=F,grid=F)
          }
          if(input$regression) {
              abline(lm(data2[,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[2]]~data2[,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[1]]), col="green",lwd=2)
          }
        }
      } else if (length(input$select)>1 & is.null(clicks$dblclick)){
        if(input$bygroup){
          if(!input$smooth) {
            scatterplot(data2[,2],data2[,1],boxplots=F,legend.plot=F, reg.line=F, smooth=F,
                        xlab=paste(names(data2)[2]),
                        ylab=paste(names(data2)[1]),pch=c(19,19), by.groups=T,grid=F, groups=!group$group)
          } else {
            scatterplot(data2[,2],data2[,1],boxplots=F,legend.plot=F, reg.line=F,
                        xlab=paste(names(data2)[2]),
                        ylab=paste(names(data2)[1]),pch=c(19,19), by.groups=T,grid=F, groups=!group$group)
          }
          if(input$regression) {
            abline(lm(data2[group$group,1]~data2[group$group,2]), col="green",lwd=2)
            abline(lm(data2[!group$group,1]~data2[!group$group,2]), col="darkgreen",lwd=2)
          }

        } else {
          if(!input$smooth) {
            scatterplot(data2[,2],data2[,1],boxplots=F,legend.plot=F, reg.line=F, smooth=F,
                        xlab=paste(names(data2)[2]),
                        ylab=paste(names(data2)[1]),pch=c(19,19), by.groups=F,grid=F, groups=!group$group)
          } else {
            scatterplot(data2[,2],data2[,1],boxplots=F,legend.plot=F, reg.line=F,
                        xlab=paste(names(data2)[2]),
                        ylab=paste(names(data2)[1]),pch=c(19,19), by.groups=F,grid=F, groups=!group$group)
          }
          if(input$regression) {
            abline(lm(data2[,1]~data2[,2]), col="green",lwd=2)
          }
        }



        }

    }, height = height[3], width = width[3])

    output$metr <- renderDataTable({

      level=list()

      if(sum(faktor(data1)[[2]]) > 1) {
        for(i in 1:sum(faktor(data1)[[2]])) {
          level[[i]]=data.frame(levels_kat=c(names(data1)[faktor(data1)[[2]]][i],levels(data1[,names(data1)[faktor(data1)[[2]]]][,i])),
                          levels_met=c("",1:length(levels(data1[,names(data1)[faktor(data1)[[2]]]][,i]))))
        }
      } else {
        level=data.frame(levels_kat=c(names(data1)[faktor(data1)[[2]]],levels(data1[,names(data1)[faktor(data1)[[2]]]])),
                        levels_met=c("",1:length(levels(data1[,names(data1)[faktor(data1)[[2]]]]))))
      }
      Ergebnis=data.frame()
      if(sum(faktor(data1)[[2]]) == 1) {
        Ergebnis=data.frame(level$levels_kat,level$levels_met)
      } else if (sum(faktor(data1)[[2]]) < 1) {
        Ergebnis=NULL
      }else {
        for( i in 1: (sum(faktor(data1)[[2]]))) {
          Ergebnis=rbind(Ergebnis,level[[i]])
        }
      }
      names(Ergebnis)=c("categorical names", "metric equivalent")
      row.names(Ergebnis)=NULL
      datatable(Ergebnis,options = list(dom = 't'))
    })

    output$dataset= renderDataTable({

      if(input$data_show) {
        datatable(data[!group$group,])
      }

    })
  }
  shinyApp(ui = ui, server = server)
}

