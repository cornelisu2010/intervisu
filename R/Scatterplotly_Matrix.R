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






Scatterplotly_Matrix= function(data, metr_data=F,width=c(400,700,400), height=c(500,700,500)) {

  theme_bw=function (base_size = 12, base_family = "") {
    theme_grey(base_size = base_size, base_family = base_family) %+replace%
      theme(axis.text = element_text(size = rel(0.8)), axis.ticks = element_line(colour = "black"),
            legend.key = element_rect(colour = "black"),legend.position="none",
            panel.background = element_rect(fill = "white",colour = NA),
            panel.border = element_rect(fill = NA,colour = "black"),
            panel.grid.major = element_line(colour = "white",size = 0.2),
            panel.grid.minor = element_line(colour = "white",size = 0.5),
            strip.background = element_rect(fill = "black",  colour = "black", size = 0.2))
  }

  data=data[complete.cases(data),]
  faktor1=faktor(data)
  data1=data
  data=faktor1[[1]]
  position= function(x,y,data) {
    count=ncol(data)
    Ergebnis=list(c(),c())
    for ( i in 1:count) {
      if(x>(i*(1/count))) {Ergebnis[[1]][i]=TRUE} else {Ergebnis[[1]][i]=FALSE}
      if(y>(i*(1/count))) {Ergebnis[[2]][i]=TRUE} else {Ergebnis[[2]][i]=FALSE}
    }
    return(c(sum(Ergebnis[[1]])+1, count-sum(Ergebnis[[2]])))
  }

  ui <- fluidPage(
    tags$div( HTML(
      "<h2><center>Scatterplot-Matrix</center></h2>")
    ),
    splitLayout(cellWidths = width,
                cellArgs = list(style = "padding: 6px"),
                plotlyOutput("Plot2",width = width[1],height = height[1]),
                plotOutput("Plot",width = width[2],height = height[2],click="click",
                           dblclick = "dblclick"),
                plotlyOutput("Plot3",width = width[3],height = height[3])),
    fluidRow(
      column(12, selectInput("select", choices = names(data),"Variables",multiple = TRUE,width = "100%")))
    ,
    fluidRow(
      column(2, checkboxInput("regression", label = "Draw Regressionline", value = F)),
      column(2, checkboxInput("smooth", label = "Draw Loessline", value = F)),
      column(2, checkboxInput("data_show", label = "Show Data", value = F)),
      column(2, conditionalPanel(
        condition = "input.regression == true | input.smooth == true",
        checkboxInput("group", label = "By Group", value = F)
      )),
      column(4,if(metr_data){
        DT::dataTableOutput("metr")
      })
    ),br(),br(),
    fluidRow(
      column(10,offset = 1,DT::dataTableOutput("dataset"))
    )
  )

  server <- function(input, output) {

    clicks  <- reactiveValues(
      click1=NULL, dblclick=NULL,key=NULL
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

    output$Plot <- renderPlot( {

      if(length(input$select)==0){
        plot.new()
        plot.window(xlim=c(0,1),ylim=c(0,2))
        text("Please choose at least \n two metric variables \n to be able to plot \n a scatterplot-matrix",x = 0.5,y = 1.5,cex = 2)
      } else if(length(input$select)==1){
        plot.new()
        plot.window(xlim=c(0,1),ylim=c(0,2))
        text("Please choose at least \n two metric variables \n to be able to plot \n a scatterplot-matrix \n (one more needed) ",x = 0.5,y = 1.5,cex = 2)
      }

      if(length(input$select)>1) {
        data2=data[,input$select]
        group=list(group=rep(T,nrow(data2)))
        data3=event_data("plotly_selected")
        if(!is.null(data3)){
          group$group[data3$key]=F
        }
      }

      if(input$group) {
        if(length(input$select)>=2){
          if(input$regression & input$smooth) {
            if(is.null(input$click)){}
            scatterplotMatrix(data2,pch=c(19,19),legend.plot = F,groups = !group$group,smoother=loessLine,by.groups = T)
          }
          if(input$regression & !input$smooth) {
            if(is.null(input$click)){}
            scatterplotMatrix(data2,smoother = F,pch=c(19,19),legend.plot = F,groups = !group$group,by.groups = T)
          }
          if(!input$regression &input$smooth) {
            if(is.null(input$click)){}
            scatterplotMatrix(data2,reg.line = F,pch=c(19,19),legend.plot = F,groups = !group$group,smoother=loessLine,by.groups = T)
          }
          if(!input$regression & !input$smooth) {
            if(is.null(input$click)){}
            scatterplotMatrix(data2,smoother =F,reg.line = F,pch=c(19,19),legend.plot = F,groups = !group$group,by.groups = T)
          } }
        } else {
          if(length(input$select)>=2){
            if(input$regression & input$smooth) {
              if(is.null(input$click)){}
              scatterplotMatrix(data2,pch=c(19,19),legend.plot = F,groups = !group$group,smoother=loessLine)
            }
            if(input$regression & !input$smooth) {
              if(is.null(input$click)){}
              scatterplotMatrix(data2,smoother = F,pch=c(19,19),legend.plot = F,groups = !group$group)
            }
            if(!input$regression &input$smooth) {
              if(is.null(input$click)){}
              scatterplotMatrix(data2,reg.line = F,pch=c(19,19),legend.plot = F,groups = !group$group,smoother=loessLine)
            }
            if(!input$regression & !input$smooth) {
              if(is.null(input$click)){}
              scatterplotMatrix(data2,smoother =F,reg.line = F,pch=c(19,19),legend.plot = F,groups = !group$group)
            }
      }


       }


    }, height = height[2], width = width[2])

    output$Plot3 <- renderPlotly({
      if(!is.null(input$select)){
        data2=data[,input$select]
      }

      if(length(input$select)>1) {
        group=list(group=rep(F,nrow(data2)))
      }
      data3=event_data("plotly_selected")
      if(!is.null(data3)& (length(input$select)>1)){
        group$group[data3$key]=T
      }

      if(length(input$select)>1 & !is.null(clicks$dblclick)){
        x=data2[,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[1]]
        y=data2[,position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[2]]

        data2$key=seq(1,nrow(data2))
        p <- ggplot(data2, aes(x = x, y = y,key=data2$key, colour=group$group)) +
          geom_point(size = 0.7) +
          scale_colour_manual(values=c("black","red")) +
          guides(fill=F) +
          theme_bw() +
          labs(x=paste(names(data2)[position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[1]]),
               y=paste(names(data2)[position(clicks$dblclick$x,clicks$dblclick$y,data[,input$select])[2]]))
        if(input$group) {
          if(input$smooth) {
            p=p + stat_smooth(se = FALSE,size = 0.4, fill=NA)
          }
          if(input$regression) {
            p=p + stat_smooth(method=lm,se = FALSE,size = 0.4, fill=NA)
          }
        } else {
          if(input$smooth) {
            p=p + stat_smooth(se = FALSE,size = 0.4, fill=NA,colour="red")
          }
          if(input$regression) {
            p=p + stat_smooth(method=lm,se = FALSE,size = 0.4, fill=NA, colour="green")
          }
        }
        ggplotly(p,tooltip = c("x","y")) %>%
          plotly::layout(dragmode = "select")
      } else {
        plotly_empty()
      }
    })

    output$Plot2 <- renderPlotly({
      if(!is.null(input$select)){
        data2=data[,input$select]
      }

      if(length(input$select)>1) {
        group=list(group=rep(F,nrow(data2)))
      }
      data3=event_data("plotly_selected")
      if(!is.null(data3)&(length(input$select)>1)){
        group$group[data3$key]=T
      }

      if(length(input$select)>1 & !is.null(clicks$click1)){
        x=data2[,position(clicks$click1$x,clicks$click1$y,data[,input$select])[1]]
        y=data2[,position(clicks$click1$x,clicks$click1$y,data[,input$select])[2]]


        data2$key=seq(1,nrow(data2))
        p <- ggplot(data2, aes(x = x, y = y,key=data2$key, colour=group$group)) +
          geom_point(size = 0.7) +
          scale_colour_manual(values=c("black","red")) +
          labs(x=paste(names(data2)[position(clicks$click1$x,clicks$click1$y,data[,input$select])[1]]),
               y=names(data2)[position(clicks$click1$x,clicks$click1$y,data[,input$select])[2]])+
          theme_bw() +
          guides(fill=F)
        if(input$group) {
          if(input$smooth) {
            p=p + stat_smooth(se = FALSE,size = 0.4, fill=NA)
          }
          if(input$regression) {
            p=p + stat_smooth(method=lm,se = FALSE,size = 0.4, fill=NA)
          }
        } else {
          if(input$smooth) {
            p=p + stat_smooth(se = FALSE,size = 0.4, fill=NA,colour="red")
          }
          if(input$regression) {
            p=p + stat_smooth(method=lm,se = FALSE,size = 0.4, fill=NA, colour="green")
          }
        }

        if(!is.null(p)) {
          ggplotly(p,tooltip = c("x","y")) %>% plotly::layout(dragmode = "select")
        }
      } else {
        plotly_empty()
      }
    })

    output$metr <- DT::renderDataTable({

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

    output$dataset= DT::renderDataTable({
      if(length(input$select)>1) {

        data2=data[,input$select]
        group=list(group=rep(F,nrow(data2)))
        data3=event_data("plotly_selected")
        if(!is.null(data3)){
          group$group[data3$key]=T
        }

        if(input$data_show) {
          DT::datatable(data[group$group,])
        }
      }

    })
  }
  shinyApp(ui = ui, server = server)
}
