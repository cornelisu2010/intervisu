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

Scatterplot_3d = function(data,n=10,height=c(1500,500)) {
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
        plotly::plotlyOutput("plot2",height = height[2])
      ),
      mainPanel(
        plotly::plotlyOutput("plot1",width = height[1],height = height[1])
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

    data1 <- reactiveValues(
      data1=NULL
    )





    observeEvent(input$checkbox2, {
      if(!is.null(input$slider2)) {
        data1$data1=data_cut(data, var = data[,names(data)[as.numeric(input$check4)]], value = input$slider2)
      }

    })

    observeEvent(input$slider2, {
      if(!is.null(input$slider2)) {
        data1$data1=data_cut(data, var = data[,names(data)[as.numeric(input$check4)]], value = input$slider2)
      }

    }
    )

    output$plot1 <- renderPlotly({

      if(input$checkbox2) {

        if(!is.null(input$slider2)) {
          group=list(group=rep(F,nrow(data1$data1)))
          data3=event_data("plotly_selected")
          if(!is.null(data3)){
            group$group[data3$key]=T
          }
          if(length(group$group)!=length(data1$data1[,names(data)[as.numeric(input$check1)]])) {
            group=list(group=rep(F,nrow(data1$data1)))
          }

          x=data1$data1[,names(data)[as.numeric(input$check1)]]
          y=data1$data1[,names(data)[as.numeric(input$check2)]]
          z=data1$data1[,names(data)[as.numeric(input$check3)]]
          obs=cbind(x,y,z)
          # collect everything in a data-frame
          df <- stats::setNames(data.frame(obs), c(names(data)[as.numeric(input$check1)],names(data)[as.numeric(input$check2)],names(data)[as.numeric(input$check3)]))
          color=array(dim = length(group$group),"black")
          color[group$group]="red"
          text = paste(names(data)[as.numeric(input$check1)],":", data1$data1[,names(data)[as.numeric(input$check1)]],"<br>",names(data)[as.numeric(input$check2)],":", data1$data1[,names(data)[as.numeric(input$check2)]],
                       "<br>",names(data)[as.numeric(input$check3)],":", data1$data1[,names(data)[as.numeric(input$check3)]])

          plotly::plot_ly(df, x = x, y = y, z = z, type = "scatter3d", mode = "markers", marker = list(size = 5,color=color),text = text,hoverinfo="text") %>%
            layout(scene = list(xaxis = list(title = names(data)[as.numeric(input$check1)]),
                                yaxis = list(title = names(data)[as.numeric(input$check2)]),
                                zaxis = list(title = names(data)[as.numeric(input$check3)])), showlegend = FALSE)



        } else {
          plotly::plotly_empty()
        }
      } else {
        if(!is.null(input$slider2)) {
          data3=event_data("plotly_selected")
          group=list(group=rep(F,nrow(data)))

          if(!is.null(data3)){
            group$group[data3$key]=T
          }
          if(length(group$group)!=length(data[,names(data)[as.numeric(input$check1)]])) {
            group=list(group=rep(F,nrow(data)))
          }

          x=data[,names(data)[as.numeric(input$check1)]]
          y=data[,names(data)[as.numeric(input$check2)]]
          z=data[,names(data)[as.numeric(input$check3)]]
          obs=cbind(x,y,z)
          # collect everything in a data-frame
          df <- setNames(data.frame(obs), c(names(data)[as.numeric(input$check1)],names(data)[as.numeric(input$check2)],names(data)[as.numeric(input$check3)]))
          color=array(dim = length(group$group),"black")
          color[group$group]="red"
          text = paste(names(data)[as.numeric(input$check1)],":", data[,names(data)[as.numeric(input$check1)]],"<br>",names(data)[as.numeric(input$check2)],":", data[,names(data)[as.numeric(input$check2)]],
                       "<br>",names(data)[as.numeric(input$check3)],":", data[,names(data)[as.numeric(input$check3)]])

          plotly::plot_ly(df, x = x, y = y, z = z, type = "scatter3d", mode = "markers", marker = list(size = 5,color=color),text = text,hoverinfo="text") %>%
            layout(scene = list(xaxis = list(title = names(data)[as.numeric(input$check1)]),
                                yaxis = list(title = names(data)[as.numeric(input$check2)]),
                                zaxis = list(title = names(data)[as.numeric(input$check3)])), showlegend = FALSE)
        }else {
          plotly::plotly_empty()
        }
      }



    })

    output$plot2= renderPlotly({
      if(input$checkbox2) {
        if(!is.null(input$slider2)) {

          if(!is.null(data1$data1)) {
            data4=data1$data1
            group1=list(group=rep(F,nrow(data1$data1)))
            data3=event_data("plotly_selected")
            if(!is.null(data3)){
              group1$group[data3$key]=T
            }

            x=data4[,names(data)[as.numeric(input$check1)]]
            y=data4[,names(data)[as.numeric(input$check2)]]
            data4$key=seq(1,nrow(data4))
            if(length(group1$group)==length(x)) {
              p <- ggplot(data4, aes(x = x, y = y,key=data4$key, colour=group1$group)) +
                geom_point(size = 1.3) +
                scale_colour_manual(values=c("black","red")) +
                guides(fill=F) +
                theme_bw() +
                labs(x=paste(names(data)[as.numeric(input$check1)]),
                     y=paste(names(data)[as.numeric(input$check2)]))
            } else {
              p <- ggplot(data4, aes(x = x, y = y,key=data4$key)) +
                geom_point(size = 1.3) +
                scale_colour_manual(values=c("black","red")) +
                guides(fill=F) +
                theme_bw() +
                labs(x=paste(names(data)[as.numeric(input$check1)]),
                     y=paste(names(data)[as.numeric(input$check2)]))
            }

            ggplotly(p,tooltip = c("x","y")) %>%
              plotly::layout(dragmode = "select")


          }

        } else {
          plotly_empty()
        }
      } else if (!input$checkbox2){
        if(!is.null(input$slider2)) {
          group=list(group=rep(F,nrow(data)))
          data3=event_data("plotly_selected")
          if(!is.null(data3)){
            group$group[data3$key]=T
          }

          x=data[,names(data)[as.numeric(input$check1)]]
          y=data[,names(data)[as.numeric(input$check2)]]
          data$key=seq(1,nrow(data))
          p <- ggplot(data, aes(x = x, y = y,key=data$key, colour=group$group)) +
            geom_point(size = 1.3) +
            scale_colour_manual(values=c("black","red")) +
            guides(fill=F) +
            theme_bw() +
            labs(x=paste(names(data)[as.numeric(input$check1)]),
                 y=paste(names(data)[as.numeric(input$check2)]))
          ggplotly(p,tooltip = c("x","y")) %>%
            plotly::layout(dragmode = "select")



        } else {
          plotly_empty()
        }
      } else {
        plotly_empty()
      }
    })
  }
  shinyApp(ui = ui, server = server)
}
