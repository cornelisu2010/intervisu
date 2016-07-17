#' Single Variable Analysis of a metric variable
#'
#' With this application you can graphically analyse a single metric variable. From a given data-set you can plots boxplots, density-estimations and histograms, each with many interactive elements. If you
#' want to reproduce the same graph you built in the app, you can always get the code for the desired representation by clicking the 'Show R Code'-Button.
#'
#' @param data A data.frame object that is to be analyzed (only metric variables will be used in this application)
#' @param n A numeric value indicating the limit from what number of different values a variable is seen as categorical variable, all variables that have more than n different values are being treated as metric values
#' @param width A numeric value indicating the width of the shown plot
#' @param height A numeric value indicating the height of the shown plot
#' @param a A numeric value indicating how much freedom the slider telling the boxplot what xlim values should be allowed should have
#' @author Cornelius Fritz <cornelius.fritz@campus.lmu.de>
#' @example demo/demo6.R
#' @details There are several interactive elements to each interactive data visualizations. If you want to plot a \strong{boxplot} you can at first decide what metric variable
#' of the chosen data should be plotted. You can also plot the points in the boxplot by pressing the button named 'Show Points'. In order to see all observed data a jitter function
#' is used to randomize the points in the x-coordinates. Another possibility is to show a horizontal boxplot. With two sliders you can also decide what values of the observed
#' variable should be used and what values should be ploted in the boxplot. In order to see the R-code leading to the desired graphical presentation check the 'Show R Code' button.
#' You can copy and paste the code in you R console.
#' @details Plotting a \strong{density estimator} also gives you many different possibilities. At first you can decide what metric variable should be plotted. You can also change the used kernel, the options are a gaussian, epanechnikov, triangular, rectangular, cosine and opt-cosine kernel.
#' With two numeric inputs you can decide from what value to what value the slider input for the chosen bandwidth should be shown. You can also just plot only the observed
#' values of a variable that lie within a given range (with a slider you can decide what values should be used).
#' @details  The last optional graphical representation is a \strong{histogram} of the relative frequency. With a numeric input you can set the upper limit of the y-axis.
#' You can also change the origin of the histogram. The origin of a histogram is where the first block of the histogram starts, and to plot all optional values it can't be greater than
#' the smallest observed value. The last interactive parameter is the used bandwidth in the plot, this is the absolute length of each single bars in the histogram.
#' @export


Metric_Single_Variable_Analysis = function(data, n=10,a=50,width=700, height=700) {
  data=data[complete.cases(data),]

  fraction_variable = function(variable,a=100) {
    span= summary(variable)[6]-summary(variable)[1]
    return(span/a)
  }
  data=faktor2(data,n)[[1]]
  vec=faktor2(data,n)[[2]]
  if(sum(!vec)<1) {
    stop("Error: In order to use this function you need at least one metric variable")
  }

  choices = list()
  for (i in 1: length(names(data)[!vec])) {
    choices[[i]]=i
  }
  names(choices)=names(data)[!vec]


  ui <- navbarPage("Descriptive Analysis",
                   tabPanel("Boxplot",
                            sidebarLayout(
                              sidebarPanel(
                                helpText(h3("Boxplot")),
                                selectInput("select1", label = "Metric Variable",
                                            choices = choices,
                                            selected = 1),
                                checkboxInput("points", label = "Show Points", value = F),
                                checkboxInput("horizontal", label = "Plot Boxplot horizontally", value = F),
                                checkboxInput("code1", label = "Show R Code", value = F),
                                uiOutput("ui1"),
                                uiOutput("ui2")

                              ),

                              mainPanel(
                                plotOutput("plot1", width = width, height = height),
                                textOutput("code1_1"),
                                textOutput("code1_2"),
                                textOutput("code1_3"),
                                textOutput("code1_4")

                              )
                            )),
                   tabPanel("Histogram",
                            sidebarLayout(
                              sidebarPanel(
                                helpText(h3("Histogram")),
                                selectInput("select3", label = "Metric Variable",
                                            choices = choices,
                                            selected = 1),
                                checkboxInput("code3", label = "Show R Code", value = F),
                                fluidRow(column(6,numericInput("num4", label = "Upper Y-Axis Limit", value = 1))),
                                uiOutput("ui5"),
                                uiOutput("ui6"),
                                uiOutput("ui7")

                              ),
                              mainPanel(
                                plotOutput("plot3", width = width, height = height),
                                textOutput("code3_1"),
                                textOutput("code3_2"),
                                textOutput("code3_3"),
                                textOutput("code3_4"),
                                textOutput("code3_5")
                              )
                            )),
                   tabPanel("Density-Estimation",
                            sidebarLayout(
                              sidebarPanel(
                                helpText(h3("Density-Estimation")),
                                selectInput("select2", label = "Metric Variable",
                                            choices = choices,
                                            selected = 1),
                                selectInput("kernel", label = "Kernel",
                                            choices = list("Gaussian"="gaussian","Epanechnikov"="epanechnikov","Rectangular"= "rectangular",
                                                           "Triangular"="triangular","Bisquare"="biweight"),
                                            selected = "gaussian"),
                                checkboxInput("code2", label = "Show R Code", value = F),
                                fluidRow(column(6,numericInput("num1", label = "Bandwidth from", value = 0.01)),
                                         column(6,numericInput("num2", label = "Bandwidth to", value = 1.5))),
                                uiOutput("ui3"),
                                uiOutput("ui4")
                              ),

                              mainPanel(
                                plotOutput("plot2", width = width, height = height),
                                textOutput("code2_1"),
                                textOutput("code2_2")

                              )
                            ))
  )



  server <- function(input, output) {

    lim  <- reactiveValues(
      min=1,max=2
    )


    observeEvent(input$select1, {
      lim$min=min(data[,names(data)[!vec][as.numeric(input$select1)]])
      lim$max=max(data[,names(data)[!vec][as.numeric(input$select1)]])
    })

    observeEvent(input$select2, {
      lim$min=min(data[,names(data)[!vec][as.numeric(input$select2)]])
      lim$max=max(data[,names(data)[!vec][as.numeric(input$select2)]])
    })

    observeEvent(input$select3, {
      lim$min=min(data[,names(data)[!vec][as.numeric(input$select3)]])
      lim$max=max(data[,names(data)[!vec][as.numeric(input$select3)]])
    })

    output$ui1 = renderUI({

      a=sliderInput("dynamic1", "Use Data From/To",
                  min = lim$min, max = lim$max, value = c(lim$min,lim$max))
      a$children[[2]]$attribs$`data-keyboard-step`=round(a$children[[2]]$attribs$`data-keyboard-step`,digits = 2)
      a$children[[2]]$attribs$`data-step`=as.character(round(as.numeric(a$children[[2]]$attribs$`data-step`),digits = 2))
      a

    })

    output$ui2 = renderUI({

      a=sliderInput("dynamic2", "Plot Data From/To",
                    min = lim$min-fraction_variable(data[,names(data)[!vec][as.numeric(input$select1)]],a),
                    max = lim$max+fraction_variable(data[,names(data)[!vec][as.numeric(input$select1)]],a),
                    value = c(lim$min-fraction_variable(data[,names(data)[!vec][as.numeric(input$select1)]],a),lim$max+fraction_variable(data[,names(data)[!vec][as.numeric(input$select1)]],a)))
      a$children[[2]]$attribs$`data-keyboard-step`=round(a$children[[2]]$attribs$`data-keyboard-step`,digits = 2)
      a$children[[2]]$attribs$`data-step`=as.character(round(as.numeric(a$children[[2]]$attribs$`data-step`),digits = 2))
      a

    })

    output$ui3 = renderUI({
      if(input$num2>input$num1 & input$num2>0 & input$num1>0 ) {
        a=sliderInput("dynamic3", "Bandwidth",
                      min = input$num1, max =input$num2, value = (input$num2-input$num1)/2)
        a$children[[2]]$attribs$`data-keyboard-step`=round(a$children[[2]]$attribs$`data-keyboard-step`,digits = 2)
        a$children[[2]]$attribs$`data-step`=as.character(round(as.numeric(a$children[[2]]$attribs$`data-step`),digits = 2))
        a
      } else {
        a=sliderInput("dynamic3", "Bandwidth",
                      min = 0.01, max =1, value = 0.1)
        a$children[[2]]$attribs$`data-keyboard-step`=round(a$children[[2]]$attribs$`data-keyboard-step`,digits = 2)
        a$children[[2]]$attribs$`data-step`=as.character(round(as.numeric(a$children[[2]]$attribs$`data-step`),digits = 2))
        a
        warning("The numerical from argument has to be smaller than the to argument. Negative values are also forbiden for the bandwidth.")
      }


    })

    output$ui4 = renderUI({

      a=sliderInput("dynamic4", "Use Data From/To",
                    min = lim$min, max = lim$max, value = c(lim$min,lim$max))
      a$children[[2]]$attribs$`data-keyboard-step`=round(a$children[[2]]$attribs$`data-keyboard-step`,digits = 2)
      a$children[[2]]$attribs$`data-step`=as.character(round(as.numeric(a$children[[2]]$attribs$`data-step`),digits = 2))
      a

    })

    output$ui5 = renderUI({

      a=sliderInput("dynamic5", "Use Data From/To",
                    min = lim$min, max = lim$max, value = c(lim$min,lim$max))
      a$children[[2]]$attribs$`data-keyboard-step`=round(a$children[[2]]$attribs$`data-keyboard-step`,digits = 2)
      a$children[[2]]$attribs$`data-step`=as.character(round(as.numeric(a$children[[2]]$attribs$`data-step`),digits = 2))
      a

    })

    output$ui6 = renderUI({

      a=sliderInput("dynamic6", "Origin of the Histogram",
                    min = 0, max = lim$min, value = 0)
      a$children[[2]]$attribs$`data-keyboard-step`=round(a$children[[2]]$attribs$`data-keyboard-step`,digits = 2)
      a$children[[2]]$attribs$`data-step`=as.character(round(as.numeric(a$children[[2]]$attribs$`data-step`),digits = 2))
      a

    })

    output$ui7 = renderUI({

      a=sliderInput("dynamic7", "Bandwidth of the Histogram",
                    min = 0.1, max = lim$max, value = lim$max/10)
      a$children[[2]]$attribs$`data-keyboard-step`=round(a$children[[2]]$attribs$`data-keyboard-step`,digits = 2)
      a$children[[2]]$attribs$`data-step`=as.character(round(as.numeric(a$children[[2]]$attribs$`data-step`),digits = 2))
      a
    })



    output$plot1 = renderPlot(width = width,height = height,{

      if(!is.null(input$dynamic1)&!is.null(input$dynamic2)) {
        data1=data[(data[,names(data)[!vec][as.numeric(input$select1)]]<=input$dynamic1[2])&(data[,names(data)[!vec][as.numeric(input$select1)]]>=input$dynamic1[1]),]
        if(input$horizontal & input$points ) {
          boxplot(data1[,names(data)[!vec][as.numeric(input$select1)]], ylim=c(input$dynamic2[1],input$dynamic2[2]),
                  ylab=names(data)[!vec][as.numeric(input$select1)],horizontal = T,outline=FALSE)
          set.seed(123)
          stripchart(data1[,names(data)[!vec][as.numeric(input$select1)]],
                     vertical = F, method = "jitter",
                     pch = 1, col = "maroon", bg = "bisque",
                     add = TRUE)
        }
        if(!input$horizontal & input$points ) {
          boxplot(data1[,names(data)[!vec][as.numeric(input$select1)]], ylim=c(input$dynamic2[1],input$dynamic2[2]),
                  ylab=names(data)[!vec][as.numeric(input$select1)],outline=FALSE)
          set.seed(123)
          stripchart(data1[,names(data)[!vec][as.numeric(input$select1)]],
                     vertical = T, method = "jitter",
                     pch = 1, col = "maroon", bg = "bisque",
                     add = TRUE)
        }
        if(!input$horizontal & !input$points ) {
          boxplot(data1[,names(data)[!vec][as.numeric(input$select1)]], ylim=c(input$dynamic2[1],input$dynamic2[2]),
                  ylab=names(data)[!vec][as.numeric(input$select1)])

        }
        if(input$horizontal & !input$points ) {
          boxplot(data1[,names(data)[!vec][as.numeric(input$select1)]], ylim=c(input$dynamic2[1],input$dynamic2[2]),
                  ylab=names(data)[!vec][as.numeric(input$select1)],horizontal = T)

        }
      } else {
        plot.new()
      }


    })

    output$code1_1 = renderText({
      if(input$code1) {
        if(!is.null(input$dynamic1)&!is.null(input$dynamic2)) {
          if(input$horizontal & input$points ) {
            a=paste("data1=data[data$",names(data)[!vec][as.numeric(input$select1)],"<=",input$dynamic1[2],"& data$",names(data)[!vec][as.numeric(input$select1)],">=", input$dynamic1[1],",]",sep = "")
          }
          if(!input$horizontal & input$points ) {
            a=paste("data1=data[data$",names(data)[!vec][as.numeric(input$select1)],"<=",input$dynamic1[2],"& data$",names(data)[!vec][as.numeric(input$select1)],">=", input$dynamic1[1],",]",sep = "")

          }
          if(!input$horizontal & !input$points ) {
            a=paste("data1=data[data$",names(data)[!vec][as.numeric(input$select1)],"<=",input$dynamic1[2],"& data$",names(data)[!vec][as.numeric(input$select1)],">=", input$dynamic1[1],",]",sep = "")


          }
          if(input$horizontal & !input$points ) {
            a=paste("data1=data[data$",names(data)[!vec][as.numeric(input$select1)],"<=",input$dynamic1[2],"& data$",names(data)[!vec][as.numeric(input$select1)],">=", input$dynamic1[1],",]",sep = "")


          }
        } else {
          a=paste("boxplot(data$",names(data)[!vec][as.numeric(input$select1)],",ylim=","c(",lim$min,",",lim$max,")",", xlab=", names(data)[!vec][as.numeric(input$select1)],
                  ",horizontal= TRUE, outline=FALSE",sep = "")
        }
        paste(a)
      }


    })

    output$code1_2 = renderText({
      if(input$code1) {
        if(!is.null(input$dynamic1)&!is.null(input$dynamic2)) {
          if(input$horizontal & input$points ) {
            b=paste("boxplot(data1$",names(data)[!vec][as.numeric(input$select1)],",ylim=","c(",input$dynamic2[1],",",input$dynamic2[2],")",", xlab='", names(data)[!vec][as.numeric(input$select1)],
                    "',horizontal= TRUE, outline=FALSE)",sep = "")
          }
          if(!input$horizontal & input$points ) {
            b=paste("boxplot(data1$",names(data)[!vec][as.numeric(input$select1)],",ylim=","c(",input$dynamic2[1],",",input$dynamic2[2],")",", xlab='", names(data)[!vec][as.numeric(input$select1)],
                    "',horizontal= FALSE, outline=TRUE)",sep = "")

          }
          if(!input$horizontal & !input$points ) {
            b=paste("boxplot(data1$",names(data)[!vec][as.numeric(input$select1)],",ylim=","c(",input$dynamic2[1],",",input$dynamic2[2],")",", xlab='", names(data)[!vec][as.numeric(input$select1)],
                    "',horizontal= FALSE, outline=FALSE)",sep = "")

          }
          if(input$horizontal & !input$points ) {
            b=paste("boxplot(data1$",names(data)[!vec][as.numeric(input$select1)],",ylim=","c(",input$dynamic2[1],",",input$dynamic2[2],")",", xlab='", names(data)[!vec][as.numeric(input$select1)],
                    "',horizontal= TRUE, outline=FALSE)",sep = "")

          }
        } else {
          b=""
        }
        paste(b)
      }


    })

    output$code1_3 = renderText({
      if(input$code1) {
        if(!is.null(input$dynamic1)&!is.null(input$dynamic2)) {
          if(input$horizontal & input$points ) {

            c="set.seed(123)"

          }
          if(!input$horizontal & input$points ) {

            c="set.seed(123)"

          }
          if(!input$horizontal & !input$points ) {
            c=""

          }
          if(input$horizontal & !input$points ) {
            c=""

          }
        } else {
          c=""
        }
        paste(c)
      }


    })

    output$code1_4 = renderText({
      if(input$code1) {
        if(!is.null(input$dynamic1)&!is.null(input$dynamic2)) {
          if(input$horizontal & input$points ) {

            d=paste("stripchart(data1[,'",names(data)[!vec][as.numeric(input$select1)],"'],vertical = FALSE, method = 'jitter',","\n\t","pch = 1, col = 'maroon', bg = 'bisque', ","\n\t","add = TRUE)",sep = "")
          }
          if(!input$horizontal & input$points ) {

            d=paste("stripchart(data1[,'",names(data)[!vec][as.numeric(input$select1)],"'],vertical = TRUE, method = 'jitter',","\n\t","pch = 1, col = 'maroon', bg = 'bisque', ","\n\t","add = TRUE)",sep = "")
          }
          if(!input$horizontal & !input$points ) {
            d=""

          }
          if(input$horizontal & !input$points ) {
            d=""

          }
        } else {
          d=""
        }
        paste(d)
      }

    })

    output$plot2 = renderPlot(width = width,height = height,{

      if(!is.null(input$dynamic3) &!is.null(input$dynamic4)) {

        data1=data[(data[,names(data)[!vec][as.numeric(input$select2)]]<=input$dynamic4[2])&(data[,names(data)[!vec][as.numeric(input$select2)]]>=input$dynamic4[1]),]
        if(nrow(data1)==0) {
          plot(density(data[,names(data)[!vec][as.numeric(input$select2)]],kernel=input$kernel,bw = input$dynamic3),main = "Density-Estimator",
               xlab=paste(names(data)[!vec][as.numeric(input$select2)]))
        } else {
          plot(density(data1[,names(data)[!vec][as.numeric(input$select2)]],kernel=input$kernel,bw = input$dynamic3),main = "Density-Estimator",
               xlab=paste(names(data)[!vec][as.numeric(input$select2)]))
        }

      }
    })

    output$code2_1 = renderText({
      if(input$code2){
        if(!is.null(input$dynamic3)&!is.null(input$dynamic4)) {
          a=paste("data1=data[data$",names(data)[!vec][as.numeric(input$select2)],"<=",input$dynamic4[2],"& data$",names(data)[!vec][as.numeric(input$select2)],">=", input$dynamic4[1],",]",sep = "")
        } else {
          a=""

        }
        paste(a)
      }

    })

    output$code2_2 = renderText({
      if(input$code2){
        if(!is.null(input$dynamic3)&!is.null(input$dynamic4)) {

          a=paste("plot(density(data1[,'",names(data)[!vec][as.numeric(input$select2)],"'], kernel='", input$kernel,"',bw=",input$dynamic3,"), main= 'Density-Estimator', xlab='", names(data)[!vec][as.numeric(input$select2)],"')",sep = "")
        } else {
          a=""

        }
        paste(a)
      }
    })

    output$plot3 = renderPlot(width = width,height = height,{

      if(!is.null(input$dynamic5)&!is.null(input$dynamic6)&!is.null(input$dynamic7)) {
        data1=data[(data[,names(data)[!vec][as.numeric(input$select3)]]<=input$dynamic5[2])&(data[,names(data)[!vec][as.numeric(input$select3)]]>=input$dynamic5[1]),]
        if(nrow(data1)==0) {
          hist(x=data[,names(data)[!vec][as.numeric(input$select3)]] ,main = "Histogram",xlab = paste(names(data)[!vec][as.numeric(input$select3)]),freq=F, xlim=c(lim$min-1/5*abs(lim$min -lim$max), lim$max+1/10*abs(lim$min -lim$max)))
        } else {
          breaks <- c(seq(from=input$dynamic6, to=max(data1[,names(data)[!vec][as.numeric(input$select3)]]), by=input$dynamic7)) #hier werden die breaks des Histogramms definiert

            if(breaks[length(breaks)]!=max(data1[,names(data)[!vec][as.numeric(input$select3)]])) { #falls das Maximum der Daten nicht genau das Ende der breaks ist, fügen wir es noch als letzten Punkt ein
              breaks=c(breaks,max(data1[,names(data)[!vec][as.numeric(input$select3)]]))            #somit gehen die breaks immer über die gesamte Bandbreite der Daten und es kommt kein Wert öfter vor
            }
            if(!is.numeric(input$num4)) {
              warning("The greater plot limit of a histogram needs to be a numerical value.")
              hist(x=data1[,names(data)[!vec][as.numeric(input$select3)]] ,breaks = breaks,main = "Histogram",
                   xlab = paste(names(data)[!vec][as.numeric(input$select3)]),freq=F,
                   xlim=c(lim$min-1/5*abs(lim$min -lim$max),lim$max+1/10*abs(lim$min -lim$max)))

            }else if(input$num4>0){
              hist(x=data1[,names(data)[!vec][as.numeric(input$select3)]] ,breaks = breaks,main = "Histogram",xlab = paste(names(data)[!vec][as.numeric(input$select3)]),freq=F, xlim=c(lim$min-1/5*abs(lim$min -lim$max), lim$max+1/10*abs(lim$min -lim$max)),
                   ylim=c(0,input$num4))
            }else {
              warning("The greater plot limit of a histogram needs to be greater than zero.")
              hist(x=data1[,names(data)[!vec][as.numeric(input$select3)]] ,breaks = breaks,main = "Histogram",xlab = paste(names(data)[!vec][as.numeric(input$select3)]),freq=F, xlim=c(lim$min-1/5*abs(lim$min -lim$max), lim$max+1/10*abs(lim$min -lim$max)))
            }
          }
        }

    })

    output$code3_1 = renderText({
      if(input$code3){
        if(!is.null(input$dynamic5)&!is.null(input$dynamic6)&!is.null(input$dynamic7)) {
          a=paste("data1=data[data$",names(data)[!vec][as.numeric(input$select3)],"<=",input$dynamic5[2],"& data$",names(data)[!vec][as.numeric(input$select3)],">=", input$dynamic5[1],",]",sep = "")
        } else {a=""}

        paste(a)
      }

    })
    output$code3_2 = renderText({
      if(input$code3){
        data1=data[(data[,names(data)[!vec][as.numeric(input$select3)]]<=input$dynamic5[2])&(data[,names(data)[!vec][as.numeric(input$select3)]]>=input$dynamic5[1]),]
        if(!is.null(input$dynamic5)&!is.null(input$dynamic6)&!is.null(input$dynamic7)) {
          a=paste0("breaks <- c(seq(from=",input$dynamic6,",to=",max(data1[,names(data)[!vec][as.numeric(input$select3)]]),",by=", input$dynamic7,"))")
        } else {a=""}

        paste(a)
      }

    })

    output$code3_3 = renderText({
      if(input$code3){
        data1=data[(data[,names(data)[!vec][as.numeric(input$select3)]]<=input$dynamic5[2])&(data[,names(data)[!vec][as.numeric(input$select3)]]>=input$dynamic5[1]),]

        if(!is.null(input$dynamic5)&!is.null(input$dynamic6)&!is.null(input$dynamic7)) {
          a=paste0("if(breaks[length(breaks)]!=",max(data1[,names(data)[!vec][as.numeric(input$select3)]]),") {")
        } else {a=""}

        paste(a)
      }

    })
    output$code3_4 = renderText({
      if(input$code3){
        data1=data[(data[,names(data)[!vec][as.numeric(input$select3)]]<=input$dynamic5[2])&(data[,names(data)[!vec][as.numeric(input$select3)]]>=input$dynamic5[1]),]

        if(!is.null(input$dynamic5)&!is.null(input$dynamic6)&!is.null(input$dynamic7)) {
          a=paste0("breaks=c(breaks,",max(data1[,names(data)[!vec][as.numeric(input$select3)]]),") }")
        } else {a=""}

        paste(a)
      }

    })
    output$code3_5 = renderText({
      if(input$code3){
        if(!is.null(input$dynamic5)&!is.null(input$dynamic6)&!is.null(input$dynamic7)) {
          a=paste0("hist(x= data1[,'",names(data)[!vec][as.numeric(input$select3)],"'] ,breaks = breaks,main = 'Histogram',xlab ='", names(data)[!vec][as.numeric(input$select3)],"', freq=FALSE, ylim=c(0,",input$num4,"))")
        } else {a=""}

        paste(a)
      }

    })



  }
  shinyApp(ui = ui, server = server)
}
