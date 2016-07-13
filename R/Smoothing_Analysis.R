#' Interactive smoothing with two metric variables
#'
#' Takes any metric data and plots a two-dimensional scatterplot. You can interactively build a polynomial or smoothing spline fit and plot those fits with a confidence band.
#'
#' @param data A data.frame object that is to be analyzed (only metric variables are shown)
#' @param n A numeric value indicating the limit from what number of different values a variable is seen as categorical variable, all variables that have more than n different values are being treated as metric values
#' @param height A numeric value indicating the height of the shown scatterplot
#' @author Cornelius Fritz <cornelius.fritz@campus.lmu.de>
#' @example demo/demo3.R
#' @details On the sidebar you can chose two metric variables from the data set to be plotted in a scatterplot. If you wish you can also transform any of the used variables (options are: identity, log, exp, sin, cos).
#' On the right side you see the R^2 values of the current model. To fit a polynomial model chose the 'Polynomial'-option in the type of regression.With a numeric input you can change the degree of the fit.
#' The other option is to fit "Polynomial-Splines", one can do that by selecting the according type of regression.
#' At first you can set the amount of knots and decide weather you want to penalize the fit. You can also use different kind of smoothing basis you want to use. There are three different possible smoothing bases:
#' thing plate regression splines, cubic regression splinesand p-splines.
#' If you want to penalize the fit check the box 'Penalize', you can set the numeric value lambda, indicating the importance of the penalty term. In case you are fitting a p-Spline Basis without a penalty the model is equivalent to b-splines, so you can also set the order of the fit.
#' The last thing you are also free to change is the family and link-Function used in the the model. For further reading on these options see the documentation in the stats-Package (\code{\link{family}} and \code{\link{make.link}}).
#' @export
#'
#'



Smoothing_Analysis = function(data, n=10,height=500) {
  data=data[complete.cases(data),]

  data=faktor2(data,n)[[1]]
  vec=faktor2(data,n)[[2]]
  if(sum(!vec)<=1) {
    stop("Error: In order to use this function there are at lease two metric variables needed")
  }
  choices = list()
  for (i in 1: length(names(data)[!vec])) {
    choices[[i]]=i
  }
  names(choices)=names(data)[!vec]

  choices1 = list()
  for (i in 1: length(names(data)[!vec])) {
    choices1[[i]]=i
  }
  names(choices1)=names(data)[!vec]

  type=list()
  type$Polynomial=1
  type$"Polynomial-Splines"=2

  basis=list()
  basis$"Thin plate regression splines"='tp'
  basis$"Cubic regression splines"='cr'
  basis$"P-splines"='ps'

  ui <- fluidPage(
    tags$div( HTML(
      "<h2><center>Smoothing Analysis</center></h2>")
    ),

    sidebarLayout(
      sidebarPanel(
        selectInput("check1", label = "1. Metric Variable",
                    choices = choices,
                    selected = 1),
        selectInput("check2", label = "2. Metric Variable",
                    choices = choices,
                    selected = 2),
        selectInput("check3", label = "Type of Regression",
                    choices = type,
                    selected = 1),
        sliderInput("num8",label = "Alpha",min = 0.00001,max = 0.2,value = 0.05),
        conditionalPanel(
          condition = "input.check3 == 1",
          numericInput("num",label = "Degree",value = 1)
        ),
        conditionalPanel(
          condition = "input.check3 == 2",
          selectInput("check5", label = "Base-Function",
                      choices = basis,
                      selected = "tp"),
          checkboxInput("check6",label = "Penalize",value = F),
          conditionalPanel(
            condition = "input.check5 == 'ps' & input.check6 == false",
            splitLayout(cellWidths = c("50%", "50%"),numericInput("num2",label = "Degree",value = 1),
                        numericInput("num3",label = "Knots",value = 1))
          ),
          conditionalPanel(
            condition = "input.check5 != 'ps' | (input.check5 == 'ps' & input.check6 == true)",
            numericInput("num5",label = "Knots",value = 2),
            conditionalPanel(
              condition = "input.check6 == true",
              splitLayout(cellWidths = c("50%", "50%"),numericInput("num7",label = "Lambda",value = 1))
            )
          ),
          fluidRow(
            column(6,uiOutput("dynamic1")),
            column(6,uiOutput("dynamic2"))
          )
          ),
        fluidRow(
          column(6,uiOutput("dynamic3")),
          column(6,uiOutput("dynamic4"))
        )
      ),
      mainPanel(
        splitLayout(cellWidths = c("65%", "35%"),plotOutput("plot1",height = height),plotOutput("plot2",height = height))
      )
    )
  )



  server <- function(input, output) {

    values= reactiveValues(
      a=NULL ,b=NULL, R=NULL, a_id=NULL,b_id=NULL
    )

    observeEvent(input$check1, {
      if(!is.null(input$transform_x) &!is.null(input$transform_y)) {
        if(input$transform_x=="id") {
          values$a=data[,names(choices)[as.numeric(input$check1)]]
        } else if(input$transform_x=="log") {
          values$a=log(data[,names(choices)[as.numeric(input$check1)]])
        } else if(input$transform_x=="exp") {
          values$a=exp(data[,names(choices)[as.numeric(input$check1)]])
        }else if(input$transform_x=="sin") {
          values$a=sin(data[,names(choices)[as.numeric(input$check1)]])
        }else if(input$transform_x=="cos") {
          values$a=cos(data[,names(choices)[as.numeric(input$check1)]])
        }

        if(input$transform_y=="id") {
          values$b=data[,names(choices)[as.numeric(input$check2)]]
        } else if(input$transform_y=="log") {
          values$b=log(data[,names(choices)[as.numeric(input$check2)]])
        } else if(input$transform_y=="exp") {
          values$b=exp(data[,names(choices)[as.numeric(input$check2)]])
        }else if(input$transform_y=="sin") {
          values$b=sin(data[,names(choices)[as.numeric(input$check2)]])
        }else if(input$transform_y=="cos") {
          values$b=cos(data[,names(choices)[as.numeric(input$check2)]])
        }
      }
    })

    observeEvent(input$check2, {
      if(!is.null(input$transform_x) &!is.null(input$transform_y)) {
        if(input$transform_x=="id") {
          values$a=data[,names(choices)[as.numeric(input$check1)]]
        } else if(input$transform_x=="log") {
          values$a=log(data[,names(choices)[as.numeric(input$check1)]])
        } else if(input$transform_x=="exp") {
          values$a=exp(data[,names(choices)[as.numeric(input$check1)]])
        }else if(input$transform_x=="sin") {
          values$a=sin(data[,names(choices)[as.numeric(input$check1)]])
        }else if(input$transform_x=="cos") {
          values$a=cos(data[,names(choices)[as.numeric(input$check1)]])
        }

        if(input$transform_y=="id") {
          values$b=data[,names(choices)[as.numeric(input$check2)]]
        } else if(input$transform_y=="log") {
          values$b=log(data[,names(choices)[as.numeric(input$check2)]])
        } else if(input$transform_y=="exp") {
          values$b=exp(data[,names(choices)[as.numeric(input$check2)]])
        }else if(input$transform_y=="sin") {
          values$b=sin(data[,names(choices)[as.numeric(input$check2)]])
        }else if(input$transform_y=="cos") {
          values$b=cos(data[,names(choices)[as.numeric(input$check2)]])
        }
      }

    })

    observeEvent(input$transform_x, {
      if(input$transform_x=="id") {
        values$a=data[,names(choices)[as.numeric(input$check1)]]
      } else if(input$transform_x=="log") {
        values$a=log(data[,names(choices)[as.numeric(input$check1)]])
      } else if(input$transform_x=="exp") {
        values$a=exp(data[,names(choices)[as.numeric(input$check1)]])
      }else if(input$transform_x=="sin") {
        values$a=sin(data[,names(choices)[as.numeric(input$check1)]])
      }else if(input$transform_x=="cos") {
        values$a=cos(data[,names(choices)[as.numeric(input$check1)]])
      }

      if(input$transform_y=="id") {
        values$b=data[,names(choices)[as.numeric(input$check2)]]
      } else if(input$transform_y=="log") {
        values$b=log(data[,names(choices)[as.numeric(input$check2)]])
      } else if(input$transform_y=="exp") {
        values$b=exp(data[,names(choices)[as.numeric(input$check2)]])
      }else if(input$transform_y=="sin") {
        values$b=sin(data[,names(choices)[as.numeric(input$check2)]])
      }else if(input$transform_y=="cos") {
        values$b=cos(data[,names(choices)[as.numeric(input$check2)]])
      }
    })

    observeEvent(input$transform_y, {
      if(input$transform_x=="id") {
        values$a=data[,names(choices)[as.numeric(input$check1)]]
      } else if(input$transform_x=="log") {
        values$a=log(data[,names(choices)[as.numeric(input$check1)]])
      } else if(input$transform_x=="exp") {
        values$a=exp(data[,names(choices)[as.numeric(input$check1)]])
      }else if(input$transform_x=="sin") {
        values$a=sin(data[,names(choices)[as.numeric(input$check1)]])
      }else if(input$transform_x=="cos") {
        values$a=cos(data[,names(choices)[as.numeric(input$check1)]])
      }

      if(input$transform_y=="id") {
        values$b=data[,names(choices)[as.numeric(input$check2)]]
      } else if(input$transform_y=="log") {
        values$b=log(data[,names(choices)[as.numeric(input$check2)]])
      } else if(input$transform_y=="exp") {
        values$b=exp(data[,names(choices)[as.numeric(input$check2)]])
      }else if(input$transform_y=="sin") {
        values$b=sin(data[,names(choices)[as.numeric(input$check2)]])
      }else if(input$transform_y=="cos") {
        values$b=cos(data[,names(choices)[as.numeric(input$check2)]])
      }
    })

    output$dynamic1= renderUI({
      family=list()

      if(!is.element(F,values$b<=1 & values$b>=0)) {
        family$binomial="binomial"
        family$gaussian="gaussian"
        family$Gamma="Gamma"
        family$inverse.gaussian="inverse.gaussian"
        family$nb="nb"
      } else if (round_check(values$b)) {
        family$gaussian="gaussian"
        family$Gamma="Gamma"
        family$inverse.gaussian="inverse.gaussian"
        family$poisson="poisson"
        family$nb="nb"
      } else {
        family$gaussian="gaussian"
        family$Gamma="Gamma"
        family$inverse.gaussian="inverse.gaussian"
        family$nb="nb"
      }


      selectInput("check4", label = "Family",
                  choices = family,
                  selected = "gaussian")
    }

    )

    output$dynamic2= renderUI({

      link=list()

      if(!is.null(input$check4)) {
        if(input$check4=="inverse.gaussian") {
          link$"identity"='identity'
          link$"inverse"='inverse'
          link$"log"='log'
          link$"1/mu^2"='1/mu^2'
        } else if(input$check4=="Gamma") {
          link$"identity"='identity'
          link$"inverse"='inverse'
          link$"log"='log'
        } else if(input$check4=="gaussian") {
          link$"identity"='identity'
          link$"inverse"='inverse'
          link$"log"='log'
        } else if(input$check4=="nb") {
          link$"identity"='identity'
          link$"sqrt"='sqrt'
        } else if(input$check4=="poisson") {
          link$"identity"='identity'
          link$"inverse"='inverse'
          link$"log"='log'
          link$"1/mu^2"='1/mu^2'
        } else if(input$check4=="binomial") {
          link$"logit"='logit'
          link$"probit"='probit'
          link$"cloglog"='cloglog'
        }
      }


      selectInput("check7", label = "Link",
                  choices = link,
                  selected = names(link)[1])
    })

    output$dynamic3 = renderUI({
      transform_opts= list()
      transform_opts$"identity"='id'
      transform_opts$"exp"='exp'
      transform_opts$"sin"='sin'
      transform_opts$"cos"='cos'

      if(!is.element(F,data[,names(choices)[as.numeric(input$check1)]]>=0)) {
        transform_opts$"Log_e"='log'
      }

      selectInput(inputId = "transform_x", label = "Transformation X",
                  selected = 'id' ,choices = transform_opts)
    })

    output$dynamic4 = renderUI({
      transform_opts= list()
      transform_opts$"identity"="id"
      transform_opts$"exp"="exp"
      transform_opts$"sin"="sin"
      transform_opts$"cos"="cos"

      if(!is.element(F,data[,names(choices)[as.numeric(input$check2)]]>=0)) {
        transform_opts$"Log_e"="log"
      }

      selectInput(inputId = "transform_y", label = "Transformation Y",
                  selected = "id" ,choices = transform_opts)
    })




    output$plot1 <- renderPlot({
      if(!is.null(input$transform_x) &!is.null(input$transform_y)) {
        if(input$transform_x!= "id" & input$transform_y!= "id") {
          plot(values$a,values$b,
               xlab=paste0(input$transform_x,"(",names(choices)[as.numeric(input$check1)],")"),
               ylab=paste0(input$transform_y,"(",names(choices)[as.numeric(input$check2)],")"))
        } else if (input$transform_x!= "id" & input$transform_y== "id") {
          plot(values$a,values$b,
               xlab=paste0(input$transform_x,"(",names(choices)[as.numeric(input$check1)],")"),
               ylab=paste(names(choices)[as.numeric(input$check2)]))
        } else if (input$transform_x == "id" & input$transform_y!= "id") {
          plot(values$a,values$b,
               xlab=paste(names(choices)[as.numeric(input$check1)]),
               ylab=paste0(input$transform_y,"(",names(choices)[as.numeric(input$check2)],")"))
        } else {
          plot(values$a,values$b,
               xlab=paste(names(choices)[as.numeric(input$check1)]),
               ylab=paste(names(choices)[as.numeric(input$check2)]))
        }


        if(as.numeric(input$check3)==1){

          if(input$num<1 | (input$num- ceiling(input$num))!=0) {
            warning("The degree of the function muste be a natural number")
          } else {
            a1=sort(values$a)
            b1=values$b[order(values$a)]
            fit1 <- polynomfit(d = input$num,x=a1,y=b1)
            values$R= fit1$r.squared
            lines(fit1$prediction_x, fit1$prediction_y, lwd = 2, col = "red")
            upr <- fit1$prediction_y + (stats::qnorm(1-input$num8/2) * fit1$se)
            lwr <- fit1$prediction_y - (stats::qnorm(1-input$num8/2) * fit1$se)
            lines(fit1$prediction_x, upr, lwd = 2, col = "blue",lty=2)
            lines(fit1$prediction_x, lwr, lwd = 2, col = "blue",lty=2)
          }

        } else {
          if(input$check5=="ps" & !input$check6){
            if(input$num2<1 | (input$num2- ceiling(input$num2))!=0) {
              warning("The degree of the function must be a natural number")
            }else if(input$num3<1 | (input$num3- ceiling(input$num3))!=0) {
              warning("The number of used knots must be a natural number")
            } else {
              a=values$a
              b=values$b
              fit=c()
              code = paste0("fit=gam(b ~ splines::bs(a, degree = ",input$num2,",df=",input$num3+1, "),family=",input$check4,"(link=",input$check7,"))")
              eval(parse(text = code))
              values$R=summary(fit)$r.sq
              new_data=data.frame(a=seq(min(a),max(a),length.out = 1000))
              p=predict.gam(fit,newdata = new_data,type = "response",se.fit = TRUE)
              upr <- p$fit + (stats::qnorm(1-input$num8/2) * p$se.fit)
              lwr <- p$fit - (stats::qnorm(1-input$num8/2) * p$se.fit)
              predicted.intervals=cbind(p$fit, upr,lwr)
              lines(new_data$a,predicted.intervals[,1],lwd=2,lty=1,col="red")
              lines(new_data$a,predicted.intervals[,2],lwd=2,lty=2, col="blue")
              lines(new_data$a,predicted.intervals[,3],lwd=2,lty=2, col="blue")
            }
          } else {
            if(input$num5<1 | (input$num5- ceiling(input$num5))!=0) {
              warning("The degree of the function must be a natural number")
            }else {
              a=values$a
              b=values$b
              if(input$check6) {
                fit=c()
                code = paste0("fit=gam(b ~ s(a, bs = '",input$check5,"',k=",input$num5+2,",m = ", input$num6,"),sp=", input$num7,",family=",input$check4,"(link=",input$check7,"))")
                eval(parse(text = code))
                values$R=summary(fit)$r.sq
                new_data=data.frame(a=seq(min(a),max(a),length.out = 1000))
                p=predict.gam(fit,newdata = new_data,type = "response",se.fit = TRUE)

                upr <- p$fit + (stats::qnorm(1-input$num8/2) * p$se.fit)
                lwr <- p$fit - (stats::qnorm(1-input$num8/2) * p$se.fit)

                predicted.intervals=cbind(p$fit, upr,lwr)
                lines(new_data$a,predicted.intervals[,1],lwd=2,lty=1,col="red")
                lines(new_data$a,predicted.intervals[,2],lwd=2,lty=2, col="blue")
                lines(new_data$a,predicted.intervals[,3],lwd=2,lty=2, col="blue")

              } else {
                fit=c()
                code = paste0("fit=gam(b ~ s(a, bs = '",input$check5,"',k=",input$num5+2,",fx=T), family=",input$check4,"(link=",input$check7,"))")
                eval(parse(text = code))
                values$R=summary(fit)$r.sq
                new_data=data.frame(a=seq(min(a),max(a),length.out = 1000))
                p=predict.gam(fit,newdata = new_data,type = "response",se.fit = TRUE)

                upr <- p$fit + (stats::qnorm(1-input$num8/2) * p$se.fit)
                lwr <- p$fit - (stats::qnorm(1-input$num8/2) * p$se.fit)

                predicted.intervals=cbind(p$fit, upr,lwr)
                lines(new_data$a,predicted.intervals[,1],lwd=2,lty=1,col="red")
                lines(new_data$a,predicted.intervals[,2],lwd=2,lty=2, col="blue")
                lines(new_data$a,predicted.intervals[,3],lwd=2,lty=2, col="blue")
              }
            }
          }


          }
        }





    },height = height)

    output$plot2= renderPlot({
      if(!is.null(values$R)) {
        plot.new()
        plot.window(xlim=c(-1,1),ylim=c(-1,1))
        text=bquote(R^2 == .(round(values$R,digits = 3)))
        text(x=0,y=0,text,cex=1.5)
      }

    },height = height)

  }
  shinyApp(ui = ui, server = server)
}

