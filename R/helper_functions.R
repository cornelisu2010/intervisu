#' Help-Function
#'
#' This function factorizes a variables with more than n different values.
#'
#' @param data A data.frame object
#' @param n A numeric value indicating from what number of different values a variable is seen as categorical variables, all variables that have more different values than n are being treated as metric values
#' @author Cornelius Fritz <cornelius.fritz@campus.lmu.de>
#' @export



faktor2= function(data,n) {
  vec=c()
  for (i in 1: ncol(data)) {
    if( dim(table(data[,i]))<=n) {
      data[,i]=as.factor(data[,i])
      vec[i]=T
    } else {
      vec[i]=F
    }
  }
  Ergebnis=list(data, vec)
  return(Ergebnis)
}

#' Help-Function
#'
#' This function calculates a polynomial-fit
#'
#' @param d The degree of the fit
#' @param x The independent metric variable
#' @param y The dependent metric variable
#' @note The output also features a prediction of the dependent variable in the interval from min(x) to max(x)
#' @author Cornelius Fritz <cornelius.fritz@campus.lmu.de>
#' @export


polynomfit <- function(d,x,y){
  traindata <- data.frame(y = y)
  for(i in 1:d){
    traindata <- cbind(traindata, x^i)
    names(traindata)[length(traindata)] <- paste("x^",i)
  }
  x1=seq(min(x), max(x),length.out = 1000)
  testdata <- data.frame(y = x1)
  for(i in 1:d){
    testdata <- cbind(testdata, x1^i)
    names(testdata)[length(testdata)] <- paste("x^",i)
  }
  fit <- lm(y ~ . , data = traindata)
  pred <- predict(fit, newdata = testdata,se.fit = T)
  out <- list(fit = fit,
              prediction_y = pred$fit,
              prediction_x= x1,
              rss = sum(resid(fit)^2),
              se = pred$se.fit,
              r.squared=summary(fit)$r.squared)

  return(out)
}

#' Help-Function
#'
#' This function checks if a numeric vector only includes whole numbers
#'
#' @param x The vector to be checked
#' @author Cornelius Fritz <cornelius.fritz@campus.lmu.de>
#' @export
#'
round_check= function(x) {
  if(is.element(T,x-ceiling(x)!=0)) {
    return(F)
  } else {
    return(T)
  }
}

#' Help-Function
#'
#' This function checks if a a vector of packages named each as a string are installed. Every package that is installed will be loaded and all not yet
#' installed packages will be installed and loaded.
#'
#' @param packages A vector of packages
#' @author Cornelius Fritz <cornelius.fritz@campus.lmu.de>
#' @export
#'

check_package = function(packages) {
  new_packages=packages[!packages %in% installed.packages()[,"Package"]]
  old_packages=packages[packages %in% installed.packages()[,"Package"]]
  final=c()
  if(length(new_packages)>0){
    install.packages(new_packages)
    code=paste0("library(",paste(new_packages,collapse = ", "),")")
    eval(parse(text = code))
  } else {
    final=c("All packages are already installed")}
  if(length(old_packages)>0){
    code=paste0("library(",paste(old_packages,collapse = ", "),")")
    eval(parse(text = code))
  }
  if(length(final)==1) {
    return(final)
  }
}


#' Help-Function
#'
#' This function melts two factor levels into one factor level.
#'
#' @param factor_leveln1 The number of the first factor level
#' @param factor_leveln2 The number of the second factor level
#' @param factor1 The factor variable itself
#' @author Cornelius Fritz <cornelius.fritz@campus.lmu.de>
#' @export
#'

factor_melt= function(factor_leveln1, factor_leveln2, factor1) { #n1 wird mit n2 zusammengelegt und an Stelle von n2 geschoben
  factor2=factor(levels = levels(factor1))
  for(i in 1: length(factor1)) {
    if(factor1[i]==levels(factor1)[factor_leveln1]) {
      factor2[i]= levels(factor1)[factor_leveln2]
    } else {
      factor2[i]=factor1[i]
    }
  }

  levels(factor2)[factor_leveln2]=paste(levels(factor2)[factor_leveln1],"&",levels(factor2)[factor_leveln2])
  factor2=factor(factor2,levels = levels(factor2)[-factor_leveln1])
  return(factor2)
}


#' Help-Function
#'
#' This function tells you what variables of a given dataset are factorial variables
#'
#' @param data The data to be analysed
#' @author Cornelius Fritz <cornelius.fritz@campus.lmu.de>
#' @export
#'

factor_check=function(data){
  log_fac=c()
  for(i in 1:ncol(data)){
    if(is.factor(data[,i])) {log_fac[i]=F}
    else{log_fac[i]=T}
  }
  return(data[,log_fac])
}

#' Help-Function
#'
#' This function turns all factorial variables in a dataset into metric variables and returns a logical vector what variable were factor-variables.
#'
#' @param data The data to be analysed
#' @author Cornelius Fritz <cornelius.fritz@campus.lmu.de>
#' @export
#'
faktor=function(data) {
  factor=c()
  for (i in 1: ncol(data)) {
    if(is.factor(data[,i])) {
      data[,i]=as.numeric(data[,i])
      names(data)[i]=paste(names(data)[i])
      factor[i]=T
    }
    else {
      factor[i]=F
    }
  }
  return(list(data, factor))
}

#' Help-Function
#'
#' This function is used on the Timeseries-functions and recognizes from the observed y variable
#' of a click in what the used is clicking.
#'
#' @param y The y-value of a click
#' @author Cornelius Fritz <cornelius.fritz@campus.lmu.de>
#' @export
#'

position4 = function(y){
  if(y<0.215){
    x=1
  } else if (y>=0.254 & y< 0.469){
    x=2
  } else if (y>=0.508 & y< 0.723){
    x=3
  } else if (y>=0.723 ){
    x=4
  }
  return(x)

}

#' Help-Function
#'
#' This function is used on the Timeseries-functions and recognizes the y-limits of the 'brush'-interaction in the applications.
#'
#' @param x A numeric value ether 1,2,3 or 4. There are four plots shown underneath each other, what plot should be zoomed in?
#' @param ymin The ymin value of the 'brush'-interaction
#' @param ymax The ymax value of the 'brush'-interaction
#' @param ts_w The analysed time series
#' @author Cornelius Fritz <cornelius.fritz@campus.lmu.de>
#' @export
#'

position5= function(x,ymin,ymax,ts_w) {
  stl_w=stl(ts_w,s.window = "periodic")
  if(x==4){
    dim=c(0.758+0.008,0.973-0.008)
    if(ymin<dim[1]) {
      ymin=dim[1]
    }
    if(ymax>dim[2]) {
      ymax=dim[2]
    }
    s = c(dim[1],ymin,ymax,dim[2])
    s= range01(s)
    ylim=c(range(ts_w)[1]+s[2]*abs(range(ts_w)[1]-range(ts_w)[2]),
           range(ts_w)[2]-(1-s[3])*abs(range(ts_w)[1]-range(ts_w)[2]))

  } else if(x==3) {
    dim=c(0.507+0.008,0.72-0.008)
    if(ymin<dim[1]) {
      ymin=dim[1]
    }
    if(ymax>dim[2]) {
      ymax=dim[2]
    }
    s = c(dim[1],ymin,ymax,dim[2])
    s= range01(s)
    ylim=c(range(stl_w$time.series[,1])[1]+s[2]*abs(range(stl_w$time.series[,1])[1]-range(stl_w$time.series[,1])[2]),
           range(stl_w$time.series[,1])[2]-(1-s[3])*abs(range(stl_w$time.series[,1])[1]-range(stl_w$time.series[,1])[2]))
  } else if(x==2) {
    dim=c(0.255+0.008,0.468-0.008)
    if(ymin<dim[1]) {
      ymin=dim[1]
    }
    if(ymax>dim[2]) {
      ymax=dim[2]
    }
    s = c(dim[1],ymin,ymax,dim[2])
    s= range01(s)
    ylim=c(range(stl_w$time.series[,2])[1]+s[2]*abs(range(stl_w$time.series[,2])[1]-range(stl_w$time.series[,2])[2]),
           range(stl_w$time.series[,2])[2]-(1-s[3])*abs(range(stl_w$time.series[,2])[1]-range(stl_w$time.series[,2])[2]))
  } else if(x==1) {
    dim=c(0+0.008,0.215-0.008)
    if(ymin<dim[1]) {
      ymin=dim[1]
    }
    if(ymax>dim[2]) {
      ymax=dim[2]
    }
    s = c(dim[1],ymin,ymax,dim[2])
    s= range01(s)
    ylim=c(range(stl_w$time.series[,3])[1]+s[2]*abs(range(stl_w$time.series[,3])[1]-range(stl_w$time.series[,3])[2]),
           range(stl_w$time.series[,3])[2]-(1-s[3])*abs(range(stl_w$time.series[,3])[1]-range(stl_w$time.series[,3])[2]))
  }

  return(as.numeric(ylim))
}

#' Help-Function
#'
#' This function is used on the Timeseries-functions and recognizes the x-limits of the 'brush'-interaction in the applications.
#'
#' @param xmin The ymin value of the 'brush'-interaction
#' @param xmax The ymax value of the 'brush'-interaction
#' @param beginning The beginning of the analysed time series
#' @param ending The ending of the analysed time series
#' @author Cornelius Fritz <cornelius.fritz@campus.lmu.de>
#' @export
#'

position6= function(xmin, xmax, beginning, ending) {
  dim= c(0.04,0.96)
  if(xmin<dim[1]) {
    xmin=dim[1]
  }
  if(xmax>dim[2]) {
    xmax=dim[2]
  }
  xlim=c((xmin-0.04)/((0.96-0.04)/abs(beginning-ending)),
         (xmax-0.04)/((0.96-0.04)/abs(beginning-ending)))

  return(xlim)
}

#' Help-Function
#'
#' This function scales the values of a ordered vector from 0 to 1
#'
#' @param x A ordered numeric vector
#' @author Cornelius Fritz <cornelius.fritz@campus.lmu.de>
#' @export
#'

range01 <- function(x){
  (x-min(x))/(max(x)-min(x))
}


