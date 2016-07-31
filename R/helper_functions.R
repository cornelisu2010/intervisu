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




