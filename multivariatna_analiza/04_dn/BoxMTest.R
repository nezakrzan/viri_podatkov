#' Box's test for equivalence of covariance matrices
#'
#' @description The function performs Box's test for testing the null hypothesis that two or more covariance matrices are equal.
#' @param X A data frame with the values of numberical variables.
#' @param cl An normial or ordinal variable which defines groups (a partition) (must be of type \code{factor}).
#' @param alpha Significance level (default \code{0.05}).
#' @param test Wheter the F-test (\code{test = "F"}) or Chi-square (\code{test = "ChiSq"}) test should be forced (see Details). In the case of default value \code{any}, the test is chosen based on the number of units by groups.
#' @return
#' \itemize{
#' \item \code{MBox} - The value of the Box's M statistic.
#' \item \code{ChiSq} or \code{F} - The approximation statistic test.
#' \item \code{p} - An observed significance level.
#' }
#' @details If the size of any group is at least 20 units (sufficiently large),
#' the test takes a Chi-square approximation, otherwise it takes
#' an F approximation.
#' @examples
#' BoxMTest(X = mtcars[, c(1, 3, 4, 5)], cl = as.factor(mtcars[, 2]), alpha = 0.05)
#' @author
#' Ale� �iberna based on Douglas R. White's original REGE and REGD
#' @references Stevens, J. (1996). Applied multivariate statistics for the social sciences . 1992. Hillsdale, NJ: Laurence Erlbaum.



BoxMTest <- function(X, cl, alpha=0.05, test="any") {
  if (alpha <= 0 || alpha >= 1)
    stop('significance level must be between 0 and 1')
  g = nlevels(cl) ## Number of groups.
  n = table(cl) ## Vector of groups-size.
  N = nrow(X)
  p = ncol(X)
  bandera = 2
  if (any(n >= 20)) bandera = 1
  if(test=="F") bandera=2
  if(test=="ChiSq") bandera=1
  ## Partition of the group covariance matrices.
  #covList <- tapply(as.matrix(X), rep(cl, ncol(X)), function(x, nc) cov(matrix(x, nc = nc)), ncol(X))
  covList <- by(X, INDICES = cl, FUN = cov)
  deno = sum(n) - g
  suma = array(0, dim=dim(covList[[1]]))
  for (k in 1:g)
    suma = suma + (n[k] - 1) * covList[[k]]
  Sp = suma / deno ## Pooled covariance matrix.
  Falta=0
  for (k in 1:g){
    Falta = Falta + ((n[k] - 1) * log(det(covList[[k]])))  #; print(log(det(covList[[k]])))
  }
  MB = (sum(n) - g) * log(det(Sp)) - Falta ## Box's M statistic.
  #print(log(det(Sp)))
  suma1 = sum(1 / (n[1:g] - 1))
  suma2 = sum(1 / ((n[1:g] - 1)^2))
  C = (((2 * p^2) + (3 * p) - 1) / (6 * (p + 1) * (g - 1))) *
    (suma1 - (1 / deno)) ## Computing of correction factor.
  if (bandera == 1)
  {
    X2 = MB * (1 - C) ## Chi-square approximation.
    v = as.integer((p * (p + 1) * (g - 1)) / 2) ## Degrees of freedom.
    ## Significance value associated to the observed Chi-square statistic.
    P = pchisq(X2, v, lower=FALSE)  #RM: corrected to be the upper tail
    cat('------------------------------------------------\n');
    cat(sprintf("%10s%11s%12s%13s\n", "MBox", "Chi-sqr", "df", "P"))
    cat('------------------------------------------------\n')
    cat(sprintf("%10.4f%11.4f%12.i%13.4f\n", MB, X2, v, P))
    cat('------------------------------------------------\n')
    if (P >= alpha) {
      cat('Covariance matrices are not significantly different.\n')
    } else {
      cat('Covariance matrices are significantly different.\n')
    }
    invisible(list(MBox=MB, ChiSq=X2, df=v, pValue=P))
  }
  else
  {
    ## To obtain the F approximation we first define Co, which combined to
    ## the before C value are used to estimate the denominator degrees of
    ## freedom (v2); resulting two possible cases.
    Co = (((p-1) * (p+2)) / (6 * (g-1))) * (suma2 - (1 / (deno^2)))
    if (Co - (C^2) >= 0) {
      v1 = as.integer((p * (p + 1) * (g - 1)) / 2) ## Numerator DF.
      #v21 = as.integer(trunc((v1 + 2) / (Co - (C^2)))) ## Denominator DF.
      v21 = (((v1 + 2) / (Co - (C^2)))) ## Denominator DF.
      F1 = MB * ((1 - C - (v1 / v21)) / v1) ## F approximation.
      ## Significance value associated to the observed F statistic.
      P1 = pf(F1, v1, v21, lower=FALSE)
      cat('\n------------------------------------------------------------\n')
      cat(sprintf("%10s%11s%11s%14s%13s\n", "MBox", "F", "df1", "df2", "p"))
      cat('------------------------------------------------------------\n')
      cat(sprintf("%10.4f%11.4f%11.i%14.3f%13.4f\n", MB, F1, v1, v21, P1))
      cat('------------------------------------------------------------\n')
      if (P1 >= alpha) {
        cat('Covariance matrices are not significantly different.\n')
      } else {
        cat('Covariance matrices are significantly different.\n')
      }
      invisible(list(MBox=MB, F=F1, df1=v1, df2=v21, pValue=P1))
    } else {
      v1 = as.integer((p * (p + 1) * (g - 1)) / 2) ## Numerator df.
      #v22 = as.integer(trunc((v1 + 2) / ((C^2) - Co))) ## Denominator df.
      v22 = (((v1 + 2) / ((C^2) - Co))) ## Denominator df.
      b = v22 / (1 - C - (2 / v22))
      F2 = (v22 * MB) / (v1 * (b - MB)) ## F approximation.
      ## Significance value associated to the observed F statistic.
      P2 = pf(F2, v1, v22, lower=FALSE)
      cat('\n------------------------------------------------------------\n')
      cat(sprintf("%10s%11s%11s%14s%13s\n", "MBox", "F", "df1", "df2", "p"))
      cat('------------------------------------------------------------\n')
      cat(sprintf('%10.4f%11.4f%11.i%14.3f%13.4f\n', MB, F2, v1, v22, P2))
      cat('------------------------------------------------------------\n')
      if (P2 >= alpha) {
        cat('Covariance matrices are not significantly different.\n')
      } else {
        cat('Covariance matrices are significantly different.\n')
      }
      invisible(list(MBox=MB, F=F2, df1=v1, df2=v22, pValue=P2))
    }
  }
}



