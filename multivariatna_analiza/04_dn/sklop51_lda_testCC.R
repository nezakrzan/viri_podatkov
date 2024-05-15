#' Test of canonical correlations
#'
#' @description The function perform the Wilk's test for the statistical significance of canonical correlations.
#' @param cor Vector with canonical correlations.
#' @param n Number of units.
#' @param p Number of variables in the first group of variables.
#' @param q Number of variables in the second group of variables.
#' @return The results are organized in a \code{list} format with two data tables:
#'
#' \code{sigTest}
#' \itemize{
#' \item \code{WilksL} - Value of the Wilk's lambda statistic (it is a generalization of the multivariate R2; values near 0 indicate high correlation while values near 1 indicate low correlation).
#' \item \code{F} - Corresponding (to Wilk's lambda) F-ratio.
#' \item \code{df1} - Degrees of freedom for the corresponding F-ratio.
#' \item \code{df2} - Degrees of freedom for the corresponding F-ratio.
#' \item \code{p} - Probability value (p-value) for the corresponding F-ratio (Ho: The current and all the later canonical correlations equal to zero).
#' }
#' \code{eigModel}
#' \itemize{
#' \item \code{Eigenvalues} - Eigenvalues of the canonical roots.
#' \item \code{\%} - Proportion of explained variance of correlation.
#' \item \code{Cum \%} - Cumulative proportion of explained variance of correlation.
#' \item \code{Cor} - Canonical correlation coeficient.
#' \item \code{Sq. Cor} - Squared canonical correlation coeficient.
#' }
#' @examples
#' testCC(cor = c(0.76, 0.51, 0.35, 0.28, 0.10), n = 51, p = 5, q = 5)
#' @author Ale� �iberna
#' @references
#' R Data Analysis Examples: Canonical Correlation Analysis, UCLA: Statistical Consulting Group. From http://www.ats.ucla.edu/stat/r/dae/canonical.htm (accessed Decembar 27, 2013).
#' @export



testCC <- function (cor, n, p, q){
  ev <- (1 - cor^2)
  k <- min(p, q)
  m <- n - 3/2 - (p + q)/2
  w <- rev(cumprod(rev(ev)))
  d1 <- d2 <- f <- vector("numeric", k)
  for (i in 1:k) {
    s <- ifelse(p^2 + q^2==5, 1,sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5)))
    si <- 1/s
    d1[i] <- p * q
    d2[i] <- m * s - p * q/2 + 1
    r <- (1 - w[i]^si)/w[i]^si
    f[i] <- r * d2[i]/d1[i]
    p <- p - 1
    q <- q - 1
  }
  pv <- stats::pf(f, d1, d2, lower.tail = FALSE)
  eig <- cor^2/(1 - cor^2)
  eigModel <- cbind(Eigenvalues = eig, `%` = eig/sum(eig) *
                      100, `Cum %` = cumsum(eig/sum(eig)) * 100, Cor = cor,
                    `Sq. Cor` = cor^2)
  dmat <- cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv)
  rownames(dmat) <- paste(1:k, "to", k)
  return(list(sigTest = dmat, eigModel = eigModel))
}



