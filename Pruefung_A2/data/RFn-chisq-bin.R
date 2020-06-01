#' Chi-square goodness of fit test for binomial distribution
#'
#' This function performs a chi-square goodness of fit test for a binomial distribution.
#' @param x The observed values.
#' @param f The observed counts.
#' @param m Binomial parameter m.
#' @param p Binomial parameter pi.
#' @author Raul Eyzaguirre.
#' @details If \code{p} is not specified, then it is estimated from the data.
#' If there are categories with expected counts less than 5 a warning is shown.
#' @return It returns a table with the contribution to the chi-square statistic for each category,
#' the chi-square statistic, the degrees of freedom, and the p-value.
#' @examples
#' x <- 0:6
#' f <- c(334, 369, 191, 63, 22, 12, 9)
#' chisq.bin(x, f, m = 10)
#' @importFrom stats dbinom pchisq
#' @export

chisq.bin <- function(x, f, m = NULL, p = NULL) {
    ## R code from Raul Eyzaguirre, modified by Andreas Ruckstuhl
    ##Estimate parameter if unkown
    if (is.null(p)){
        p <- sum(x * f) / sum(m * f)
        k <- 1 # correction of df
    } else {
        k <- 0
    }

    ## Expected frequencies
    xc <- 0:m
    mc <- m + 1
    obs <- rep(0, mc)
    obs[x + 1] <- f
    prob <- dbinom(xc, m, p)
    esp <- sum(f) * prob

    ## Grouping categories
    lz <- which((cumsum(esp) >= 1) & rev(cumsum(rev(esp)) >= 1))
    xc <- as.character(xc[min(lz):max(lz)])
    if(min(lz) > 1)
        xc[1] <- paste("0-", xc[1], sep = "")
    if(max(lz) < mc)
        xc[length(xc)] <- paste(xc[length(xc)], "-", m, collapse="", sep="")
    obs[min(lz)] <- sum(obs[1:min(lz)])
    obs[max(lz)] <- sum(obs[max(lz):mc])
    obs <- obs[min(lz):max(lz)]
    esp[min(lz)] <- sum(esp[1:min(lz)])
    esp[max(lz)] <- sum(esp[max(lz):mc])
    esp <- esp[min(lz):max(lz)]

    ## Chi-square statistic
    chisq <- (obs - esp)^2 / esp
    chisq.t <- sum(chisq)
    dft <- length(chisq) - 1 - k
    pvt <- 1 - pchisq(chisq.t, dft)

    ## Warnings
    if (sum(esp < 5) ==  1)
        warning("One expected frequency less than 5.")
    if (sum(esp < 5) > 1)
        warning(paste(sum(esp < 5), "expected frequencies less than 5."))

    ## Return
    dist.info <- paste("Chi-square goodness of fit test for a binomial(",
                       m, ", ", format(p, digits = 4), ") distribution",
                       sep = "")
    tabla <- data.frame(x = xc, obs.f = obs, exp.f = esp, chisq.cont = chisq)
    tabla$x <- as.character(tabla$x)

    list(Test=dist.info, pi_estimated=(k==1), Contribution_table=tabla,
              Chi_square_test=chisq.t, Degrees_of_freedom=dft, p_value=pvt)
}
