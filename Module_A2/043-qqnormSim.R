qqnormSim <- function(obj, rob=FALSE, SEED=NULL, Nsim=19)
{
    ## rkst, Version Oct/2016
    mu <- if(rob) median(obj) else mean(obj)
    sigma <- if(rob) mad(obj) else sd(obj)
    n <- length(obj)

    ## normal plot
    SIM <- matrix(NaN, ncol=Nsim, nrow=n)
    if(Nsim>0){
        if(!is.null(SEED)) set.seed(SEED)
        for(i in 1: Nsim){
            SIM[,i] <- sort(qqnorm(rnorm(n,mu,sigma), plot.it=FALSE)$y)
            }
    }
    OQQN <- qqnorm(obj, plot.it=FALSE)
    ylim <- range(c(OQQN$y, SIM), finite=TRUE)
    plot(range(OQQN$x, finite=TRUE), ylim, type="n",
         xlab = "Theoretical Quantiles",
         ylab = "Ordered (Bootstrap) Observations")
    if(Nsim>0)
        points(rep(sort(OQQN$x),Nsim), as.vector(SIM), col="gray")
        points(OQQN$x, OQQN$y, lwd=2, col="blue")
    invisible()
}
