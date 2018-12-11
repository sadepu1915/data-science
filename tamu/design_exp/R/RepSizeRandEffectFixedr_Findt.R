


repfixr <- function(alpha, gamma, r, tau)
{   
    t     <- 2
    power <- 0
  
    while(power < gamma) {
        t      <- t+1
        nu1   <- t-1
        nu2    <- t*(r-1)
        lambda <- sqrt(1+r*tau)
        Fcr    <- qf(1-alpha, nu1, nu2)
        C      <- (1/lambda^2)*Fcr
        power  <- 1-pf(C, nu1, nu2)
     }
    print(cbind(r, t, nu1, nu2, Fcr, lambda, power)) }

repfixr(.05,.85,5,.8225)



