


repfixt <- function(alpha, gamma, t, sigma_a, sigma_e)
{   r     <- 1
    power <- 0
    nu1   <- t-1
    while(power < gamma) {
        r      <- r+1
        nu2    <- t*(r-1)
        tau    <- (sigma_a/sigma_e)^2
        lambda <- sqrt(1+r*tau)
        Fcr    <- qf(1-alpha, nu1, nu2)
        C      <- (1/lambda^2)*Fcr
        power  <- 1-pf(C, nu1, nu2)
    }
    print(cbind(t, r, nu1, nu2, Fcr, lambda, power)) }

repfixt(.05,.85,5,2.5,2.7566)


