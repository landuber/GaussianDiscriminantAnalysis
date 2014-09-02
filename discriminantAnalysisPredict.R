discriminantAnalysisPredict <- function(model, Xtest) {
        dimXtest <- dim(Xtest)
        N <- dimXtest[1]
        d <- dimXtest[2]
        classPrior = model$classPrior
        Nclasses = model$Nclasses
        loglik = matrix(0, nrow=N, ncol=Nclasses)
        
        for(c in 1:Nclasses) {
                switch(tolower(model$type),
                       lda = {
                             loglik[, c] <- gaussLogprob(model$mu[ ,c], model$SigmaPooled, Xtest)
                       })
        }
        logjoint <- t(apply(loglik, 1, function(v) {
                v + log(model$classPrior)
        }))
        
        logpost <- t(apply(logjoint, 1, function(v) {
                v - log(sum(exp(v)))
        }))
        
        post <- exp(logpost)
}

gaussLogprob <- function(mu, Sigma, X) {
        
        logp <- 0
      # if mu is a scalar we need to X need to be converted to a vector
        if(length(mu) == 1) {
                X <- as.vector(X);
        }
        dimX <- dim(X)
        N <- dimX[0]
        d <- dimX[1]
        
        # if X is single variable
        if(d == 1) {
                X <- as.vector(X) - as.vector(mu)
        } else {
                X <- t(apply(X, 1, function(x) {
                        x - as.vector(mu)           
                }))
        }
        
        # for vectors -> Diagonal case
        # if(is.vector(Sigma) &&  length(Sigma) > 1) {  
        #        sig2 <- matrix(rep(Sigma, N), nrow=N, byrow=T)
        #        tmp = -(X * X) / (2 * sig2) - 0.5 * log(2*pi*sig2)
        #} else {
                logp <- -0.5 *  apply((X %*% solve(Sigma)) * X, 1, sum)
                logZ <- (d/2) * log(2 * pi) + 0.5 * det(Sigma)
                logp <- logp - logZ
        #}
      
      logp
}

gaussLogprobMissingData <- function(structure, X) {
        
        # if 
}