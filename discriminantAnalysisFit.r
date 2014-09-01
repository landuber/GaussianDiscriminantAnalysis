library(mvtnorm)
buildTrain <- function(sets=1, num, mu) {
        
        for(iter in 1:(sets)) {
                if(exists("samples")) {
                        samples <- rbind(samples, rmvnorm(n=num, mean=mu[iter, ]))
                } else {
                        samples <- rmvnorm(n=num, mean=mu[iter, ])
                }
                
        }
   return(samples)
}




discriminantAnalysisFit <- function(X, y, type, lambda=c(), R=c(), V=c(), pseudoCount=1) {
        
        model.modelType = 'discriminantAnalysis'
        model.lambda = lambda
        model.type = type
        
        classes <- levels(factor(y))
        Nclasses <- length(classes)
        model.Nclasses <- Nclasses
        Nclass <- table(y)
        
        
        
        dimX <- dim(X)
        N <- dimX[1]
        D <- dimX[2]
        model.mu = matrix(0, nrow=D, ncol=Nclasses)
        model.classPrior <- c()
        xbar <- mean(X)
        for(k in 1:Nclasses) {
                ndx <- (y == classes[k])
                model.mu[, k] <- t(apply(X[ndx, ], 2, mean))
        }
        
        
        
        return(model.mu)   
}
