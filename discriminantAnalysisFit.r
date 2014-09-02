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
        
        modelType = 'discriminantAnalysis'
        lambda = lambda
        type = type
        
        y <- factor(y, labels=1:length(unique(y)), ordered=TRUE)
        
        support <- levels(y)
        Nclasses <- length(support)
        Nclasses <- Nclasses
        classPrior <- c()
        Nclass <- table(y)
        
        
        
        
        dimX <- dim(X)
        N <- dimX[1]
        D <- dimX[2]
        mu = matrix(0, nrow=D, ncol=Nclasses)
        SigmaPooled = matrix(0, ncol=D, nrow=D)
        classPrior <- c()
        xbar <- mean(X)
        for(k in 1:Nclasses) {
                ndx <- (y == k)
                classPrior[k] <- Nclass[k] + pseudoCount
                mu[, k] <- t(apply(X[ndx, ], 2, mean))
        }
        
        classPrior = classPrior / sum(classPrior)
        
        switch(tolower(type),
               lda = {   
                       for(c in 1:Nclasses) {
                               ndx = (y == c)
                               nc <- sum(ndx)
                               dat = X[ndx, ]
                               Sigma <- cov(dat)
                               SigmaPooled = SigmaPooled + (nc - 1) * Sigma
                               
                       }
                       SigmaPooled = SigmaPooled/N
               },
               {
                     print('invalid type')   
               }
        )

        
        return(list(modelType=modelType, type=type, support=support, Nclasses=Nclasses, classPrior=classPrior, mu=mu, SigmaPooled=SigmaPooled))   
}
