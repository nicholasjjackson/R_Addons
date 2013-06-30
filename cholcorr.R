cholcorr<-function(cmatrix, obs=1000, seed=5, printit=TRUE,...){

	#Determine if Seed needs to be Set
  		if(seed)
    	 	  set.seed(seed)
	##Determin Length of Correlation Martix
		row=nrow(cmatrix)
		col=ncol(cmatrix)	

	if (row==col) {

		#Cholesky Decomposition of C Matrix
			U=chol(cmatrix)

		#Uncorrelated Normally Distributed Random variables 
			R=matrix(rnorm(obs*col), nrow=obs)

		#Matrix of Correlated Variables 
			M=R %*% U

		#Store as a data frame
			output=data.frame(M)
			rdata=data.frame(R)

		#Show Correlations of the now correlated variables
		if(printit){
			cat("Observed Correlation\n\n")
			cat("Seed: ",seed,"\n")
			cat("Number of observations: ", obs, "\n\n")
			output.print = cor(output)
			output.print = apply(output.print, 2, format, digits=4, nsmall=TRUE)
			print(as.data.frame(output.print))
		}
		#Store as list
			invisible(list(cordata=output, randata=rdata, obscor=cor(output), obs=obs, seed=seed))

	}
	else {
		stop("Correlation Matrix must be of dimensions n X n")
	}

}
