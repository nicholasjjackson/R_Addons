
cholcorr<-function(rho, obs=1000, data, seed...){

	#Determine if Seed needs to be Set
  		if(seed)
    	 	  set.seed(seed)

	#Correlation Matrix's
		C=matrix(c(1, rho,  rho, 1), nrow=2)	

	#Cholesky Decomposition of C Matrix
		U=chol(C)

	#Matrix of Random Normal Variates
		R=matrix(c(rnorm(obs), rnorm(obs)), nrow=obs)
		
	#Matrix of Correlated Variables 
		M=R %*% U
	
	#Store as a data frame
		data=data.frame(M)
	
	#Maybe remove unncessary Matrices	
		rm(C, U, R, M)
	
	#Show Correlations of the now correlated variables
		cor(data)
}






