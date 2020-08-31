# Jags-ExampleScript.R 
# Accompanies the book:
#   Kruschke, J. K. (2014). Doing Bayesian Data Analysis: 
#   A Tutorial with R, JAGS, and Stan. 2nd Edition. Academic Press / Elsevier.

# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

# Load the functions used below:
source("DBDA2E-utilities.R") # Must be in R's current working directory.
require(rjags)               # Must have previously installed package rjags.

fileNameRoot="Jags-ExampleScript" # For output file names.

# Load the data:
myData = read.csv("z15N50.csv") # Read data file; must be in curr. work. dir.
y = myData$y        # The y values are in the column named y.
Ntotal = length(y)  # Compute the total number of flips.
dataList = list(    # Put the information into a list.
  y = y ,
  Ntotal = Ntotal 
)

# Define the model:
modelString = "
model {
  for ( i in 1:Ntotal ) {
    y[i] ~ dbern( theta )
  }
  theta ~ dbeta( 1 , 1 )
}
" # close quote for modelString
# Notice that the data-specifying variable names, y and Ntotal, matches the
# names used in the dataList defined in the previous section
writeLines( modelString , con="TEMPmodel.txt" )

# Initialize the chains based on MLE of data.
# Option: Use single initial value for all chains:
#  thetaInit = sum(y)/length(y)
#  initsList = list( theta=thetaInit )
# Option: Use function that generates random values for each chain:
initsList = function() {
  resampledY = sample( y , replace=TRUE )
  thetaInit = sum(resampledY)/length(resampledY)
  thetaInit = 0.001+0.998*thetaInit # keep away from 0,1
  return( list( theta=thetaInit ) )
}

# Run the chains:
# first step-get all the info. into JAGS and let JAGS figure out appropriate samplers 
# for the model
jagsModel = jags.model( file="TEMPmodel.txt" , data=dataList , inits=initsList , 
                        n.chains=3 , n.adapt=500 ) # n.adapt is the number of steps
 # for adapting (or tuning) the samplers, not burning!
 # Thus the jags.model returns a list of functions that encapsulate, in JAGS term, 
 # the model and samplers

# second step-runs the chains for a burn-in period
update( jagsModel , n.iter=500 ) # run the chain some number of steps to accomplish burn in
 # This update function returns no values, it merely changes the internal state 
 # of the jagsModel object; it does not record the sampled parameter

#third step-run and record the MCMC sample that we will subsequently examine
codaSamples = coda.samples( jagsModel , variable.names=c("theta") ,
                            n.iter=3334 )
 # Generate MCMC samples which we will actually use to represent the posterior
 # distribution; using the coda.samples function from the rjags package.
 # The variable.names specifies which arameters will have their values recorded
 # during the MCMC walk! and it must be a vector of character strings!
 # n.iter is the number of iterations, or steps, which yield a total of 10,002
 # steps in total because of three chains specified.
 # The coda.samples returns a coda-formatted object which contains all the sampled
 # parameter values in all the chains; it is a list of matrices.Each component of
 # the list corresponds to a chain.

save( codaSamples , file=paste0(fileNameRoot,"Mcmc.Rdata") )

# Examine the chains:
# Convergence diagnostics:
diagMCMC( codaObject=codaSamples , parName="theta" ) # The diagMCMC is defined in
 # the DBDA2E-utilities.R script; in models with many parameters, call this function
 # repeatedly, once for each parameter of interest. 
saveGraph( file=paste0(fileNameRoot,"ThetaDiag") , type="eps" )
# Posterior descriptives:
openGraph(height=3,width=4)
par( mar=c(3.5,0.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
plotPost( codaSamples[,"theta"] , main="theta" , xlab=bquote(theta) ) # The 
 # plotPost function is defined in DBDA2E-utilities.R script.
saveGraph( file=paste0(fileNameRoot,"ThetaPost") , type="eps" )
# Re-plot with different annotations:
plotPost( codaSamples[,"theta"] , main="theta" , xlab=bquote(theta) , 
          cenTend="median" , compVal=0.5 , ROPE=c(0.45,0.55) , credMass=0.90 )
 # mode for cenTend (central tendency) can be "mode", "mean", or "median", mode is
 # most meaningful but median is typically stable; other configurations can be
 # added, refer to page 206 in the book.
saveGraph( file=paste0(fileNameRoot,"ThetaPost2") , type="eps" )
