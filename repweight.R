#Create a function which returns the standard error for a different
#number of doctors using the replicate weights with the mean of 1 
#and width of 1/5.

SEcalc <- function(no_doc){
           repdoc <- rep(NA, 80)
 
           for (i in 1:80){
                       set.seed(i)
                       repdoc[i] <- sum(rnorm(no_doc,1,1/5))  
               }
   
            differencesq <- (repdoc - no_doc)^2
            SE <- sqrt(4/80 * sum(differencesq))
            rm(repdoc,differencesq)
            SE
}

#Get SEs for a different number of docs using the function above.
#Number of docs will range from 1 to 500

doctors <- 1:500
SEresult <- rep(NA,500)

for (i in doctors){
        SEresult[i] <- SEcalc(i)
}

SEdoc <- data.frame(doctors, SEresult)
plot(doctors, SEresult,col="1",xlim=c(0,500),type="l",
     xlab="Number of Doctors",ylab="Standard Error")