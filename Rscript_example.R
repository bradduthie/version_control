simpleboot <- function(freqs, repli = 1000, alpha = 0.05){    
    vals  <- NULL;                                                          
    i     <- 0;                                         
    while(i < repli){                                                   
        boot  <- sample(x = freqs, size = length(freqs), replace = TRUE);           
        strap <- mean(boot);                                                
        vals  <- c(vals, strap);                                             
        i     <- i + 1;                                                     
    }                                                                       
    vals   <- sort(x = vals, decreasing = FALSE);                            
    lowCI  <- vals[round( (alpha*0.5) * repli)];                               
    highCI <- vals[round( (1 - (alpha*0.5)) * repli)];                           
    CIs    <- c(lowCI, highCI);                                          
    return(CIs);                                                            
}  