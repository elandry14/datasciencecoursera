complete <- function(directory, id = 1:332) {
    
    nobs <- numeric()
    
    for(i in id) {   
        counter <- 0
        tmp <- i
        
        while (tmp >= 1) {
            tmp <- tmp / 10
            counter <- counter + 1
        }
        
        result <- 3 - counter
        
        if (result == 0) {
            string <- toString(i)
        } else if (result == 1) {
            string <- toString(i)
            string <-paste("0", string, sep = "")
        } else {
            string <- toString(i)
            string <-paste("00", string, sep = "")
        }
        
        file <- paste(directory, "/", string, ".csv", sep = "")
        #print(file)
        openfile<- read.csv(file)
        #print(openfile)
        
        sulfate <- openfile[,2]
        nitrate <- openfile[,3]
        good <- complete.cases(sulfate, nitrate)
        nob <- length(sulfate[good])
        #print(nob)
        nobs <- c(nobs, nob)
    }
    x <- data.frame (id, nobs)
    print(x)
}