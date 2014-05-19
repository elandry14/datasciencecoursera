corr <- function(directory, threshold = 0, id = 1:332) {
    
    corr.vector <- numeric()
    
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
        
        if(length(sulfate[good]) > threshold) {
            corr.result <- cor(sulfate[good], nitrate[good])
            #print(corr.result)
            corr.vector <- c(corr.vector, corr.result)
        }
    }
    corr.vector
}