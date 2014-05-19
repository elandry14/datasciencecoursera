pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    allcleandata <- c()
    
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
    
        if(pollutant == "sulfate") {
            data <- openfile[,2]
        }
        else if(pollutant == "nitrate") {
            data <- openfile[,3]
        }
        
        clean.data <- is.na(data)
        data[!clean.data]
        mean(data[!clean.data])
        allcleandata <- c(allcleandata, data[!clean.data])
    }
    
    #print(allcleandata)
    mean(allcleandata)
}