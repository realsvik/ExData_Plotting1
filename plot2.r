#FilterMyElectroFile copies relevant data from 01.02.2007 to 02.02.2007 into new file, household_power_consumption2007.txt
#This helps to avoid heavy data reading for each plot.

FilterMyElectroFile <- function(){
        inputFile <- "household_power_consumption.txt"
        temporaryFile <- "household_power_consumption2007.txt"
        con  <- file(inputFile, open = "r")
        con2 <- file(temporaryFile, open = "w")
        oneLine <- readLines(con, n = 1, warn = FALSE)
        cat(oneLine, file = con2, sep = "\n") #set headers the 2007 file
        while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
               
                fieldlist <- strsplit(oneLine, split = ";") 
                if (grepl(fieldlist[[1]][1], "1/2/2007")|grepl(fieldlist[[1]][1], "2/2/2007")){
                        cat(oneLine, file = con2, sep = "\n") 
                }
        } 
        
        close(con)
        close(con2)
}
#ReadMyElectrofile reads data into dataframe. Used in every plot.
ReadMyElectrofile <- function(){
        #!!!!!Uncomment the next line, if file household_power_consumption2007.txt has not been created.
        #FilterMyElectroFile()
        InputFile <- "household_power_consumption2007.txt"
        ElectroDF <- read.table(InputFile, sep=";", na.strings = "?", stringsAsFactors=FALSE, header=TRUE)
        ElectroDF <- transform(ElectroDF, DateTime = paste(ElectroDF$Date, ElectroDF$Time))
        ElectroDF <- transform(ElectroDF, DateTime = strptime(ElectroDF$DateTime,  format="%d/%m/%Y %H:%M:%S"))
        head(ElectroDF)
        return(ElectroDF)
}

#Plot2 draws the 2nd plot of the assignment
plot2 <- function(){
        library(datasets)
        #read data from file
        ElectroDF<-ReadMyElectrofile()
        par(mar = c(4,4,4,4))
        #Override my Windows locale
        Sys.setlocale("LC_TIME", "English")
        png(filename = "plot2.png", width = 480, height = 480,
            units = "px", pointsize = 12, bg = "white", res = NA,
            restoreConsole = TRUE)
        with(ElectroDF, plot(ElectroDF$DateTime, ElectroDF$Global_active_power, type="l", ylab="Global Active Power (kilowatts)", xlab=""))
        dev.off()
}
