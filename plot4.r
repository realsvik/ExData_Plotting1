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
#Plot1 draws the first plot of the assignment

#Plot4 draws the 4th plot of the assignment. Even though firts 3 plots are very similar to 
#already created, I do not draw them from already created functions, in order to avoid confusion
#when running the scirpts
plot4 <- function(){
        library(datasets)
        ElectroDF<-ReadMyElectrofile()
        ElectroDFSubMeter <- ElectroDF[,7:9]
        #Initialize png device
        png(filename = "plot4.png", width = 480, height = 480,
            units = "px", pointsize = 12, bg = "white", res = NA, 
            restoreConsole = TRUE)
        #specify columns and rows for png device
        par(mfrow = c(2, 2), ps=12)
        #Override my Windows locale
        Sys.setlocale("LC_TIME", "English")
        #Draw plot(1,1)
        plot(ElectroDF$DateTime, ElectroDF$Global_active_power, type="l", ylab="Global Active Power", xlab="")
        #Draw plot(1,2)
        plot(ElectroDF$DateTime, ElectroDF$Voltage, type="l", ylab="Voltage", xlab="datetime")
        #Draw plot(2,1)
        plot(ElectroDF$DateTime, ElectroDFSubMeter[,1], ylab="Energy sub metering", xlab="", type="l")
        lines(ElectroDF$DateTime, ElectroDFSubMeter[,2], col="red")
        lines(ElectroDF$DateTime, ElectroDFSubMeter[,3], col="blue")
        legend(x="topright", legend=colnames(ElectroDFSubMeter), lty=1, col=c("black", "red", "blue"), bty = "n")
        #Draw plot(2,2)
        plot(ElectroDF$DateTime, ElectroDF$Global_reactive_power, type="l", ylab="Global_reactive_power", xlab="datetime")

        dev.off()
}
