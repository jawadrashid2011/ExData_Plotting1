#By Jawad Rashid
#This function reads in the dataset. It also makes a new column for fullTime which was
#not used as there was no need for time which merges date and time. 
#The date was converted into R Date. First the whole dataset is read in R and then
#only those records are kept which fall under 2007-02-01 to 2007-02-02 and all 
#other columns changed to numeric as read.table read numbers as characters
readDataset <- function(filename) {
        data <- read.table(filename, sep=";", header=TRUE, stringsAsFactors = FALSE,na.strings = "?")
        data$fullTime <- as.POSIXct(paste(data$Date, data$Time), format="%d/%m/%Y %H:%M:%S")
        data$Date <- as.Date(data$Date, format="%d/%m/%Y")
        subsetData <- subset(data, Date <= as.Date("2007-02-02"))
        subsetData <- subset(subsetData, Date >= as.Date("2007-02-01"))
        for(i in 3:9) {
                subsetData[, i] <- as.numeric(subsetData[, i])
        }
        
        subsetData
}

data <- readDataset("household_power_consumption.txt")

png(filename="plot4.png", width = 480, height = 480)
par(mfcol =c(2,2))
#First plot for Global Active Power vs time
plot(data$fullTime, data$Global_active_power, xlab="", ylab = "Global Active Power", type="l")
#Second plot for Metering vs time
plot(data$fullTime, data$Sub_metering_1, xlab="", ylab = "Energy sub metering", type="n")
lines(data$fullTime, data$Sub_metering_1, col="black")
lines(data$fullTime, data$Sub_metering_2, col="red")
lines(data$fullTime, data$Sub_metering_3, col="blue")
legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=c("black", "red", "blue"), lwd=c(1, 1, 1))
#Third plot for Voltage vs time
plot(data$fullTime, data$Voltage, xlab="datetime", ylab = "Voltage", type="l")
#First plot for Global Reactive Power vs time
plot(data$fullTime, data$Global_reactive_power, xlab="datetime",ylab="Global_reactive_power", type="l")
dev.off();