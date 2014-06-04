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

png(filename="plot2.png", width = 480, height = 480)
plot(data$fullTime, data$Global_active_power, xlab="", ylab = "Global Active Power(kilowatts)", type="l")
dev.off();