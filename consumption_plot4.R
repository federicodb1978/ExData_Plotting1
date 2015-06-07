consumption_plot4 <- function (household_consumption = "./data/household_power_consumption.txt") {
        memory_required <- 8*9*2075259 # check the memory required by the dataset
        #aggiungeere verifica memoria del sistema e controllo
        power_data <- read.table(household_consumption, sep=";", header=TRUE) # load data
        date_start <- as.POSIXlt("2007-02-01") # starting day of the analysis
        date_end <- as.POSIXlt("2007-02-03") # ending day of the analysis 
        power_data$DateTime <- paste(power_data$Date, power_data$Time, sep = " ") #add a column with the whole date and time
        power_data$DateTime <- strptime(power_data$DateTime, format = "%d / %m / %Y %H : %M: %S") # coerce DateTime to date format
        power_data_analysed <- subset(power_data, power_data$DateTime >= date_start & power_data$DateTime <= date_end)
        power_data_analysed[as.character(power_data_analysed) == "?"] <- NA # set NA to all missing values represneted originally with "?"
        good_data <- na.omit(power_data_analysed) # consider only rows with valid values
        png("plot4.png", width=480, height=480)
        par(mfcol=c(2,2))
        with(good_data, {
                plot(good_data$DateTime, as.numeric(as.character(good_data$Global_active_power)), type="l", xlab="", ylab="Global Active Power")
                plot(good_data$DateTime, as.numeric(as.character(good_data$Sub_metering_1)), type="l", col="black", xlab="", ylab="Energy sub metering")
                lines(good_data$DateTime, as.numeric(as.character(good_data$Sub_metering_2)), type="l", col="red")
                lines(good_data$DateTime, as.numeric(as.character(good_data$Sub_metering_3)), type="l", col="blue")
                legend("topright", col=c("black", "red", "blue"), lty=c(1,1,1), legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
                plot(good_data$DateTime, as.numeric(as.character(good_data$Voltage)), type="l", xlab="datetime", ylab="Voltage")
                plot(good_data$DateTime, as.numeric(as.character(good_data$Global_reactive_power)), type="l", xlab="datetime", ylab="Global reactive power")
        })
        dev.off()
        return()
}        