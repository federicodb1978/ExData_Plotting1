consumption_plot1 <- function (household_consumption = "./data/household_power_consumption.txt") {
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
        with(good_data, hist(as.numeric(as.character(good_data$Global_active_power)), main = "Global Active Power", xlab="Global Active Power (kilowatts)", col="red", xlim=c(0,6), ylim=c(0,1200)))
        dev.copy(png, file="plot1.png") # create the png copy of the histogram
        dev.off() # closes the png device
        return()
}        