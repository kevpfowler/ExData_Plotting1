# Make sure the working directory is set to the ExData_Plotting1 clone,
# and that the household_power_consumption.txt datafile is downloaded 
# to the parent directory. E.g., on my system:
# setwd("~/Dropbox/Projects/coursera-R/ExploratoryDataAnalysis/Project_1/ExData_Plotting1")
# 
library(dplyr)

read_raw_file <- function() {
    # Check that the raw data file is in the expected location 
    # (in the parent directory)
    if (!file.exists("../household_power_consumption.txt")) {
        stop("File household_power_consumption.txt not in parent directory")
    }
    
    # Read in the raw data file
    power <- read.csv("../household_power_consumption.txt", 
                      sep = ";",
                      nrows = 2075260,
                      na.strings = c("?"),
                      colClasses = c("character", "character", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric"))

    # - Convert the Date and Time columns to POSIXct type datetime
    # - Filter out the specific dates we are interested in.
    # - Reorder to put datetime as first column and drop the 
    #   original Date and Time cols.
    power <- power %>%
        mutate(datetime = as.POSIXct(strptime(paste(Date, Time), 
                                              "%d/%m/%Y %H:%M:%S"))) %>%
        filter(datetime >= "2007-02-01" & datetime < "2007-02-03") %>%
        select(datetime, 
               Global_active_power, 
               Global_reactive_power,
               Voltage,
               Global_intensity,
               Sub_metering_1,
               Sub_metering_2,
               Sub_metering_3)
    
    # Note - according to some "tidy" people the colNames here could use
    # some work, but the assignment directions imply they want them left 
    # alone, aside from the combined datetime.
    
    return(power)
}

# Read the raw dataset into the power object using above function
power <- read_raw_file()

# Open PNG graphics device - writing file to working directory
png(filename = "plot3.png")

# Create the desired plot, suppressing the default x-axis formatting
with(power, plot(datetime, Sub_metering_1, type = "n", 
                 xlab = "", xaxt = "n",
                 ylab = "Energy sub metering"))

# Format the x-axis labels
daterange <- c(min(power$datetime), (max(power$datetime) + 60))
axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="day"), format = "%a")

# Add points for each sub-metering stations
with(power, points(datetime, Sub_metering_1, type = "l"))
with(power, points(datetime, Sub_metering_2, type = "l", col = "red"))
with(power, points(datetime, Sub_metering_3, type = "l", col = "blue"))
legend("topright",                                     # location
       lty = c(1, 1, 1),                               # type is line
       col = c("black", "red", "blue"),                # colors
       legend = names(select(power, contains("Sub")))) # legend names

# Close the PNG device
dev.off()
