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
png(filename = "plot2.png")

# Create the desired plot, suppressing the default x-axis formatting
with(power, plot(datetime, Global_active_power, type = "l", 
                 xlab = "", ylab = "Global Active Power (kilowatts)"))

# Close the PNG device
dev.off()
