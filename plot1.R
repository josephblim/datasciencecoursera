plot1 <- function(){
  # Obtain data
  file <- "household_power_consumption.txt"
  mydata = read.table(file, sep=";")
  
  # Process the data, focusing on data collected for 2/2/2007 and 2/1/2007
  # Create a data frame
  DF <- data.frame(mydata)
  # Subset the data for 2/1/2007
  DF21 <- subset(DF, V1=="1/2/2007")
  # Subset the data for 2/2/2007
  DF22 <- subset(DF, V1=="2/2/2007")
  # Convert the lists into vectors, focusing on global active power
  x21 <- as.vector(DF21['V3'])
  x22 <- as.vector(DF22['V3'])
  # Combine the vectors into a list
  xx <- as.vector(rbind(x21,x22))
  # Convert back into a numeric vector
  finalx <- unlist(xx)
  finalx <- as.vector(finalx)
  finalx <- as.numeric(finalx)
  
  # Create plot
  png("plot1.png")
  hist(finalx, main = "Global Active Power", xlab="Global Active Power (kilowatts)", ylab="Frequency", col="red")
  dev.off()
}