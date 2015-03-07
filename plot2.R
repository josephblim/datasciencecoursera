plot2 <- function(){
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
  # Convert the lists into vectors, focusing on global active power, time, and date
  x21 <- as.vector(DF21['V3'])
  x22 <- as.vector(DF22['V3'])
  t21 <- as.vector(DF21['V2'])
  t22 <- as.vector(DF22['V2'])
  d21 <- as.vector(DF21['V1'])
  d22 <- as.vector(DF22['V1'])
  # Combine the vectors into a list
  xx <- as.vector(rbind(x21, x22))
  tt <- as.vector(rbind(t21, t22))
  dd <- as.vector(rbind(d21, d22))
  # Convert back into a numeric vector
  finalx <- unlist(xx)
  finalx <- as.vector(finalx)
  finalx <- as.numeric(finalx)
  # Convert into dates
  ttf <- unlist(tt)
  ttf <- as.vector(ttf)
  ddf <- unlist(dd)
  ddf <- as.vector(ddf)
  dt <- paste(ddf,ttf)
  dty <- strptime(dt, format="%d/%m/%Y %H:%M")
  
  # Create plot
  png("plot2.png")
  plot(dty,finalx, xlab="", ylab="Global Active Power (kilowatts)", type="l")
  dev.off()
}