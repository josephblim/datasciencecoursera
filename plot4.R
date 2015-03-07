plot4 <- function(){
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
  # Convert the lists into vectors, focusing on global active power, voltage, 
  # Sub_metering_1-3, and global reactive power, and time, and date
  x21 <- as.vector(DF21['V3'])
  x22 <- as.vector(DF22['V3'])
  s11 <- as.vector(DF21['V7'])
  s12 <- as.vector(DF22['V7'])
  s21 <- as.vector(DF21['V8'])
  s22 <- as.vector(DF22['V8'])
  s31 <- as.vector(DF21['V9'])
  s32 <- as.vector(DF22['V9'])
  v1 <- as.vector(DF21['V5'])
  v2 <- as.vector(DF22['V5'])
  r1 <- as.vector(DF21['V4'])
  r2 <- as.vector(DF22['V4'])
  t21 <- as.vector(DF21['V2'])
  t22 <- as.vector(DF22['V2'])
  d21 <- as.vector(DF21['V1'])
  d22 <- as.vector(DF22['V1'])
  # Combine the vectors into a list
  xx <- as.vector(rbind(x21, x22))
  s1 <- as.vector(rbind(s11, s12))
  s2 <- as.vector(rbind(s21, s22))
  s3 <- as.vector(rbind(s31, s32))
  v <- as.vector(rbind(v1, v2))
  r <- as.vector(rbind(r1, r2))
  tt <- as.vector(rbind(t21, t22))
  dd <- as.vector(rbind(d21, d22))
  # Convert back into a numeric vector
  finalx <- unlist(xx)
  finalx <- as.vector(finalx)
  finalx <- as.numeric(finalx)
  finals1 <- unlist(s1)
  finals1 <- as.vector(finals1)
  finals1 <- as.numeric(finals1)
  finals2 <- unlist(s2)
  finals2 <- as.vector(finals2)
  finals2 <- as.numeric(finals2)
  finals3 <- unlist(s3)
  finals3 <- as.vector(finals3)
  finals3 <- as.numeric(finals3)
  finalv <- unlist(v)
  finalv <- as.vector(finalv)
  finalv <- as.numeric(finalv)
  finalr <- unlist(r)
  finalr <- as.vector(finalr)
  finalr <- as.numeric(finalr)
  # Convert into dates
  ttf <- unlist(tt)
  ttf <- as.vector(ttf)
  ddf <- unlist(dd)
  ddf <- as.vector(ddf)
  dt <- paste(ddf,ttf)
  dty <- strptime(dt, format="%d/%m/%Y %H:%M")
  
  # Create plot
  png("plot4.png")
  par(mfrow = c(2, 2))
  plot(dty,finalx, xlab="", ylab="Global Active Power", type="l")
  plot(dty,finalv, xlab="datetime", ylab="Voltage", type="l")
  plot(dty,finals1, xlab="", ylab="Energy sub metering", type="l", col="black")
  lines(dty,finals2, type="l", col="red")
  lines(dty,finals3, type="l", col="blue")
  legend("topright", lty=1, bty="n", col=c("black","red","blue"), 
         legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
  plot(dty,finalr, xlab="datetime", ylab="Global_reactive_power", type="l")
  dev.off()
}