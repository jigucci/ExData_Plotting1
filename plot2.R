Coltype<-c("character","character",replicate(7,"numeric"))
## read file
plots<-subset(read.csv2("household_power_consumption.txt",header=TRUE,sep=";",stringsAsFactors=F)[,c(1:9)],Date=="1/2/2007"|Date=="2/2/2007")
colnames=names(plots)
convert.magic <- function(obj, type){
      FUN1 <- switch(type,
                     character = as.character,
                     numeric = as.numeric,
                     factor = as.factor)
      out <- lapply(obj, FUN1)
      as.data.frame(out)
}
##convert to numeric
plots[, colnames[3:9]] <- convert.magic(plots[, colnames[3:9]], "numeric")
## add DateTime column as POSIXct class
plots$DateTime<-as.POSIXct(paste(plots$Date,plots$Time),format="%d/%m/%Y %k:%M:%S")
##print plots
png(file="plot2.png",width = 480, height = 480,bg = "white")
plot(plots$DateTime,plots$Global_active_power,pch=".",xlab="",ylab="Global Active Power (kilowatts)",type="l")
dev.off()

