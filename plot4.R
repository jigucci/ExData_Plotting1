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
png(file="plot4.png",width = 480, height = 480,bg = "white")
par(mfrow=c(2,2),mar=c(4,4,2,1),oma=c(0,0,2,0))

with(plots,{
     plot(plots$DateTime,plots$Global_active_power,pch=".",xlab="",ylab="Global Active Power",type="l")
     plot(plots$DateTime,plots$Voltage,pch=".",xlab="datetime",ylab="Voltage", type="l") 
     ##print plot 3
     yMax<-max(plots$Sub_metering_1)
     plot(DateTime,Global_active_power,xlab="",ylab="Energy sub metering",ylim=c(0,yMax),type="n") 
     points(DateTime,Sub_metering_1,pch=".",type="l")
     points(DateTime,Sub_metering_2,pch=".",col="red",type="l")
     points(DateTime,Sub_metering_3,pch=".",col="blue",type="l")
     legend("topright",bty="n",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
     lty=c(1,1),lwd(2.5,2.5),col=c("black","red","blue"))

     plot(plots$DateTime,plots$Global_reactive_power,pch=".",xlab="datetime",ylab="Global_reactive_power", type="l")
})
dev.off()