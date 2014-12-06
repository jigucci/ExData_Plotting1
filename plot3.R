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
png(file="plot3.png",width = 480, height = 480,bg = "white")
yMax<-max(plots$Sub_metering_1)
with(plots,plot(DateTime,Global_active_power,xlab="",ylab="Energy sub metering",ylim=c(0,yMax),type="n")) 
with(plots,points(DateTime,Sub_metering_1,pch=".",type="l"))
with(plots,points(DateTime,Sub_metering_2,pch=".",col="red",type="l"))
with(plots,points(DateTime,Sub_metering_3,pch=".",col="blue",type="l"))
legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=c(1,1),lwd(2.5,2.5),col=c("black","red","blue"))
dev.off()

