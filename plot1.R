Coltype<-c("character","character",replicate(7,"numeric"))
##read file
plots<-subset(read.csv2("household_power_consumption.txt",header=TRUE,sep=";",stringsAsFactors=F)[,c(1:9)],Date=="1/2/2007"|Date=="2/2/2007")
##convert to Date
plots$Date <-as.Date(plots$Date,format="%d/%m/%Y")
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
##print plots
png(file="plot1.png",width = 480, height = 480,bg = "white")
hist(plots$Global_active_power,col="red", main="Global Active Power",xlab="Global Active Power (kilowatts)")
dev.off()