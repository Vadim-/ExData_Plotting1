# Function that checks and installs missing packages. Works for one argument
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

# Dependencies check is not robust
if (!file.exists(".dep_test_flag")) {
  pkgTest("Rcpp")
  pkgTest("devtools")
  devtools::install_github("hadley/dplyr")
  file.create(".dep_test_flag")
}

# To load the data you need to have file in /data directory. We always return a two day slice
library(dplyr)
setwd("R/project 1/")
df = read.table("data/household_power_consumption.txt", header=T, sep=";")

df[]          <- lapply(df, as.character)
df_slice      <- subset(df, Date=="1/2/2007" | Date=="2/2/2007")
df_slice$Date <- strptime(df_slice$Date, "%d/%m/%Y", tz="UTC")

# 2) Days(Thu,Fri,Sat) - Global Active Power (kilowatts)(0,6)
gen_event_distribution_chart <- function(vec1, vec2, xlab, ylab, export)
{
  if (export == TRUE) {
    png('plot2.png', height=480, width=480)
  }
  plot(vec1, vec2, type="l", xlab=xlab, ylab=ylab) 
  if (export == TRUE) {
    dev.off()
  }
}

# 3) Days(Thu,Fri,Sat) - 3 Submetering__(0,30)
gen_submetering_chart <- function(vec1, vec2, vec3, vec4, export)
{
  plot_colors <- c("black","red","blue")
  plot_legend <- c(expression(paste(bold("Sub_metering_1"))), expression(paste(bold("Sub_metering_2"))), expression(paste(bold("Sub_metering_3"))))
  if (export == TRUE) {
    png('plot3.png', height=480, width=480)
  }
  plot(vec1, vec2, type="l", ylab="Energy sub metering", col=plot_colors[1], xlab="")
  lines(vec1, vec3, type="l", col=plot_colors[2]) 
  lines(vec1, vec4, type="l", col=plot_colors[3])
  
  if (export == TRUE) {
    legend("topright", plot_legend, col=plot_colors, lty=1, pt.cex = 1)
    dev.off()
  } else {
    legend("topright", plot_legend, col=plot_colors, lty=1, pt.cex = 1, bty="n") 
  }
}

#4) Additional plots can be created using exsiting functions 
#   Days(Thu,Fri,Sat) - Voltage(234,246)
#   Days(Thu,Fri,Sat) - Global_reactive_power(0.0,0.5)

gen_four_in_one_chart <- function() {
  
  
  voltage  <- df_slice$Voltage %>% as.numeric()
  gap  <- df_slice$Global_active_power %>% as.numeric()
  grp  <- df_slice$Global_reactive_power %>% as.numeric()
  days <- strptime(paste(df_slice$Date, df_slice$Time), "%Y-%m-%d %H:%M:%S", tz="UTC")
  sub1  <- df_slice$Sub_metering_1 %>% as.numeric()
  sub2  <- df_slice$Sub_metering_2 %>% as.numeric()
  sub3  <- df_slice$Sub_metering_3 %>% as.numeric()
  
  png('plot4.png', height=480, width=480)
  par(mfrow = c(2,2))
  
  par(mfg=c(1,1))
  gen_event_distribution_chart(days, gap, "", "Global Active Power", FALSE)
  
  par(mfg=c(2,1))
  gen_submetering_chart(days, sub1, sub2, sub3, FALSE)
  
  par(mfg=c(1,2))
  
  gen_event_distribution_chart(days, voltage, "datetime", "Voltage", FALSE)
  
  par(mfg=c(2,2))
  gen_event_distribution_chart(days, grp, "datetime", "Global_reactive_power", FALSE)
  dev.off()
}
gen_four_in_one_chart()