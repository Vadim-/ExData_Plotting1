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
gap  <- df_slice$Global_active_power %>% as.numeric()
days <- strptime(paste(df_slice$Date, df_slice$Time), "%Y-%m-%d %H:%M:%S", tz="UTC")

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
