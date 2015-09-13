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


# 1) Global Active Power (kilowatts) - Plot Frequency histogram
gap  <- df_slice$Global_active_power %>% as.numeric()

gen_density_histogram <- function(vec)
{
  png('plot1.png', height=480, width=480)
  hist(vec, col="red", xlab="Global Active Power (kilowatts)", main="Global Active Power")
  dev.off()
  
}
gen_density_histogram(gap)
