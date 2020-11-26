north <- read.csv("North_median_df.csv")
west <- read.csv("West_median_df.csv")
east <- read.csv("East_median_df.csv")
south <- read.csv("South_median_df.csv")
north <- north[,c(2,3)]
west <- west[,c(2,3)]
east <- east[,c(2,3)]
south <- south[,c(2,3)]

##################################
####### Trace ###################
#################################

pdf(file = "trace_north.pdf", height = 5, width = 10)
par(mfrow = c(1,2))
plot.ts(north[,1], ylab = "PM2.5")
plot.ts(north[,2], ylab = "PM10")
dev.off()

pdf(file = "trace_west.pdf", height = 5, width = 10)
par(mfrow = c(1,2))
plot.ts(west[,1], ylab = "PM2.5")
plot.ts(west[,2], ylab = "PM10")
dev.off()

pdf(file = "trace_east.pdf", height = 5, width = 10)
par(mfrow = c(1,2))
plot.ts(east[,1], ylab = "PM2.5")
plot.ts(east[,2], ylab = "PM10")
dev.off()

pdf(file = "trace_south.pdf", height = 5, width = 10)
par(mfrow = c(1,2))
plot.ts(south[,1], ylab = "PM2.5")
plot.ts(south[,2], ylab = "PM10")
dev.off()

######################################
########## ACF plots ##################
######################################

pdf(file = "acf_north.pdf", height = 5, width = 10)
par(mfrow = c(1,2))
acf(north[,1], ylab = "PM2.5")
acf(north[,2], ylab = "PM10")
dev.off()

pdf(file = "acf_west.pdf", height = 5, width = 10)
par(mfrow = c(1,2))
acf(west[,1], ylab = "PM2.5")
acf(west[,2], ylab = "PM10")
dev.off()

pdf(file = "acf_east.pdf", height = 5, width = 10)
par(mfrow = c(1,2))
plot.ts(east[,1], ylab = "PM2.5")
plot.ts(east[,2], ylab = "PM10")
dev.off()

pdf(file = "acf_south.pdf", height = 5, width = 10)
par(mfrow = c(1,2))
acf(south[,1], ylab = "PM2.5")
acf(south[,2], ylab = "PM10")
dev.off()


