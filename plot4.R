nei <- readRDS("measures of pm/summarySCC_PM25.rds") 
scc <- readRDS("measures of pm/Source_Classification_Code.rds") 

nei$year <- as.factor(nei$year) 

coalIndex <- grep("coal", scc$SCC.Level.Three, ignore.case=TRUE)  
coalSources <- scc[coalIndex, c(1, 9)]   
combVals <- merge(nei, coalSources) 

yearsUSA <- aggregate(Emissions ~ year, FUN = sum, data = combVals) 
years <- as.integer(as.character(yearsUSA[, 1])) 
pmv <- as.integer(yearsUSA[, 2]) 

par(mar = c(4, 4, 1, 1))

png("plot4.png", width = 480, height = 480) 
plot(years, pmv, main = " Emissions from coal combustion in the US in 1999, 2002, 2005 and 2008", xlab = "Year", ylab = "Emissions Level", cex.main = 0.9) 
model <- lm(pmv ~ years) 
abline(model, lwd = 1, col = "grey50") 
dev.off() 


