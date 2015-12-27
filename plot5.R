nei <- readRDS("measures of pm/summarySCC_PM25.rds") 
scc <- readRDS("measures of pm/Source_Classification_Code.rds") 

nei$year <- as.factor(nei$year) 

measuresBaltimore <- nei[nei$fips == '24510', ] 

motorsIndex <- grep("motor", scc$SCC.Level.Three, ignore.case=TRUE)  
motorsIndex <- motorsIndex[1: length(motorsIndex) - 1] 
motSources <- scc[motorsIndex, c(1, 9)]  
combVals <- merge(measuresBaltimore, motSources) 

yearsBaltimore <- aggregate(Emissions ~ year, FUN = sum, data = combVals) 
years <- as.integer(as.character(yearsBaltimore[, 1])) 
pmv <- as.numeric(yearsBaltimore[, 2]) 

par(mar = c(4, 4, 1, 1)) 

png("plot5.png", width = 480, height = 480) 
plot(years, pmv, main = "Motor vehicles emissions in Baltimore in 1999, 2002, 2005 and 2008", xlab = "Year", ylab = "Emissions Level", cex.main = 0.9 ) 
model <- lm(pmv ~ years) 
abline(model, lwd = 1, col = "steelblue") 
dev.off()  

