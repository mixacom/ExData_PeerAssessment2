nei <- readRDS("measures of pm/summarySCC_PM25.rds") 
scc <- readRDS("measures of pm/Source_Classification_Code.rds") 

nei$year <- as.factor(nei$year) 

measuresCalifornia <- nei[nei$fips == '06037', ] 
motorsIndex <- grep("motor", scc$SCC.Level.Three, ignore.case=TRUE)  
motorsIndex <- motorsIndex[1: length(motorsIndex) - 1] 
motSources <- scc[motorsIndex, c(1, 9)] 
measuresBaltimore <- nei[nei$fips == '24510', ] 

motCalif <- merge(measuresCalifornia, motSources) 
motBalt <- merge(measuresBaltimore, motSources) 

yearsCalif <- aggregate(Emissions ~ year, FUN = sum, data = motCalif) 
yearsBaltimore <- aggregate(Emissions ~ year, FUN = sum, data = motBalt) 

years <- as.integer(as.character(yearsBaltimore[, 1]))  
mvCalif <- as.numeric(yearsCalif[, 2]) 
mvBalt <- as.numeric(yearsBaltimore[, 2]) 

par(mar = c(4, 4, 1, 1)) 

png("plot6.png", width = 480, height = 480) 
plot(years, mvCalif, main = "Motor vehicles emissions in Baltimore and California in 1999, 2002, 2005 and 2008", xlab = "Year", ylab = "Emissions Level", ylim = c(0, 20), cex.main = 0.9) 
legend("topleft", legend = c("Baltimore", "California"), col = c("steelblue", "orange"), pch =  NULL, lty = 1, cex = 0.79) 
points(years, mvBalt) 

modelBalt <- lm(mvBalt ~ years) 
abline(modelBalt, lwd = 1, col = "steelblue")

modelCalif <- lm(mvCalif ~ years) 
abline(modelCalif, lwd = 1, col = "orange") 
dev.off() 

