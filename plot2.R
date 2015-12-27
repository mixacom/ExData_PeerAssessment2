nei <- readRDS("measures of pm/summarySCC_PM25.rds") 
scc <- readRDS("measures of pm/Source_Classification_Code.rds") 

nei$year <- as.factor(nei$year) 

measuresBaltimore <- nei[nei$fips == '24510', ] 
yearsBaltimore <- aggregate(Emissions ~ year, FUN = sum, data = measuresBaltimore) 
yearsBaltimore$year <- as.numeric(as.character(yearsBaltimore$year)) 

png("plot2.png", width = 480, height = 480) 

plot(yearsBaltimore$year, yearsBaltimore$Emissions, main = "Total emissions from PM2.5 in Baltimore in 1999, 2002, 2005 and 2008", xlab = "Year", ylab = "Emissions Level", cex.main = 0.9)  
modelBaltimoreYears <- lm(yearsBaltimore$Emissions ~ yearsBaltimore$year)  
abline(modelBaltimoreYears, lwd = 1, col = "steelblue")  

dev.off() 

