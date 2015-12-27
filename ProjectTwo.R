url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(url, "NEI_data.zip") 
unzip("NEI_data.zip", exdir = "measures of pm") 

nei <- readRDS("measures of pm/summarySCC_PM25.rds") 
scc <- readRDS("measures of pm/Source_Classification_Code.rds") 

nei$year <- as.factor(nei$year) 
yrs <- split(nei, nei$year) 
values <- lapply(seq_along(yrs), function(x) {
    assign(c("y1999", "y2002", "y2005", "y2008")[x], yrs[[x]], envir=.GlobalEnv)
    }
) 

te <- data.frame() 
te[1, 1] <- sum(y1999$Emissions) 
te[1, 2] <- sum(y2002$Emissions) 
te[1, 3] <- sum(y2005$Emissions)
te[1, 4] <- sum(y2008$Emissions)
te <- setNames(te, c("1999", "2002", "2005", "2008"))
 
years <- as.integer(colnames(te)) 
pmv <- as.integer(te[1, ])

par(mar = c(4, 4, 1, 1))

plot(years, pmv, main = "Total emissions in the US in 1999, 2002, 2005 and 2008", xlab = "Year", ylab = "Level") 
model <- lm(pmv ~ years) 
abline(model, lwd = 1, col = "orange") 


measuresBaltimore <- nei[nei$fips == '24510', ] 
yearsBaltimore <- aggregate(Emissions ~ year, FUN = sum, data = measuresBaltimore) 
yearsBaltimore$year <- as.numeric(as.character(yearsBaltimore$year)) 
plot(yearsBaltimore$year, yearsBaltimore$Emissions, main = "Total emissions in Baltimore in 1999, 2002, 2005 and 2008", xlab = "Year", ylab = "Emissions")  
modelBaltimoreYears <- lm(yearsBaltimore$Emissions ~ yearsBaltimore$year)  
abline(modelBaltimoreYears, lwd = 1, col = "orange")  

library(ggplot2) 

g <- ggplot(data = measuresBaltimore, aes(year, Emissions)) 
g + stat_summary(fun.y = sum, geom = "Point") + 
    geom_smooth(method =  "lm") + 
    facet_grid(. ~ type) + 
    labs(title = "Emissions in Baltimore divided by a type of source") 


coalIndex <- grep("coal", scc$SCC.Level.Three, ignore.case=TRUE)  
coalSources <- scc[columnsMeanSTDV, c(1, 9)]   
combVals <- merge(nei, coalSources) 

yearsUSA <- aggregate(Emissions ~ year, FUN = sum, data = combVals) 
years <- as.integer(as.character(yearsUSA[, 1])) 
pmv <- as.integer(yearsUSA[, 2]) 

par(mar = c(4, 4, 1, 1))

plot(years, pmv, main = "Coal combustion in the US in 1999, 2002, 2005 and 2008", xlab = "Year", ylab = "Level") 
model <- lm(pmv ~ years) 
abline(model, lwd = 1, col = "orange") 


motorsIndex <- grep("motor", scc$SCC.Level.Three, ignore.case=TRUE)  
motorsIndex <- motorsIndex[1: length(motorsIndex) - 1] 
motSources <- scc[motorsIndex, c(1, 9)]  
combVals <- merge(measuresBaltimore, motSources) 

yearsBaltimore <- aggregate(Emissions ~ year, FUN = sum, data = combVals) 
years <- as.integer(as.character(yearsBaltimore[, 1])) 
pmv <- as.numeric(yearsBaltimore[, 2]) 

par(mar = c(4, 4, 1, 1)) 

plot(years, pmv, main = "Motor vehicles emissions in Baltimore in 1999, 2002, 2005 and 2008", xlab = "Year", ylab = "Level") 
model <- lm(pmv ~ years) 
abline(model, lwd = 1, col = "orange") 


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

plot(years, mvCalif, main = "Motor vehicles emissions in Baltimore and California in 1999, 2002, 2005 and 2008", xlab = "Year", ylab = "Level", ylim = c(0, 20)) 
legend("topleft", legend = c("Baltimore", "California"), col = c("steelblue", "orange"), pch =  NULL, lty = 1, cex = 0.59) 
points(years, mvBalt) 

modelBalt <- lm(mvBalt ~ years) 
abline(modelBalt, lwd = 1, col = "steelblue")

modelCalif <- lm(mvCalif ~ years) 
abline(modelCalif, lwd = 1, col = "orange") 

