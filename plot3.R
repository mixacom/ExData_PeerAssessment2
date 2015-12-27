
library(ggplot2) 

nei <- readRDS("measures of pm/summarySCC_PM25.rds") 
scc <- readRDS("measures of pm/Source_Classification_Code.rds") 

nei$year <- as.factor(nei$year) 

measuresBaltimore <- nei[nei$fips == '24510', ] 

png("plot3.png", width = 480, height = 480) 
g <- ggplot(data = measuresBaltimore, aes(year, Emissions)) 
g + stat_summary(fun.y = sum, geom = "Point") + 
    geom_smooth(method =  "lm") + 
    facet_grid(. ~ type) + 
    labs(title = "Emissions from PM2.5 in Baltimore divided by a type of source") +
    labs(x = "Year") + 
    labs(y = "Emissions Level")  
dev.off() 

