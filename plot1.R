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

png("plot1.png", width = 480, height = 480) 
plot(years, pmv, main = "Total emissions from PM2.5 in the US in 1999, 2002, 2005 and 2008", xlab = "Year", ylab = "Emissions Level", yaxt = "n", cex.main = 0.9) 
axis(2, at = pmv ,labels = format(pmv, scientific=FALSE))
model <- lm(pmv ~ years) 
abline(model, lwd = 1, col = "steelblue") 
dev.off() 

