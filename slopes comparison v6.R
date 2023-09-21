
path1 = dirname(sys.frame(1)$ofile)
path1 = paste0(path1,"/")

day_0_1 <- as.numeric(readline(prompt="Start day for first category: "))
day_last_1 <- as.numeric(readline(prompt="End day for first category: "))
day_0_2 <- as.numeric(readline(prompt="Start day for second category: "))
day_last_2 <- as.numeric(readline(prompt="End day for second category: "))
Var <- readline(prompt="Independent variable: ")
C1 <- readline(prompt="Name of first category: ")
C2 <- readline(prompt="Name of second category: ")
plotname <- readline(prompt='Desired title of plot: ')

locfiles <- list.files(path=path1)
locfiles <- locfiles[grepl("data.txt", locfiles)]
for (i in 1:length(locfiles)){
  if(grepl(".txt",locfiles[i]) & grepl(C1, locfiles[i])) {
      data1 <- read.table(locfiles[i], header=TRUE, stringsAsFactors=FALSE)
    }
    else if (grepl(C2, locfiles[i])){
      data2 <- read.table(locfiles[i], header=TRUE, stringsAsFactors=FALSE)
    }
    #else if (grepl(C3, locfiles[i])){
    #  data3 <- read.table(locfiles[i], header=TRUE, stringsAsFactors=FALSE)
    #}
}

data1$Logs <- log(data1$avg)
data2$Logs <- log(data2$avg)

mod1 <- lm(data1$Logs[day_0_1:day_last_1]~data1$day[day_0_1:day_last_1])
mod2 <- lm(data2$Logs[day_0_2:day_last_2]~data2$day[day_0_2:day_last_2])


tiff(filename=paste0(plotname,'.tiff'), width=9, height=6,unit="in", res=500)

simdata1 <- cbind.data.frame(Day= data1$day[day_0_1:day_last_1], LogFluor = data1$Logs[day_0_1:day_last_1], Concentration =C1) 
simdata2 <- cbind.data.frame(Day= data2$day[day_0_2:day_last_2], LogFluor = data2$Logs[day_0_2:day_last_2], Concentration =C2)
alldata <- rbind.data.frame(simdata1, simdata2)

ymin <-round(min(alldata$LogFluor))-0.5
ymax <-round(max(alldata$LogFluor))+1

par(mar=c(5, 4.8, 3, 6.2), xpd=FALSE)
plot(x = data1$day[day_0_1:day_last_1], y = data1$Logs[day_0_1:day_last_1], ylim=c(ymin,ymax),  col='red3', pch=16, xlab="ln(day)", ylab="ln(average fluorescence)",bty='l',las=1, cex = 1.9, cex.axis = 1.5, cex.lab=2,font=2)
box(bty='l', lwd=3)
axis(1, lwd.ticks=3,cex.axis=1.5)
axis(2, lwd.ticks=3, las=1,cex.axis=1.5)

points(x = data2$day[day_0_2:day_last_2], y = data2$Logs[day_0_2:day_last_2], ylim=c(ymin,ymax), col="green", cex = 1.8,pch=15)
abline(mod1, col='red3', lwd=2.5)
abline(mod2, col='green', lwd=2.5)
legend("topright",  xpd=TRUE, inset=c(-0.1,0), legend=c(C1, C2), col = c("red3","green"), pch=c(16, 15), lty=1, lwd=2.5, bty="n", cex=2)
dev.off()



#stats

mod <- lm(LogFluor ~ Day * Concentration, data=alldata)
result <- anova(mod)


#Look at fit of each set of points to line, each returns an R squared and p-value for the fit
summary(mod1)
summary(mod2)


output <- data.frame(matrix(ncol = 5, nrow = 4), row.names=c(Var, "Day", paste0('Day:',Var),"Residuals"))
colnames(output) <- c('Df', 'Sum Sq', 'Mean Sq', 'F value', 'Pr(>F)')
for(i in 1:ncol(output)){
  output[,i] <- result[,i]
}

write.table(output, "Anova.csv", sep=",", col.names=NA)

sink(paste0(C1," line fit.txt"))
print(summary(mod1))
sink()

sink(paste0(C2," line fit.txt"))
print(summary(mod2))
sink()
#Make sure to open these "line fit" documents with notepad, not excel!


















