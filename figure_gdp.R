library(MQ)
library(ggplot2)
library(devtools)
library(ggridges)

data_url = 'http://bit.ly/2cLzoxH'
# read data from url as dataframe
gapminder = read.csv(data_url)

gapminder = data.frame("year" = gapminder$year, "lifeExp" = gapminder$lifeExp)

ggplot(gapminder, aes(y=as.factor(year),
                      x=lifeExp)) +
  geom_density_ridges(alpha=0.5) +
  scale_y_discrete(expand = c(0.01, 0)) +  
  scale_x_continuous(expand = c(0, 0))+
  theme(axis.text=element_text(size=20))


library(MQ)
library(xts)
data("GDP")

vY = as.matrix(GDP$GDP_CCH)

vTau = seq(0.05, 0.95, 0.05)

# Fit = DMQ_Fit(vY, vTau, type = "Plain_mod")
Fit = DMQ_Fit(vY, vTau, type = "TwoComponents_mod")

mQ = t(Fit$lFilter$mQ)

lSim = list()

iB = 5000

for (i in 1:(nrow(mQ) - 1)) {
  
  lSim[[i]] = numeric(iB)
  for (b in 1:iB) {
    
    foo = NA
    while(is.na(foo)) {
      foo = suppressWarnings(as.numeric(try(Sim_Sp(mQ[i,], vTau), silent = TRUE)))
    }
    
    lSim[[i]][b] = Sim_Sp(mQ[i,], vTau)
  }
  
}

vKeep = seq(289, 1, -4)[1:38]

vSim = do.call(c, lSim[vKeep])

vYears_solo = rownames(vY)[vKeep]

vYears = do.call(c, lapply(vYears_solo, rep, iB))

vYears = substr(vYears, 1, 4)

mData = data.frame("Year" = vYears, "GDP" = vSim)

pdf(file = "C:/Users/au588008/Dropbox/CataniaHarveyLuati/MQuantiles/Conferences/ECB/Figures/Densities_GDP.pdf", height = 15, width = 10)

ggplot(mData, aes(y = Year,
                      x = GDP)) +
  geom_density_ridges(alpha = 0.5) +
  scale_y_discrete(expand = c(0.01, 0)) +  
  scale_x_continuous(expand = c(0, 0)) +
  theme(axis.text = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

dev.off()

vKeep = seq(289, 1, -4)

vYears_solo = rownames(vY)[vKeep]

vYears = do.call(c, lapply(vYears_solo, rep, iB))

vYears = substr(vYears, 1, 4)

pdf(file = "C:/Users/au588008/Dropbox/CataniaHarveyLuati/MQuantiles/Conferences/ECB/Figures/Series_GDP.pdf", height = 6.5, width = 12)

plot.ts(vY, col = "black", lwd = 2, type = "n", las = 1, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
axis(2, cex.axis = 2.0, las = 1)
grid(10, 10, col = "gray80", lwd = 2)

lines(vY, col = "black", lwd = 1)

axis(1, at = vKeep[seq(1, length(vKeep), 2)], labels = substr(vYears_solo[seq(1, length(vKeep), 2)], 1, 4), las = 2, cex.axis = 2.0)
axis(1, at = 1:289, labels = FALSE, tcl = -0.2)

dev.off()

pdf(file = "C:/Users/au588008/Dropbox/CataniaHarveyLuati/MQuantiles/Conferences/ECB/Figures/Quantiles_GDP.pdf", height = 6.5, width = 12)

plot.ts(vY, col = "red", lwd = 2, type = "n", las = 1, xlab = "", ylab = "", xaxt = "n", ylim = range(mQ),  yaxt = "n")
axis(2, cex.axis = 2.0, las = 1)
grid(10, 10, col = "gray80", lwd = 2)

for (i in 1:length(vTau)) {

  lines(mQ[, i], col =  1, lwd = 0.5)

}

axis(1, at = vKeep[seq(1, length(vKeep), 2)], labels = substr(vYears_solo[seq(1, length(vKeep), 2)], 1, 4), las = 2, cex.axis = 2.0)
axis(1, at = vKeep, labels = FALSE, tcl = -0.2)
dev.off()
