

rm(list=ls())
library(wordcloud)
attach(data)
lm(gdp~cor)
#par(mar = rep(2, 4))
plot(cor,gdp)
abline(lm(gdp ~ cor))
text(cor, gdp, labels=country, cex= 0.5,pos=3)



lm2<-lm(log(gdp)~cor) # test
#par(mar = rep(2, 4))
plot(cor,log(gdp))
abline(lm(log(gdp) ~ cor))
text(cor, log(gdp), labels=country, cex= 0.5,pos=3)

attach(dat3)
lm(gdp~cpi)
#par(mar = rep(2, 4))
plot(cpi,gdp)
abline(lm(gdp ~ cpi))
text(cpi, gdp, labels=country, cex= 0.5,pos=2)


attach(dat2)
model<-lm(dat2$gdp~dat2$cor)
#par(mar = rep(2, 4))
plot(x=cor,y=gdp,xlim=c(0,100), ylim=c(0,100))
abline(model)
#name = dat2$country[1:170]   
#identify(cor, gdp, labels = name,cex=0.5, plot=TRUE)
text(cor,gdp, labels=country, cex= 0.60,pos=3)
#dev.copy(png, 'myplot.png', width=600, height=600)
#dev.off()



# library
library(ggplot2)

# The mtcars dataset is proposed in R
data=dat2

# 1/ add text with geom_text, use nudge to nudge the text
p<-ggplot(dat2, aes(x=cor, y=gdp)) +theme(axis.title=element_text(size=14), axis.text=element_text(size=14))+
  geom_point() + 
  geom_text(label=country, nudge_x = 0.25, nudge_y = 3, check_overlap = T,size=5)


p + ylim(0, 100)
require(stats)
reg<-lm(gdp ~ cor, data = dat2)
reg
#p + stat_smooth(method="lm", se=FALSE)
p + geom_abline(intercept = -22.2679, slope = 0.8213,color='blue',size=1)+xlab("2017 CPI Scores") + ylab("2016 GDP per capita ($US)")



