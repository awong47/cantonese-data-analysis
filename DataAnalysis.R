#alldata_adjMost <- read.csv("~/Documents/cantonesePropAdj/adjMost/data/alldata_adjMost.csv")
#alldata_propMost <- read.csv("~/Documents/cantonesePropAdj/propMost/data/alldata_propMost.csv")

load("dataset.RData")

alldata_adjMost$correct <- with(alldata_adjMost, ifelse(corrAns == TRUE, key_resp_TrueFalse.keys == "f", key_resp_TrueFalse.keys == "j"))
head(alldata_adjMost[c("corrAns","key_resp_TrueFalse.keys","correct")])
alldata_propMost$correct <- with(alldata_propMost, ifelse(corrAns == TRUE, key_resp_TrueFalse.keys == "f", key_resp_TrueFalse.keys == "j"))
head(alldata_propMost[c("corrAns","key_resp_TrueFalse.keys","correct")])

source("BAhelperFunctions.R")
library(ggplot2)
library(plyr)
library(gridExtra)

#first get rid of every trial where the response wasn't 1 or 0
alldata_propMost$correct <- as.numeric(alldata_propMost$correct)
alldata_adjMost$correct <- as.numeric(alldata_adjMost$correct)

#see what ratios we have -- we can abstract away from these and just use bins for plots 
#NB: ratio for propMost condition is defined as target:total; ratio for adjMost condition is defined as target:largestSubset 
levels(as.factor(alldata_propMost$ratio))
levels(as.factor(alldata_adjMost$ratio))

#combine detMost and adjMost 
allData <- rbind(alldata_propMost, alldata_adjMost)

#how many participants? 
length(levels(as.factor(allData$participant))) -> numParticipants

#get overall participant avgs by bins 
bySubj <- ddply(allData, .(question, participant, ratioBin), summarize, meanAcc=mean(correct))

#get overall avgs by bins 
groupAvgs <- ddply(bySubj, .(question, ratioBin), summarize, meanCorrect=mean(meanAcc), se=sd(meanAcc)/sqrt(numParticipants))

#plot the overall group accuracy
ggplot(data=groupAvgs) + geom_point(aes(x=ratioBin, y=meanCorrect, color=question)) + theme_bw() + 
  scale_y_continuous(limits = c(.5,1)) + ggtitle("Overall Group Data")

##Now we need to look at how this changes given # of distractor sets 

#
ggplot(alldata_propMost, aes(x=ratioBin, y = correct))+
  stat_summary(geom = "point", fun.y = "mean") 
ggplot(alldata_adjMost, aes(x=ratioBin, y = correct))+
  stat_summary(geom = "point", fun.y = "mean")

fit.prop<-mle.weber.guess(r=alldata_propMost$ratio,y=alldata_propMost$correct)
fit.prop$par
fit.adj<-mle.weber.guess(r=alldata_adjMost$ratio,y=alldata_adjMost$correct)
fit.adj$par

pred <- data.frame(x=seq(1,max(alldata_adjMost$ratio), 0.001))
pred$yadj <- with(alldata_adjMost, weber.model.guess(params = fit.adj, r = pred$x))
pred$yprop <- with(alldata_propMost, weber.model.guess(params = fit.prop, r = pred$x))

pred$yadj <- weber.model.guess(params = fit.adj$par, r = pred$x)
pred$yprop <- weber.model.guess(params = fit.prop$par, r = pred$x)
ggplot(alldata_adjMost, aes(x=ratio, y = correct))+
  stat_summary(y.fun = mean, geom="point")+
  geom_path(data = pred, aes(x = x, y = yadj))
dd.adj <- ddply(alldata_adjMost, ~ratio, summarise, acc = mean(correct), se = sd(correct) / sqrt(length(correct)))
dd.prop <- ddply(alldata_propMost, ~ratio, summarise, acc = mean(correct), se = sd(correct) / sqrt(length(correct)))

ggplot(dd.adj, aes(y=acc, x = ratio))+
  geom_point()+
  geom_path(data=pred, mapping = aes(x = x, y = yadj))+
  geom_errorbar(aes(ymax = acc+se, ymin = acc-se))
ggplot(dd.prop, aes(y=acc, x = ratio))+
  geom_point()+
  geom_path(data=pred, mapping = aes(x = x, y = yadj))+
  geom_errorbar(aes(ymax = acc+se, ymin = acc-se))

ind_adj<-ddply(alldata_adjMost,~participant,summarize,
              g=mle.weber.guess(r=ratio,y=correct)$par[[2]],
              w=mle.weber.guess(r=ratio,y=correct)$par[[1]])
ind_prop<-ddply(alldata_propMost,~participant,summarize,
               g=mle.weber.guess(r=ratio,y=correct)$par[[2]],
               w=mle.weber.guess(r=ratio,y=correct)$par[[1]])

indplot <- function(dd.prop,dd.adj,Sub) {
  fit.prop<-mle.weber.guess(r=dd.prop$ratio,y=dd.prop$correct)
  fit.adj<-mle.weber.guess(r=dd.adj$ratio,y=dd.adj$correct)
  params.all <- data.frame(w=numeric(),g=numeric())
  params.all['prop',] <- fit.prop$par
  params.all['adj',] <- fit.adj$par
  params.all <- round(params.all,3)
  
  dd.adj <- ddply(dd.adj, ~ratio, summarise, acc = mean(correct), se = sd(correct) / sqrt(length(correct)))
  dd.prop <- ddply(dd.prop, ~ratio, summarise, acc = mean(correct), se = sd(correct) / sqrt(length(correct)))
  pred.prop <- data.frame(x=seq(1,max(dd.adj$ratio), 0.001))
  pred.prop$y <- weber.model.guess(params=fit.prop$par, r=pred.prop$x)
  pred.adj <- data.frame(x=seq(1,max(dd.adj$ratio), 0.001))
  pred.adj$y <- weber.model.guess(params=fit.adj$par, r=pred.adj$x)
  p <- ggplot(dd.prop, aes(x=ratio, y=acc))+
    geom_point(color='cyan')+
    geom_point(data=dd.adj, color='magenta', aes(x=ratio, y=acc))+
    geom_point(data=pred.prop, color='blue', size=0.5, aes(x=x, y=y))+
    geom_point(data=pred.adj, color='red', size=0.5, aes(x=x, y=y))+
    labs(x='Ratio',y='Mean Accuracy', title=paste("Subject ",Sub,sep=" "))+
    xlim(c(1,2)) + ylim(c(0,1)) +
    annotation_custom(tableGrob(params.all,theme=ttheme_minimal()),
                      xmin=1.5, xmax=2, ymin=0, ymax=0.5)
  return(p)
  #add table of w d g by annotation_custom(tableGrob)
}

plotlist <- list(rep(NA,15))
for (i in 1:15) {
  plotlist[[i]] <- indplot(subset(alldata_propMost,participant==i),
                           subset(alldata_adjMost,participant==i),
                           as.character(i))
}
grid.arrange(grobs=plotlist[c(1:15)],ncol=3,
             top="Weber Curves by Subject")

dd.prop.bar<-ddply(alldata_propMost, ~numberDistractorSets, summarise,
                   mean.acc = mean(correct),
                   se.acc = sd(correct) / sqrt(length(correct)))

ggplot(dd.prop.bar, aes(x = numberDistractorSets, y = mean.acc))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymax= mean.acc + se.acc, ymin=mean.acc - se.acc))+
  ylim(c(0,1))

dd.adj.bar<-ddply(alldata_adjMost, ~numberDistractorSets, summarise,
                   mean.acc = mean(correct),
                   se.acc = sd(correct) / sqrt(length(correct)))

ggplot(dd.adj.bar, aes(x = numberDistractorSets, y = mean.acc))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymax= mean.acc + se.acc, ymin=mean.acc - se.acc))+
  ylim(c(0,1))

t.test(subset(alldata_adjMost, numberDistractorSets == 2)$correct,
       subset(alldata_propMost, numberDistractorSets == 2)$correct)
