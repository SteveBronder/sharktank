library(ggplot2)
sharks <- read.csv("./sharkdata3.csv",header=TRUE,na.strings="NULL")
sharks$date <- paste(sharks$season,sharks$episode,sep="")
sharks$date <- as.numeric(sharks$date)

sharks$offer.inv <- as.factor(ifelse(sharks$Inv.offer==1,"Offer","No.Offer"))
sharks$no.offer.inv <- as.factor(ifelse(sharks$no_offer==1,"No.Offer","Offer"))
sharks$who.offer <- as.factor(sharks$who.offer)

stat.sub <- subset(sharks,select=c(offer.inv ,evaluation, ask_dollar , ask_equity , profit_term_mos , revenue , profit))

qplot(who.offer,data=sharks[sharks$who.offer!="None",],fill=who.offer,xlab=" Which Shark Makes an Offer")+scale_x_discrete(breaks=NULL)

qplot(ask_equity,ask_dollar,data=sharks[sharks$who.offer!="None",],colour=who.offer) + geom_smooth(method=lm,se=FALSE)+ylim(0,200000)

library(mgcv)


gam.ft <- gam(Inv.offer ~s(ask_dollar)+ s(ask_equity) + s(revenue)+0,data=sharks,select=TRUE,family=binomial)

gam.ft.eq.do <- gam(dollar_accepted + equity_accepted~s(ask_dollar)+ s(ask_equity) + s(revenue)  +  Kevin_Oleary,data=sharks,select=TRUE)

gam.ft <- gam(Inv.offer~I(ask_dollar/ask_equity)+ ask_equity + 0,data=sharks,select=TRUE,family=binomial)

library(C50)

costs.c <- matrix(c(1,1,1,1),2,2,byrow=TRUE)
colnames(costs.c) <- c("No Offer", "Offer")
rownames(costs.c) <- c("No Offer", "Offer")
c.fitt <- C5.0(offer.inv ~evaluation+ ask_equity + revenue + profit_term_mos,data=sharks,costs=costs.c)
summary(c.fitt)


c.fitt <- C5.0(offer.inv ~evaluation+ ask_dollar + ask_equity + profit_term_mos + revenue + profit,data=sharks,trials=3)


library(caret)

library(doParallel)
 cl <- makeCluster(3)
  stopCluster(cl)
 registerDoParallel(cl)

c50Grid <- expand.grid(.trials = c(1:14),
                       .model = c("tree", "rules"),
                       .winnow = c(TRUE, FALSE))


ctrl <- trainControl(method = "repeatedcv", 
             repeats = 10,
             returnResamp = "final",
             savePredictions = FALSE,
             classProbs = TRUE,
             summaryFunction = defaultSummary,
             selectionFunction = "best",
             preProcOptions = list(thresh = 0.95, ICAcomp = 3, k = 5),
             allowParallel = TRUE)

c5Fitvac <- train(offer.inv ~evaluation+ ask_dollar + ask_equity + profit_term_mos + revenue + profit,
                  data = sharks,
                  method = "C5.0",
                  tuneGrid = c50Grid,
                  metric = "Kappa", # not needed it is so by default
                  trControl = ctrl,
                  importance=TRUE, # not needed
                  #preProc = c("center", "scale"))
                  preProc=NULL)


c5Fitvac4 <- train(no.offer.inv ~evaluation+ ask_dollar + ask_equity + profit_term_mos + revenue + profit,
                   data = sharks,
                   method = "C5.0",
                   tuneGrid = c50Grid,
                   metric = "Kappa", # not needed it is so by default
                   trControl = ctrl,
                   importance=TRUE, # not needed
                   preProc = c("center", "scale"))
                 


plot(c5Fitvac2)

c5Fitvac2$results
c5Fitvac2$finalModel
c5Fitvac2$bestTune
summary(c5Fitvac2)

plot(c5Fitvac4)

c5Fitvac4$results
c5Fitvac4$finalModel
c5Fitvac4$bestTune
summary(c5Fitvac4)

