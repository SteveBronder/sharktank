
sharks <- read.csv("./sharkdata2.csv",header=TRUE,na.strings="NULL")
sharks$date <- paste(sharks$season,sharks$episode,sep="")
sharks$date <- as.numeric(sharks$date)

sharks$offer.inv <- as.factor(ifelse(sharks$Inv.offer==1,"Offer","No Offer"))
sharks$who.offer <- as.factor(sharks$who.offer)
shark.graph <- ggplot(sharks, aes(,colour=ask_dollar)) + geom_bar(width=.3) +
  theme_bw() + scale_fill_grey(end=.8)
shark.graph


qplot(who.offer,data=sharks[sharks$who.offer!="None",],fill=who.offer,xlab=" Which Shark Makes an Offer")+scale_x_discrete(breaks=NULL)
