indir <- "/Users/angelmak/Desktop/2015_0530_Vizathon_HiddenHunger/data"
fp.mwi.nutr <- "/Users/angelmak/Dropbox/git/nutrition_indicators/Malawi/mwi_nutrition.csv"
fp.mwi.anthro <- "/Users/angelmak/Dropbox/git/nutrition_indicators/Malawi/mwi_anthropometry.csv"
fp.eth.nutr <- "/Users/angelmak/Dropbox/git/nutrition_indicators/Ethiopia/eth_nutrition.csv"
fp.eth.anthro <- "/Users/angelmak/Dropbox/git/nutrition_indicators/Ethiopia/eth_anthropometry.csv"

t.m.nutr <- read.csv(fp.mwi.nutr)
t.m.anthro <- read.csv(fp.mwi.anthro)
t.e.nutr <- read.csv(fp.eth.nutr)
t.e.anthro <- read.csv(fp.eth.anthro)

names(t.e.anthro)[grep("weight",names(t.e.anthro))]
names(t.m.nutr)[grep("^def",names(t.m.nutr))]
t.m.nutr.defCol <- grep("^def",names(t.m.nutr))

plot(t.m.nutr[,t.m.nutr.defCol])
t.m.merge <- merge(t.m.nutr,t.m.anthro)
for (i in 1:nrow(t.m.nutr)){
  t.m.nutr[i,"defCount"] <- sum(t.m.nutr[i,257:284] == "Yes")
}
names(t.m.nutr[,257:284])

for (i in 1:nrow(t.m.nutr)){
  t.m.nutr[i,"defCountMicro"] <- sum(t.m.nutr[i,259:284] == "Yes")
}

confusionMatrix(t.m.nutr$def_kcal_rec_hh, t.m.nutr$def_kcal_req_hh)

req.names <- names(t.m.nutr)[grep("req_hh",names(t.m.nutr))]
for (i in 1:nrow(t.m.nutr)){
  t.m.nutr[i,"defCountReq"] <- sum(t.m.nutr[i,req.names] == "Yes")
}

plot(t.m.nutr$b6, t.m.nutr$defCount)
plot(t.m.nutr$b6, t.m.nutr$defCountMicro)
plot(t.m.nutr$b6)
plot(t.m.nutr$b8)
hist(t.m.nutr$gap_kcal_ae)
plot(t.m.nutr$b6,t.m.nutr$gap_kcal_ae)
library("ggplot2")
options("scipen"=100, "digits"=4) #Help print numeric text into numbers
plot <- ggplot() + theme(panel.background = element_rect(fill=0,colour="black"), axis.ticks=element_blank(), panel.grid = element_blank()) 
plot <- plot + theme(axis.text.x = element_text(angle=45, hjust =1))
#plot <- plot + geom_boxplot(data=t.m.nutr,aes(t.m.nutr$b6,t.m.nutr$gap_kcal_ae))
#plot <- plot + geom_boxplot(data=t.m.nutr,aes(t.m.nutr$b6,t.m.nutr$outsidefood))
#plot <- plot + geom_boxplot(data=t.m.nutr,aes(t.m.nutr$b6,t.m.nutr$shannon_exp))

#Education -> wealth
#plot <- plot + geom_boxplot(data=t.m.nutr,aes(t.m.nutr$b6,t.m.nutr$wealth))
#plot <- plot + geom_boxplot(data=t.m.nutr,aes(factor(t.m.nutr$edLvl),t.m.nutr$wealth))
#plot <- plot + geom_boxplot(data=t.m.nutr,aes(factor(t.m.nutr$edLvl),t.m.nutr$defCountMicro))
#plot <- plot + geom_boxplot(data=t.m.nutr,aes(factor(t.m.nutr$edLvl),t.m.nutr$shannon_exp))
plot <- plot + geom_boxplot(data=t.m.nutr,aes(factor(t.m.nutr$edLvl),t.m.nutr$gap_kcal_ae))
#plot <- plot + geom_boxplot(data=t.m.nutr,aes(t.m.nutr$ca4,t.m.nutr$b6))
#plot <- plot + geom_boxplot(data=t.m.nutr,aes(t.m.nutr$b6,t.m.nutr$defCountMicro))
#plot <- plot + geom_boxplot(data=t.m.nutr,aes(t.m.nutr$purch_val,t.m.nutr$defCountMicro))
plot

rowi.2 <- grep("Form",t.m.nutr$b6)
rowi.1 <- grep("Std|No",t.m.nutr$b6)
rowi.3 <- grep("College|Diploma",t.m.nutr$b6)
t.m.nutr$edLvl[rowi.1] <- 1
t.m.nutr$edLvl[rowi.2] <- 2
t.m.nutr$edLvl[rowi.3] <- 3
