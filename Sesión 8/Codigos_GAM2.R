
####################################################
############# Modelo Contaminación #################
####################################################
Pollution_Data <- read.csv2("Pollution_Data.csv")

set.seed(1)
train  = sample(nrow(Pollution_Data), nrow(Pollution_Data)*2/3)
test=-train
Pollution_Data.train=Pollution_Data[train,]
Pollution_Data.test=Pollution_Data[test,]
PM2.5.test=Pollution_Data$PM2.5[test]

library(tree)
tree.PM2.5 =tree(PM2.5~., data=Pollution_Data.train)
summary(tree.PM2.5)

pdf("treePM25.pdf",height=10,width=10)
plot(tree.PM2.5)
text(tree.PM2.5,pretty =1)
dev.off()

cv.PM2.5=cv.tree(tree.PM2.5)
cv.PM2.5

pdf("cvPM25.pdf",height=10,width=10)
plot(cv.PM2.5$size, cv.PM2.5$dev, type='b')
dev.off()

prune.PM2.5=prune.tree(tree.PM2.5, best=7)

pdf("prunePM25.pdf",height=10,width=10)
plot(prune.PM2.5)
text(prune.PM2.5)
dev.off()


yhat.PM2.5=predict(prune.PM2.5, newdata=Pollution_Data.test)
mse.PM2.5=mean((PM2.5_predi-yhat.PM2.5)^2)
R2=1-(mse.PM2.5/var(PM2.5_predi))
R2


library(gam)

gam.lr=gam(PM2.5~ s(PM10) + s(VeL_Viento)+
             s(Temperatura)+
             s(Humedad)+
             s(Rad_Solar)
           ,data=Pollution_Data.train)
summary(gam.lr)

pdf("GamPM25.pdf",height=10,width=10)
par(mfrow=c(1,5))
plot(gam.lr,se=T,col="blue")
dev.off()


yhat.gam=predict(gam.lr, newdata=Pollution_Data.test)
mse.gam=mean((PM2.5_predi-yhat.gam)^2)
R2=1-(mse.gam/var(PM2.5_predi))
R2
