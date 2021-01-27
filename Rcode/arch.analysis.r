setwd("C:/Users/FWL/Documents/TripleC")


class.data=read.csv("Data/classvars_v2_6JAN21.csv")
str(class.data)

## data manipulation
cd=class.data[,c(1,4,5,6,7,8,9,13,14,16,17,20,32,33,34,35,37,38,11)]
str(cd)

cd[,2]=log(cd[,2]) ## log(area)
cd[,3]=log(cd[,3]) ## log(shorelinedevfactor)
cd[,4]=log(cd[,4]-min(cd[,4]-100)) ## log(elev)
cd[,8]=log(cd[,8]) ## log(precip mm per year)
cd[,10]=log(cd[,10]+1) ## log(h12_runoff+1)
cd[,19]=log(cd[,19])
cd$pct.for=apply(cd[,14:16],1,sum) ## summing across forest types
cd$pct.wet=apply(cd[,17:18],1,sum) ## summing across wet types
#cd$conn.class.num=as.numeric(cd[,5]) ## making connectivity class numeric


## install.packages("archetypes")
library(archetypes)
na.idx=which(is.na(apply(cd[,-c(1,5)],1,sum)))
length(na.idx)
cd=cd[-na.idx,]
lonlat=cd[,c(1,7:6)]
cd.for.fit=cd[,-c(5:7,11,14:18)]
head(cd.for.fit)
head(lonlat)
## sample.idx=sample(1:nrow(cd.for.fit),2000)
fit=stepArchetypes(cd.for.fit[,-1],k=2:12,nrep=3,verbose=TRUE)
screeplot(fit,lwd=3,cex=3,pch=20,main="Choosing Number of Archetypes")
#savePlot("screePlot.jpg")
fit$archetypes

fit6=bestModel(fit[[5]])
fit6$archetypes
save(fit6, file="fit6.Rdata")

fit9=bestModel(fit[[8]])
fit9$archetypes
save(fit9,file="fit9.Rdata")

##
## PLOTS FOR 6 ARCHETYPE FIT
##

load("fit6.Rdata")
predVals=predict(fit6,newdata=cd.for.fit[,-1])
## re-ordering archetypes by popularity
idx.high.to.low=sort(apply(predVals,2,mean),decreasing=TRUE,index.return=TRUE)$ix
predVals=predVals[,idx.high.to.low]
archetypes=fit6$archetypes[idx.high.to.low,]
colnames(predVals) <- c("w.arch1","w.arch2","w.arch3","w.arch4","w.arch5","w.arch6")
str(predVals)
cd=cbind(cd,predVals)
head(cd)
summary(cd)

arch.scaled=archetypes
cd.scaled=cd.for.fit
##
for(i in 2:ncol(cd.for.fit)){
    arch.scaled[,i-1]=(arch.scaled[,i-1]-min(cd.scaled[,i]))/(max(cd.scaled[,i])-min(cd.scaled[,i]))
    cd.scaled[,i]=(cd.scaled[,i]-min(cd.scaled[,i]))/(max(cd.scaled[,i])-min(cd.scaled[,i]))
}
names.items=c("log(area)","shore.dev.fact","elev","log(precip)","tmeanC","h12runoff","OrgCarbon","glac.pct","ws.lake.area","for.pct","wet.pct","conn.class")
names(cd.scaled)[2:13]=names.items
colnames(arch.scaled)=names.items

boxplot(cd.scaled[,2:(length(names.items)+1)])
for(i in 1:nrow(arch.scaled)){
    points(arch.scaled[i,],col=i+1,type="b",lwd=4,pch=i+1,lty=1,cex=2)
}
legend(x=0,y=.75,legend=c("Archetype 1 ","Archetype 2","Archetype 3","Archetype 4","Archetype 5","Archetype 6"),col=2:7,lwd=4,pch=2:7,cex=2,bg=grey(.95))
savePlot("archetypes-20210113.jpg")

boxplot(cd[,23:28],col=2:7,names=c("Archetype 1","Archetype 2","Archetype 3","Archetype 4","Archetype 5","Archetype 6"),main="Weights of each Archetype (boxplots of estimated weights in all lakes)")
savePlot("weights.boxplot.jpg")
apply(cd[,23:28],2,mean)

aa.results <- list(cd=cd,archetypes=archetypes)
save(aa.results,file="aa.results6-20210113.Rdata")


str(cd)


lake.col=rep(NA,ncol(cd))
for(i in 1:6){
    idx=which(predVals[,i]>.5)
    lake.col[idx]=i+1
}
summary(lake.col)
plot(cd[,7:6],pch=20,cex=.5,col=lake.col)
savePlot("allarch.jpg")

par(mfrow=c(2,3))
for(i in 1:6){
    plot(cd[,7:6],pch=".",cex=.5,main=paste("arch ",i),col=grey(.95))
    idx=which(predVals[,i]>.5)
    points(cd[idx,7:6],pch=20,cex=0.5,col=i+1)
    if(i==6){
        points(cd[idx,7:6],pch=20,cex=1,col=i+1)
        points(cd[idx,7:6],pch=20,cex=.5,col=1)
    }
}
savePlot("split.arch.jpg")

for(i in 1:6){
    layout(matrix(c(1,1,1,1,2,2),nrow=3,byrow=TRUE))
    plot(cd[,7:6],pch=".",cex=.5,main=paste("arch ",i),col=grey(.95))
    idx=which(predVals[,i]>.5)
    points(cd[idx,7:6],pch=20,cex=0.5,col=i+1)
    boxplot(cd.scaled[,2:(length(names.items)+1)])
    points(arch.scaled[i,],col=i+1,type="b",lwd=4,pch=i+1,lty=1,cex=2)
    savePlot(paste("arch",i,".jpg",sep=""))
}

lake.col


##
## PLOTS FOR 9 ARCHETYPE FIT
##

load("fit9.Rdata")
predVals=predict(fit9,newdata=cd.for.fit[,-1])
## re-ordering archetypes by popularity
idx.high.to.low=sort(apply(predVals,2,mean),decreasing=TRUE,index.return=TRUE)$ix
predVals=predVals[,idx.high.to.low]
archetypes=fit9$archetypes[idx.high.to.low,]
colnames(predVals) <- c("w.arch1","w.arch2","w.arch3","w.arch4","w.arch5","w.arch6","w.arch7","w.arch8","w.arch9")
str(predVals)
cd=cbind(cd,predVals)
head(cd)
summary(cd)

arch.scaled=archetypes
cd.scaled=cd.for.fit
##
for(i in 2:ncol(cd.for.fit)){
    arch.scaled[,i-1]=(arch.scaled[,i-1]-min(cd.scaled[,i]))/(max(cd.scaled[,i])-min(cd.scaled[,i]))
    cd.scaled[,i]=(cd.scaled[,i]-min(cd.scaled[,i]))/(max(cd.scaled[,i])-min(cd.scaled[,i]))
}
names.items=c("log(area)","shore.dev.fact","elev","log(precip)","tmeanC","h12runoff","OrgCarbon","glac.pct","ws.lake.area","for.pct","wet.pct","conn.class")
names(cd.scaled)[2:13]=names.items
colnames(arch.scaled)=names.items

boxplot(cd.scaled[,2:(length(names.items)+1)])
for(i in 1:nrow(arch.scaled)){
    points(arch.scaled[i,],col=i+1,type="b",lwd=4,pch=i+1,lty=1,cex=2)
}
legend(x=0,y=.75,legend=c("Archetype 1 ","Archetype 2","Archetype 3","Archetype 4","Archetype 5","Archetype 6"),col=2:7,lwd=4,pch=2:7,cex=2,bg=grey(.95))
savePlot("archetypes9-20210113.jpg")

boxplot(cd[,23:31],col=2:7,names=c("Archetype 1","Archetype 2","Archetype 3","Archetype 4","Archetype 5","Archetype 6","Archetype 7","Archetype 8","Archetype 9"),main="Weights of each Archetype (boxplots of estimated weights in all lakes)")
savePlot("weights.boxplot.9.jpg")

aa.results <- list(cd=cd,archetypes=archetypes)
save(aa.results,file="aa.results9-20210113.Rdata")


str(cd)


lake.col=rep(NA,ncol(cd))
for(i in 1:9){
    idx=which(predVals[,i]>.5)
    lake.col[idx]=i+1
}
summary(lake.col)
plot(cd[,7:6],pch=20,cex=.5,col=lake.col)
savePlot("allarch9.jpg")

par(mfrow=c(3,3))
for(i in 1:9){
    plot(cd[,7:6],pch=".",cex=.5,main=paste("arch ",i),col=grey(.95))
    idx=which(predVals[,i]>.5)
    points(cd[idx,7:6],pch=20,cex=0.5,col=i+1)
    ## if(i==6){
    ##     points(cd[idx,7:6],pch=20,cex=1,col=i+1)
    ##     points(cd[idx,7:6],pch=20,cex=.5,col=1)
    ## }
}
savePlot("split.arch.9.jpg")

for(i in 1:9){
    layout(matrix(c(1,1,1,1,2,2),nrow=3,byrow=TRUE))
    plot(cd[,7:6],pch=".",cex=.5,main=paste("arch ",i),col=grey(.95))
    idx=which(predVals[,i]>.5)
    points(cd[idx,7:6],pch=20,cex=0.5,col=i+1)
    boxplot(cd.scaled[,2:(length(names.items)+1)])
    points(arch.scaled[i,],col=i+1,type="b",lwd=4,pch=i+1,lty=1,cex=2)
    savePlot(paste("arch9",i,".jpg",sep=""))
}

lake.col


most.arch.lakes=integer()
for(i in 1:9){
    idx=which.max(cd[,22+i])
    most.arch.lakes=rbind(most.arch.lakes,cd[idx,])
}
most.arch.lakes
idx


aa.results <- list(cd=cd,archetypes=archetypes,most.arch.lakes=most.arch.lakes)
save(aa.results,file="aa.results-20210113.Rdata")
