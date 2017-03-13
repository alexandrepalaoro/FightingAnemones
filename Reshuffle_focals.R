rm(list=ls())

## Here is the code for the graphs on reallocating the focal individuals.
## I am not providing the code for the reallocation because I did not perform it
## in R. Rather, I performed in Microsoft Excel (the data was already there,
## so it was easier for me).

Data<-read.csv("reshuffle_focals.csv",h=T)

#tiff(file="reshuffling_focals.tiff",units="mm",width=210,height=180,res=600,
#     compression="lzw")

prop.won<-prop.table(table(Data$Contest_outcome,
                           interaction(Data$Focal_treatment,Data$Opponent_treatment)),
                     margin=2)[2,]
prop.won<-prop.won[-1]
prop.won<-prop.won[-3]
names(prop.won)<-c("No flow","Flow")
par(mfrow=c(2,3))
barplot(prop.won,ylab="Proportion of fights won",ylim=c(0,1),xlab="Focal individual",
        col="black",las=1,bty='l')
text(2.17,0.908,"(a)")


prop.won1<-prop.table(table(Data$Contest_1,
                            interaction(Data$Focal_1,Data$Opponent_1)),
                      margin=2)[2,]
prop.won1<-prop.won1[-1]
prop.won1<-prop.won1[-3]
names(prop.won1)<-c("No flow","Flow")
barplot(prop.won1,ylab="Proportion of fights won",ylim=c(0,1),xlab="Focal individual",
        col="black",las=1,bty='l')
text(2.17,0.908,"(b)")


prop.won2<-prop.table(table(Data$Contest_2,
                            interaction(Data$Focal_2,Data$Opponent_2)),
                      margin=2)[2,]
prop.won2<-prop.won2[-1]
prop.won2<-prop.won2[-3]
names(prop.won2)<-c("No flow","Flow")
barplot(prop.won2,ylab="Proportion of fights won",ylim=c(0,1),xlab="Focal individual",
        col="black",las=1,bty='l')
text(2.17,0.908,"(c)")

prop.won3<-prop.table(table(Data$Contest_3,
                            interaction(Data$Focal_3,Data$Opponent_3)),
                      margin=2)[2,]
prop.won3<-prop.won3[-1]
prop.won3<-prop.won3[-3]
names(prop.won3)<-c("No flow","Flow")
barplot(prop.won3,ylab="Proportion of fights won",ylim=c(0,1),xlab="Focal individual",
        col="black",las=1,bty='l')
text(2.17,0.908,"(d)")

prop.won4<-prop.table(table(Data$Contest_4,
                            interaction(Data$Focal_4,Data$Opponent_4)),
                      margin=2)[2,]
prop.won4<-prop.won4[-1]
prop.won4<-prop.won4[-3]
names(prop.won4)<-c("No flow","Flow")
barplot(prop.won4,ylab="Proportion of fights won",ylim=c(0,1),xlab="Focal individual",
        col="black",las=1,bty='l')
text(2.17,0.908,"(e)")

prop.won5<-prop.table(table(Data$Contest_5,interaction(Data$Focal_5,Data$Opponent_5)),
                      margin=2)[2,]
prop.won5<-prop.won5[-1]
prop.won5<-prop.won5[-3]
names(prop.won5)<-c("No flow","Flow")
barplot(prop.won5,ylab="Proportion of fights won",ylim=c(0,1),xlab="Focal individual",
        col="black",las=1,bty='l')
text(2.17,0.908,"(f)")
#dev.off()


#DONE :D
