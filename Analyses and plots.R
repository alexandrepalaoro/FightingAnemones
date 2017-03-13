rm(list=ls())

##
###PACKAGES NEEDED
##

require("bbmle")

##LOAD DATA

Data<-read.csv("Anemones_FightingData.csv",h=T)

### In what follows, our three tests will be coded. After each model
### I will check the model for inconsistencies (i.e. residual deviance on degrees of freedom)
### if there aren't any I will carry on to significance testing via the anova() function.

############################
## CONTEST OUTCOME MODELS ##
############################

m3 <- glm(Contest_outcome ~ Focal_treatment * Opponent_treatment + 
            scale(log(StartleResp_2_focal)) + scale(log(StartleResp_2_Opponent)) + 
            scale(log(Dry_weight_focal+1)) + scale(log(Dry_weight_opponent+1)) + 
            scale(log(Nemato_length_focal+1)) + scale(log(Nemato_length_opponent+1)), data=Data, family=binomial)
summary(m3)
anova(m3, test="Chisq")


###############################
## CONTEST ESCALATION MODELS ##
###############################

### Similar to the previous section, but using whether the contest escalated to
### acrorhagial contact or not (1,0) as the response variable 

e3<-glm(Contest_escalation ~ Focal_treatment * Opponent_treatment + 
          scale(log(StartleResp_2_focal)) + scale(log(StartleResp_2_Opponent)) + 
          scale(log(Dry_weight_focal+1)) + scale(log(Dry_weight_opponent+1)) + 
          scale(log(Nemato_length_focal+1)) + scale(log(Nemato_length_opponent+1)), data=Data, family=binomial)
summary(e3)
anova(e3, test="Chisq")


#############################
## CONTEST DURATION MODELS ##
#############################

c3<-lm(log(Contest_duration) ~ Focal_treatment * Opponent_treatment + 
         scale(log(StartleResp_2_focal)) + scale(log(StartleResp_2_Opponent)) + 
         scale(log(Dry_weight_focal+1)) + scale(log(Dry_weight_opponent+1)) + 
         scale(log(Nemato_length_focal+1)) + scale(log(Nemato_length_opponent+1)),data = Data)
summary(c3)
anova(c3)


###############################
## STARTLE RESPONSE ANALYSIS ##
###############################

dataSR<-read.csv("Anemones_StartleResponse.csv",h=T)

fligner.test(log(dataSR$Startle_Response_2)~log(dataSR$Startle_Response_1))

## Homocedasticity seems OK for a linear model, but we will check it visually later on as well

## Scaling the continuous variable 

SR1.scale<-as.vector(scale(log(dataSR$Startle_Response_1),center=T,scale=T))

## Modelling...

model1<-lm(log(Startle_Response_2)~SR1.scale*treatment,data=dataSR)

## Checking residuals 

plot(model1,col=dataSR$treatment,which=1)

## Cool, the flow group (in red) has less variance in X-axis than the no-flow group.
## Otherwise, residuals seem fine...

summary(model1)
summary.aov(model1)

## R-squared is pretty low. Flow is probably not a very important factor to alter the mean,
## but it may affect repeatability.

##############
## PLOTTING ##
##############

## This will be a four-panel figure. I will make the necessary table adjustments before plotting.

## If you don't want to save a tiff file of your plot, skip this line

tiff(file="FIGUREPANEL.tiff",units="mm",width=210,height=180,res=600,
     compression="lzw")

## This is needed to plot the figures in a panel 

par(mfrow=c(2,2),las=1,bty='l')


##############-----------PLOT 1 / Startle Response -----------######################

plot(log(Startle_Response_2)~log(Startle_Response_1),data=dataSR,
     pch=c(1,21)[as.numeric(dataSR$treatment)],
     bg=c("white","black")[as.numeric(dataSR$treatment)],cex=1.4,las=1,bty='l',
     ylab="Startle response after treatment (log)",
     xlab="Startle response before treatment (log)")

## Before proceeding to calculate the intercepts and slopes to plot them on the figure,
## we need to run a model that has the same range as plotted Y-axis (i.e. we ran the
## model before using a scaled SR1, now we need to use a log-transformed SR1). 

modelPlot<-lm(log(Startle_Response_2)~log(Startle_Response_1)*treatment,data=dataSR)

## Good! Now, to plot the fitted curves is a little bit tricky. First, we need
## to create a vector with the same range of values as our x-axis

sr.seq<-seq(min(log(dataSR$Startle_Response_1)),max(log(dataSR$Startle_Response_1)),
            by=0.01) #creating the vector

## Second, we have to create a factor with one of the levels of our data 
## (e.g. flow), and it should have the same length as the vector created
## above

flow.trt<-factor(rep("flow",length(sr.seq))) 

## Third, we generate the predicted values and create a vector with those
## values

yv<-predict(modelPlot,list(Startle_Response_1=sr.seq,treatment=flow.trt),type="response") 

#Fourth, we plot the line on the graph

lines(sr.seq,yv,lwd=2,lty=2) #plotando no gráfico

#Lastly, we rinse and repeat until we plotted all levels we want.
#In our case, we have just one extra level (i.e. no flow)

noflow.trt<-factor(rep("no-flow",length(sr.seq)))

yv.nf<-predict(modelPlot,list(Startle_Response_1=sr.seq,treatment=noflow.trt),type="response")

lines(sr.seq,yv.nf,lwd=2)

#As our last measure, we need to plot the legend

text(3.817,8.54,"(a)")

#############----------------PLOT 2 / Contest outcome-------------#####################

## This is kind of tricky plot. We need to show the proportion of fights won by each
## treatment in each role (i.e. focal/opponent). Thus, we need to calculate those 
## proportions from our table. 

prop.won<-prop.table(table(Data$Contest_outcome,
                           interaction(Data$Focal_treatment,Data$Opponent_treatment)),
                     margin=2)[2,]

## Now that we have the proportions that each group has won, we can transform it in
## a 2-by-2 matrix. This needs to be done because I will use the barplot() function,
## and it will plot the bars beside each other if it is in that formar (or so I learned).

out.plot<-matrix(prop.won,2,2,byrow=T) 
dimnames(out.plot)<-list(c("Flow","No flow"),c("Flow","No flow"))
out.plot

## Off to the plot now

barplot(out.plot,beside=T,ylab="Proportion of fights won",ylim=c(0,1),xlab="Focal individual",
        col=c("black","white"),las=1,bty='l')
legend("top", title="Opponent",c("Flow","No flow"),pch=c(15,0),bty="n",
       col=c("black",1),cex=1.2)
box()
text(1.15,0.95,"(b)")


#########------------------PLOT 3 / Contest escalation-------------------####################

## We need to do the same thing as we did in previous plot: a proportion table.

esc<-prop.table(table(Data$Contest_escalation,
                      interaction(Data$Focal_treatment,Data$Opponent_treatment)),
                margin=2)[2,]

## And then transform it to a 2-by-2 matrix

esc.plot<-matrix(esc,2,2,byrow=T)
dimnames(esc.plot)<-list(c("Flow","No flow"),c("Flow","No flow"))
esc.plot
barplot(esc.plot,beside=T,ylab="Proportion of fights that escalated to physical contact",
        ylim=c(0,1),xlab="Focal individual",col=c("black","white"),las=1)
box()

text(1.1,0.96,"(c)")

#######--------------------PLOT 4 / Contest duration------------------#####################

## We need to calculate the means and the error bars to make barplots of contest duration
## So, let's calculate it.

xxx<-tapply(log(Data$Contest_duration),list(Data$Focal_treatment,Data$Opponent_treatment),mean)
yyy<-tapply(log(Data$Contest_duration),list(Data$Focal_treatment,Data$Opponent_treatment),sd)
zzz<-tapply(log(Data$Contest_duration),list(Data$Focal_treatment,Data$Opponent_treatment),length)

## Now, the error bars

errorbar<-yyy/sqrt(zzz)

## And finally, let's plot!

barx<-barplot(xxx,beside=T,ylim=c(0,9),yaxt="n",ylab="Contest duration (log)",
              xlab="Focal individual",col=c("black","white"),xaxt='n')
arrows(barx,xxx+errorbar,barx,xxx,angle=90,code=1,length=0.1)

axis(side=2,at=c(0,1,2,3,4,5,6,7,8,9),las=1)
axis(1,at=c(2,5),c("Flow","No flow"))

box()
text(1.1,8.68,"(d)")


#DONE :D
