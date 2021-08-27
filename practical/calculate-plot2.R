#Calculate_means.R: run this file to perform calculations and statistics. Run all lines of Data_entry.R and totalhairs_calculate_plot.R to load data first!!!
library(signal)
library(rgl)
require(plotrix)
require(ggplot2)

home<-"/Users/lwaldrop/Dropbox/PNAS-SK-LW/data_LW_2015_11_19"
work<-"/Users/Spectre/Dropbox/PNAS-SK-LW/data_LW_2015_11_19"

location<-work

n=3   #number of replicates

findMeans<-function(set1data.time,set1data.Cadj,set2data.time,set2data.Cadj,set3data.time,set3data.Cadj){
	if(length(set1data.Cadj)==length(set2data.Cadj) & length(set2data.Cadj)==length(set3data.Cadj)){		
		meanCadj<-matrix(0,length(set1data.time),1)
		SDCadj<-matrix(0,length(set1data.time),1)
		flux<-matrix(0,length(set1data.time),1)
		for (i in 1:length(set1data.time)){
			a<-c(set1data.Cadj[i],set2data.Cadj[i],set3data.Cadj[i])
			meanCadj[i]<-mean(a)
			SDCadj[i]<-sd(a)
		}
		for(i in 2:length(flux)){
			flux[i] = abs(meanCadj[i+1]-meanCadj[i])/abs(set1data.time[i+1]-set1data.time[i])
		}
		dataresult<-data.frame(set1data.time,set1data.Cadj,set2data.Cadj,set3data.Cadj,meanCadj,SDCadj,flux)
		names(dataresult)<-c("time","set1","set2","set3","mean","SD","flux")
		return(dataresult)
		
	} else{
		if(length(set1data.Cadj)<=length(set2data.Cadj) & length(set1data.Cadj)<=length(set3data.Cadj)){
			timeint<-set1data.time
		} else if (length(set2data.Cadj)<=length(set1data.Cadj) & length(set2data.Cadj)<=length(set3data.Cadj)) {
			timeint<-set2data.time
		} else if(length(set3data.Cadj)<=length(set1data.Cadj) & length(set3data.Cadj)<=length(set2data.Cadj)) {
			timeint<-set3data.time	
		} else {
			timeint<-set1data.time
		}
		set1dataint.Cadj<-interp1(set1data.time,set1data.Cadj,timeint,method=c("linear"),extrap=FALSE)
		set2dataint.Cadj<-interp1(set2data.time,set2data.Cadj,timeint,method=c("linear"),extrap=FALSE)
		set3dataint.Cadj<-interp1(set3data.time,set3data.Cadj,timeint,method=c("linear"),extrap=FALSE)
		
		meanCadj<-matrix(0,length(timeint),1)		
		SDCadj<-matrix(0,length(timeint),1)
		flux<-matrix(0,length(timeint),1)
		for (i in 1:length(timeint)){
			a<-c(set1dataint.Cadj[i],set2dataint.Cadj[i],set3dataint.Cadj[i])
			meanCadj[i]<-mean(a)
			SDCadj[i]<-sd(a)
		}
		for(i in 2:length(flux)){
			flux[i] = abs(meanCadj[i+1]-meanCadj[i])/abs(timeint[i+1]-timeint[i])
		}
		

		dataresult<-data.frame(timeint,set1dataint.Cadj,set2dataint.Cadj,set3dataint.Cadj,meanCadj,SDCadj,flux)
		names(dataresult)<-c("time","set1","set2","set3","mean","SD","flux")
		return(dataresult)
	}
}

findMeans2<-function(set1data.time,set1data.Cadj,set2data.time,set2data.Cadj){
	if(length(set1data.Cadj)==length(set2data.Cadj)){		
		meanCadj<-matrix(0,length(set1data.time),1)
		SDCadj<-matrix(0,length(set1data.time),1)
		flux<-matrix(0,length(set1data.time),1)
		for (i in 1:length(set1data.time)){
			a<-c(set1data.Cadj[i],set2data.Cadj[i])
			meanCadj[i]<-mean(a)
			SDCadj[i]<-sd(a)
		}
		#for(i in 2:length(flux)){
		#	flux[i] = abs(meanCadj[i+1]-meanCadj[i])/abs(set1data.time[i+1]-set1data.time[i])
		#}
		dataresult<-data.frame(set1data.time,set1data.Cadj,set2data.Cadj,meanCadj,SDCadj,flux)
		names(dataresult)<-c("time","set1","set2","mean","SD","flux")
		return(dataresult)
		
	} else{
		if(length(set1data.Cadj)<=length(set2data.Cadj)){
			timeint<-set1data.time
		} else if (length(set2data.Cadj)<=length(set1data.Cadj) & length(set2data.Cadj)<=length(set3data.Cadj)) {
			timeint<-set2data.time
		#} else if(length(set3data.Cadj)<=length(set1data.Cadj) & length(set3data.Cadj)<=length(set2data.Cadj)) {
		#	timeint<-set3data.time	
		} else {
			timeint<-set1data.time
		}
		set1dataint.Cadj<-interp1(set1data.time,set1data.Cadj,timeint,method=c("linear"),extrap=FALSE)
		set2dataint.Cadj<-interp1(set2data.time,set2data.Cadj,timeint,method=c("linear"),extrap=FALSE)

		
		meanCadj<-matrix(0,length(timeint),1)		
		SDCadj<-matrix(0,length(timeint),1)
		flux<-matrix(0,length(timeint),1)
		for (i in 1:length(timeint)){
			a<-c(set1dataint.Cadj[i],set2dataint.Cadj[i])
			meanCadj[i]<-mean(a)
			SDCadj[i]<-sd(a)
		}
		for(i in 2:length(flux)){
			flux[i] = abs(meanCadj[i+1]-meanCadj[i])/abs(timeint[i+1]-timeint[i])
		}
		

		dataresult<-data.frame(timeint,set1dataint.Cadj,set2dataint.Cadj,meanCadj,SDCadj,flux)
		names(dataresult)<-c("time","set1","set2","mean","SD","flux")
		return(dataresult)
	}
}


#Sets working directory 
setwd(location)

#WARNING! Run totalhairs_calculate_plot.R script for getting effective/all capture area!

#95% Confidence intervals:

findCI<-function(data){
	mean<-max(data$mean,na.rm=TRUE)  #mean
	CIhigh<-max(data$mean,na.rm=TRUE)+qt(0.975,df=n-1)*max(data$SD,na.rm=TRUE) # high 
	CIlow<-max(data$mean,na.rm=TRUE)-qt(0.975,df=n-1)*max(data$SD,na.rm=TRUE) # low
	out<-data.frame(mean,CIlow,CIhigh)
	return(out)}


# Sets up lines for plotting.
marine.flickx = c(0.0152,0.0152)
marine.returnx = c(0.0152+0.025,0.0152+0.025)
hermit.flickx = c(0.0782,0.0782)
hermit.returnx = c(0.0782+0.0603,0.0782+0.0603)
y.line = c(-1,1e8)
clwd=1



###############################
##### Thin filament plot ######

#calculate mean at each time step

hermitair1<-findMeans(hermitair11.perstepdata$V1,hermitair11.perstepdata$Cadj/hermitair1.d,hermitair12.perstepdata$V1,hermitair12.perstepdata$Cadj/hermitair1.d,hermitair13.perstepdata$V1,hermitair13.perstepdata$Cadj/hermitair1.d)

hermitwater1<-findMeans(hermitwater11.perstepdata$V1,hermitwater11.perstepdata$Cadj/hermitwater1.d,hermitwater12.perstepdata$V1,hermitwater12.perstepdata$Cadj/hermitwater1.d,hermitwater13.perstepdata$V1,hermitwater13.perstepdata$Cadj/hermitwater1.d)

marineair1<-findMeans(marineair11.perstepdata$V1,marineair11.perstepdata$Cadj/marineair1.d,marineair12.perstepdata$V1,marineair12.perstepdata$Cadj/marineair1.d,marineair13.perstepdata$V1,marineair13.perstepdata$Cadj/marineair1.d) 

marinewater1<-findMeans(marinewater11.perstepdata$V1,marinewater11.perstepdata$Cadj/marinewater1.d,marinewater12.perstepdata$V1,marinewater12.perstepdata$Cadj/marinewater1.d,marinewater13.perstepdata$V1,marinewater13.perstepdata$Cadj/marinewater1.d)



hermitairdwater<-findMeans2(hermitairdwater2.perstepdata$V1,hermitairdwater2.perstepdata$Cadj/hermitairdwater.d,hermitairdwater3.perstepdata$V1,hermitairdwater3.perstepdata$Cadj/hermitairdwater.d)

hermitwaterdair<-findMeans(hermitwaterdair1.perstepdata$V1,hermitwaterdair1.perstepdata$Cadj/hermitwaterdair.d,hermitwaterdair2.perstepdata$V1,hermitwaterdair2.perstepdata$Cadj/hermitwaterdair.d,hermitwaterdair3.perstepdata$V1,hermitwaterdair3.perstepdata$Cadj/hermitwaterdair.d)

marineairdwater<-findMeans(marineairdwater1.perstepdata$V1,marineairdwater1.perstepdata$Cadj/marineairdwater.d,marineairdwater2.perstepdata$V1,marineairdwater2.perstepdata$Cadj/marineairdwater.d,marineairdwater3.perstepdata$V1,marineairdwater3.perstepdata$Cadj/marineairdwater.d)

marinewaterdair<-findMeans(marinewaterdair1.perstepdata$V1,marinewaterdair1.perstepdata$Cadj/marinewaterdair.d,marinewaterdair2.perstepdata$V1,marinewaterdair2.perstepdata$Cadj/marinewaterdair.d,marinewaterdair3.perstepdata$V1,marinewaterdair3.perstepdata$Cadj/marinewaterdair.d)


###############################


###############################
##### Thick filament plot ######

#calculate mean at each time step

hermitair2<-findMeans(hermitair21.perstepdata$V1,hermitair21.perstepdata$Cadj/hermitair2.d,hermitair22.perstepdata$V1,hermitair22.perstepdata$Cadj/hermitair2.d,hermitair23.perstepdata$V1,hermitair23.perstepdata$Cadj/hermitair2.d)

hermitwater2<-findMeans(hermitwater21.perstepdata$V1,hermitwater21.perstepdata$Cadj/hermitwater2.d,hermitwater22.perstepdata$V1,hermitwater22.perstepdata$Cadj/hermitwater2.d,hermitwater23.perstepdata$V1,hermitwater23.perstepdata$Cadj/hermitwater2.d)

marineair2<-findMeans(marineair21.perstepdata$V1,marineair21.perstepdata$Cadj/marineair2.d,marineair22.perstepdata$V1,marineair22.perstepdata$Cadj/marineair2.d,marineair23.perstepdata$V1,marineair23.perstepdata$Cadj/marineair2.d)

marinewater2<-findMeans(marinewater21.perstepdata$V1,marinewater21.perstepdata$Cadj/marinewater2.d,marinewater22.perstepdata$V1,marinewater22.perstepdata$Cadj/marinewater2.d,marinewater23.perstepdata$V1,marinewater23.perstepdata$Cadj/marinewater2.d)

hermitairTswap<-findMeans(hermitairTswap1.perstepdata$V1,hermitairTswap1.perstepdata$Cadj/hermitairTswap.d,hermitairTswap2.perstepdata$V1,hermitairTswap2.perstepdata$Cadj/hermitairTswap.d,hermitairTswap3.perstepdata$V1,hermitairTswap3.perstepdata$Cadj/hermitairTswap.d)

hermitwaterTswap<-findMeans(hermitwaterTswap1.perstepdata$V1,hermitwaterTswap1.perstepdata$Cadj/hermitwaterTswap.d,hermitwaterTswap2.perstepdata$V1,hermitwaterTswap2.perstepdata$Cadj/hermitwaterTswap.d,hermitwaterTswap3.perstepdata$V1,hermitwaterTswap3.perstepdata$Cadj/hermitwaterTswap.d)

marineairTswap<-findMeans(marineairTswap1.perstepdata$V1,marineairTswap1.perstepdata$Cadj/marineairTswap.d,marineairTswap2.perstepdata$V1,marineairTswap2.perstepdata$Cadj/marineairTswap.d,marineairTswap3.perstepdata$V1,marineairTswap3.perstepdata$Cadj/marineairTswap.d)

marinewaterTswap<-findMeans(marinewaterTswap1.perstepdata$V1,marinewaterTswap1.perstepdata$Cadj/marinewaterTswap.d,marinewaterTswap2.perstepdata$V1,marinewaterTswap2.perstepdata$Cadj/marinewaterTswap.d,marinewaterTswap3.perstepdata$V1,marinewaterTswap3.perstepdata$Cadj/marinewaterTswap.d)

############### RESHAPE DATA FOR PLOTS #################

plot1<-data.frame(marineair1$time,marineair1$mean,marineair1$SD,marinewater1$mean,marinewater1$SD,hermitair1$mean,hermitair1$SD,hermitwater1$mean,hermitwater1$SD)
names(plot1)<-c("time","marineairMean","marineairSD","marinewaterMean","marinewaterSD","hermitairMean","hermitairSD","hermitwaterMean","hermitwaterSD")

############### PLOTS #################
marine.flickx = c(0.0152)
marine.returnx = c(0.0152+0.025)
hermit.flickx = c(0.0782)
hermit.returnx = c(0.0782+0.0603)


p1<-ggplot(marineair1,aes(time))+ylab(expression(C/(C[infinity]%.%d)~(mm^-1)))+xlab('Time (s)')+ylim(min(hermitwater1$mean-qt(0.975,df=2)*hermitwater1$SD),0.15)+xlim(0,hermit.returnx)+geom_line(aes(y=mean),colour="red",lty=1)+geom_ribbon(aes(ymin=mean-qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="red",alpha=0.2) +geom_line(data=marinewater1,aes(x=time,y=mean),colour="blue",lty=1)+geom_ribbon(data=marinewater1,aes(ymin=mean- qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="blue",alpha=0.2) +geom_line(data=hermitwater1,aes(x=time,y=mean),colour="blue",lty=2)+geom_ribbon(data=hermitwater1,aes(ymin=mean-qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="blue",alpha=0.2) +geom_line(data=hermitair1,aes(x=time,y=mean),colour="red",lty=2)+geom_ribbon(data=hermitair1,aes(ymin=mean-qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="red",alpha=0.2) +geom_vline(xintercept=marine.flickx,linetype="dotted",color="gray")+geom_vline(xintercept=marine.returnx,linetype="dotted",color="black")+geom_vline(xintercept=hermit.flickx,color="gray")+geom_vline(xintercept=hermit.returnx,color="black") +annotate("text", label = "Re water", x = 0.07, y = 0.045, size = 3, colour = "blue") +annotate("text", label = "Re water", x = 0.1, y = 0.015, size = 3, colour = "blue") +annotate("text", label = "Re air", x = 0.1, y = 0.070, size = 3, colour = "red") +annotate("text", label = "Re air", x = 0.1, y = 0.135, size = 3, colour = "red")+theme_bw()+theme(axis.title.x=element_text(face="plain"))

p2<-ggplot(marinewaterdair,aes(time))+ylab(expression(C/(C[infinity]%.%d)~(mm^-1)))+xlab('Time (s)')+ylim(min(marineairdwater$mean- qt(0.975,df=2)*marineairdwater$SD),0.15)+xlim(0,hermit.returnx)+geom_line(aes(y=mean),colour="darkred",lty=1)+geom_ribbon(aes(ymin=mean-qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="darkred",alpha=0.2) +geom_line(data=marineairdwater,aes(x=time,y=mean),colour="darkblue",lty=1)+geom_ribbon(data=marineairdwater,aes(ymin=mean- qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="darkblue",alpha=0.2) +geom_line(data=hermitairdwater,aes(x=time,y=mean),colour="darkblue",lty=2)+geom_ribbon(data=hermitairdwater,aes(ymin=mean-qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="darkblue",alpha=0.2) +geom_line(data=hermitwaterdair,aes(x=time,y=mean),colour="darkred",lty=2)+geom_ribbon(data=hermitwaterdair,aes(ymin=mean-qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="darkred",alpha=0.2) +geom_vline(xintercept=marine.flickx,linetype="dotted",color="gray")+geom_vline(xintercept=marine.returnx,linetype="dotted",color="black")+geom_vline(xintercept=hermit.flickx,color="gray")+geom_vline(xintercept=hermit.returnx,color="black") +annotate("text", label = "Re air", x = 0.065, y = 0.05, size = 3, colour = "darkblue") +annotate("text", label = "Re air", x = 0.1, y = 0.020, size = 3, colour = "darkblue") +annotate("text", label = "Re water", x = 0.1, y = 0.095, size = 3, colour = "darkred") +annotate("text", label = "Re water", x = 0.1, y = 0.13, size = 3, colour = "darkred")+theme_bw()+theme(axis.title.x=element_text(face="plain"))

p3<-ggplot(marineair2,aes(time))+ylab(expression(C/(C[infinity]%.%d)~(mm^-1)))+xlab('Time (s)')+ylim(min(hermitwater2$mean-qt(0.975,df=2)*hermitwater2$SD)-2,32)+xlim(0,hermit.returnx)+geom_line(aes(y=mean),colour="red",lty=1)+geom_ribbon(aes(ymin=mean-qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="red",alpha=0.2) +geom_line(data=marinewater2,aes(x=time,y=mean),colour="blue",lty=1)+geom_ribbon(data=marinewater2,aes(ymin=mean- qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="blue",alpha=0.2) +geom_line(data=hermitwater2,aes(x=time,y=mean),colour="blue",lty=2)+geom_ribbon(data=hermitwater2,aes(ymin=mean-qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="blue",alpha=0.2) +geom_line(data=hermitair2,aes(x=time,y=mean),colour="red",lty=2)+geom_ribbon(data=hermitair2,aes(ymin=mean-qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="red",alpha=0.2) +geom_vline(xintercept=marine.flickx,linetype="dotted",color="gray")+geom_vline(xintercept=marine.returnx,linetype="dotted",color="black")+geom_vline(xintercept=hermit.flickx,color="gray")+geom_vline(xintercept=hermit.returnx,color="black") +annotate("text", label = "T Terrestrial", x = 0.10, y = 16, size = 3, colour = "red") +annotate("text", label = "T Marine", x = 0.1, y = 7, size = 3, colour = "red") +annotate("text", label = "T Marine", x = 0.12, y = 3, size = 3, colour = "blue") +annotate("text", label = "T Terrestrial", x = 0.11, y = -1.75, size = 3, colour = "blue")+theme_bw()+theme(axis.title.x=element_text(face="plain"))

p4<-ggplot(marinewaterTswap,aes(time))+ylab(expression(C/(C[infinity]%.%d)~(mm^-1)))+xlab('Time (s)')+ylim(-2,32)+xlim(0,hermit.returnx)+geom_line(aes(y=mean),colour="darkblue",lty=1)+geom_ribbon(aes(ymin=mean-qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="darkblue",alpha=0.2) +geom_line(data=marineairTswap,aes(x=time,y=mean),colour="darkred",lty=1)+geom_ribbon(data=marineairTswap,aes(ymin=mean- qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="darkred",alpha=0.2) +geom_line(data=hermitairTswap,aes(x=time,y=mean),colour="darkred",lty=2)+geom_ribbon(data=hermitairTswap,aes(ymin=mean-qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="darkred",alpha=0.2) +geom_line(data=hermitwaterTswap,aes(x=time,y=mean),colour="darkblue",lty=2)+geom_ribbon(data=hermitwaterTswap,aes(ymin=mean-qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="darkblue",alpha=0.2) +geom_vline(xintercept=marine.flickx,linetype="dotted",color="gray")+geom_vline(xintercept=marine.returnx,linetype="dotted",color="black")+geom_vline(xintercept=hermit.flickx,color="gray")+geom_vline(xintercept=hermit.returnx,color="black")+annotate("text", label = "T Terrestrial", x = 0.11, y = 21, size = 3, colour = "darkred") +annotate("text", label = "T Terrestrial", x = 0.11, y = 11, size = 3, colour = "darkblue") +annotate("text", label = "T Marine", x = 0.12, y = 4.5, size = 3, colour = "darkred") +annotate("text", label = "T Marine", x = 0.1, y = -1.75, size = 3, colour = "darkblue")+theme_bw()+theme(axis.title.x=element_text(face="plain"))

plot_grid(p3)

setwd(location)

pdf("Combined_bothconditions.pdf",width=7*0.85,height=6.5*0.85) # For an EPS.

plot_grid(p1,p2,p3,p4,labels=c("A","B","C","D"),label_size=12)

dev.off()



p5<-ggplot(marineair1,aes(time))+ylab(expression(paste(Delta,C/(C[infinity]%.%d),Delta,t)))+xlab('Time (s)')+ylim(min(hermitwater1$mean-qt(0.975,df=2)*hermitwater1$SD),0.15)+xlim(0,hermit.returnx)+geom_line(aes(y=mean),colour="red",lty=1)+geom_ribbon(aes(ymin=mean-qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="red",alpha=0.2) +geom_line(data=marinewater1,aes(x=time,y=mean),colour="blue",lty=1)+geom_ribbon(data=marinewater1,aes(ymin=mean- qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="blue",alpha=0.2) +geom_line(data=hermitwater1,aes(x=time,y=mean),colour="blue",lty=2)+geom_ribbon(data=hermitwater1,aes(ymin=mean-qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="blue",alpha=0.2) +geom_line(data=hermitair1,aes(x=time,y=mean),colour="red",lty=2)+geom_ribbon(data=hermitair1,aes(ymin=mean-qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="red",alpha=0.2) +geom_vline(xintercept=marine.flickx,linetype="dotted",color="gray")+geom_vline(xintercept=marine.returnx,linetype="dotted",color="black")+geom_vline(xintercept=hermit.flickx,color="gray")+geom_vline(xintercept=hermit.returnx,color="black")

p6<-ggplot(marinewaterdair,aes(time))+ylab(expression(paste(Delta,C/(C[infinity]%.%d),Delta,t)))+xlab('Time (s)')+ylim(min(marineairdwater$mean- qt(0.975,df=2)*marineairdwater$SD),0.15)+xlim(0,hermit.returnx)+geom_line(aes(y=mean),colour="darkred",lty=1)+geom_ribbon(aes(ymin=mean-qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="darkred",alpha=0.2) +geom_line(data=marineairdwater,aes(x=time,y=mean),colour="darkblue",lty=1)+geom_ribbon(data=marineairdwater,aes(ymin=mean- qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="darkblue",alpha=0.2) +geom_line(data=hermitairdwater,aes(x=time,y=mean),colour="darkblue",lty=2)+geom_ribbon(data=hermitairdwater,aes(ymin=mean-qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="darkblue",alpha=0.2) +geom_line(data=hermitwaterdair,aes(x=time,y=mean),colour="darkred",lty=2)+geom_ribbon(data=hermitwaterdair,aes(ymin=mean-qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="darkred",alpha=0.2) +geom_vline(xintercept=marine.flickx,linetype="dotted",color="gray")+geom_vline(xintercept=marine.returnx,linetype="dotted",color="black")+geom_vline(xintercept=hermit.flickx,color="gray")+geom_vline(xintercept=hermit.returnx,color="black")

p7<-ggplot(marineair2,aes(time))+ylab(expression(paste(Delta,C/(C[infinity]%.%d),Delta,t)))+xlab('Time (s)')+ylim(min(hermitwater2$mean-qt(0.975,df=2)*hermitwater2$SD),32)+xlim(0,hermit.returnx)+geom_line(aes(y=mean),colour="red",lty=1)+geom_ribbon(aes(ymin=mean-qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="red",alpha=0.2) +geom_line(data=marinewater2,aes(x=time,y=mean),colour="blue",lty=1)+geom_ribbon(data=marinewater2,aes(ymin=mean- qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="blue",alpha=0.2) +geom_line(data=hermitwater2,aes(x=time,y=mean),colour="blue",lty=2)+geom_ribbon(data=hermitwater2,aes(ymin=mean-qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="blue",alpha=0.2) +geom_line(data=hermitair2,aes(x=time,y=mean),colour="red",lty=2)+geom_ribbon(data=hermitair2,aes(ymin=mean-qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="red",alpha=0.2) +geom_vline(xintercept=marine.flickx,linetype="dotted",color="gray")+geom_vline(xintercept=marine.returnx,linetype="dotted",color="black")+geom_vline(xintercept=hermit.flickx,color="gray")+geom_vline(xintercept=hermit.returnx,color="black")

p8<-ggplot(marinewaterTswap,aes(time))+ylab(expression(paste(Delta,C/(C[infinity]%.%d),Delta,t)))+xlab('Time (s)')+ylim(0,32)+xlim(0,hermit.returnx)+geom_line(aes(y=mean),colour="darkblue",lty=1)+geom_ribbon(aes(ymin=mean-qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="darkblue",alpha=0.2) +geom_line(data=marineairTswap,aes(x=time,y=mean),colour="darkred",lty=1)+geom_ribbon(data=marineairTswap,aes(ymin=mean- qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="darkred",alpha=0.2) +geom_line(data=hermitairTswap,aes(x=time,y=mean),colour="darkred",lty=2)+geom_ribbon(data=hermitairTswap,aes(ymin=mean-qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="darkred",alpha=0.2) +geom_line(data=hermitwaterTswap,aes(x=time,y=mean),colour="darkblue",lty=2)+geom_ribbon(data=hermitwaterTswap,aes(ymin=mean-qt(0.975,df=2)*SD,ymax=mean+qt(0.975,df=2)*SD),fill="darkblue",alpha=0.2) +geom_vline(xintercept=marine.flickx,linetype="dotted",color="gray")+geom_vline(xintercept=marine.returnx,linetype="dotted",color="black")+geom_vline(xintercept=hermit.flickx,color="gray")+geom_vline(xintercept=hermit.returnx,color="black")
