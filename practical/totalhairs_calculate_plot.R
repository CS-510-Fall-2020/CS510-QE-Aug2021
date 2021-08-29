#Totalhairs_calculate_plot.R: Calculates mean values on data after loading with data_entry.R for totalhairs only!

require(ggplot2)
require(cowplot)

home<-"/Users/lwaldrop/Dropbox/PNAS-SK-LW/data_LW_2015_11_19"
work<-"/Users/waldrop/Dropbox (Chapman)/courses/CS510/QualifyingExams/CS510-QE-Aug2021/practical"

location<-work


#Setting cutoff for hair inclusion.
cutoffuse<-1e-10 # Use -1 for all area

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


# Total capture areas determined by summing circumferences of all hairs for each model.
hermit.d <-0.0519500076160704
marine.d <-0.5796


###############################
###### Hair Calculations ######

#### Hermit Crabs #####

setwd(paste(location,"/set1",sep=""))
hermithairsset1<-read.csv('hermithairseffectarea.csv',header=FALSE)
setwd(paste(location,"/set2",sep=""))
hermithairsset2<-read.csv('hermithairseffectarea.csv',header=FALSE)
setwd(paste(location,"/set3",sep=""))
hermithairsset3<-read.csv('hermithairseffectarea.csv',header=FALSE)

setwd(location)

hermitair1.d<-((hermithairsset1$V1+hermithairsset2$V1+hermithairsset3$V1)/n)*hermit.d
hermitwater1.d<-((hermithairsset1$V2+hermithairsset2$V2+hermithairsset3$V2)/n)*hermit.d
hermitwaterdair.d<-((hermithairsset1$V3+hermithairsset2$V3+hermithairsset3$V3)/n)*hermit.d
hermitairdwater.d<-((hermithairsset1$V4+hermithairsset2$V4+hermithairsset3$V4)/n)*hermit.d
hermitair2.d<-((hermithairsset1$V5+hermithairsset2$V5+hermithairsset3$V5)/n)*hermit.d
hermitwater2.d<-((hermithairsset1$V6+hermithairsset2$V6+hermithairsset3$V6)/n)*hermit.d
hermitwaterTswap.d<-((hermithairsset1$V7+hermithairsset2$V7+hermithairsset3$V7)/n)*hermit.d
hermitairTswap.d<-((hermithairsset1$V8+hermithairsset2$V8+hermithairsset3$V8)/n)*hermit.d

hermitair1totals<-findMeans(hermitair11.totals$V1,hermitair11.totals$V3/hermitair1.d,hermitair12.totals$V1,hermitair12.totals$V3/hermitair1.d,hermitair13.totals$V1,hermitair13.totals$V3/hermitair1.d)
hermitair1totals$ratios<-hermitair1totals$mean/sum(hermitair1totals$mean)*100

hermitwater1totals<-findMeans(hermitwater11.totals$V1,hermitwater11.totals$V3/hermitwater1.d,hermitwater12.totals$V1,hermitwater12.totals$V3/hermitwater1.d,hermitwater13.totals$V1,hermitwater13.totals$V3/hermitwater1.d)
hermitwater1totals$ratios<-hermitwater1totals$mean/sum(hermitwater1totals$mean)*100

hermitairdwatertotals<-findMeans(hermitairdwater1.totals$V1,hermitairdwater1.totals$V3/hermitairdwater.d,hermitairdwater2.totals$V1,hermitairdwater2.totals$V3/hermitairdwater.d,hermitairdwater3.totals$V1,hermitairdwater3.totals$V3/hermitairdwater.d)
hermitairdwatertotals$ratios<-hermitairdwatertotals$mean/sum(hermitairdwatertotals$mean)*100

hermitwaterdairtotals<-findMeans(hermitwaterdair1.totals$V1,hermitwaterdair1.totals$V3/hermitwaterdair.d,hermitwaterdair2.totals$V1,hermitwaterdair2.totals$V3/hermitwaterdair.d,hermitwaterdair3.totals$V1,hermitwaterdair3.totals$V3/hermitwaterdair.d)
hermitwaterdairtotals$ratios<-hermitwaterdairtotals$mean/sum(hermitwaterdairtotals$mean)*100

hermitair2totals<-findMeans(hermitair21.totals$V1,hermitair21.totals$V3/hermitair2.d,hermitair22.totals$V1,hermitair22.totals$V3/hermitair2.d,hermitair23.totals$V1,hermitair23.totals$V3/hermitair2.d)
hermitair2totals$ratios<-hermitair2totals$mean/sum(hermitair2totals$mean)*100

hermitwater2totals<-findMeans(hermitwater21.totals$V1,hermitwater21.totals$V3/hermitwater2.d,hermitwater22.totals$V1,hermitwater22.totals$V3/hermitwater2.d,hermitwater23.totals$V1,hermitwater23.totals$V3/hermitwater2.d)
hermitwater2totals$ratios<-hermitwater2totals$mean/sum(hermitwater2totals$mean)*100

hermitairTswaptotals<-findMeans(hermitairTswap1.totals$V1,hermitairTswap1.totals$V3/hermitairTswap.d,hermitairTswap2.totals$V1,hermitairTswap2.totals$V3/hermitairTswap.d,hermitairTswap3.totals$V1,hermitairTswap3.totals$V3/hermitairTswap.d)
hermitairTswaptotals$ratios<-hermitairTswaptotals$mean/sum(hermitairTswaptotals$mean)*100

hermitwaterTswaptotals<-findMeans(hermitwaterTswap1.totals$V1,hermitwaterTswap1.totals$V3/hermitwaterTswap.d,hermitwaterTswap2.totals$V1,hermitwaterTswap2.totals$V3/hermitwaterTswap.d,hermitwaterTswap3.totals$V1,hermitwaterTswap3.totals$V3/hermitwaterTswap.d)
hermitwaterTswaptotals$ratios<-hermitwaterTswaptotals$mean/sum(hermitwaterTswaptotals$mean)*100


#### Marine Crabs #####

marineair1totals<-findMeans(marineair11.totals$V1,marineair11.totals$V5,marineair12.totals$V1,marineair12.totals$V5,marineair13.totals$V1,marineair13.totals$V5)
marineair1totals$ratios<-marineair1totals$mean/sum(marineair1totals$mean)*100
marineair1.numhairs=length(marineair1totals$mean[marineair1totals$mean>=cutoffuse])
marineair1.d<-marine.d*(marineair1.numhairs/205)
marineair1totals<-findMeans(marineair11.totals$V1,marineair11.totals$V5/marineair1.d,marineair12.totals$V1,marineair12.totals$V5/marineair1.d,marineair13.totals$V1,marineair13.totals$V5/marineair1.d)

marinewater1totals<-findMeans(marinewater11.totals$V1,marinewater11.totals$V5,marinewater12.totals$V1,marinewater12.totals$V5,marinewater13.totals$V1,marinewater13.totals$V5)
marinewater1totals$ratios<-marinewater1totals$mean/sum(marinewater1totals$mean)*100
marinewater1.numhairs=length(marinewater1totals$mean[marinewater1totals$mean>=cutoffuse])
marinewater1.d<-marine.d*(marinewater1.numhairs/205)
marinewater1totals<-findMeans(marinewater11.totals$V1,marinewater11.totals$V5/marinewater1.d,marinewater12.totals$V1,marinewater12.totals$V5/marinewater1.d,marinewater13.totals$V1,marinewater13.totals$V5/marinewater1.d)

marineairdwatertotals<-findMeans(marineairdwater1.totals$V1,marineairdwater1.totals$V5,marineairdwater2.totals$V1,marineairdwater2.totals$V5,marineairdwater3.totals$V1,marineairdwater3.totals$V5)
marineairdwatertotals$ratios<-marineairdwatertotals$mean/sum(marineairdwatertotals$mean)*100
marineairdwater.numhairs=length(marineairdwatertotals$mean[marineairdwatertotals$mean>=cutoffuse])
marineairdwater.d<-marine.d*(marineairdwater.numhairs/205)
marineairdwatertotals<-findMeans(marineairdwater1.totals$V1,marineairdwater1.totals$V5/marineairdwater.d,marineairdwater2.totals$V1,marineairdwater2.totals$V5/marineairdwater.d,marineairdwater2.totals$V1,marineairdwater3.totals$V5/marineairdwater.d)

marinewaterdairtotals<-findMeans(marinewaterdair1.totals$V1,marinewaterdair1.totals$V5,marinewaterdair2.totals$V1,marinewaterdair2.totals$V5,marinewaterdair3.totals$V1,marinewaterdair3.totals$V5)
marinewaterdairtotals$ratios<-marinewaterdairtotals$mean/sum(marinewaterdairtotals$mean)*100
marinewaterdair.numhairs=length(marinewaterdairtotals$mean[marinewaterdairtotals$mean>=cutoffuse])
marinewaterdair.d<-marine.d*(marinewaterdair.numhairs/205)
marinewaterdairtotals<-findMeans(marinewaterdair1.totals$V1,marinewaterdair1.totals$V5/marinewaterdair.d,marinewaterdair2.totals$V1,marinewaterdair2.totals$V5/marinewaterdair.d,marinewaterdair2.totals$V1,marinewaterdair2.totals$V5/marinewaterdair.d)

marineair2totals<-findMeans(marineair21.totals$V1,marineair21.totals$V5,marineair22.totals$V1,marineair22.totals$V5,marineair22.totals$V1,marineair22.totals$V5)
marineair2totals$ratios<-marineair2totals$mean/sum(marineair2totals$mean)*100
marineair2.numhairs=length(marineair2totals$mean[marineair2totals$mean>=cutoffuse])
marineair2.d<-marine.d*(marineair2.numhairs/205)
marineair2totals<-findMeans(marineair21.totals$V1,marineair21.totals$V5/marineair2.d,marineair22.totals$V1,marineair22.totals$V5/marineair2.d,marineair22.totals$V1,marineair22.totals$V5/marineair2.d)

marinewater2totals<-findMeans(marinewater21.totals$V1,marinewater21.totals$V5,marinewater22.totals$V1,marinewater22.totals$V5,marinewater23.totals$V1,marinewater23.totals$V5)
marinewater2totals$ratios<-marinewater2totals$mean/sum(marinewater2totals$mean)*100
marinewater2.numhairs=length(marinewater2totals$mean[marinewater2totals$mean>=cutoffuse])
marinewater2.d<-marine.d*(marinewater2.numhairs/205)
marinewater2totals<-findMeans(marinewater21.totals$V1,marinewater21.totals$V5/marinewater2.d,marinewater22.totals$V1,marinewater22.totals$V5/marinewater2.d,marinewater22.totals$V1,marinewater22.totals$V5/marinewater2.d)

marineairTswaptotals<-findMeans(marineairTswap1.totals$V1,marineairTswap1.totals$V5,marineairTswap2.totals$V1,marineairTswap2.totals$V5,marineairTswap2.totals$V1,marineairTswap2.totals$V5)
marineairTswaptotals$ratios<-marineairTswaptotals$mean/sum(marineairTswaptotals$mean)*100
marineairTswap.numhairs=length(marineairTswaptotals$mean[marineairTswaptotals$mean>=cutoffuse])
marineairTswap.d<-marine.d*(marineairTswap.numhairs/205)
marineairTswaptotals<-findMeans(marineairTswap1.totals$V1,marineairTswap1.totals$V5/marineairTswap.d,marineairTswap2.totals$V1,marineairTswap2.totals$V5/marineairTswap.d,marineairTswap2.totals$V1,marineairTswap2.totals$V5/marineairTswap.d)

marinewaterTswaptotals<-findMeans(marinewaterTswap1.totals$V1,marinewaterTswap1.totals$V5,marinewaterTswap2.totals$V1,marinewaterTswap2.totals$V5,marinewaterTswap3.totals$V1,marinewaterTswap3.totals$V5)
marinewaterTswaptotals$ratios<-marinewaterTswaptotals$mean/sum(marinewaterTswaptotals$mean)*100
marinewaterTswap.numhairs=length(marinewaterTswaptotals$mean[marinewaterTswaptotals$mean>=cutoffuse])
marinewaterTswap.d<-marine.d*(marinewaterTswap.numhairs/205)
marinewaterTswaptotals<-findMeans(marinewaterTswap1.totals$V1,marinewaterTswap1.totals$V5/marinewaterTswap.d,marinewaterTswap2.totals$V1,marinewaterTswap2.totals$V5/marinewaterTswap.d,marinewaterTswap2.totals$V1,marinewaterTswap2.totals$V5/marinewaterTswap.d)



cutoff<-c(1e-10,1e-9,1e-8,1e-7,1e-6,5e-6,1e-5,5e-5,1e-4,2e-4,3e-4,4e-4,5e-4,6e-4,7e-4,8e-4,9e-4,1e-3,2e-3,3e-3,4e-3,5e-3,6e-3,7e-3,8e-3,1e-2,1e-1)



marineair1.numhairs<-array(0,length(cutoff))
marinewater1.numhairs<-array(0,length(cutoff))
marineair2.numhairs<-array(0,length(cutoff))
marinewater2.numhairs<-array(0,length(cutoff))
marinewaterdair.numhairs<-array(0,length(cutoff))
marineairdwater.numhairs<-array(0,length(cutoff))

a<-marineair11.totals$V5#*cinf1.marineair
b<-marinewater11.totals$V5#*cinf1.marinewater
c<-marineair21.totals$V5#*cinf2.marineair
d<-marinewater21.totals$V5#*cinf2.marinewater
e<-marinewaterdair1.totals$V5#*cinf1.marineair
f<-marineairdwater1.totals$V5#*cinf1.marinewater

for(i in 1:length(cutoff)){
	marineair1.numhairs[i]=length(a[a>cutoff[i]])
	marinewater1.numhairs[i]=length(b[b>cutoff[i]])
	marineair2.numhairs[i]=length(c[c>cutoff[i]])
	marinewater2.numhairs[i]=length(d[d>cutoff[i]])
	marinewaterdair.numhairs[i]=length(e[e>cutoff[i]])
	marineairdwater.numhairs[i]=length(f[f>cutoff[i]])
}


setEPS()
postscript("Conc_hair_numbers.eps",width=7*0.85,height=6.5*0.85) # For an EPS.
plot(cutoff,marinewater1.numhairs,col="blue",type="p",log="x",xlab="Odor concentration",ylab="Number of hairs that capture odor",ylim=c(0,205))
points(cutoff,marineair1.numhairs,col="red",pch=21)
points(cutoff,marinewater2.numhairs,col="blue",pch=19)
points(cutoff,marineair2.numhairs,col="red",pch=19)
points(cutoff,marinewaterdair.numhairs,col="darkred",pch=19)
points(cutoff,marineairdwater.numhairs,col="darkblue",pch=19)
lines(c(cutoffuse,cutoffuse),c(-10,250),col="black",lty=2)
legend("topright",bg="white",legend=c("Water Cond 1","Air Cond 1", "Water Cond 2", "Air Cond 2","Dair Rewater","Dwater Reair"),pch=c(21,21,19,19,19,19),col=c("blue","red","blue","red","darkred","darkblue"))
dev.off()


###############################
######   Hairy plots     ######

conc_water2<-marinewater2totals$mean[marinewater2totals$mean>cutoffuse]
pos_water2<-marinewater2totals$time[marinewater2totals$mean>cutoffuse]
conc_air2<-marineair2totals$mean[marineair2totals$mean>cutoffuse]
pos_air2<-marineair2totals$time[marineair2totals$mean>cutoffuse]

marinewater1cols<-(marinewater1totals$mean/max(marinewater1totals$mean))
#marinewater1cols[marinewater1cols<(-5)]<-(-5)
#marinewater1cols[is.na(marinewater1cols)]<-(-5)

marineair1cols<-(marineair1totals$mean/max(marineair1totals$mean))
#marineair1cols[marineair1cols<(-5)]<-(-5)
#marineair1cols[is.na(marineair1cols)]<-(-5)

marineairdwatercols<-(marineairdwatertotals$mean/max(marineairdwatertotals$mean))
#marineairdwatercols[marineairdwatercols<(-5)]<-(-5)
#marineairdwatercols[is.na(marineairdwatercols)]<-(-5)

marinewaterdaircols<-(marinewaterdairtotals$mean/max(marinewaterdairtotals$mean))
#marinewaterdaircols[marinewaterdaircols<(-5)]<-(-5)
#marinewaterdaircols[is.na(marinewaterdaircols)]<-(-5)

theme_set(theme_bw())
mp2<-qplot(x=marinewater11.totals$V3,y=marinewater11.totals$V4,color=marinewater1cols,xlab="X Position",ylab="Y Position",size=marinewater1cols)+theme(legend.position='none')+scale_colour_gradient(high="yellow",low="blue")
mp1<-qplot(x=marinewater11.totals$V3,y=marinewater11.totals$V4,color=marineair1cols,xlab="X Position",ylab="Y Position",size=marineair1cols)+theme_bw()+theme(legend.position='none')+scale_colour_gradient(high="yellow",low="blue")
mp4<-qplot(x=marinewater11.totals$V3,y=marinewater11.totals$V4,color=marineairdwatercols,xlab="X Position",ylab="Y Position")+theme_bw()+theme(legend.position='none')+scale_colour_gradient(high="yellow",low="blue")
mp3<-qplot(x=marinewater11.totals$V3,y=marinewater11.totals$V4,color=marinewaterdaircols,xlab="X Position",ylab="Y Position")+theme_bw()+theme(legend.position='none')+scale_colour_gradient(high="yellow",low="blue")


setEPS()
postscript("hairs_thinfilament.eps",width=7*0.85,height=6.5*0.85) # For an EPS.

plot_grid(mp1,mp2,mp3,mp4,labels=c("A","B","C","D"),label_size=12)

#multiplot(p1+annotate("text",x=-0.1,y=1.6,label="A")+theme_bw(),p3+theme_bw(),p2+theme_bw(),p4+theme_bw())  #Old function
dev.off()


###############################
###############################


work<-"/Users/Spectre/Dropbox/PNAS-SK-LW/"
home<-"/Users/lwaldrop/Dropbox/PNAS-SK-LW/"

location<-work
setwd(paste(location,"data_LW_2015_09_14/set1",sep=""))

run<-c(3116,3161,3163,3118)

for(i in 1:length(run)){
name<-paste('run',run[i],'_hermithair.csv',sep="")
data<-read.csv(name,header=FALSE)
assign(paste('run',run[i],sep=''),data)}


setwd(paste(location,"data_LW_2015_11_19",sep=""))

run3116cols<-(run3116$V3/max(run3116$V3))
run3161cols<-(run3161$V3/max(run3161$V3))
run3163cols<-(run3163$V3/max(run3163$V3))
run3118cols<-(run3118$V3/max(run3118$V3))

theme_set(theme_bw())
hp1<-qplot(x=run3116$V1+0.095,y=run3116$V2,color=run3116cols,xlab="X Position",ylab="Y Position",xlim=c(0.025,0.09),size=run3116cols)+theme(legend.position='none')+scale_colour_gradient(high="yellow",low="blue")
hp2<-qplot(x=run3161$V1+0.08,y=run3161$V2,color=run3161cols,xlab="X Position",ylab="Y Position",xlim=c(0.025,0.09),size=run3161cols)+theme(legend.position='none')+scale_colour_gradient(high="yellow",low="blue")
hp3<-qplot(x=run3163$V1,y=run3163$V2,color=run3163cols,xlab="X Position",ylab="Y Position",xlim=c(-0.055,0.01))+theme(legend.position='none')+scale_colour_gradient(high="yellow",low="blue")
hp4<-qplot(x=run3118$V1,y=run3118$V2,color=run3118cols,xlab="X Position",ylab="Y Position",xlim=c(-0.07,-0.01))+theme(legend.position='none')+scale_colour_gradient(high="yellow",low="blue")
plot_grid(hp1,hp2)

setEPS()
postscript("hairs_hermit.eps",width=7*0.85,height=6.5*0.85) # For an EPS.

plot_grid(hp1,hp2,hp3,hp4,labels=c("A","B","C","D"),label_size=12)

dev.off()


###############################
###############################
######  Combined figure  ######
setEPS()
postscript("Combined_hairs.eps",width=7,height=6) # For an EPS.

plot_grid(mp2,hp2,mp1,hp1,labels=c("A","B","C","D"),label_size=12,nrow=2,ncol=2)
dev.off()
###############################
###############################



#multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=2, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]]+theme(legend.position='none'), vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
