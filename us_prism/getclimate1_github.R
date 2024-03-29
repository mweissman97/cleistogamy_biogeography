library(raster)
library(sp)
library(rgdal)
library(rgeos)
fdstr<-c("precipitation/","mean_temp/")
flstr<-c("ppt","tmean")
iter<-as.numeric(commandArgs()[4])
yrseq<-seq(1981,2021)

dir<-"your directory"
pts<-readOGR(dsn=paste(dir,"US",sep=""),layer="plant_occurence_usa")
for(iy in 1:length(yrseq)){

	fd<-paste(dir,"PRISM/1981_2022/",fdstr[iter],"unzip/PRISM_",flstr[iter],"_stable_4kmM3_",yrseq[iy],"_all_bil",sep="")
    ras<-raster(paste(fd,"/PRISM_",flstr[iter],"_stable_4kmM3_",yrseq[iy],"_bil.bil",sep=""))
    pts_tran<-spTransform(pts,proj4string(ras))
    a<-extract(ras,pts_tran)#extracted raster values
    a<-data.frame(a)
    outname<-paste(flstr[iter],"_",yrseq[iy],sep="")
    colnames(a)<-outname
    write.table(a,paste(dir,"PRISM/1981_2022/",fdstr[iter],"tables/",outname,".txt",sep=""),col.names=T,row.names=F,append=F)
}

##test read:
#d<-read.table(paste(dir,"PRISM/1981_2022/",fdstr[iter],"tables/",outname,".txt",sep=""),header=T,sep=" ")

#precipitation:
#folder: 
#PRISM_ppt_stable_4kmM3_1981_all_bil
#file:
#PRISM_ppt_stable_4kmM3_1981_bil.bil

#mean_temp
#folder:
#PRISM_tmean_stable_4kmM3_1981_all_bil
#file:
#PRISM_tmean_stable_4kmM3_1981_bil.bil
