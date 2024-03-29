#library(raster)
library(sp)
library(rgdal)
#library(rgeos)
fdstr<-c("precipitation/","mean_temp/")
flstr<-c("ppt","tmean")
#iter<-as.numeric(commandArgs()[4])
yrseq<-seq(1981,2021)
dir<-"your directory"
pts<-readOGR(dsn=paste(dir,"US",sep=""),layer="plant_occurence_usa")
for (j in 1:length(fdstr)){

    for(iy in 1:length(yrseq)){
        
        outname<-paste(flstr[j],"_",yrseq[iy],sep="")
        d<-read.table(paste(dir,"PRISM/1981_2022/",fdstr[j],"tables/",outname,".txt",sep=""),header=T,sep=" ")
        pts@data[outname]<-d[,1]
    }
}
writeOGR(pts,dsn=paste(dir,"PRISM/",sep=""),layer="plant_occurence_usa_w_climate",driver="ESRI Shapefile")

##test read:
library(rgdal)
pts<-readOGR(dsn=paste(dir,"PRISM",sep=""),layer="plant_occurence_usa_w_climate")
data<-pts@data #data frame you can use
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
