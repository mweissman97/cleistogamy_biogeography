dir<-"PRISM files directory"
iter<-as.numeric(commandArgs()[4])
str<-c("precipitation/","mean_temp/")
dir1<-str[iter]
#dir1<-"mean_temp/"

zipfiles<-list.files(paste(dir,dir1,"zip/",sep=""),full.names=T,pattern=".zip")
names0<-list.files(paste(dir,dir1,"zip/",sep=""),full.names=F,pattern=".zip")
names1<-sapply(names0,function(x){unlist(strsplit(x,".zip"))})
names(names1)<-NULL
for(i in 1:length(zipfiles)){
    
    dir.create(paste(dir,dir1,"unzip/",names1[i],"/",sep=""))
	unzip(zipfiles[i],exdir=paste(dir,dir1,"unzip/",names1[i],"/",sep=""))
}