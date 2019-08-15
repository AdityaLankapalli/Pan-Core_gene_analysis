args = commandArgs(trailingOnly = TRUE)
n=as.character(args[1])
m=as.character(args[2])
setwd(getwd())
x=read.table(n)
y=read.delim(m,header=F)
combi=function(s){
if(length(s)==1){
a=s[1]}else{
a=s[1]
for(j in 2:length(s)){
a=paste(a,s[j],sep=",")
a=a
}}
return(a)}
makeorthotable=function(x,y){
k=NULL
k=strsplit(as.character(y)," ")[[1]]
p=NULL
for(i in 1:dim(x)[1]){
s=k[grep(x[i,1],k)]
if(length(s)>0){
p=cbind(p,combi(s))
}else{p=cbind(p,"-")}
}
r=NULL
r=cbind(r,k[1],p)
return(r)}
q=NULL
for(l in 1:dim(y)[1]){
q=rbind(q,makeorthotable(x,y[l,]))
}
colnames(q)=c("cluster_ID",as.character(x[,1]))
qname=paste(m,"table",sep="_")
write.table(q,qname,quote=F,row.names=F,col.names=T,sep="\t")
#########################################################
rm(x)
rm(y)

a1=NULL
a2=NULL
a3=NULL
a4=NULL
a5=NULL
for(i in 1:dim(q)[1]){
p=NULL
s=NULL
for(j in 2:dim(q)[2]){
if(q[i,j]=="-"){
p=c(p,0)
s=cbind(s,0)}else{
p=c(p,1)
s=cbind(s,1)
}
}
a1=rbind(a1,cbind(q[i,1],s))
if(sum(p)==((dim(q)[2])-1)){
a2=rbind(a2,cbind(q[i,1],"CORE"))
a3=rbind(a3,q[i,])}else{
a4=rbind(a4,cbind(q[i,1],"ACCESSORY"))
a5=rbind(a5,q[i,])
}
}
a1name=paste(m,"matrix",sep="_")
colnames(a1)=colnames(q)
a2name=paste(m,"CORE_LIST",sep="_")
a3name=paste(m,"CORE",sep="_")
a4name=paste(m,"ACCESSORY_LIST",sep="_")
a5name=paste(m,"ACCESSORY",sep="_")

write.table(a1,a1name,quote=F,row.names=F,col.names=T,sep="\t")
write.table(a2,a2name,quote=F,row.names=F,col.names=F,sep="\t")
write.table(a3,a3name,quote=F,row.names=F,col.names=T,sep="\t")
write.table(a4,a4name,quote=F,row.names=F,col.names=F,sep="\t")
write.table(a5,a5name,quote=F,row.names=F,col.names=T,sep="\t")








