
library("readxl")
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("mixOmics", version = "3.8")
require("mixOmics")
library('scales')
library("gplots")
library("RColorBrewer")
library("RVAideMemoire")
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ropls", version = "3.8")
require("ropls")


#shape the dataset and add annotation
data1=read_excel("annotation2.xlsx",sheet="annotation",col_names = TRUE)
annotation=c()
people=data1$people
normal=data1$normal[!is.na(data1$normal)]
prediabetes=data1$'pre-diabetes'[!is.na(data1$'pre-diabetes')]
diabetes=data1$diabetes[!is.na(data1$diabetes)]
for (ii in 1:length(people))
{
  if (is.element(people[ii],normal))
  {
    annotation[ii]="normal"
  }
  else if (is.element(people[ii],prediabetes))
  {
    annotation[ii]="prediabetes"
  }
  else if (is.element(people[ii],diabetes))
  {
    annotation[ii]="diabetes"
  }
}

data_3min=read_excel("t2d_sex_combine_analysis_2222019.xlsx",sheet="summary_3min",col_names = TRUE)
data_2200=read_excel("t2d_sex_combine_analysis_2222019.xlsx",sheet="summary_2200",col_names = FALSE)
#View(data_3min)
#View(data_2200)
data_3min$mz=NULL
data_2200$..1=NULL
colnames(data_2200)=colnames(data_3min)
data_all=rbind(data_3min,data_2200)
#View(data_all)
data_3min$feature_id=NULL
hmdb_id_3min=data_3min$hmdb_id
data_3min$hmdb_id=NULL
data_3min$hmdb_name=NULL

data_2200$feature_id=NULL
hmdb_id_2200=data_2200$hmdb_id
data_2200$hmdb_id=NULL
data_2200$hmdb_name=NULL

data_all$feature_id=NULL
hmdb_id_all=data_all$hmdb_id
data_all$hmdb_id=NULL
data_all$hmdb_name=NULL

#seperate normal, prediabetes and diabetes
data_metab_name=read_excel("annotation2.xlsx",sheet="id",col_names = TRUE)
data_name=data_metab_name$name_new
colnames(data_all)=NULL
data_all=data.frame(t(data_all))
colnames(data_all)=data_name
data=cbind(annotation,data_all)
data_n_p=data[data$annotation == "normal" | data$annotation =="prediabetes", ]
data_n_d=data[data$annotation == "normal" | data$annotation =="diabetes", ]
data_p_d=data[data$annotation == "prediabetes" | data$annotation =="diabetes", ]

# do a test pls-DA for seperate 3 groups
colnames(data)=NULL
X=as.matrix(data[,2:dim(data)[2]])
X=matrix(as.numeric(X),dim(X)[1],dim(X)[2])

Y=as.factor(data[,1])
model.man=plsda(X,Y,ncomp = 3 )
plotIndiv(model.man,ind.names = TRUE, ellipse = TRUE, legend =TRUE,col.per.group =c("Red","Blue","Green"),pch = 19,title = "Score Plot") #seperation map

#--------------------------------------------------------------------------------------------------------------------------
# normal VS prediabetes
colnames(data_n_p)=NULL
X=as.matrix(data_n_p[,2:dim(data)[2]])
X=matrix(as.numeric(X),dim(X)[1],dim(X)[2])
colnames(X)=data_name
Y=as.factor(data_n_p[,1])
model.man=plsda(X,Y,ncomp = 2 )
plotIndiv(model.man,ind.names = TRUE, ellipse = TRUE, legend =TRUE,col.per.group =c("Blue","Orange"),pch = 19,title = "Score Plot") #seperation map

model.man2=opls(X,as.character(Y)) # to get R2X and Q2 using "ropls"

#VIP plots
score=PLSDA.VIP(model.man,graph = FALSE)
a=score$tab
a$"name"=rownames(a)
a1=a[order(a$VIP),1]
a2=a[order(a$VIP),2]
vip_score=a1[a1 > 1.8]
vip_score=data.frame(vip_score)
list_name=a2[(length(a2)-dim(vip_score)[1]+1):length(a2)]
rownames(vip_score)=list_name
colnames(vip_score)="VIP"

theme_set(theme_classic())
ggplot(vip_score,aes(x=factor(list_name, levels = list_name),y=vip_score$VIP)) + geom_point(col="black",size=3) +
  geom_segment(aes(y=min(vip_score), yend=max(vip_score), 
                   x=list_name, xend=list_name), linetype="dashed", size=0.1) + coord_flip() #use factor() to retain order

#heatmap
X1=colnames(X)
choice=c()
for (ii in 1:length(X1))
{
  choice[ii]=is.element(X1[ii],list_name)
}
X2=X[,choice]
c=colnames(X2)
colnames(X2)=NULL
X2=data.frame(X2)
colnames(X2)=c

temp1=Y
for (ii in 1:length(list_name))
{
  temp=c()
  for (jj in 1:length(c))
  {
    temp[jj]=is.element(c[jj],list_name[ii])
  }
  temp1=cbind(temp1,X2[,temp])
}
temp1=data.frame(temp1)
temp1$temp1=NULL
colnames(temp1)=list_name
X2=temp1
rm(temp1)
rm(c)
Y1=data.frame(Y)
colnames(Y1)="annotaion"

X3=X2[Y1$annotaion=="normal",]
X4=X2[Y1$annotaion=="prediabetes",]

X5=rbind(colMeans(X3),colMeans(X4))
rownames(X5)=c("normal","prediabetes")

display.brewer.all()
heatmap.2(t(X5),Rowv=NA,col=brewer.pal(9,"Reds"),scale="row",margins = c(8,8),symm=TRUE,revC = TRUE,
          trace="none",density.info= "none", cexCol=2,dendrogram = "none", key.title = "NA", sepwidth=c(0,0.001),
          sepcolor=c("black"), rowsep=1:ncol(X5))



#---------------------------------------------------------------------------------------------------------------------------
#normal vs diabetes
colnames(data_n_d)=NULL
X=as.matrix(data_n_d[,2:dim(data)[2]])
X=matrix(as.numeric(X),dim(X)[1],dim(X)[2])
colnames(X)=data_name
Y=as.factor(data_n_d[,1])
model.man=plsda(X,Y,ncomp = 2 )
plotIndiv(model.man,ind.names = TRUE, ellipse = TRUE, legend =TRUE,col.per.group =c("Red","Blue"),pch = 19,title = "Score Plot") #seperation map

model.man2=opls(X,as.character(Y)) # to get R2X and Q2 using "ropls"

#VIP plots
score=PLSDA.VIP(model.man,graph = FALSE)
a=score$tab
a$"name"=rownames(a)
a1=a[order(a$VIP),1]
a2=a[order(a$VIP),2]
vip_score=a1[a1 > 1.8]
vip_score=data.frame(vip_score)
list_name=a2[(length(a2)-dim(vip_score)[1]+1):length(a2)]
rownames(vip_score)=list_name
colnames(vip_score)="VIP"

theme_set(theme_classic())
ggplot(vip_score,aes(x=factor(list_name, levels = list_name),y=vip_score$VIP)) + geom_point(col="black",size=3) +
  geom_segment(aes(y=min(vip_score), yend=max(vip_score), 
                   x=list_name, xend=list_name), linetype="dashed", size=0.1) + coord_flip() #use factor() to retain order

#heatmap
X1=colnames(X)
choice=c()
for (ii in 1:length(X1))
{
  choice[ii]=is.element(X1[ii],list_name)
}
X2=X[,choice]
c=colnames(X2)
colnames(X2)=NULL
X2=data.frame(X2)
colnames(X2)=c

temp1=Y
for (ii in 1:length(list_name))
{
  temp=c()
  for (jj in 1:length(c))
  {
    temp[jj]=is.element(c[jj],list_name[ii])
  }
  temp1=cbind(temp1,X2[,temp])
}
temp1=data.frame(temp1)
temp1$temp1=NULL
colnames(temp1)=list_name
X2=temp1
rm(temp1)
rm(c)
Y1=data.frame(Y)
colnames(Y1)="annotaion"

X3=X2[Y1$annotaion=="normal",]
X4=X2[Y1$annotaion=="diabetes",]

X5=rbind(colMeans(X3),colMeans(X4))
rownames(X5)=c("normal","diabetes")

display.brewer.all()
heatmap.2(t(X5),Rowv=NA,col=brewer.pal(9,"Reds"),scale="row",margins = c(8,8),symm=TRUE,revC = TRUE,
          trace="none",density.info= "none", cexCol=2,dendrogram = "none", key.title = "NA", sepwidth=c(0,0.001),
          sepcolor=c("black"), rowsep=1:ncol(X5))



#---------------------------------------------------------------------------------------------------------------------------
#prediabetes vs diabetes
colnames(data_p_d)=NULL
X=as.matrix(data_p_d[,2:dim(data)[2]])
X=matrix(as.numeric(X),dim(X)[1],dim(X)[2])
colnames(X)=data_name
Y=as.factor(data_p_d[,1])
model.man=plsda(X,Y,ncomp = 2 )
plotIndiv(model.man,ind.names = TRUE, ellipse = TRUE, legend =TRUE, col.per.group =c("Red","Orange"),pch = 19,title = "Score Plot") #seperation map

model.man2=opls(X,as.character(Y),predI = 2) # to get R2X and Q2 using "ropls"

#VIP plots
score=PLSDA.VIP(model.man,graph = FALSE)
a=score$tab
a$"name"=rownames(a)
a1=a[order(a$VIP),1]
a2=a[order(a$VIP),2]
vip_score=a1[a1 > 1.8]
vip_score=data.frame(vip_score)
list_name=a2[(length(a2)-dim(vip_score)[1]+1):length(a2)]
rownames(vip_score)=list_name
colnames(vip_score)="VIP"

theme_set(theme_classic())
ggplot(vip_score,aes(x=factor(list_name, levels = list_name),y=vip_score$VIP)) + geom_point(col="black",size=3) +
  geom_segment(aes(y=min(vip_score), yend=max(vip_score), 
                   x=list_name, xend=list_name), linetype="dashed", size=0.1) + coord_flip() #use factor() to retain order

#heatmap
X1=colnames(X)
choice=c()
for (ii in 1:length(X1))
{
  choice[ii]=is.element(X1[ii],list_name)
}
X2=X[,choice]
c=colnames(X2)
colnames(X2)=NULL
X2=data.frame(X2)
colnames(X2)=c

temp1=Y
for (ii in 1:length(list_name))
{
  temp=c()
  for (jj in 1:length(c))
  {
    temp[jj]=is.element(c[jj],list_name[ii])
  }
  temp1=cbind(temp1,X2[,temp])
}
temp1=data.frame(temp1)
temp1$temp1=NULL
colnames(temp1)=list_name
X2=temp1
rm(temp1)
rm(c)
Y1=data.frame(Y)
colnames(Y1)="annotaion"

X3=X2[Y1$annotaion=="prediabetes",]
X4=X2[Y1$annotaion=="diabetes",]

X5=rbind(colMeans(X3),colMeans(X4))
rownames(X5)=c("prediabetes","diabetes")

display.brewer.all()
heatmap.2(t(X5),Rowv=NA,col=brewer.pal(9,"Reds"),scale="row",margins = c(8,8),symm=TRUE,revC = TRUE,
          trace="none",density.info= "none", cexCol=2,dendrogram = "none", key.title = "NA", sepwidth=c(0,0.001),
          sepcolor=c("black"), rowsep=1:ncol(X5))
























































































































