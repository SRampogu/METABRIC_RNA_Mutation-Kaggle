library(dplyr)
library(shiny)

a<-read.csv("METABRIC_RNA_Mutation.csv", header = T , sep = ",",stringsAsFactors = T)
View(a)
#missiong values
is.null(a)
sum(is.na(a)) #overall missing values
colSums(is.na(a)) # columnwise missing values

#clean data
b<-na.omit(a)
sum(is.na(b)) # columnwise missing values
sum(is.na(b$type_of_breast_surgery))

#Adding na to empty cells and then delete them

b1 <- b      # Duplicate data frame
b1[b1 == ""] <- NA  # Replace blank with NA
b1

#deleting the 'NA'
c<-na.omit(b1)


#Relationship
library(ggplot2)
library(tidyverse)

ggplot(c,aes(tumor_size,stat_bins = 30))+geom_histogram(color="orange",fill="red")


ggplot(c,aes(tumor_size,tumor_stage))+geom_violin(color="blue")

ggplot(c,aes(tumor_size,tumor_stage))+geom_raster()


ggplot(c,aes(tumor_size,tumor_stage))+geom_tile(color="pink")

ggplot(c,aes(tumor_size,hormone_therapy, fill=chemotherapy))+geom_density()


ggplot(c,aes(tumor_size,tumor_stage))+geom_point(color="pink")
#subset
subC<-select(c,1,2,3,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50)
subCC<-select(c,1:3,32:50)
class(c)


is.null(subC)
sum(is.na(subC)) #overall missing values
colSums(is.na(subC)) # columnwise missing values

table(subC$type_of_breast_surgery)

table(c$her2_status)

plot(c$her2_status)
class(c$her2_status)
d<-as.factor(c$her2_status)#changing char to factor and then plot
class(d)
plot(d,col='blue')

#___________________________________________
table(subC$type_of_breast_surgery)
class(c$type_of_breast_surgery)
plot(e,col='coral',xlab='type_of_breast_surgery',ylab='Frequency',
     main='Distribution',ylim=c(0,800),border="green")
#__________________________________________
table(c$cellularity)
class(c$cellularity)
plot(c$cellularity,col="magenta",xlab='Cellularity',ylab='Frequency',
     main='Distribution of Cellularity',ylim=c(0,600))
#______________________________________________________
brca<-select(subCC,4,5)

View(brca)
plot(brca,col='blue')
ggplot(brca,aes(x=brca1,y=brca2))+geom_point(color = 'violet')
barplot(subCC)
#_________________________________________________

ggplot(c,aes(x= cancer_type_detailed,y=cellularity))+
  geom_point(color='red')
#____________________________________________________

ggplot(c,aes(x= nf1))+geom_histogram(bins = 50, fill = "steelblue4", col="white")+
  xlab("nf1")

  # Fill with catagorical col

ggplot(c,aes(x= nf1,fill = er_status ))+geom_histogram(bins = 50,col='black')+
  xlab("nf1")
#_________________________________________________________
ggplot(c,aes(x= brca1,fill = er_status ))+geom_histogram(bins = 50,col='black')+
  xlab("BRCA1")

ggplot(c,aes(x= brca2,fill = er_status ))+geom_histogram(bins = 50,col='black')+
  xlab("BRCA2")+ggtitle("BRCA2 vs ER STATUS")
#_______________________________________________________________________________

ggplot(c,aes(x= brca2,fill = her2_status ))+geom_histogram(bins = 50,col='black')+
  xlab("BRCA2")+ggtitle("BRCA2 vs HER2 STATUS")

ggplot(c,aes(x= brca2,fill = cellularity ))+geom_histogram(bins = 50,col='black')+
  xlab("BRCA2")+ggtitle("BRCA2 vs cellularity")


ggplot(c,aes(x= brca2,fill = er_status_measured_by_ihc))+geom_histogram(bins = 50,col='black')+
  xlab("BRCA2")+ggtitle("BRCA2 vs er_status_measured_by_ihc")

ggplot(c,aes(x= brca2,fill = her2_status_measured_by_snp6))+geom_histogram(bins = 50,col='black')+
  xlab("BRCA2")+ggtitle("BRCA2 vs her2_status_measured_by_snp6")

#Title to center and bold
ggplot(c,aes(x= brca2,fill = type_of_breast_surgery))+geom_histogram(bins = 50,col='black',boundary = 0)+
  xlab("BRCA2")+ggtitle("BRCA2 vs type_of_breast_surgery")+theme(plot.title = element_text(hjust = 0.5,face = 'bold', size = 30))
#bar chart only X-axis.._____________________________________________________________________
ggplot(c,aes(type_of_breast_surgery))+geom_bar(fill = "pink",col = "red")+
  labs(title = 'BC Surgery type',x='type',y = 'Frequency')+theme(plot.title=element_text(hjust = 0.5))
#------------------------------------------------------------

ggplot(c,aes(type_of_breast_surgery,fill = cellularity))+geom_bar(col = "red")+
  labs(title = 'cellularity',x='type',y = 'Frequency')+theme(plot.title=element_text(hjust = 0.5))
#------------------------------------------------------------
ggplot(c,aes(type_of_breast_surgery,fill = er_status))+geom_bar(col = "red")+
  labs(title = 'type_of_breast_surgery',x='type',y = 'Frequency')+theme(plot.title=element_text(hjust = 0.5))
#--------------dodge--------------------------
ggplot(c,aes(type_of_breast_surgery,fill = cellularity))+geom_bar(col = "white",position = 'dodge')+
  labs(title = 'cellularity',x='type',y = 'Frequency')+theme(plot.title=element_text(hjust = 0.5))

ggplot(c,aes(cancer_type_detailed,fill = cellularity))+geom_bar(col = "black",position = 'dodge')+
  labs(title = 'cancer_type_detailed vs cellularity',x='type',y = 'Frequency')+theme(plot.title=element_text(hjust = 0.5))
#----------scatter polt 2 nuem------------

h<-ggplot(c,aes(x= pten, y=atm))
h+geom_point(alpha = 0.2,col="magenta")

hp<-ggplot(c,aes(x= pam50_._claudin.low_subtype, y=atm))
hp+geom_point()

ggplot(c,aes(x= pam50_._claudin.low_subtype, y=atm,col=er_status))+
  geom_point()

ggplot(c,aes(x= mutation_count, y=atm,col=er_status))+
  geom_point()

ggplot(c,aes(x= mutation_count))+
  geom_bar(fill="purple")

ggplot(c,aes(x= mutation_count,fill = er_status))+
  geom_bar()
#---colour code manual------------------------------
ggplot(c,aes(x= pten,y= chek2,col = er_status))+
  geom_point()+
  scale_color_manual(values = c("magenta",'green'))+
  ggtitle('pten vs chek2 vs er_status')+
  theme(plot.title=element_text(hjust = 0.5,face = 'bold', size = 30))

#-------ER and PR status Vs type_of_breast_surgery--------
ggplot(c,aes(x= type_of_breast_surgery,fill =er_status ))+
  geom_bar(position = 'dodge')+
  ggtitle('type_of_breast_surgery Vs er_status ')+
  theme(plot.title=element_text(hjust = 0.5,face = 'bold', size = 30))


ggplot(c,aes(x= type_of_breast_surgery,fill =pr_status ))+
  geom_bar(position = 'dodge')+
  ggtitle('type_of_breast_surgery Vs pr_status ')+
  theme(plot.title=element_text(hjust = 0.5,face = 'bold', size = 30))
  
#---------------Er/PR/type_of_breast_surgery Vs death---

ggplot(c,aes(x= type_of_breast_surgery,fill = death_from_cancer ))+
  geom_bar(position = 'dodge',col = 'black')+
  ggtitle('type_of_breast_surgery Vs death from cancer ')+
  theme(plot.title=element_text(hjust = 0.5,face = 'bold', size = 30))


ggplot(c,aes(x=er_status  ,fill = death_from_cancer  ))+
  geom_bar(position = 'dodge', col = 'black')+
  scale_fill_manual(values = c('lightskyblue3', 'navajowhite1', 'mistyrose1'))+
  ggtitle('ER status Vs Death from cancer ')+
  theme(plot.title=element_text(hjust = 0.5,face = 'bold', size = 30))

ggplot(c,aes(x= pr_status ,fill = death_from_cancer))+
  geom_bar(position = 'dodge',col = 'black')+
  scale_fill_manual(values = c ('deeppink','salmon', 'springgreen'))+
  ggtitle('PR status Vs Death from cancer')+
  labs(x= "Death from Cancer")+
  theme(plot.title=element_text(hjust = 0.5,face = 'bold', size = 30))

#-------------------cancer type vs death----

ggplot(c,aes(x= cancer_type_detailed ,fill = death_from_cancer))+
  geom_bar(position = 'dodge',col = 'black')+
  scale_fill_manual(values = c('yellow','orange','purple'))+
  ggtitle('cancer type Vs death from cancer')+
  labs(x= "Cancer Type")+
  theme(plot.title=element_text(hjust = 0.5,face = 'bold', size = 30))
#---------------manopause/ primary tumor-------------------------------

ggplot(c,aes(x= inferred_menopausal_state ,fill = primary_tumor_laterality))+
  geom_bar(position = 'dodge',col = 'black')+
  scale_fill_manual(values = c("green","yellow"))+
  ggtitle('menopausal State Vs primary Tumor Laterality')+
  labs(x= "inferred_menopausal_state primary_tumor_laterality")+
  theme(plot.title=element_text(hjust = 0.5,face = 'bold', size = 30))
