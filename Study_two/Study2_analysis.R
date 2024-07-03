####Loading Packages####

library(readxl)
library(psych)
library(ggplot2)
library(lavaan)
library(reshape2)
library(plyr)

####Examining Overlap Samples Test####

#ctgan 128/3000- top model - Diagnostic = 100%; Eval. Score = 93.22%

ctgan.Overlap.best <- read.csv("./Output/New Analyses/ctgan/bs-128/epoch-3000/ctgan_row_overlap.csv")
ctgan.Overlap.best$Model<-"ctgan - Best"

#ctgan 500/500 - worst model - Diagnostic = 100%; Eval. Score = 90.87%

ctgan.Overlap.worst <- read.csv("./Output/New Analyses/ctgan/bs-500/epoch-500/ctgan_row_overlap.csv")
ctgan.Overlap.worst$Model<-"ctgan - Worst"

#copula - base

copula.Overlap <- read.csv("./Output/copula/copula_row_overlap.csv")
copula.Overlap$Model<-"copula - base"

#Binding and Visualizing - Overlap

Overlap.df.summary<-rbind(ctgan.Overlap.best,
                          ctgan.Overlap.worst#,
                          #copula.Overlap
                          )

####Examining Differences in Distributions####

#ctgan 128/3000- top model - Diagnostic = 100%; Eval. Score = 93.22%

ctgan.Cohen.best <- read.csv("./Output/New Analyses/ctgan/bs-128/epoch-3000/ctgan_synth_stats.csv")
ctgan.Cohen.best$Model<-"ctgan - Best"

#ctgan 500/500 - worst model - Diagnostic = 100%; Eval. Score = 90.87%

ctgan.Cohen.worst <- read.csv("./Output/New Analyses/ctgan/bs-500/epoch-500/ctgan_synth_stats.csv")
ctgan.Cohen.worst$Model<-"ctgan - Worst"

#copula - base

copula.Cohen <- read.csv("./Output/copula/copula_synth_stats.csv")
copula.Cohen$Model<-"copula - base"


#Skewness and Kurtosis Differences

ctgan.Cohen.best$Data[is.na(ctgan.Cohen.best$Cohen.s.d)]<-"Original"
ctgan.Cohen.best$Data[!(is.na(ctgan.Cohen.best$Cohen.s.d))]<-"Synthetic"

describe(ctgan.Cohen.best$kurtosis.abs)

describe(ctgan.Cohen.best$skewness.abs)

corr.test(ctgan.Cohen.best[,c("skewness",
                              "kurtosis",
                              "kurtosis.abs",
                              "skewness.abs")])

#Binding and Visualizing - Cohen's D

Cohen.df.summary<-rbind(ctgan.Cohen.best,
                        ctgan.Cohen.worst,
                        copula.Cohen)

describeBy(abs(Cohen.df.summary$Cohen.s.d),
           Cohen.df.summary$Model)

ggplot(data=Cohen.df.summary,
       aes(x=Cohen.s.d,
           fill=Model))+
  geom_histogram(alpha=.50)+
  xlab("Mean Difference (Cohen's d) for Synthetic and Observed Data")+
  theme_classic()+
  theme(legend.position = "bottom")+
  ylab("N")

####Examining Differences in Correlations####

#ctgan 128/3000- top model - Diagnostic = 100%; Eval. Score = 93.22%

ctgan.bestcorr.diff <- read.csv("./Output/ctgan/bs-500/epoch-500/ctgan_corr_diff.csv")
ctgan.bestcorr.diff<-ctgan.bestcorr.diff[,c(2:66)]
rownames(ctgan.bestcorr.diff)<-colnames(ctgan.bestcorr.diff)
ctgan.bestcorr.diff<-as.data.frame(ctgan.bestcorr.diff)

ctgan.bestcorr.diff[upper.tri(ctgan.bestcorr.diff)] <- NA
ctgan.bestcorr.diff<-data.matrix(ctgan.bestcorr.diff)
diag(ctgan.bestcorr.diff)<-NA

ctgan.bestcorr.diff.melt<-melt(ctgan.bestcorr.diff)
ctgan.bestcorr.diff.melt<-ctgan.bestcorr.diff.melt[!(is.na(ctgan.bestcorr.diff.melt$absolute)),]
ctgan.bestcorr.diff.melt$absolute<-abs(ctgan.bestcorr.diff.melt$value)

describe(ctgan.bestcorr.diff.melt$absolute)

ggsave("AbsMeanDiffr.png",width=8,height=8)
ggplot(data=ctgan.bestcorr.diff.melt,
       aes(x=absolute))+
  geom_histogram(alpha=.50)+
  xlab("Absolute Difference in Correlation (r) for Synthetic and Observed")+
  theme_classic()+
  theme(text = element_text(size = 15),
        legend.position = "none")+
  ylab("N")
dev.off()

####Importing and Managing Original and Synthetic Data####

#Original

original.df <- read.csv("./Output/New Analyses/real_data.csv")
colnames(original.df)

#Copula

copula.df <- read.csv("./Output/copula/copula_synth_data.csv")
colnames(copula.df)
copula.df$X<-NULL

#CTGAN - Best

ctgan.best.df <- read.csv("./Output/New Analyses/ctgan/bs-128/epoch-3000/ctgan_synth_data.csv")
ctgan.best.df$X<-NULL

#CTGAN - Worst

ctgan.worst.df <- read.csv("./Output/New Analyses/ctgan/bs-500/epoch-500/ctgan_synth_data.csv")
ctgan.worst.df$X<-NULL

#CTGAN - Alternatives

ctgan.0.df <- read.csv("./Output/New Analyses/ctgan/bs-128/epoch-500/ctgan_synth_data.csv")
ctgan.1.df <- read.csv("./Output/New Analyses/ctgan/bs-128/epoch-1000/ctgan_synth_data.csv")
ctgan.2.df <- read.csv("./Output/New Analyses/ctgan/bs-128/epoch-2000/ctgan_synth_data.csv")
ctgan.4.df <- read.csv("./Output/New Analyses/ctgan/bs-256/epoch-500/ctgan_synth_data.csv")
ctgan.5.df <- read.csv("./Output/New Analyses/ctgan/bs-256/epoch-1000/ctgan_synth_data.csv")
ctgan.6.df <- read.csv("./Output/New Analyses/ctgan/bs-256/epoch-2000/ctgan_synth_data.csv")
ctgan.7.df <- read.csv("./Output/New Analyses/ctgan/bs-256/epoch-3000/ctgan_synth_data.csv")
ctgan.9.df <- read.csv("./Output/New Analyses/ctgan/bs-500/epoch-1000/ctgan_synth_data.csv")
ctgan.10.df <- read.csv("./Output/New Analyses/ctgan/bs-500/epoch-2000/ctgan_synth_data.csv")
ctgan.11.df <- read.csv("./Output/New Analyses/ctgan/bs-500/epoch-3000/ctgan_synth_data.csv")

ctgan.0.df$X<-NULL
ctgan.1.df$X<-NULL
ctgan.2.df$X<-NULL
ctgan.4.df$X<-NULL
ctgan.5.df$X<-NULL
ctgan.6.df$X<-NULL
ctgan.7.df$X<-NULL
ctgan.9.df$X<-NULL
ctgan.10.df$X<-NULL
ctgan.11.df$X<-NULL


####Creating List of Data Frames####

df.l<-list(original.df,
           copula.df,
           ctgan.best.df,
           ctgan.worst.df,
           ctgan.0.df,
           ctgan.1.df,
           ctgan.2.df,
           ctgan.4.df,
           ctgan.5.df,
           ctgan.6.df,
           ctgan.7.df,
           ctgan.9.df,
           ctgan.10.df,
           ctgan.11.df)

####Differences in Correlation Matrices####

corr.r<-corr.test(original.df[,c(5:68)])
corr.r<-corr.r$r
corr.r[upper.tri(corr.r)] <- NA
diag(corr.r)<-NA

corr.r.orig<-corr.r

corr.compare.l<-lapply(df.l,function(x){
  corr.compare<-corr.test(x[,c(5:68)])
  corr.compare<-corr.compare$r
  corr.compare[upper.tri(corr.compare)] <- NA
  diag(corr.compare)<-NA
  
  corr.compare<-corr.compare-corr.r.orig
  rownames(corr.compare)<-colnames(corr.compare)
  corr.compare.melt <- melt(corr.compare, na.rm = TRUE)
  return(corr.compare.melt)
  
})

names(corr.compare.l)<-c("orig",
                           "copula",
                           "ctgan - best",
                           "ctgan - worst",
                           
                           "ctgan - 0",
                           "ctgan - 1",
                           "ctgan - 2",
                           "ctgan - 4",
                           "ctgan - 5",
                           "ctgan - 6",
                           "ctgan - 7",
                           "ctgan - 9",
                           "ctgan - 10",
                           "ctgan - 11")


corr.compare.df<-do.call(rbind.data.frame,corr.compare.l)

Temp.df<-as.data.frame(rownames(corr.compare.df))
colnames(Temp.df)<-"Temp"
Temp.df2<-do.call(rbind.data.frame,
                  strsplit(Temp.df$Temp,"[.]"))
colnames(Temp.df2)<-c("Model","ParamNum")
corr.compare.df$Model<-Temp.df2$Model
corr.compare.df$Rel<-paste(corr.compare.df$Var1,corr.compare.df$Var2,sep=".")

describeBy(corr.compare.df$value,
           corr.compare.df$Model)

corr.diff.varcomp<-aov(value~Var1*Model,data=corr.compare.df[corr.compare.df$Model!="orig"&
                                                             !(grepl("ESI",corr.compare.df$Var1))&
                                                             !(grepl("ESI",corr.compare.df$Var2)),])
summary(corr.diff.varcomp)

ggsave("CorrDiffHistogram.png",width=12,height=8)
ggplot(data=corr.compare.df[corr.compare.df$Model!="orig",],
       aes(x=value,
           fill=Model))+
  geom_histogram(alpha=.50)+
  facet_wrap(~Model)+
  geom_vline(xintercept=0,linetype="dashed",color="red")+
  xlab("Mean Difference in Correlation (r) for Synthetic and Observed")+
  theme_classic()+
  theme(legend.position = "bottom")+
  ylab("N")
dev.off()

####Estimating EFA Using Self-Ratings for Each Data Set Using Competency Composites####

#Scree Plot

scree.l<-lapply(df.l,function(x){
  scree.p<-psych::scree(r=x[,c(5:15)])
  
  return(scree.p)
})

#EFA - Factors with Orthogonal Variables

efa.l<-lapply(df.l,function(x){
  fa.2<-fa(r=x[,c(5:15)],
           nfactors=2,
           rotate="varimax")
  
  fa.2.loadings<-fa.2$loadings
  fa.2.loadings.df<-as.data.frame(fa.2.loadings[c(1:11),c(1:2)])
  return(fa.2.loadings.df)
  })

efa.orig<-efa.l[1]

efa.orig.df<-as.data.frame(efa.orig)
efa.orig.df$sum<-rowSums(efa.orig.df[,c("MR1","MR2")])
efa.orig.df$MR1.Prop<-(efa.orig.df$MR1/efa.orig.df$sum)*100
efa.orig.df$MR2.Prop<-(efa.orig.df$MR2/efa.orig.df$sum)*100

efa.diff.l<-lapply(efa.l,function(x){
  efa.diff<-efa.orig-x
  efa.diff<-c(efa.diff$MR1.MR1,
              efa.diff$MR2.MR2)
  return(efa.diff)
})

names(efa.diff.l)<-c("orig",
                         "copula",
                         "ctgan - best",
                         "ctgan - worst",
                         
                         "ctgan - 0",
                         "ctgan - 1",
                         "ctgan - 2",
                         "ctgan - 4",
                         "ctgan - 5",
                         "ctgan - 6",
                         "ctgan - 7",
                         "ctgan - 9",
                         "ctgan - 10",
                         "ctgan - 11")

efa.diff.df<-do.call(rbind.data.frame,efa.diff.l)
rownames(efa.diff.df)<-names(efa.diff.l)
colnames(efa.diff.df)<-c("S01.F1",
                         "S02.F1",
                         "S03.F1",
                         "S04.F1",
                         "S05.F1",
                         "S06.F1",
                         "S07.F1",
                         "S08.F1",
                         "S09.F1",
                         "S010.F1",
                         "S011.F1",
                         "S01.F2",
                         "S02.F2",
                         "S03.F2",
                         "S04.F2",
                         "S05.F2",
                         "S06.F2",
                         "S07.F2",
                         "S08.F2",
                         "S09.F2",
                         "S010.F2",
                         "S011.F2")

efa.diff.df$Model<-rownames(efa.diff.df)
efa.diff.df<-melt(efa.diff.df,id.vars = "Model")

ggsave("FADiffHistogram.png",width=12,height=8)
ggplot(data=efa.diff.df[efa.diff.df$Model!="orig",],
       aes(x=value,
           fill=Model))+
  geom_histogram(alpha=.50)+
  facet_wrap(~Model)+
  geom_vline(xintercept=0,linetype="dashed",color="red")+
  xlab("Mean Difference in EFA Loadings for Synthetic and Observed")+
  theme_classic()+
  theme(legend.position = "bottom")+
  ylab("N")
dev.off()

efa.diff.df$value.abs<-abs(efa.diff.df$value)

describe(efa.diff.df[grepl("est",efa.diff.df$Model),"value.abs"])

####Creating Composite Scores####

df.l.2<-lapply(df.l,function(x){
  x$Task.S<-rowMeans(x[,c("BMK_S06_Confront","BMK_S03_Decisive")],na.rm = TRUE)
  x$Rel.S<-rowMeans(x[,c("BMK_S09_Compassion","BMK_S10_Putting")],na.rm = TRUE)
  x$Task.B<-rowMeans(x[,c("BMK_S06_Confront_boss","BMK_S03_Decisive_boss")],na.rm = TRUE)
  x$Rel.B<-rowMeans(x[,c("BMK_S09_Compassion_boss","BMK_S10_Putting_boss")],na.rm = TRUE)
  
  x$Task.S.2<-I(x$Task.S^2)
  x$Rel.S.2<-I(x$Rel.S^2)
  x$Task.B.2<-I(x$Task.B^2)
  x$Rel.B.2<-I(x$Rel.B^2)
  
  x$Task.S.B<-x$Task.S*x$Task.B
  x$Rel.S.B<-x$Rel.S*x$Rel.B
  
  x$Derail.P<-rowMeans(x[,c("BMK_D01_Interper_peer",
                            "BMK_D02_DiffBuild_peer",
                            "BMK_D03_DiffChange_peer",
                            "BMK_D04_Failure_peer",
                            "BMK_D05_Narrow_peer")],na.rm = TRUE)
  return(x)
})

names(df.l.2)<-c("orig",
                 "copula",
                 "ctgan - best",
                 "ctgan - worst",
                 
                 "ctgan - 0",
                 "ctgan - 1",
                 "ctgan - 2",
                 "ctgan - 4",
                 "ctgan - 5",
                 "ctgan - 6",
                 "ctgan - 7",
                 "ctgan - 9",
                 "ctgan - 10",
                 "ctgan - 11")

####Estimating Path Models for Each Data Set####

#Specifying Path Model: Competency --> Derailment
#Braddy et al.: DV = P rating of Derailment; IV = Self/Supervisor of Task & Rel


model.0<-'

Derail.P~b1Task*Task.S+b2Task*Task.B+b3Task*Task.S.2+b4Task*Task.S.B+b5Task*Task.B.2
Derail.P~b1Rel*Rel.S+b2Rel*Rel.B+b3Rel*Rel.S.2+b4Rel*Rel.S.B+b5Rel*Rel.B.2

#Surface Parameters

a1Task:=b1Task+b2Task
a2Task:=b3Task+b4Task+b5Task

a3Task:=b1Task-b2Task
a4Task:=b3Task-b4Task+b5Task

a1Rel:=b1Rel+b2Rel
a2Rel:=b3Rel+b4Rel+b5Rel

a3Rel:=b1Rel-b2Rel
a4Rel:=b3Rel-b4Rel+b5Rel
'
#Estimate Model Seperately for Each Data Set

model.0.fit.l<- lapply(df.l.2,function(x){
  
  sem(model = model.0,
      data = x,
      estimator="ML",
      missing="FIML",
      em.h1.iter.max= 1000000000,
      fixed.x=FALSE)
})

summary(model.0.fit.l[[1]])

#Extracting Parameters

param.0.SEM.l<-lapply(model.0.fit.l,function(x){
  parameterEstimates(x,standardized=TRUE)
  
})

names(param.0.SEM.l)<-c("orig",
                        "copula",
                        "ctgan - best",
                        "ctgan - worst",
                        
                        "ctgan - 0",
                        "ctgan - 1",
                        "ctgan - 2",
                        "ctgan - 4",
                        "ctgan - 5",
                        "ctgan - 6",
                        "ctgan - 7",
                        "ctgan - 9",
                        "ctgan - 10",
                        "ctgan - 11")

param.0.SEM.df<-do.call(rbind.data.frame,param.0.SEM.l)

Temp.df<-as.data.frame(rownames(param.0.SEM.df))
colnames(Temp.df)<-"Temp"
Temp.df2<-do.call(rbind.data.frame,
                  strsplit(Temp.df$Temp,"[.]"))
colnames(Temp.df2)<-c("Model","ParamNum")
param.0.SEM.df$Model<-Temp.df2$Model
param.0.SEM.df$ParamNum<-Temp.df2$ParamNum

table(param.0.SEM.df$Model)

param.0.SEM.df$est<-round(param.0.SEM.df$est,2)
param.0.SEM.df$se<-round(param.0.SEM.df$se,2)
param.0.SEM.df$pvalue<-round(param.0.SEM.df$pvalue,2)

param.0.SEM.df.wide<-merge(param.0.SEM.df[,c("lhs","op","rhs","label","ParamNum",
                                                                         "Model",
                                                                         "est","se","pvalue",
                                                                         "ci.lower","ci.upper")],
                           param.0.SEM.df[param.0.SEM.df$Model=="orig",c("lhs","op","rhs","label","ParamNum",
                                                                           "Model",
                                                                           "est","se","pvalue",
                                                                           "ci.lower","ci.upper")],
                           by=c("lhs","op","rhs","label","ParamNum"),
                           suffixes = c(".compare",".orig"))

param.0.SEM.df.wide<-param.0.SEM.df.wide[param.0.SEM.df.wide$Model.compare!=param.0.SEM.df.wide$Model.orig,]

param.0.SEM.df.wide$Bias<-param.0.SEM.df.wide$est.orig-param.0.SEM.df.wide$est.compare


#Bias Descriptives

describeBy(param.0.SEM.df.wide$Bias,
           param.0.SEM.df.wide$Model.compare)

#Contained within original parameters CI?

param.0.SEM.df.wide$Est.In.CI[param.0.SEM.df.wide$est.compare<=param.0.SEM.df.wide$ci.upper.orig & param.0.SEM.df.wide$est.compare>=param.0.SEM.df.wide$ci.lower.orig]<-1
param.0.SEM.df.wide$Est.In.CI[param.0.SEM.df.wide$est.compare>param.0.SEM.df.wide$ci.upper.orig | param.0.SEM.df.wide$est.compare<param.0.SEM.df.wide$ci.lower.orig]<-0


param.0.Est.In.CI.prop.table<-as.data.frame(prop.table(table(param.0.SEM.df.wide$Est.In.CI,
                 param.0.SEM.df.wide$Model.compare),2))

#Agreement in significance?

param.0.SEM.df.wide$p.test[param.0.SEM.df.wide$pvalue.compare<=.05 & param.0.SEM.df.wide$pvalue.orig<=.05]<-1
param.0.SEM.df.wide$p.test[param.0.SEM.df.wide$pvalue.compare>.05 & param.0.SEM.df.wide$pvalue.orig>.05]<-1
param.0.SEM.df.wide$p.test[param.0.SEM.df.wide$pvalue.compare>.05 & param.0.SEM.df.wide$pvalue.orig<=.05]<-0
param.0.SEM.df.wide$p.test[param.0.SEM.df.wide$pvalue.compare<=.05 & param.0.SEM.df.wide$pvalue.orig>.05]<-0

param.0.p.test.prop.table<-as.data.frame(prop.table(table(param.0.SEM.df.wide$p.test,
                 param.0.SEM.df.wide$Model.compare),2))


#Exporting Parameters

write.csv(param.0.SEM.df.wide,"SEM Parameter Comparisons.csv")

####Response Surface Comparisons####
library(RSA)

#original
df.sem<-param.0.SEM.l[[1]]

plotRSA.Rel.orig<-RSA::plotRSA(x=df.sem[df.sem$label=="b1Rel","est"],
             y=df.sem[df.sem$label=="b2Rel","est"],
             x2=df.sem[df.sem$label=="b3Rel","est"],
             y2=df.sem[df.sem$label=="b4Rel","est"],
             xy=df.sem[df.sem$label=="b5Rel","est"],
             xlab="Self",
             ylab="Supervisor",
             zlab="Derailment (Peer)",
             main = "Original - Relationship Leadership",
             legend = FALSE)

plotRSA.Task.orig<-RSA::plotRSA(x=df.sem[df.sem$label=="b1Task","est"],
                                y=df.sem[df.sem$label=="b2Task","est"],
                                x2=df.sem[df.sem$label=="b3Task","est"],
                                y2=df.sem[df.sem$label=="b4Task","est"],
                                xy=df.sem[df.sem$label=="b5Task","est"],
                                xlab="Self",
                                ylab="Supervisor",
                                zlab="Derailment (Peer)",
                                main = "Original - Task Leadership",
                                legend = FALSE)

#ctgan-best
df.sem<-param.0.SEM.l[[3]]

plotRSA.Rel.ctgan<-RSA::plotRSA(x=df.sem[df.sem$label=="b1Rel","est"],
                                y=df.sem[df.sem$label=="b2Rel","est"],
                                x2=df.sem[df.sem$label=="b3Rel","est"],
                                y2=df.sem[df.sem$label=="b4Rel","est"],
                                xy=df.sem[df.sem$label=="b5Rel","est"],
                                xlab="Self",
                                ylab="Supervisor",
                                zlab="Derailment (Peer)",
                                main = "Synthetic - Relationship Leadership",
                                legend = FALSE)

plotRSA.Task.ctgan<-RSA::plotRSA(x=df.sem[df.sem$label=="b1Task","est"],
                                 y=df.sem[df.sem$label=="b2Task","est"],
                                 x2=df.sem[df.sem$label=="b3Task","est"],
                                 y2=df.sem[df.sem$label=="b4Task","est"],
                                 xy=df.sem[df.sem$label=="b5Task","est"],
                                 xlab="Self",
                                 ylab="Supervisor",
                                 zlab="Derailment (Peer)",
                                 main = "Synthetic - Task Leadership",
                                 legend = FALSE)
library(ggpubr)

plotRSA.grob<-ggarrange(plotRSA.Task.orig, 
            plotRSA.Task.ctgan, 
            plotRSA.Rel.orig, 
            plotRSA.Rel.ctgan, 
            ncol = 2, 
            nrow = 2)

plot_grob <- ggplotGrob(plotRSA.grob)

# Save the plot
ggsave("RSAplot.png", plot = plot_grob,width=12,height=8)

ggsave("RSAComparison.png",width=12,height=8)
plotRSA.grob
dev.off()
