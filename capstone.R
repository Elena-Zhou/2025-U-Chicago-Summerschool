library(tidyverse)
library(dplyr)
library(tidycensus)
library(sf)
library(jsonlite)
library(ggplot2)
library(margins)
library(mfx) 
library(stargazer)
library(psych)
library(gtsummary)
library(sandwich)
library(lmtest)
library(modelsummary)

file_path<-'C:\\Users\\86181\\Documents\\R芝加哥'
setwd(file_path)
data<-read_csv('IPUMS-CPS (reduced).csv')

##clean data
data<-data%>%mutate(COVID=ifelse(YEAR>=2020,1,0),EMPSTAT=case_when(
EMPSTAT>=10 & EMPSTAT<20 ~ 'Employed',
EMPSTAT>=20 & EMPSTAT<30 ~ 'Unemployed',
EMPSTAT>=30 ~ 'Not in Labor Force' ,
TRUE~NA_character_))#%>%mutate(EMPSTAT=factor(EMPSTAT))

data<-data%>%mutate(FAMSIZE=case_when(
  FAMSIZE==1~'1_person',
  FAMSIZE==2~'2_persons',
  FAMSIZE==3~'3_persons',
  FAMSIZE==4~'4_persons',
  FAMSIZE>=5~'5_more_persons',
  TRUE~NA_character_))%>%mutate(FAMSIZE=factor(FAMSIZE,levels=c('1_person','2_persons','3_persons','4_persons','5_more_persons')))

data<-data %>% mutate(NCHILD=ifelse(NCHILD==0,0,1),SEX=factor(SEX),EDUC=factor(EDUC),REGION=factor(REGION),COVID=factor(COVID)) %>% mutate(NCHILD=factor(NCHILD),RACE=factor(RACE),YEAR=factor(YEAR))%>%dplyr::select(EMPSTAT,FAMSIZE,COVID,REGION,SEX,AGE,EDUC,NCHILD,RACE,YEAR)%>%na.omit()



##diagram
data%>%ggplot(aes(x=YEAR,fill=EMPSTAT))+geom_bar(stat='count',position='dodge',width=0.7)+labs(title='Distribution of Employment Status from 2018 to 2021',x='YEAR',y='Count')+theme_minimal()



##data for 2 models
data1<-data%>%mutate(EMPSTAT=case_when(EMPSTAT=='Employed'~0,EMPSTAT=='Unemployed'~1,TRUE~NA_real_))%>%na.omit()


data2<-data%>%mutate(EMPSTAT=case_when(EMPSTAT=='Employed'~0,EMPSTAT=='Not in Labor Force'~1,TRUE~NA_real_))%>%na.omit()

##descriptive statistics (not present in laTeX)
de1<-tbl_summary(data1,include=c(EMPSTAT,FAMSIZE,COVID,NCHILD),type=list(all_continuous()~'continuous',type=all_categorical()~'categorical')) 

de2<-tbl_summary(data2,include=c(EMPSTAT,FAMSIZE,COVID,NCHILD),type=list(all_continuous()~'continuous',type=all_categorical()~'categorical'))

de_combine<-tbl_merge(tbls=list(de1,de2),tab_spanner = c('Model 1','Model 2'))
de_combine %>% as_gt() %>% gt::gtsave('Descriptive Statistics.png')


## two probit models
formula<-EMPSTAT~FAMSIZE+COVID+FAMSIZE*COVID+SEX+REGION+EDUC+AGE+RACE+NCHILD


## model 1 employed vs unemployed
model1<-glm(formula,data=data1,family=binomial(link='probit'))

#robust st.err.
robust_se1<-vcovHC(model1,type='HC1')

#combine
robust_model1<-coeftest(model1,vcov=robust_se1)

#marginal effect
model1_margin<-probitmfx(formula,data=data1,robust=TRUE,atmean=TRUE)
margin1<-as.data.frame(model1_margin$mfxest)

## model2 employed vc not in labor force
model2<-glm(formula,data=data2,family=binomial(link='probit'))

#robust st.err.
robust_se2<-vcovHC(model2,type='HC1')

#combine
robust_model2<-coeftest(model2,vcov=robust_se2)


#marginal effects
model2_margin<-probitmfx(formula,data=data2,robust=TRUE,atmean=TRUE)
margin2<-as.data.frame(model2_margin$mfxest)


##generate tables

stargazer(model1,model2,title='Binary Probit Model Result for two models',dep.var.labels=c('EMPSTAT(Unemployed=1)','EMPSTAT(Not in Labor Force=1)'),type='latex',se=list(se1),p=list(p1),star.cutoffs = c(0.05,0.01,0.001),star.char=c('*','**','***'),single.row =FALSE,align=TRUE,keep.stat = c('n','rsq','adj.rsq'))


stargazer(model1_margin$mfxest,title='Marginal Effects',type='latex',align=TRUE,digits=4)

stargazer(model2_margin$mfxest,title='Marginal Effects',type='latex',align=TRUE,digits=4)
