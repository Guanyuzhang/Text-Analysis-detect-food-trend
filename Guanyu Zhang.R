rm(list = ls())
library(wordcloud)
library(NMF)
library(tm)
library(NLP)
library(reshape2)
library(ggplot2)
library(zoo)
library(ggpmisc)
library(lubridate)
ingredients<- read.delim('D:/SIMON/Social Media/final project/ingredients.txt',header=FALSE)
dic<-as.character(ingredients$V1)
dic<-tolower(dic)
dic<-append(dic,c('cauliflower rice','vegetable noodle','veggie spiralizer',
                  'pumpkin pie','trader joe','zucchini','zoodle','noodle','veggie'))
dic<-sort(dic)
my_tokenizer <- function(x) unlist(lapply(NLP::ngrams(words(x), 1:2), paste, collapse = " "), use.names = FALSE)
Foodcount<-function(x){
    docs<-VCorpus(VectorSource(x))
    dtm <- DocumentTermMatrix(docs,list(dictionary=dic,
                                         tolower=T, 
                                         removePunctuation=T, 
                                         removeNumbers=T, 
                                         stripWhitespace=T, 
                                         stopwords=c(stopwords("english")),
                                         tokenize=my_tokenizer))
    return(dtm)
}
fnames2011 <- list.files('D:/SIMON/Social Media/final project/fb2011')
fnames2012 <- list.files('D:/SIMON/Social Media/final project/fb2012')
fnames2013 <- list.files('D:/SIMON/Social Media/final project/fb2013')
fnames2014 <- list.files('D:/SIMON/Social Media/final project/fb2014')
fnames2015 <- list.files('D:/SIMON/Social Media/final project/fb2015')


MonthlyWord<-function(x){
    df2011<-data.frame(dic)
    for (i in x){
        docs=read.csv(i,header=FALSE,sep = '\t')
        dtm<-Foodcount(docs)
        freq<-colSums(as.matrix(dtm))
        df<-data.frame(freq)
        df2011<-cbind(df2011,df)
    }
    colnames(df2011)<-c('fname',01,02,03,04,05,06,07,08,09,10,11,12)
    return(df2011)
}
setwd("D:/SIMON/Social Media/final project/fb2011")
trend2011<-MonthlyWord(fnames2011)
colnames(trend2011)<-c('fname','2011-01','2011-10','2011-11','2011-12','2011-02','2011-03','2011-04','2011-05','2011-06','2011-07','2011-08','2011-09')

setwd("D:/SIMON/Social Media/final project/fb2012")
trend2012<-MonthlyWord(fnames2012)
colnames(trend2012)<-c('fname','2012-01','2012-10','2012-11','2012-12','2012-02','2012-03','2012-04','2012-05','2012-06','2012-07','2012-08','2012-09')

setwd("D:/SIMON/Social Media/final project/fb2013")
trend2013<-MonthlyWord(fnames2013)
colnames(trend2013)<-c('fname','2013-01','2013-10','2013-11','2013-12','2013-02','2013-03','2013-04','2013-05','2013-06','2013-07','2013-08','2013-09')

setwd("D:/SIMON/Social Media/final project/fb2014")
trend2014<-MonthlyWord(fnames2014)
colnames(trend2014)<-c('fname','2014-01','2014-10','2014-11','2014-12','2014-02','2014-03','2014-04','2014-05','2014-06','2014-07','2014-08','2014-09')

setwd("D:/SIMON/Social Media/final project/fb2015")
trend2015<-MonthlyWord(fnames2015)
colnames(trend2015)<-c('fname','2015-01','2015-10','2015-11','2015-12','2015-02','2015-03','2015-04','2015-05','2015-06','2015-07','2015-08','2015-09')


df_all<-merge(trend2011,trend2012,by='fname')
df_all<-merge(df_all,trend2013,by='fname')
df_all<-merge(df_all,trend2014,by='fname')
df_all<-merge(df_all,trend2015,by='fname')

df_melted <- melt(df_all, id ="fname")
df_melted$month<-as.yearmon(df_melted$variable, "%Y-%m")
df_melted$month<-as.Date(df_melted$month)

#cauliflower rice trend plot
ggplot(df_melted[df_melted$fname=='cauliflower rice',],aes(x=month,y=value))+ 
    geom_bar(stat='identity')+
    scale_x_date(date_labels="%b %y",date_breaks  ="3 month")+ggtitle('Cauliflower Rice Trend')+
    geom_smooth(method = "loess",se = FALSE)+
    labs(x = "Month", y = "Num of Post")+
    theme(plot.title = element_text(size = 20, face = "bold"),
          legend.title = element_blank(),
          axis.text=element_text(size=10),
          axis.title=element_text(size=14,face="bold"),
          legend.text=element_text(size=15))

#cauliflower and rice correlation plot
ggplot(df_melted[df_melted$fname=='rice'|df_melted$fname=='cauliflower',],aes(x=month,y=value))+ 
    geom_line(aes(color=fname))+
    scale_x_date(date_labels="%b %y",date_breaks  ="3 month")+ggtitle('Correlation Between Cauliflower and Rice')+
    labs(x = "Month", y = "Num of Post")+
    theme(plot.title = element_text(size = 20, face = "bold"),
          legend.title = element_blank(),
          axis.text=element_text(size=10),
          axis.title=element_text(size=14,face="bold"),
          legend.text=element_text(size=15))

#vegetable noodle trend plot
ggplot(df_melted[df_melted$fname=='vegetable noodle'|df_melted$fname=='zoodle'|df_melted$fname=='veggie spiralizer',],aes(x=month,y=value))+ 
    geom_bar(stat='identity',aes(fill=fname))+
    scale_x_date(date_labels="%b %y",date_breaks  ="3 month")+ggtitle('Vegetable Noodle Trend')+
    geom_smooth(method = "loess",se = FALSE)+
    labs(x = "Month", y = "Num of Post")+
    theme(plot.title = element_text(size = 20, face = "bold"),
          legend.title = element_blank(),
          axis.text=element_text(size=10),
          axis.title=element_text(size=14,face="bold"),
          legend.text=element_text(size=15))

#vegetable, pasta and noodle correlation plot
ggplot(df_melted[df_melted$fname=='zucchini'|df_melted$fname=='noodle'|df_melted$fname=='pasta',],aes(x=month,y=value))+ 
    geom_line(aes(color=fname))+
    scale_x_date(date_labels="%b %y",date_breaks  ="3 month")+ggtitle('Correlation Between Zucchini,Pasta and Noodle')+
    labs(x = "Month", y = "Num of Post")+
    theme(plot.title = element_text(size = 20, face = "bold"),
          legend.title = element_blank(),
          axis.text=element_text(size=10),
          axis.title=element_text(size=14,face="bold"),
          legend.text=element_text(size=15))

#pumpkin pie plot
ggplot(df_melted[df_melted$fname=='pumpkin pie',],aes(x=month,y=value))+ 
    geom_line()+
    scale_x_date(date_labels="%b %Y",date_breaks  ="1 month")+ggtitle('Pumpkin Pie Trend')+
    theme(plot.title = element_text(size = 20, face = "bold"),
          legend.title = element_blank(),
          axis.text=element_text(size=10),
          axis.title=element_text(size=14,face="bold"),
          legend.text=element_text(size=15),
          axis.text.x = element_text(angle = 60))+
    stat_peaks(colour = "red")+
    stat_peaks(geom = "rug", colour = "red")

#pumpkin and pie correlation plot
ggplot(df_melted[df_melted$fname=='pumpkin'|df_melted$fname=='pie',],aes(x=month,y=value))+ 
    geom_line(aes(color=fname))+
    scale_x_date(date_labels="%b %y",date_breaks  ="3 month")+ggtitle('Correlation Between Pumpkin and Pie')+
    theme(plot.title = element_text(size = 20, face = "bold"),
          legend.title = element_blank(),
          axis.text=element_text(size=10),
          axis.title=element_text(size=14,face="bold"),
          legend.text=element_text(size=15))

