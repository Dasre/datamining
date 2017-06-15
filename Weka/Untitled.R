library(dplyr)
library(data.table)
library(ggplot2)

need <- fread("~/Documents/GitHub/datamining/Weka/merge1_new.csv")

##縣市擁有的平均醫護人員數


ggplot()+geom_bar(data=need ,
                  aes(x=reorder(縣市,as.numeric(平均醫護人員)),y=as.numeric(平均醫護人員),fill=factor(分區)),
                  stat = "identity") +theme(text=element_text(family="HanziPen TC Regular"))+labs(x = "縣市" , y = "平均醫護人員")




北部<-c("臺北市","新北市","基隆市","桃園市","新竹縣","新竹市","苗栗縣")
中部<-c("臺中市","南投縣","彰化縣","雲林縣","嘉義市","嘉義縣")
南部<-c("臺南市","高雄市","屏東縣")
東部<-c("宜蘭縣","花蓮縣","臺東縣")
離島<-c("連江縣","金門縣","澎湖縣")

need$分區 <- ""
need[縣市 %in% 北部]$分區 <-"北部"
need[縣市 %in% 中部]$分區 <-"中部"
need[縣市 %in% 東部]$分區 <-"東部"
need[縣市 %in% 南部]$分區 <-"南部"
need[縣市 %in% 離島]$分區 <-"離島"


##醫院數量

ggplot()+geom_bar(data=need ,
                  aes(x=reorder(縣市,as.numeric(totalhos)),y=as.numeric(totalhos),fill=factor(分區)),
                  stat = "identity") +theme(text=element_text(family="HanziPen TC Regular"))+labs(x = "縣市" , y = "醫院數量")


##人口和人口密度
library(data.table)
people <- fread("~/Documents/GitHub/datamining/Weka/opendata105N010.csv",skip = 1)
people$縣市 <- substr(people$區域別 , start = 1 , stop = 3)
people <- people[1:368]
people_need <-  group_by(people,縣市) %>% summarise(人口數 = sum(as.numeric(年底人口數)) , 土地面積 = round(sum(as.numeric(土地面積))))
people_need$人口密度 <- round(people_need$人口數/people_need$土地面積)



need2 <- need[,c(2,3,4,11)]
need3_merge <- merge(need2 , people_need , by="縣市")

need4 <- fread("~/Documents/GitHub/datamining/R處理過/everyday_byyearncountry.csv")

need4_change <- need4[grepl("2016",need4$年),]
need4_change <- need4_change[,c(2,4)]
need4_change$縣市 <- gsub("台","臺",need4_change$縣市)
merge_final1 <- merge(need3_merge,need4_change,by="縣市")
write.csv(merge_final1 , 'merge_final1.csv',fileEncoding = "UTF-8")

##各地區平均就診人數
merge_final1$平均就診人數 <- round(merge_final1$就診人數/merge_final1$人口數)
ggplot()+geom_bar(data=merge_final1 ,
                  aes(x=reorder(縣市,as.numeric(平均就診人數)),y=as.numeric(平均就診人數),fill=factor(分區)),
                  stat = "identity") +theme(text=element_text(family="HanziPen TC Regular"))+labs(x = "縣市" , y = "平均就診人數")

##醫院平均負擔病人
merge_final1$醫院平均負擔病人 <- round(as.numeric(merge_final1$就診人數)/as.numeric(merge_final1$totalhos))
ggplot()+geom_bar(data=merge_final1 ,aes(x=reorder(縣市,as.numeric(醫院平均負擔病人)),y=as.numeric(醫院平均負擔病人),fill=factor(分區)),stat = "identity") +theme(text=element_text(family="HanziPen TC Regular"))+labs(x = "縣市" , y = "醫院平均負擔病人")


merge_final12 <- merge_final1[,c(2,3,5,6,7,8,9)]

merge_final12$Totalhos <- as.numeric(merge_final12$totalhos)
merge_final12$Averagemedic <- as.numeric(merge_final12$平均醫護人員)
merge_final12$Population <- as.numeric(merge_final12$人口數)
merge_final12$Land <- as.numeric(merge_final12$土地面積)
merge_final12$Population_density <- as.numeric(merge_final12$人口密度)
merge_final12$Patient <- as.numeric(merge_final12$就診人數)
merge_final12$Averagepatient <- as.numeric(merge_final12$平均就診人數)
merge_final12$Averagehospatient <- as.numeric(merge_final1$醫院平均負擔病人)
##correlation

M <- cor(merge_final12,method="kendall") 
cor_properties <- corrplot(M, method = 'number',order ="hclust", tl.col="black", tl.cex = 1, tl.offset = 1, tl.srt = 40)

write.csv(merge_final12 , 'merge_final12.csv',fileEncoding = "UTF-8")

library(readr)
library(corrplot)

##correlation
M <- cor(merge_final12,method="kendall") 
cor_properties <- corrplot(M, method = 'number',order ="hclust", tl.col="black", tl.cex = 1, tl.offset = 1, tl.srt = 40)


##散佈
ggplot(data=merge_final1) +
  geom_point(aes(x=as.numeric(人口密度),
                 y=as.numeric(平均醫護人員),fill = factor(分區),colour = factor(分區), size = 3)) +theme(text=element_text(family="HanziPen TC Regular"))+labs(x = "人口密度" , y = "平均醫護人員")
##散佈
ggplot(data=merge_final1) +
  geom_point(aes(x=as.numeric(平均就診人數),
                 y=as.numeric(平均醫護人員),fill = factor(分區),colour = factor(分區), size = 3))+theme(text=element_text(family="HanziPen TC Regular"))+labs(x = "平均就診人數" , y = "平均醫護人員")

##就診人口/醫院
##merge_final1$醫院負擔病人 <- round(as.numeric(merge_final1$就診人數) / as.numeric(merge_final1$totalhos))
##merge_final12$hosXpatient <- merge_final1$醫院負擔病人

##散佈
ggplot(data=merge_final1) +
  geom_point(aes(x=as.numeric(醫院負擔病人),
                 y=as.numeric(平均醫護人員),fill = factor(分區),colour = factor(分區), size = 3))+theme(text=element_text(family="HanziPen TC Regular"))+labs(x = "醫院負擔病人" , y = "平均醫護人員")

##詭異的迴歸分析
model <- lm(formula= Averagemedic ~ Totalhos + Population + Land + Population_density + Patient + Averagepatient + hosXpatient,
            data=merge_final12)
summary(model)


##迴歸分析是我
model <- lm(formula= Averagemedic ~ Totalhos + Population + Land + Population_density + Patient + Averagepatient,
            data=merge_final12)
summary(model)


##年齡x
library(data.table)
age <- fread("~/Documents/GitHub/datamining/NHI_Influenza_like_illness(new).txt")
age1 <- age[grepl("2016",age$年)]

零到二十四<-c("零到四","五到十四","十五到二十四")
二十五到六十四<-c("二十五到六十四")
六十五以上<-c("六十五以上")


age1$年齡 <- ""
age1[年齡別 %in% 零到二十四]$年齡 <-"零到二十四"
age1[年齡別 %in% 二十五到六十四]$年齡 <-"二十五到六十四"
age1[年齡別 %in% 六十五以上]$年齡 <-"六十五以上"

age1$平均就診數 <- 


library(ggplot2)

ggplot()+geom_bar(data=age1 ,aes(x=reorder(縣市,as.numeric(健保就診總人次)),y=as.numeric(健保就診總人次),fill=factor(年齡)),stat = "identity") +theme(text=element_text(family="HanziPen TC Regular"))+labs(x = "縣市" , y = "健保就診總人次")
ggplot()+geom_bar(data=age1 ,aes(x=reorder(縣市,as.numeric(平均)),y=as.numeric(健保就診總人次),fill=factor(年齡)),stat = "identity") +theme(text=element_text(family="HanziPen TC Regular"))+labs(x = "縣市" , y = "健保就診總人次")


