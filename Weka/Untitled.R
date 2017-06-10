library(dplyr)
library(data.table)
library(ggplot2)

need <- fread("~/Weka/merge1_new.csv")

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
people <- fread("~/Downloads/opendata105N010.csv",skip = 1)
people$縣市 <- substr(people$區域別 , start = 1 , stop = 3)
people <- people[1:368]
people_need <-  group_by(people,縣市) %>% summarise(人口數 = sum(as.numeric(年底人口數)) , 土地面積 = round(sum(as.numeric(土地面積))))
people_need$人口密度 <- round(people_need$人口數/people_need$土地面積)



need2 <- need[,c(2,3,4,11)]
need3_merge <- merge(need2 , people_need , by="縣市")

need4 <- fread("~/R/datamining/everyday_byyearncountry.csv")

need4_change <- need4[grepl("2016",need4$年),]
need4_change <- need4_change[,c(2,4)]
need4_change$縣市 <- gsub("台","臺",need4_change$縣市)
merge_final1 <- merge(need3_merge,need4_change,by="縣市")
write.csv(merge_final1 , 'merge_final1.csv',fileEncoding = "UTF-8")
