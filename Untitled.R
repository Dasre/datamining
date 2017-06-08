library(data.table)
hospital <- fread("~/datamining/1機構資料.txt" , colClasses = "character")
everyday <- fread("~/datamining/NHI_Influenza_like_illness(new).txt" , colClasses = "character")

library(dplyr)
hospital$縣市 <- substr(hospital$縣市鄉鎮 , start = 1 , stop = 3)

##以縣市統計醫院數
hospital_bycountry <- group_by(hospital,縣市) %>% summarise(totalhos = n())

##以縣市、醫院型態別統計
hospital_bycountrynhospital <- group_by(hospital,縣市,型態別) %>% summarise(totalhos = n())
write.csv(hospital_bycountrynhospital,'hospital_bycountrynhospital.csv')

##以縣市統計醫院數(還有個縣市各種醫生人數)
hospital_bycountry <- group_by(hospital,縣市) %>% 
  summarise(totalhos = n() ,
            西醫生 = sum(as.numeric(西醫生)) ,
            中醫師 = sum(as.numeric(中醫師)) ,
            牙醫師 = sum(as.numeric(牙醫師)) , 
            藥師 = sum(as.numeric(藥師)) , 
            護理師 = sum(as.numeric(護理師)) ,
            護士 = sum(as.numeric(護士)))
write.csv(hospital_bycountry,'hospital_bycountry.csv')

##各地區平均每間醫院護理人員數量
hospital_averageman <- group_by(hospital_bycountry,縣市) %>%
  summarise(平均醫護人員 = round(sum(西醫生+中醫師+牙醫師+藥師+護理師+護士)/totalhos))

write.csv(hospital_averageman,'hospital_averageman.csv')




##年，縣市
everyday_byyearncountry <- group_by(everyday,縣市,年) %>%
  summarise(就診人數 = sum(as.numeric(健保就診總人次)))
write.csv(everyday_byyearncountry,'everyday_byyearncountry.csv')

##
everyday_byyearncountrynage <- group_by(everyday,縣市,年,年齡別) %>%
  summarise(就診人數 = sum(as.numeric(健保就診總人次)))
write.csv(everyday_byyearncountrynage,'everyday_byyearncountrynage.csv')
