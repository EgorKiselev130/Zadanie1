#Навров Андрей Владимирович — для региона 16 рассчитайте урожайность пшеницы в 2003 году, взяв для рассчета средние суммы активных температур за предыдущие 9 лет, с метеостанций на расстоянии от 70 до 210 км
library(lubridate)
library(stringr)
library(ggplot2)
library(tidyverse)
library(rnoaa)
setwd("~/Desktop")
station_data = read.csv("station_data.csv")
#После получения всписка всех станций, получите список станций ближайших к столице вашего региона,создав таблицу с именем региона и координатами его столицы
kazan = data.frame(id = "KAZAN", latitude = 55.796127, longitude = 49.106414)
kazan_around = meteo_nearby_stations(lat_lon_df = kazan, station_data = station_data,
var = c("PRCP", "TAVG"),
year_min = 1996, year_max = 2003)
kazan_table = kazan_around[[1]]
kazan_table = filter(kazan_table, distance>70 & distance<210)
#kazan_around это список единственным элементом которого является таблица, содержащая идентификаторы метеостанций отсортированных по их
#удалленности от Казани, очевидно что первым элементом таблицы будет идентификатор метеостанции Казани, его то мы и попытаемся получить
kazan_id = kazan_around[["KAZAN"]][["id"]][1]
str(kazan_around)
#Для получения всех данных с метеостанции, зная ее идентификатор, используйте след. команду
all_kazan_data = meteo_tidy_ghcnd(stationid = kazan_id)
kazan_table=kazan_around[[1]]
summary(kazan_table)
all_i=data.frame()
all_kazan=data.frame()
for(i in 1:2)
{all_i = meteo_tidy_ghcnd(stationid=kazan_around[["KAZAN"]][["id"]][i])
all_i=all_i[ ,c("id","date", "tavg")]
print(all_i)
all_kazan=rbind(all_kazan, all_i)}
#Средняя сумма активных температур
data_kazan=all_kazan %>%
mutate(date=ymd(date),
year=year(date),
month=month(date)) %>%
mutate(tavg=case_when(tavg<50 ~ 0, TRUE ~ tavg)/10) %>%
filter (year>1996 & year<2003) %>%
group_by(id,year,month) %>%
summarize(tsum=sum(tavg, na.rm=T)) %>%
group_by(month) %>% summarize(St = mean(tsum, na.rm=T))
afi=c(0.00,0.00,0.00,32.11,26.31,25.64,32.20,18.73,
16.30,13.83,0.00,0.00)
bfi=c(0.00,0.00,0.00,11.30,9.26,9.03,8.16,6.59,5.73,
4.87,0.00,0.00)
di=c(0.00,0.00,0.00,0.33,1.00,1.00,1.00,0.32,0.00,
0.00,0.00,0.00)
y=1.0
Kf=300
Qj=1600
Lj=2.2
Ej=25
#Рассчитаем урожайность по месяцам
data_kazan= data_kazan %>%
mutate(Fi=(afi)+(bfi)*y*(data_kazan$St))
data_kazan= data_kazan %>% mutate(Yj=(((data_kazan$Fi)*(di)*Kf)/(Qj*Lj*(100-Ej))))
#Расчитываем суммарную урожайность как сумму по месяцам
YIELD=sum(data_kazan$Yj);YIELD
# Ответ: 17.65 ц/га
