##### setting directory and initialization #####

rm(list=ls())
setwd("C:/Users/우지수/Dropbox/Coursework/2020-2/R/project/Team13")
getwd()


##### loading data #####

conv_dat = read.table(file="data/convenience_store.txt", sep='\t', header=TRUE)
conv_dat = data.frame(Region=conv_dat[,1],id=conv_dat[,2],StoreNum=conv_dat[,7],Population=conv_dat[,8],Worker=conv_dat[,9])
trust_dat = read.table(file="data/social_trust.txt", sep='\t', header=TRUE)
crime_dat = read.table(file="data/crime_2019.txt", sep='\t', header=TRUE)
raped_dat = read.table(file="data/raped.txt", sep='\t', header=TRUE)
rapedT_dat = read.table(file="data/raped_total.txt", sep='\t', header=TRUE)

population = conv_dat$Population + conv_dat$Worker/1.3
conv_dat[,3:3] = conv_dat[,3:3] / population
crime_dat[,3:14] = crime_dat[,3:14] / population
raped_dat[,2:16] = raped_dat[,2:16] / population

conv_dat[,3:3] = scale(conv_dat[,3:3])
trust_dat[,3:9] = scale(trust_dat[,3:9])
crime_dat[,3:14] = scale(crime_dat[,3:14])
raped_dat[,2:16] = scale(raped_dat[,2:16])

crime_dat = crime_dat[,c(1,2,4,6,8,10,12,14)]

##### [Map] zoning #####

library(raster)
library(ggplot2)
library(dplyr)

korea = shapefile('map/TL_SCCO_SIG.shp')
korea = fortify(korea, region='SIG_CD')
seoul = korea[korea$id <= 11740, ]
gu_name = read.table(file="data/gu_name.txt", sep='\t', header=TRUE)


##### [Map] printing out the convenience store map #####

conv_map = merge(seoul, conv_dat, by='id')
ggplot() + geom_polygon(data=conv_map, aes(x=long, y=lat, group=group, fill=StoreNum), color='gray40') +
  scale_fill_gradient(low = "#DBEDFF", high = "#0081FF") +
  labs(fill = "편의점 수") +
  coord_fixed(ratio = 1.3) +
  ggtitle("서울 편의점 분포") +
  geom_text(data =gu_name, aes(x = long, y = lat, label = Region))


##### [Map] printing out the social trust map #####

trust_map = merge(seoul, cbind(trust_dat, conv_dat[,3]), by='id')
colnames(trust_map)[16] = "Store"
ggplot() + 
  geom_polygon(data=trust_map, aes(x=long, y=lat, group=group, fill=Store*Total), color='gray40') +
  scale_fill_gradient2(low = "#FF0000", high = "#0081FF") +
  labs(fill = "종합 신뢰 지수") +
  coord_fixed(ratio = 1.3) +
  ggtitle("사회적 신뢰 지수") +
  geom_text(data =gu_name, aes(x = long, y = lat, label = Region))

##### [Map] printing out the crime arrest rate map #####

crime_map = merge(seoul, cbind(crime_dat, conv_dat[,3]), by='id')
colnames(crime_map)[15] = "Store"
ggplot() + geom_polygon(data=crime_map, aes(x=long, y=lat, group=group, fill=Store*TotalR), color='gray40') +
  scale_fill_gradient2(low = "#FF0000", high = "#0081FF") +
  labs(fill = "종합 범죄 발생 검거율") +
  coord_fixed(ratio = 1.3) +
  ggtitle("범죄 발생 검거율") +
  geom_text(data =gu_name, aes(x = long, y = lat, label = Region))


##### [Cor.] social trust Pearson correlation analysis #####

library(PerformanceAnalytics)
library(corrplot)
trust_cor = cbind(conv_dat[,3], trust_dat[,3:9])
colnames(trust_cor)[1] = "Store"

chart.Correlation(trust_cor)

trust_cor = cor(trust_cor)
trust_cor2 = as.data.frame(trust_cor[1,2:8])
colnames(trust_cor2)[1] = "Corr"

corrplot(trust_cor, method="number")
ggplot(trust_cor2, aes(x=rownames(trust_cor2), y=Corr)) +
  geom_bar(stat="identity", position="dodge", fill='#0081FF') +
  coord_cartesian(ylim = c(-0.9:0.9)) +
  ggtitle("사회적 신뢰 상관 관계") +
  theme_bw() 


##### [Cor.] crime arrest rate Pearson correlation analysis #####

crime_cor = cbind(conv_dat[,3], crime_dat[,3:8])
colnames(crime_cor)[1] = "Store"

chart.Correlation(crime_cor)

crime_cor = cor(crime_cor)
crime_cor2 = as.data.frame(crime_cor[1, 2:7])
colnames(crime_cor2)[1] = "Corr"

corrplot(crime_cor, method="number")
ggplot(crime_cor2, aes(x=rownames(crime_cor2), y=Corr)) +
  geom_bar(stat="identity", position="dodge", fill='#0081FF') +
  coord_cartesian(ylim = c(-0.2:0.9)) +
  ggtitle("범죄 발생 상관 관계") +
  theme_bw() 


##### [DID] the whole #####

ggplot(rapedT_dat, aes(x=Year, y=Total, colour=Started)) + 
  geom_vline(xintercept=2013) +
  ggtitle("여성 안심 편의점 이중차분법") +
  stat_summary(geom="line") +
  geom_line(stat="smooth",method = "lm", formula = y ~ 0 + I(1/x) + I((x-1)/x),
              size = 1.2, linetype ="dashed", alpha = 0.5) +
  annotate("text", x=2008, y=5600, label="y = 477.3x - 954003", size=3) +
  annotate("text", x=2016, y=8100, label="y = 348.37x - 693275", size=3)


##### [DID] #####

scope1 = cbind(raped_dat[,c(1, 17)], F)
colnames(scope1) = c("Region", "Scope", "State")
scope2 = cbind(raped_dat[,c(1, 18)], T)
colnames(scope2) = c("Region", "Scope", "State")
scope3 = as.data.frame((scope1$Scope-scope2$Scope) / scope1$Scope * 100)
raped_scope = rbind(scope1, scope2)
raped_scope = cbind(raped_scope, rbind(scope3, scope3))
colnames(raped_scope) = c("Region", "Scope", "State", "Percent")
ggplot(raped_scope, aes(x=Region, y=Scope, fill=State)) +
  geom_bar(stat="identity", position="dodge") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  geom_line(aes(x=Region, y=Percent/2), group=1) +
  geom_text(aes(x=Region, y=Percent/2, label=as.integer(Percent))) +
  ggtitle("구 별 범죄 감소율")


##### Regression #####

crime_dat = cbind(crime_dat, conv_dat$StoreNum)
colnames(crime_dat)[9] = "Store" 

library(tidyverse)

crime_dat %>%
  select(TotalR, MurderR, RobberR, RapeR, TheftR, ForceR) %>%
  map(~lm(.x~crime_dat$Store)) %>%
  map(summary)

crime_dat %>%
  select(TotalR, MurderR, RobberR, RapeR, TheftR, ForceR) %>%
  map(~lm(.x~crime_dat$Store)) %>%
  map(summary) %>%
  map('adj.r.squared') #adj.r.squared만 불러오기

par(mfrow = c(2,2))
plot(lm(TotalR~Store, data = crime_dat))
plot(lm(MurderR~Store, data = crime_dat))
plot(lm(RobberR~Store, data = crime_dat))
plot(lm(RapeR~Store, data = crime_dat))
plot(lm(TheftR~Store, data = crime_dat))
plot(lm(ForceR~Store, data = crime_dat))

ggplot(crime_dat, aes(x=Total, y=Store)) + 
  ggtitle("총합 범죄 발생 회귀 분석") +
  stat_summary(geom="line") +
  stat_smooth(method="lm", se=F, linetype="dashed")
ggplot(crime_dat, aes(x=TotalR, y=Store)) + 
  ggtitle("총합 범죄 검거율 회귀 분석") +
  stat_summary(geom="line") +
  stat_smooth(method="lm", se=F, linetype="dashed")
ggplot(crime_dat, aes(x=MurderR, y=Store)) + 
  ggtitle("살인 검거율 회귀 분석") +
  stat_summary(geom="line") +
  stat_smooth(method="lm", se=F, linetype="dashed")
ggplot(crime_dat, aes(x=RobberR, y=Store)) + 
  ggtitle("강도 검거율 회귀 분석") +
  stat_summary(geom="line") +
  stat_smooth(method="lm", se=F, linetype="dashed")
ggplot(crime_dat, aes(x=RapeR, y=Store)) + 
  ggtitle("강간 검거율 회귀 분석") +
  stat_summary(geom="line") +
  stat_smooth(method="lm", se=F, linetype="dashed")
ggplot(crime_dat, aes(x=TheftR, y=Store)) + 
  ggtitle("절도 검거율 회귀 분석") +
  stat_summary(geom="line") +
  stat_smooth(method="lm", se=F, linetype="dashed")
ggplot(crime_dat, aes(x=ForceR, y=Store)) + 
  ggtitle("폭력 검거율 회귀 분석") +
  stat_summary(geom="line") +
  stat_smooth(method="lm", se=F, linetype="dashed")

trust_dat = cbind(trust_dat, conv_dat$StoreNum)
colnames(trust_dat)[10] = "Store" 

trust_dat %>%
  select(Total, Family, Neighbor, Friend, Public, Stranger, Foreign) %>%
  map(~lm(.x~trust_dat$Store)) %>%
  map(summary)

trust_dat %>%
  select(Total, Family, Neighbor, Friend, Public, Stranger, Foreign) %>%
  map(~lm(.x~trust_dat$Store)) %>%
  map(summary) %>%
  map('adj.r.squared')

plot(lm(Total~Store, data = trust_dat))
plot(lm(Family~Store, data = trust_dat))
plot(lm(Neighbor~Store, data = trust_dat))
plot(lm(Friend~Store, data = trust_dat))
plot(lm(Public~Store, data = trust_dat))
plot(lm(Stranger~Store, data = trust_dat))
plot(lm(Foreign~Store, data = trust_dat))

ggplot(trust_dat, aes(x=Total, y=Store)) + 
  ggtitle("종합 신뢰도 회귀 분석") +
  stat_summary(geom="line") +
  stat_smooth(method="lm", se=F, linetype="dashed")
ggplot(trust_dat, aes(x=Family, y=Store)) + 
  ggtitle("가족 신뢰도 회귀 분석") +
  stat_summary(geom="line") +
  stat_smooth(method="lm", se=F, linetype="dashed")
ggplot(trust_dat, aes(x=Neighbor, y=Store)) + 
  ggtitle("이웃 신뢰도 회귀 분석") +
  stat_summary(geom="line") +
  stat_smooth(method="lm", se=F, linetype="dashed")
ggplot(trust_dat, aes(x=Friend, y=Store)) + 
  ggtitle("친구 신뢰되 회귀 분석") +
  stat_summary(geom="line") +
  stat_smooth(method="lm", se=F, linetype="dashed")
ggplot(trust_dat, aes(x=Public, y=Store)) + 
  ggtitle("공공기관 신뢰도 회귀 분석") +
  stat_summary(geom="line") +
  stat_smooth(method="lm", se=F, linetype="dashed")
ggplot(trust_dat, aes(x=Stranger, y=Store)) + 
  ggtitle("낯선 사람 신뢰도 회귀 분석") +
  stat_summary(geom="line") +
  stat_smooth(method="lm", se=F, linetype="dashed")
ggplot(trust_dat, aes(x=Foreign, y=Store)) + 
  ggtitle("국내 거주 외국인 신뢰도 회귀 분석") +
  stat_summary(geom="line") +
  stat_smooth(method="lm", se=F, linetype="dashed")

