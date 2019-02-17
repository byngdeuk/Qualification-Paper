# remove environment #
rm(list=ls())

# set working directory #
setwd("C:\\Users\\woobd1\\Dropbox\\Iowa\\Qualification\\IOWA Method Paper\\Qualification-Paper\\replication test")

# import replication stata data into dataframe #
library(foreign)
replication <- read.dta("replication start.dta")

# generate number of adoptions for any crimes by country and year (for law and nap) #
replication$L_num = replication$L_Femicide + replication$L_forced_sterilization + replication$L_stalking + replication$L_property + replication$L_violence_against_women + replication$L_domestic + replication$L_sexual_violence + replication$L_new + replication$L_sexual_harassment + replication$L_FGM + replication$L_trafficking + replication$L_child_ealry_forced
replication$NAP_num = replication$NAP_Femicide + replication$NAP_forced_sterilization + replication$NAP_stalking + replication$NAP_property + replication$NAP_violence_against_women + replication$NAP_domestic + replication$NAP_sexual_violence + replication$NAP_new + replication$NAP_sexual_harassment + replication$NAP_FGM + replication$NAPL_trafficking + replication$NAPL_child_ealry_forced

# generate dummy variable if a country in a given year adopt any components by country and year (for law and nap) #
install.packages("dplyr")
install.packages("tidyverse")
library("dplyr")
library("tidyverse")
mutate
replication2 <- replication %>% mutate(L_any = ifelse(L_num > 0, 1, 0))
replication3 <- replication2 %>% mutate(NAP_any = ifelse(NAP_num > 0, 1, 0))

# generate cumulative number of adoptions for any components by country and year (for law and nap) #
replication4 <- replication3 %>%
  group_by(ccode) %>%
  mutate(L_has_num = cumsum(L_num),
         NAP_has_num = cumsum(NAP_num),
         L_has_any = ifelse(L_has_num > 0, 1, 0),
         NAP_has_any = ifelse(NAP_has_num > 0, 1, 0),
         lag.L_has_any = dplyr::lag(L_has_any, n=1, default = NA), # generate lagged variables for various purposes such as creating spatial lag and lagged independent variables #
         lag.NAP_has_any = dplyr::lag(NAP_has_any, n=1, default = NA)) 


replication4 <- replication4 %>%
  group_by(ccode) %>%
  mutate(lag.L_has_num = dplyr::lag(L_has_num, n=1, default = NA)) 

replication4 <- replication4 %>%
  group_by(ccode) %>%
  mutate(lag.NAP_has_num = dplyr::lag(NAP_has_num, n=1, default = NA)) 

replication4 <- replication4 %>%
  group_by(ccode) %>%
  mutate(lag.polity2 = dplyr::lag(polity2, n=1, default = NA)) 

replication4 <- replication4 %>%
  group_by(ccode) %>%
  mutate(lag.cgdppc = dplyr::lag(cgdppc, n=1, default = NA)) 

replication4 <- replication4 %>%
  group_by(ccode) %>%
  mutate(lag.vdem_gender = dplyr::lag(vdem_gender, n=1, default = NA)) 

replication4 <- replication4 %>%
  group_by(ccode) %>%
  mutate(lag.actotal = dplyr::lag(actotal, n=1, default = NA)) 

#replace na of laggded to 0#
replication4$lag.L_has_any[which(is.na(replication4$lag.L_has_any))] <- 0
replication4$lag.NAP_has_any[which(is.na(replication4$lag.NAP_has_any))] <- 0
replication4$lag.L_has_num[which(is.na(replication4$lag.L_has_num))] <- 0
replication4$lag.NAP_has_num[which(is.na(replication4$lag.NAP_has_num))] <- 0


# replace NA of ht_region and ht_colonial with first observed value by ccode  because region and colonial heritage didn't change over time #
replication4 <- replication4 %>%
  group_by(ccode) %>%
  fill(ht_colonial, ht_region, lp_legor) %>% #default direction down
  fill(ht_colonial, ht_region, lp_legor, .direction = "up")

# change ht_colonial = 0 to = 11 for later calculation #
replication4 <- replication4 %>%
  mutate(ht_colonial=replace(ht_colonial, ht_colonial == 0, 11))

# create the sum of number of countires according to ht_region, ht_colonial, and lp_legor. Replace NA count to NA #
replication4 <- replication4 %>%
  group_by(year, ht_region) %>%
  mutate(count_ht_region = n())

replication4 <- replication4 %>%
  group_by(ccode, year, ht_region) %>%
  mutate(count_ht_region = factor(ifelse(ht_region == "NA", "NA", count_ht_region)))

replication4 <- replication4 %>%
  group_by(year, ht_colonial) %>%
  mutate(count_ht_colonial = n())

replication4 <- replication4 %>%
  group_by(ccode, year, ht_colonial) %>%
  mutate(count_ht_colonial = factor(ifelse(ht_colonial == "NA", "NA", count_ht_colonial)))


replication4 <- replication4 %>%
  group_by(year, lp_legor) %>%
  mutate(count_lp_legor = n())

replication4 <- replication4 %>%
  group_by(ccode, year, lp_legor) %>%
  mutate(count_lp_legor = factor(ifelse(lp_legor == "NA", "NA", count_lp_legor)))

# numbers of countries with laws and NAP in a lagged year #
replication4 <- replication4 %>%
  group_by(year, ht_region) %>%
  mutate(Lnumcountries_ht_region = sum(lag.L_has_any))

replication4 <- replication4 %>%
  group_by(year, ht_region) %>%
  mutate(NAPnumcountries_ht_region = sum(lag.NAP_has_any))

replication4 <- replication4 %>%
  group_by(year, ht_colonial) %>%
  mutate(Lnumcountries_ht_colonial = sum(lag.L_has_any))

replication4 <- replication4 %>%
  group_by(year, ht_colonial) %>%
  mutate(NAPnumcountries_ht_colonial = sum(lag.NAP_has_any))

replication4 <- replication4 %>%
  group_by(year, lp_legor) %>%
  mutate(Lnumcountries_lp_legor = sum(lag.L_has_any))

replication4 <- replication4 %>%
  group_by(year, lp_legor) %>%
  mutate(NAPnumcountries_lp_legor = sum(lag.NAP_has_any))

# create percentage of countries with law and NAP for diffusion variables #
replication4 <- replication4 %>%
  mutate(DL_ht_region = as.numeric(Lnumcountries_ht_region)/as.numeric(count_ht_region))

replication4 <- replication4 %>%
  mutate(DNAP_ht_region = as.numeric(NAPnumcountries_ht_region)/as.numeric(count_ht_region))

replication4 <- replication4 %>%
  mutate(DL_ht_colonial = as.numeric(Lnumcountries_ht_colonial)/as.numeric(count_ht_colonial))

replication4 <- replication4 %>%
  mutate(DNAP_ht_colonial = as.numeric(NAPnumcountries_ht_colonial)/as.numeric(count_ht_colonial))

replication4 <- replication4 %>%
  mutate(DL_lp_legor = as.numeric(Lnumcountries_lp_legor)/as.numeric(count_lp_legor))

replication4 <- replication4 %>%
  mutate(DNAP_lp_legor = as.numeric(NAPnumcountries_lp_legor)/as.numeric(count_lp_legor))

# create time and time^2 variable by first adoption of law against crimes aginst women #
replication4 <- replication4 %>%
  mutate(time = year - 1908)
replication4 <- replication4 %>%
  mutate(time_sq = time * time)

# Variables Labeling #


# draw maps for countries having laws and NAP #
library("ggplot2")
install.packages("cshapes")
library("cshapes")
cshp(date=NA, useGW=FALSE)
cshp.1946 <- cshp(date=as.Date("1946-6-30"), useGW=FALSE)
my1946 <- fortify(cshp.1946)
id <- c(cshp.1946$FEATUREID)
ccode <- c(cshp.1946$COWCODE)
df1946 <- data.frame(id,ccode)
my1946M <- merge(my1946, df1946, by.my1946 = "id", by.df1946 = "id")
d1946 <- subset(replication4, year == 1946)
my1946M2 <- merge(my1946M, d1946, by.my1946M = "ccode", by.d1946 = "ccode")
ggplot(my1946M2, aes(x=long, y=lat, group=group)) + geom_path() + geom_polygon(aes(fill = L_has_any), colour = rgb(1,1,1,0.2)) + scale_fill_gradient("Blue", "Red") + ggtitle("Countries with Laws against Crimes agianst Women in 1946")

cshp.1966 <- cshp(date=as.Date("1966-6-30"), useGW=FALSE)
my1966 <- fortify(cshp.1966)
id <- c(cshp.1966$FEATUREID)
ccode <- c(cshp.1966$COWCODE)
df1966 <- data.frame(id,ccode)
my1966M <- merge(my1966, df1966, by.my1966 = "id", by.df1966 = "id")
d1966 <- subset(replication4, year == 1966)
my1966M2 <- merge(my1966M, d1966, by.my1966M = "ccode", by.d1966 = "ccode")
ggplot(my1966M2, aes(x=long, y=lat, group=group)) + geom_path() + geom_polygon(aes(fill = L_has_any), colour = rgb(1,1,1,0.2)) + scale_fill_gradient("Blue", "Red") + ggtitle("Countries with Laws against Crimes agianst Women in 1966")

cshp.1986 <- cshp(date=as.Date("1986-6-30"), useGW=FALSE)
my1986 <- fortify(cshp.1986)
id <- c(cshp.1986$FEATUREID)
ccode <- c(cshp.1986$COWCODE)
df1986 <- data.frame(id,ccode)
my1986M <- merge(my1986, df1986, by.my1986 = "id", by.df1966 = "id")
d1986 <- subset(replication4, year == 1986)
my1986M2 <- merge(my1986M, d1986, by.my1986M = "ccode", by.d1986 = "ccode")
ggplot(my1986M2, aes(x=long, y=lat, group=group)) + geom_path() + geom_polygon(aes(fill = L_has_any), colour = rgb(1,1,1,0.2)) + scale_fill_gradient("Blue", "Red") + ggtitle("Countries with Laws against Crimes agianst Women in 1986")


cshp.1996 <- cshp(date=as.Date("1996-6-30"), useGW=FALSE)
my1996 <- fortify(cshp.1996)
id <- c(cshp.1996$FEATUREID)
ccode <- c(cshp.1996$COWCODE)
df1996 <- data.frame(id,ccode)
my1996M <- merge(my1996, df1996, by.my1996 = "id", by.df1996 = "id")
d1996 <- subset(replication4, year == 1996)
my1996M2 <- merge(my1996M, d1996, by.my1996M = "ccode", by.d1996 = "ccode")
p <- ggplot(my1996M2, aes(x=long, y=lat, group=group)) + geom_path() + geom_polygon(aes(fill = L_has_any), colour = rgb(1,1,1,0.2)) + scale_fill_gradient("Blue", "Red") + ggtitle("Countries with Laws against Crimes agianst Women in 1996")
print(p)

cshp.2006 <- cshp(date=as.Date("2006-6-30"), useGW=FALSE)
my2006 <- fortify(cshp.2006)
id <- c(cshp.2006$FEATUREID)
ccode <- c(cshp.2006$COWCODE)
df2006 <- data.frame(id,ccode)
my2006M <- merge(my2006, df2006, by.my2006 = "id", by.df2006 = "id")
d2006 <- subset(replication4, year == 2006)
my2006M2 <- merge(my2006M, d2006, by.my2006M = "ccode", by.d2006 = "ccode")
ggplot(my2006M2, aes(x=long, y=lat, group=group)) + geom_path() + geom_polygon(aes(fill = L_has_any), colour = rgb(1,1,1,0.2)) + scale_fill_gradient("Blue", "Red") + ggtitle("Countries with Laws against Crimes agianst Women in 2006")

cshp.2016 <- cshp(date=as.Date("2016-6-30"), useGW=FALSE)
my2016 <- fortify(cshp.2016)
id <- c(cshp.2016$FEATUREID)
ccode <- c(cshp.2016$COWCODE)
df2016 <- data.frame(id,ccode)
my2016M <- merge(my2016, df2016, by.my2016 = "id", by.df2016 = "id")
d2016 <- subset(replication4, year == 2016)
my2016M2 <- merge(my2016M, d2016, by.my2016M = "ccode", by.d2016 = "ccode")
ggplot(my2016M2, aes(x=long, y=lat, group=group)) + geom_path() + geom_polygon(aes(fill = L_has_any), colour = rgb(1,1,1,0.2)) + scale_fill_gradient("Blue", "Red") + ggtitle("Countries with Laws against Crimes agianst Women in 2016")

# save dataframe #
write.dta(replication4, file = "replication4.dta")

# Count number of countries with laws or NAP by year for cumulative graph #
library(dplyr)
cum <- replication4 %>%
  filter(L_has_any>0) %>%
  group_by(year) %>%
  tally()

# Cumulative line graphs of Number of Countries with Laws against Crimes against Women #
cumulative <- ggplot() +
  geom_line(aes(y = n, x = year), size=1, data = cum,
            ) +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  scale_x_continuous(breaks=seq(1908,2016,8)) +
  labs(x="Year", y="Number of Countries") +
  ggtitle("Cumulative Number of Countries with Laws against Crimes against Women") +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10),
        legend.key=element_rect(fill="white", colour="white"))
cumulative

install.packages("plotly") 
library(plotly)

cumulative <- ggplotly(cumulative)
cumulative 

# Analysis #
library(survival)
library(MASS)
fit2 = coxph(Surv(time,L_has_any)~ CEDAW + DL_ht_colonial + DL_ht_region + DL_lp_legor + lag.polity2 + lag.cgdppc + lag.vdem_gender + cluster(ccode), data=replication4)
summary(fit2)

# drop before first any law adoption#
replication <- subset(replication4, year > 1907)

#subset lag.L_has_any<1: drop after first adoption#
subset1 <- subset(replication, lag.L_has_any<1)

# test cluster#
install.packages("clusterSEs")
library(clusterSEs)

##Any law##
#first adoption#
myprobit <- glm(L_has_any ~ CEDAW + DL_ht_colonial + DL_ht_region + DL_lp_legor + lag.cgdppc + lag.polity2 + lag.vdem_gender + lag.actotal + time + time_sq,family=binomial(link="probit"), data=subset1)
summary(myprobit)
nobs(myprobit)
clust.myprobit <- cluster.bs.glm(myprobit, subset1, ~ ccode, report = T)

mycox <- coxph(Surv(time,L_has_any) ~ CEDAW + DL_ht_colonial + DL_ht_region + DL_lp_legor + lag.cgdppc + lag.polity2 + lag.vdem_gender + cluster(ccode), data=subset1)
summary(mycox)

#repeated adoption#
myprobit2 <- glm(L_has_any ~ CEDAW + DL_ht_colonial + DL_ht_region + DL_lp_legor + lag.cgdppc + lag.polity2 + lag.vdem_gender + lag.actotal + time + time_sq,family=binomial(link="probit"), data=replication)
summary(myprobit2)
nobs(myprobit2)
clust.myprobit2 <- cluster.bs.glm(myprobit2, replication, ~ ccode, report = T)


mycox2 <- coxph(Surv(time,L_has_any) ~ CEDAW + DL_ht_colonial + DL_ht_region + DL_lp_legor + lag.cgdppc + lag.polity2 + lag.vdem_gender + cluster(ccode), data=replication)
summary(mycox2)

##Diffusion of NAP without considering steps##
#subset lag.NAP_has_any<1: drop after first NAP#
replicationNAP <- subset(replication4, year > 1991)
replicationNAP <- replicationNAP %>%
  mutate(time = year - 1992)
replicationNAP <- replicationNAP %>%
  mutate(time_sq = time * time)

subset1NAP <- subset(replicationNAP, lag.NAP_has_any<1)

#first NAP#
myprobitNAP <- glm(NAP_has_any ~ CEDAW + DNAP_ht_colonial + DNAP_ht_region + DNAP_lp_legor + lag.cgdppc + lag.polity2 + lag.vdem_gender + lag.actotal + time + time_sq,family=binomial(link="probit"), data=subset1NAP)
summary(myprobitNAP)
nobs(myprobitNAP)

mycoxNAP <- coxph(Surv(time,NAP_has_any) ~ CEDAW + DL_ht_colonial + DL_ht_region + DL_lp_legor + lag.cgdppc + lag.polity2 + lag.vdem_gender + cluster(ccode), data=subset1NAP)
summary(mycoxNAP)

write.dta(replicationNAP, file = "subset1NAP.dta")


#repeated NAP#
myprobit2NAP <- glm(NAP_has_any ~ CEDAW + DNAP_ht_colonial + DNAP_ht_region + DNAP_lp_legor + lag.cgdppc + lag.polity2 + lag.vdem_gender + lag.actotal + time + time_sq,family=binomial(link="probit"), data=replicationNAP)
summary(myprobit2NAP)
nobs(myprobit2NAP)

mycox2NAP <- coxph(Surv(time,NAP_has_any) ~ CEDAW + DL_ht_colonial + DL_ht_region + DL_lp_legor + lag.cgdppc + lag.polity2 + lag.vdem_gender, data=replicationNAP)
summary(mycox2NAP)


##Diffusion of NAP after Adoption of Law##
#repeated NAP after adoption of law#
subsetLNAP <- subset(replicationNAP, L_has_any>0)
myprobitLNAP <- glm(NAP_has_any ~ CEDAW + DNAP_ht_colonial + DNAP_ht_region + DNAP_lp_legor + lag.cgdppc + lag.polity2 + lag.vdem_gender + lag.actotal + time + time_sq,family=binomial(link="probit"), data=subsetLNAP)
summary(myprobitLNAP)
nobs(myprobitLNAP)

#first NAP after adoption of law#
subset1LNAP <- subset(subsetLNAP, lag.NAP_has_any<1)
myprobit1LNAP <- glm(NAP_has_any ~ CEDAW + DNAP_ht_colonial + DNAP_ht_region + DNAP_lp_legor + lag.cgdppc + lag.polity2 + lag.vdem_gender + lag.actotal + time + time_sq,family=binomial(link="probit"), data=subset1LNAP)
summary(myprobit1LNAP)
nobs(myprobit1LNAP)
