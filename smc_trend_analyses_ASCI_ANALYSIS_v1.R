library(tidyverse)
library(sf)
library(pwr)

load("data/smc_trends.Rdata")

mydf<-lu_station.df %>%
  filter(probabilistic=="true") %>%
  select(masterid, stationid, latitude, longitude, huc, county, smcshed, smc_lu, probabilistic) %>%
  # group_by(masterid) %>%
  # sample_n(size=1) %>%
  # ungroup() %>%
  
  filter(stationid %in% asci.df$stationcode) %>%
  filter(huc %in% c(401:412, 481, 800:999)) %>%
  
  inner_join(asci.df, by=c("stationid" = "stationcode")) %>%
  # filter(!is.na(csci)) %>%
  mutate(Year=lubridate::year(sampledate), 
         MasterYear=paste(masterid, Year, sep="_")) %>%
  filter(d_numbertaxa >0 & s_numbertaxa >0)

lustations2<-mydf %>%
  select(masterid, latitude, longitude, huc, smcshed, county, smc_lu) %>%
  group_by(masterid) %>%
  sample_n(size=1) %>%
  ungroup()

annual_means<-mydf %>%
  group_by(MasterYear, masterid, Year) %>%
  summarise(ASCI=mean(h_asci)) %>%
  ungroup()


replicated_sites<-annual_means %>%
  group_by(masterid) %>%
  tally(name="n_years") %>%
  filter(n_years>1) %>%
  left_join(lustations2)
# left_join(lu_station.df %>%
#             select(masterid, latitude, longitude, huc, smcshed, county, smc_lu) %>%
#             group_by(masterid) %>%
#             sample_n(size=1) %>%
#             ungroup()
#           )

# annual_means_replicated_sites %>%
#   select(masterid) %>%
#   unique() %>%
#   nrow()
# 

annual_means_replicated_sites<- annual_means %>%
  filter(masterid %in% replicated_sites$masterid) %>%
  left_join(lustations2  ) %>%
  arrange(masterid, Year)


replicated_sites %>%
  group_by(n_years) %>%
  tally()

replicated_sites_plot<-ggplot(data=replicated_sites, aes(x=n_years))+
  geom_histogram(binwidth = 1)+
  scale_x_continuous(breaks=2:10, name="# visits")+
  theme_classic()+
  ylab("# sites")
ggsave(replicated_sites_plot, filename="figures/asci/replicated_sites_plot.jpg", dpi=300, height=2.5, width=2.5)


replicated_sites$EarliestYear<-sapply(replicated_sites$masterid, function(site){
  xdf<-annual_means_replicated_sites %>%
    filter(masterid==site)
  min(xdf$Year)
})

replicated_sites$LatestYear<-sapply(replicated_sites$masterid, function(site){
  xdf<-annual_means_replicated_sites %>%
    filter(masterid==site)
  max(xdf$Year)
})

replicated_sites$PctPassing<-sapply(replicated_sites$masterid, function(site){
  xdf<-annual_means_replicated_sites %>%
    filter(masterid==site)
  xdf$Pass= xdf$ASCI>=0.88
  sum(xdf$Pass)/nrow(xdf)
})

replicated_sites$Timespan= replicated_sites$LatestYear-replicated_sites$EarliestYear
summary(replicated_sites$Timespan)

timespan_plot<-ggplot(data=replicated_sites, aes(x=Timespan))+
  geom_histogram(binwidth = 1)+
  scale_x_continuous(name="# years covered")+
  theme_classic()+
  ylab("# sites")
ggsave(timespan_plot, filename="figures/asci/timespan_plot.jpg", dpi=300, height=2.5, width=2.5)

rep.plot.dat<-replicated_sites %>% 
  # left_join((annual_means_replicated_sites %>% select(masterid, smc_lu) %>% unique())) %>%
  arrange(-EarliestYear, LatestYear) %>%
  mutate(masterid = factor(masterid, levels=masterid)) 
rep_point.plot.dat<-annual_means_replicated_sites %>%  
  select(masterid, ASCI, smc_lu, Year) %>%
  mutate(masterid=factor(masterid, levels=rep.plot.dat$masterid)) %>%
  mutate(Score = case_when(ASCI>=0.95~"Likely intact",
                           ASCI>=0.88~"Possibly altered",
                           ASCI>=0.78~"Likely altered",
                           T~"Very likely altered") %>%
           factor(levels=c("Very likely altered", "Likely altered", "Possibly altered", "Likely intact")))

visit_years_plot<-ggplot(data=rep.plot.dat,
                         aes(x=masterid, y=LatestYear, xend=masterid))+
  geom_point(data=rep_point.plot.dat, aes(y=Year), size=.5)+
  geom_segment(aes(y=EarliestYear, yend=LatestYear, color=smc_lu))+
  geom_point(data=rep_point.plot.dat, aes(y=Year), size=.5)+
  ylab("Year")+xlab("Site")+
  scale_y_continuous(breaks=seq(from=2000, to=2020, by=5))+
  theme_classic()+
  scale_color_viridis_d(name="Land use")+
  theme(legend.position = "bottom",
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank() ) +
  ggtitle("Sites with repeat visits")+
  coord_flip()

ggsave(visit_years_plot, filename="figures/asci/visit_years_plot.jpg", dpi=300, height=6, width=4)

# can you add score? Still working on it 
change_condition_plot<-ggplot(data=rep.plot.dat,
                              aes(x=masterid, y=LatestYear, xend=masterid))+
  geom_point(data=rep_point.plot.dat, aes(y=Year, fill=Score), size=2,shape=21, color="black")+
  geom_segment(aes(y=EarliestYear, yend=LatestYear), color="gray")+
  geom_point(data=rep_point.plot.dat, aes(y=Year, fill=Score), size=2,shape=21, color="black")+
  ylab("Year")+xlab("Site")+
  scale_y_continuous(breaks=seq(from=2000, to=2020, by=5))+
  facet_wrap(~smc_lu, scales="free_y")+
  theme_classic()+
  scale_fill_brewer(palette="RdYlBu")+
  # scale_color_brewer(name="Land use", palette="Greys")+
  theme(legend.position = "bottom",
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank() ) +
  ggtitle("Condition at sites with repeat visits")+
  coord_flip()
ggsave(change_condition_plot, filename="figures/asci/change_condition_plot.jpg", dpi=300, width=12, height=8)

summary(replicated_sites$LatestYear-replicated_sites$EarliestYear)

replicated_sites$EarliestASCI<-sapply(replicated_sites$masterid, function(site){
  xdf<-annual_means_replicated_sites %>%
    filter(masterid==site)
  xdf<-xdf[which.min(xdf$Year),]
  xdf$ASCI
})
replicated_sites$LatestASCI<-sapply(replicated_sites$masterid, function(site){
  xdf<-annual_means_replicated_sites %>%
    filter(masterid==site)
  xdf<-xdf[which.max(xdf$Year),]
  xdf$ASCI
})
replicated_sites$MeanASCI<-sapply(replicated_sites$masterid, function(site){
  xdf<-annual_means_replicated_sites %>%
    filter(masterid==site)
  mean(xdf$ASCI)
})


replicated_sites$MaxASCI<-sapply(replicated_sites$masterid, function(site){
  xdf<-annual_means_replicated_sites %>%
    filter(masterid==site)
  max(xdf$ASCI)
})

replicated_sites$MinASCI<-sapply(replicated_sites$masterid, function(site){
  xdf<-annual_means_replicated_sites %>%
    filter(masterid==site)
  min(xdf$ASCI)
})

replicated_sites$DeltaASCI<-  replicated_sites$LatestASCI - replicated_sites$EarliestASCI
replicated_sites$DeltaYear<- replicated_sites$LatestYear - replicated_sites$EarliestYear
replicated_sites$ASCIoverYear<-replicated_sites$DeltaASCI/replicated_sites$DeltaYear
replicated_sites$RangeASCI<-  replicated_sites$MaxASCI - replicated_sites$MinASCI

summary(replicated_sites$RangeASCI)

# 
# library(beeswarm)
# ggplot(data=replicated_sites, 
#        aes(x=smc_lu, y=PctPassing))+
#   geom_boxplot()+
#   # geom_beeswarm()
#   geom_quasirandom(method = "tukeyDense")
#   # geom_point(position=position_jitter(height=0.01, width=.1))
# 
# # masterid_pctpassing<-replicated_sites$masterid[sort(replicated_sites$PctPassing)]



plot.dat<-replicated_sites %>%
  left_join(gis.df %>%
              select(masterid, 
                     urban_2000_1k, urban_2000_5k, urban_2000_ws,
                     ag_2000_1k, ag_2000_5k, ag_2000_ws,
                     code_21_2000_1k, code_21_2000_5k, code_21_2000_ws))

ggplot(data=plot.dat, aes(x=urban_2000_ws, y=DeltaASCI))+
  geom_point()+
  geom_smooth(method=lm)

ggplot(data=annual_means_replicated_sites %>%
         filter(smc_lu!="SMC_out") %>%
         arrange(masterid, Year), 
       aes(x=Year, y=ASCI))+
  geom_path(alpha=.1, aes(group=masterid))+
  stat_summary(fun=mean, geom="line", color="red")+
  theme_classic()+
  geom_hline(yintercept=0.88)+
  facet_wrap(~smc_lu)+
  coord_cartesian(xlim=c(2009, 2020))



meanscore_vs_year<-ggplot(data=annual_means_replicated_sites %>%
                            filter(smc_lu!="SMC_out") %>%
                            arrange(masterid, Year), 
                          aes(x=Year, y=ASCI))+
  geom_path(alpha=.1, aes(group=masterid))+
  stat_summary(fun=mean, geom="line", aes(color=smc_lu), size=1)+
  # scale_color_brewer(palette="Set1")+
  scale_color_viridis_d(name="Land use")+
  theme_classic()+
  geom_hline(yintercept=0.88,linetype="dashed")+
  # facet_wrap(~smc_lu)+
  coord_cartesian(xlim=c(2009, 2020))+
  theme(legend.position = "bottom")+
  ylab("Mean ASCI score")+
  scale_x_continuous(breaks=seq(from=2000, to=2020, by=5))
ggsave(meanscore_vs_year, filename="figures/asci/meanscore_vs_year.jpg", dpi=300, width=6, height=5)


ggplot(data=annual_means_replicated_sites %>%
         filter(smc_lu!="SMC_out") %>%
         arrange(masterid, Year), 
       aes(x=Year, y=ASCI))+
  geom_path(alpha=.1, aes(group=masterid))+
  stat_summary(fun=mean, geom="line", color="red", size=1)+
  scale_color_brewer(palette="Set1")+
  theme_classic()+
  geom_hline(yintercept=0.88)+
  facet_wrap(~smc_lu)+
  coord_cartesian(xlim=c(2009, 2020))

annual_means_replicated_sites2<-left_join(annual_means_replicated_sites,
                                          replicated_sites %>%
                                            select(masterid, EarliestASCI)) %>%
  mutate(ASCI_Class=case_when(ASCI >=0.95 ~"Class 1",
                              ASCI >=0.88 ~ "Class 2",
                              ASCI >=0.78~ "Class 3",
                              T~"Class 4") %>%
           factor(levels=c("Class 1","Class 2","Class 3", "Class 4")))


ggplot(data=annual_means_replicated_sites2 %>%
         # filter(smc_lu!="SMC_out") %>%
         arrange(masterid, Year), 
       aes(x=Year, y=ASCI-EarliestASCI))+
  geom_path(alpha=.1, aes(group=masterid))+
  stat_summary(fun=mean, geom="line", color="red", size=1)+
  scale_color_brewer(palette="Set1")+
  theme_classic()+
  geom_hline(yintercept=0.88)+
  facet_wrap(~county)+
  coord_cartesian(xlim=c(2009, 2020))


ggplot(data=annual_means_replicated_sites2 %>%
         filter(smc_lu!="SMC_out") %>%
         arrange(masterid, Year), 
       aes(x=Year, y=ASCI-EarliestASCI))+
  geom_path(alpha=.1, aes(group=masterid))+
  # stat_summary(fun=mean, geom="line", aes(color=smc_lu), size=1)+
  geom_smooth(aes(color=smc_lu))+
  scale_color_brewer(palette="Set1")+
  theme_classic()+
  # geom_hline(yintercept=0.88)+
  facet_wrap(~smc_lu)+
  coord_cartesian(xlim=c(2009, 2020))




#####

annual_means_replicated_sites_REGRESSIONS<-
  lapply(replicated_sites$masterid, function(site){
    xdf<-annual_means_replicated_sites %>%
      filter(masterid==site)
    lm(ASCI~Year, data=xdf)
  })


replicated_sites$Slope<- sapply(annual_means_replicated_sites_REGRESSIONS, function(myreg){
  myreg$coefficients[2]
})

replicated_sites$SlopeSE<- sapply(annual_means_replicated_sites_REGRESSIONS, function(myreg){
  mysum<-summary(myreg)
  mysum$coefficients[2,2]
})

replicated_sites$Slope_p<- sapply(annual_means_replicated_sites_REGRESSIONS, function(myreg){
  mysum<-summary(myreg)
  mysum$coefficients[2,4]
})

replicated_sites$Slope_rsq<- sapply(annual_means_replicated_sites_REGRESSIONS, function(myreg){
  mysum<-summary(myreg)
  mysum$r.squared
})

replicated_sites %>%
  filter(n_years>2) %>% select(Slope_rsq) %>%
  summary()

alpha<-0.1
minrsq<-0.2
minpow<-0.5

library(pwr)
replicated_sites$Slope_power<-sapply(annual_means_replicated_sites_REGRESSIONS, function(myreg){
  mysum<-summary(myreg)
  u.i<-mysum$fstatistic[2]
  v.i<-mysum$fstatistic[3]
  f2.i<-(mysum$r.squared)/(1-mysum$r.squared)
  # print(paste(u.i, v.i, f2.i))
  # print(f2.i>0)
  if(f2.i <= 0 | v.i<1)  
    NA
  else
  {
    mypow<-pwr.f2.test(
      u=u.i,
      v=v.i,
      # f2=f2.i,
      f2= (minrsq/(1-minrsq)),
      sig.level=alpha)
    mypow$power
  }
})


#classification rules:
#Increasing if Slope > 0 and Slope_p< 0.1
#Decreasing if Slope < 0 and Slope_p< 0.1


replicated_sites2 <-replicated_sites %>%
  mutate(SlopeClass = case_when(
    n_years<=2~"Indeterminate_low n",
    Slope>0 & Slope_p<alpha ~ "Increasing",
    Slope<0 & Slope_p<alpha ~ "Decreasing",
    Slope_power>minpow~ "Stable",
    Slope_power<=minpow~ "Indeterminate",
    T~"I don't know what happened"),
    RangeClass = case_when(
      n_years<=2~"Indeterminate_low n",
      Slope>0 & Slope_p<alpha ~ "Increasing",
      Slope<0 & Slope_p<alpha ~ "Decreasing",
      RangeASCI <= (0.09*1.96)~"Stable",
      
      T~"Indeterminate"    )
  )

table(replicated_sites2$RangeClass, replicated_sites2$SlopeClass)

replicated_sites2 %>%
  # filter(n_years>2) %>%
  group_by(SlopeClass) %>%
  tally()

replicated_sites2 %>%
  # filter(n_years>2) %>%
  group_by(smc_lu, RangeClass) %>%
  tally() %>% 
  ungroup() %>%
  mutate(x=sum(n))

replicated_sites2 %>%
  filter(SlopeClass=="Decreasing") %>%
  arrange(-Slope) %>%
  select(masterid, Slope, n_years)

mysite<-"SMC00873"
ggplot(data=annual_means_replicated_sites %>%
         filter(masterid==mysite), aes(x=Year, y=ASCI))+
  geom_path() +
  geom_point()+
  coord_cartesian(ylim=c(0.5,1.2))


ggplot(data=annual_means_replicated_sites2 %>%
         filter(masterid %in% replicated_sites2$masterid[replicated_sites2$RangeClass=="Decreasing"]), aes(x=Year, y=ASCI))+
  geom_path() +
  geom_point() + 
  facet_wrap(~masterid) + 
  geom_hline(yintercept=0.88, linetype="dashed")

ggplot(data=annual_means_replicated_sites2 %>%
         filter(masterid %in% replicated_sites2$masterid[replicated_sites2$RangeClass=="Increasing"]), aes(x=Year, y=ASCI))+
  geom_path() +
  geom_point() + 
  facet_wrap(~masterid) + 
  geom_hline(yintercept=0.88, linetype="dashed")


plot.dat2<-annual_means_replicated_sites2 %>%
  left_join(replicated_sites2 %>% select(masterid, Slope, RangeClass)) %>%
  filter(RangeClass %in% c("Increasing","Decreasing"))

ggplot(plot.dat2 , aes(x=Year, y=ASCI))+
  geom_path() +
  geom_point() + 
  facet_wrap(~masterid) + 
  geom_hline(yintercept=0.88, linetype="dashed")

changing_sites_plot<-ggplot(plot.dat2 , aes(x=Year, y=ASCI))+
  geom_path() +
  geom_point() + 
  facet_wrap(RangeClass~masterid, ncol=4) + 
  geom_hline(yintercept=0.88, linetype="dashed") + 
  theme_bw()
ggsave(changing_sites_plot, filename="figures/asci/changing_sites_plot.jpg", dpi=300, height=6, width=12)


replicated_sites2 %>%
  filter(n_years>2) %>%
  group_by(RangeClass, smc_lu) %>%
  tally() %>%
  pivot_wider(names_from=smc_lu, values_from = n)


replicated_sites2 %>%
  filter(n_years>2) %>%
  group_by(RangeClass) %>%
  tally() 


plot.dat<-annual_means_replicated_sites %>%
  left_join(replicated_sites2) %>%
  arrange(masterid, Year)

trends_plot<-ggplot(data=plot.dat, aes(x=Year, y=ASCI))+
  geom_path(alpha=.1, aes(group=masterid))+
  geom_path(data=plot.dat %>% filter(RangeClass %in% c("Increasing", "Decreasing","Stable")), aes(group=masterid, color=RangeClass), size=1)+
  geom_path(data=plot.dat %>% filter(RangeClass %in% c("Increasing", "Decreasing")), aes(group=masterid, color=RangeClass), size=1)+
  geom_path(data=plot.dat %>% filter(RangeClass %in% c("Increasing")), aes(group=masterid, color=RangeClass), size=1)+
  # stat_summary(fun=mean, geom="line", aes(color=smc_lu), size=1)+
  # scale_color_brewer(palette="Set1", name="Trajectory")+
  # scale_color_viridis_d(name="Land use")+
  scale_color_manual(values=c("#fc8d59","#74add1","#a1d99b"), name="Trajectory")+
  theme_classic()+
  geom_hline(yintercept=0.88,linetype="dashed")+
  facet_wrap(~smc_lu)+
  coord_cartesian(xlim=c(2009, 2020))+
  theme(legend.position = "bottom")+
  ylab("Mean ASCI score")+
  ggtitle("High-confidence trajectories at revisited sites")+
  scale_x_continuous(breaks=seq(from=2000, to=2020, by=5))
ggsave(trends_plot, filename="figures/asci/trends_plot.jpg", dpi=300, height=6, width=12)

plot.dat %>%
  filter(smc_lu=="Open" & RangeClass=="Decreasing") %>%
  as.data.frame()

ggsave(meanscore_vs_year, filename="figures/asci/meanscore_vs_year.jpg", dpi=300, width=6, height=5)
######
library(sf)
smc_sheds_sf<-st_read("data/SMCSheds2009/SMCSheds2009.shp") %>%
  st_transform(crs=4326)

replicated_sites_sf<-replicated_sites2 %>%
  st_as_sf(coords = c("longitude", "latitude"), # can use numbers here too
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326)

map_effort<-ggplot()+
  geom_sf(data=smc_sheds_sf)+
  geom_sf(data=replicated_sites_sf, aes(color=as.factor(n_years)))+
  geom_sf(data=replicated_sites_sf %>% filter(n_years>2), aes(color=as.factor(n_years)), size=2)+
  scale_color_viridis_d()

ggsave(map_effort, filename="figures/asci/map_effort.jpg", dpi=300, width=12, height=6)

map_classification<-ggplot()+
  geom_sf(data=smc_sheds_sf)+
  geom_sf(data=replicated_sites_sf, color="gray")+
  geom_sf(data=replicated_sites_sf %>% filter(RangeClass %in% c("Stable","Increasing","Decreasing")), aes(color=RangeClass), size=2)+
  scale_color_viridis_d(name="Classification")

ggsave(map_classification, filename="figures/asci/map_classification.jpg", dpi=300, height=6, width=12)


changing_sites<-replicated_sites2 %>%
  filter(RangeClass %in% c("Increasing","Decreasing")) %>%
  select(masterid, latitude, longitude, smc_lu, county,
         Class=RangeClass,n_years,
         Slope, Slope_p, MaxASCI, MinASCI, MeanASCI) %>%
  arrange(Class, masterid) 
# write.table(file="clipboard", sep="\t", row.names=F)
write.csv(changing_sites, file="asci_changing_sites.csv", row.names=F)


stacked_dat<-crossing(smc_lu=replicated_sites2$smc_lu %>% unique(),
                      RangeClass=replicated_sites2$RangeClass %>% unique())
stacked_dat$n_total<-sapply(1:nrow(stacked_dat),function(i){
  class.i<-stacked_dat$RangeClass[i]
  lu.i<-stacked_dat$smc_lu[i]
  xdf<-replicated_sites2 %>%
    filter(smc_lu==lu.i )
  nrow(xdf)
})

stacked_dat$n_total_high<-  sapply(1:nrow(stacked_dat),function(i){
  class.i<-stacked_dat$RangeClass[i]
  lu.i<-stacked_dat$smc_lu[i]
  xdf<-replicated_sites2 %>%
    filter(smc_lu==lu.i & RangeClass!="Indeterminate_low n")
  nrow(xdf)
})

stacked_dat$n_sites<-sapply(1:nrow(stacked_dat),function(i){
  class.i<-stacked_dat$RangeClass[i]
  lu.i<-stacked_dat$smc_lu[i]
  xdf<-replicated_sites2 %>%
    filter(smc_lu==lu.i & RangeClass==class.i)
  nrow(xdf)
})

stacked_dat$pct_total<-stacked_dat$n_sites/stacked_dat$n_total
stacked_dat$pct_total_highn<-stacked_dat$n_sites/stacked_dat$n_total_high

stacked_dat$RangeClass<-factor(stacked_dat$RangeClass,
                               levels= rev(c("Indeterminate_low n",
                                             "Indeterminate",
                                             "Decreasing",
                                             "Stable",
                                             "Increasing")))
# 
# ggplot(data=stacked_dat, 
#        aes(x=smc_lu,
#            y=pct_total))+
#   geom_bar(aes(fill=RangeClass), stat="identity", position=position_stack())

stacked_plot<-ggplot(data=stacked_dat %>% 
                       # filter(RangeClass!="Indeterminate_low n"), 
                       filter(RangeClass %in% c("Increasing","Decreasing","Stable")),
                     aes(x=smc_lu,
                         y=pct_total_highn))+
  geom_bar(aes(fill=RangeClass), stat="identity", position=position_stack(), color="gray25")+
  ggtitle("Trends in ASCI scores")+
  # scale_fill_viridis_d(name="ASCI trend is:")+
  scale_fill_brewer(palette="RdYlBu", name="ASCI trend:", direction= -1)+
  theme_classic()+
  theme(legend.position="bottom")+
  xlab("")+
  scale_y_continuous(name="Proportion of sites visited\n3 or more times",
                     breaks=seq(from=0, to=1, by =0.25), limits=c(0,1))
ggsave(stacked_plot, filename="figures/asci/stacked_plot.jpg", dpi=300, height=5, width=6)


#####
#Categorical analyses

#rank-change of sites
site_list_change<-replicated_sites2 %>%
  arrange(Slope) %>%
  select(masterid)

plot_dat_catchange<-annual_means_replicated_sites2 %>% 
  left_join(replicated_sites2 %>% 
              select(masterid, EarliestYear, LatestYear, MeanASCI, LatestASCI, DeltaASCI, Slope,PctPassing)) %>%
  mutate(Class12 = case_when(ASCI>=0.88~"Above",T~"Below")) %>%
  arrange(PctPassing, LatestASCI) %>%
  group_by(masterid) %>%
  mutate(RankYear=rank(Year)) %>%
  ungroup() %>%
  mutate(mid2 = factor(masterid, levels=masterid %>% unique()))
plot_dat_catchange$ASCI_fromPrevious<-sapply(1:nrow(plot_dat_catchange), function(i){
  year.i=plot_dat_catchange$RankYear[i]
  
  if(year.i==1)
    NA
  else
  {
    csci.now=plot_dat_catchange$ASCI[i]
    csci.prev=plot_dat_catchange$ASCI[i-1]
    ifelse(csci.now>csci.prev,"Higher","Lower")
  }
})

plot_dat_catchange$ASCI_DifffromPrevious<-sapply(1:nrow(plot_dat_catchange), function(i){
  year.i=plot_dat_catchange$RankYear[i]
  
  if(year.i==1)
    NA
  else
  {
    csci.now=plot_dat_catchange$ASCI[i]
    csci.prev=plot_dat_catchange$ASCI[i-1]
    csci.now-csci.prev
  }
})



catchange_plot<-ggplot(data=plot_dat_catchange,
                       aes(x=mid2, y=RankYear))+
  # geom_point(aes(color=Class12), shape=15)+
  geom_tile(aes(fill=Class12), color="white")+
  scale_y_reverse("Sampling event", breaks=(1:9), labels=c("Most\nrecent",rep("",7), "Least\nrecent"))+
  # scale_y_discrete("Sampling event", breaks=(1:9))+
  xlab("site")+
  # scale_color_viridis_d()+
  scale_fill_manual(values=c("#91bfdb","#fc8d59"), name="Condition",
                    # labels=c("Above 0.88","Below 0.88"))+
                    labels=c("Passing","Failing"))+
  theme_classic()+
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        # panel.grid.major.y = element_line(color="gray90"),
        legend.position = "bottom"
        # panel.grid.major = element_line(color="gray90")
  ) +
  facet_wrap(~smc_lu, scales="free_y")+
  coord_flip()
ggsave(catchange_plot, filename="figures/asci/catchange_plot.jpg", dpi=300, height=7, width=5)

#Plot of pass/fail:
catchange_plot_pf<-ggplot(data=plot_dat_catchange,
                          aes(x=mid2, y=Year))+
  # geom_point(aes(color=Class12), shape=15)+
  geom_tile(aes(fill=Class12), color="white")+
  # scale_y_reverse("Sampling event", breaks=(1:9), labels=c("Most\nrecent",rep("",7), "Least\nrecent"))+
  # scale_y_discrete("Sampling event", breaks=(1:9))+
  xlab("site")+
  # scale_color_viridis_d()+
  scale_fill_manual(values=c("#91bfdb","#fc8d59"), name="Condition",
                    # labels=c("Above 0.88","Below 0.88"))+
                    labels=c("Passing","Failing"))+
  theme_classic()+
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        # panel.grid.major.y = element_line(color="gray90"),
        legend.position = "bottom"
        # panel.grid.major = element_line(color="gray90")
  ) +
  facet_wrap(~smc_lu, scales="free_y")+
  coord_flip()
ggsave(catchange_plot_pf, filename="figures/asci/catchange_plot_pf.jpg", dpi=300, height=7, width=5)

#Plot of raw score
catchange_plot_raw<-ggplot(data=plot_dat_catchange,
                           aes(x=mid2, y=Year))+
  # geom_point(aes(color=Class12), shape=15)+
  geom_tile(aes(fill=ASCI), color="white")+
  # scale_y_reverse("Sampling event", breaks=(1:9), labels=c("Most\nrecent",rep("",7), "Least\nrecent"))+
  # scale_y_discrete("Sampling event", breaks=(1:9))+
  xlab("site")+
  # scale_color_viridis_d()+
  scale_fill_gradient2(high="#2c7bb6",low="#d7191c", mid="#ffffbf", midpoint=0.88, name="ASCI score"
                       # labels=c("Above 0.88","Below 0.88"))
  )+
  theme_classic()+
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        # panel.grid.major.y = element_line(color="gray90"),
        legend.position = "bottom"
        # panel.grid.major = element_line(color="gray90")
  ) +
  facet_wrap(~smc_lu, scales="free_y")+
  coord_flip()
ggsave(catchange_plot_raw, filename="figures/asci/catchange_plot_raw.jpg", dpi=300, height=7, width=5)



#Plot of raw chnage from previous year
catchange_plot_changefromprevious<-ggplot(data=plot_dat_catchange,
                                          aes(x=mid2, y=Year))+
  # geom_point(aes(color=Class12), shape=15)+
  geom_tile(aes(fill=ASCI_DifffromPrevious), color="white")+
  # scale_y_reverse("Sampling event", breaks=(1:9), labels=c("Most\nrecent",rep("",7), "Least\nrecent"))+
  # scale_y_discrete("Sampling event", breaks=(1:9))+
  xlab("site")+
  # scale_color_viridis_d()+
  scale_fill_gradient2(high="#2c7bb6",low="#d7191c", mid="#ffffbf", midpoint=0, name="Change from\nprevious sample",
                       na.value = "gray75", breaks=c(-.25, 0,.25)
                       # labels=c("Above 0.88","Below 0.88"))
  )+
  theme_classic()+
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        # panel.grid.major.y = element_line(color="gray90"),
        legend.position = "bottom"
        # panel.grid.major = element_line(color="gray90")
  ) +
  facet_wrap(~smc_lu, scales="free_y")+
  coord_flip()
ggsave(catchange_plot_changefromprevious, filename="figures/asci/catchange_plot_changefromprevious.jpg", dpi=300, height=7, width=5)


#Plot of class chnage from previous year
catchange_plot_changefromprevious_class<-ggplot(data=plot_dat_catchange,
                                                aes(x=mid2, y=Year))+
  # geom_point(aes(color=Class12), shape=15)+
  geom_tile(aes(fill=ASCI_fromPrevious), color="white")+
  # scale_y_reverse("Sampling event", breaks=(1:9), labels=c("Most\nrecent",rep("",7), "Least\nrecent"))+
  # scale_y_discrete("Sampling event", breaks=(1:9))+
  xlab("site")+
  # scale_color_viridis_d()+
  scale_fill_manual(values=c("#91bfdb","#fc8d59"), name="Change from\nprevious sampling",na.value="gray75",
                    # labels=c("Above 0.88","Below 0.88"))+
                    labels=c("Higher","Lower","First event")
  )+
  theme_classic()+
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        # panel.grid.major.y = element_line(color="gray90"),
        legend.position = "bottom"
        # panel.grid.major = element_line(color="gray90")
  ) +
  facet_wrap(~smc_lu, scales="free_y")+
  coord_flip()
ggsave(catchange_plot_changefromprevious_class, filename="figures/asci/catchange_plot_changefromprevious_class.jpg", dpi=300, height=7, width=5)