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
  
  filter(stationid %in% csci.df$stationcode) %>%
  filter(huc %in% c(401:412, 481, 800:999)) %>%
  
  inner_join(csci.df, by=c("stationid" = "stationcode")) %>%
  filter(!is.na(csci)) %>%
  mutate(Year=sampleyear, 
         MasterYear=paste(masterid, Year, sep="_")) %>%
  filter(count > 250 & pcnt_ambiguous_taxa<50 & pcnt_ambiguous_individuals<50)

lustations2<-mydf %>%
  select(masterid, latitude, longitude, huc, smcshed, county, smc_lu) %>%
  group_by(masterid) %>%
  sample_n(size=1) %>%
  ungroup()

annual_means<-mydf %>%
  group_by(MasterYear, masterid, Year) %>%
  summarise(CSCI=mean(csci)) %>%
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
ggsave(replicated_sites_plot, filename="figures/replicated_sites_plot.jpg", dpi=300, height=2.5, width=2.5)

  
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
  xdf$Pass= xdf$CSCI>=0.79
  sum(xdf$Pass)/nrow(xdf)
})

replicated_sites$Timespan= replicated_sites$LatestYear-replicated_sites$EarliestYear
summary(replicated_sites$Timespan)

timespan_plot<-ggplot(data=replicated_sites, aes(x=Timespan))+
  geom_histogram(binwidth = 1)+
  scale_x_continuous(name="# years covered")+
  theme_classic()+
  ylab("# sites")
ggsave(timespan_plot, filename="figures/timespan_plot.jpg", dpi=300, height=2.5, width=2.5)

rep.plot.dat<-replicated_sites %>% 
  # left_join((annual_means_replicated_sites %>% select(masterid, smc_lu) %>% unique())) %>%
  arrange(-EarliestYear, LatestYear) %>%
  mutate(masterid = factor(masterid, levels=masterid)) 
rep_point.plot.dat<-annual_means_replicated_sites %>%  
  select(masterid, CSCI, smc_lu, Year) %>%
  mutate(masterid=factor(masterid, levels=rep.plot.dat$masterid)) %>%
  mutate(Score = case_when(CSCI>=0.92~"Likely intact",
                           CSCI>=0.79~"Possibly altered",
                           CSCI>=0.63~"Likely altered",
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

ggsave(visit_years_plot, filename="figures/visit_years_plot.jpg", dpi=300, height=6, width=4)

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
ggsave(change_condition_plot, filename="figures/change_condition_plot.jpg", dpi=300, width=12, height=8)

summary(replicated_sites$LatestYear-replicated_sites$EarliestYear)

replicated_sites$EarliestCSCI<-sapply(replicated_sites$masterid, function(site){
  xdf<-annual_means_replicated_sites %>%
    filter(masterid==site)
  xdf<-xdf[which.min(xdf$Year),]
  xdf$CSCI
})
replicated_sites$LatestCSCI<-sapply(replicated_sites$masterid, function(site){
  xdf<-annual_means_replicated_sites %>%
    filter(masterid==site)
  xdf<-xdf[which.max(xdf$Year),]
  xdf$CSCI
})
replicated_sites$MeanCSCI<-sapply(replicated_sites$masterid, function(site){
  xdf<-annual_means_replicated_sites %>%
    filter(masterid==site)
  mean(xdf$CSCI)
})


replicated_sites$MaxCSCI<-sapply(replicated_sites$masterid, function(site){
  xdf<-annual_means_replicated_sites %>%
    filter(masterid==site)
  max(xdf$CSCI)
})

replicated_sites$MinCSCI<-sapply(replicated_sites$masterid, function(site){
  xdf<-annual_means_replicated_sites %>%
    filter(masterid==site)
  min(xdf$CSCI)
})

replicated_sites$DeltaCSCI<-  replicated_sites$LatestCSCI - replicated_sites$EarliestCSCI
replicated_sites$DeltaYear<- replicated_sites$LatestYear - replicated_sites$EarliestYear
replicated_sites$CSCIoverYear<-replicated_sites$DeltaCSCI/replicated_sites$DeltaYear
replicated_sites$RangeCSCI<-  replicated_sites$MaxCSCI - replicated_sites$MinCSCI

summary(replicated_sites$RangeCSCI)

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

ggplot(data=plot.dat, aes(x=urban_2000_ws, y=DeltaCSCI))+
  geom_point()+
  geom_smooth(method=lm)

ggplot(data=annual_means_replicated_sites %>%
         # filter(smc_lu!="SMC_out") %>%
         arrange(masterid, Year), 
       aes(x=Year, y=CSCI))+
  geom_path(alpha=.1, aes(group=masterid))+
  stat_summary(fun=mean, geom="line", color="red")+
  theme_classic()+
  geom_hline(yintercept=0.79)+
  facet_wrap(~smc_lu)+
  coord_cartesian(xlim=c(2009, 2020))



meanscore_vs_year<-ggplot(data=annual_means_replicated_sites %>%
         # filter(smc_lu!="SMC_out") %>%
         arrange(masterid, Year), 
       aes(x=Year, y=CSCI))+
  geom_path(alpha=.1, aes(group=masterid))+
  stat_summary(fun=mean, geom="line", aes(color=smc_lu), size=1)+
  # scale_color_brewer(palette="Set1")+
  scale_color_viridis_d(name="Land use")+
  theme_classic()+
  geom_hline(yintercept=0.79,linetype="dashed")+
  # facet_wrap(~smc_lu)+
  coord_cartesian(xlim=c(2009, 2020))+
  theme(legend.position = "bottom")+
  ylab("Mean CSCI score")+
  scale_x_continuous(breaks=seq(from=2000, to=2020, by=5))
ggsave(meanscore_vs_year, filename="figures/meanscore_vs_year.jpg", dpi=300, width=6, height=5)


ggplot(data=annual_means_replicated_sites %>%
         filter(smc_lu!="SMC_out") %>%
         arrange(masterid, Year), 
       aes(x=Year, y=CSCI))+
  geom_path(alpha=.1, aes(group=masterid))+
  stat_summary(fun=mean, geom="line", color="red", size=1)+
  scale_color_brewer(palette="Set1")+
  theme_classic()+
  geom_hline(yintercept=0.79)+
  facet_wrap(~smc_lu)+
  coord_cartesian(xlim=c(2009, 2020))

annual_means_replicated_sites2<-left_join(annual_means_replicated_sites,
                                         replicated_sites %>%
                                           select(masterid, EarliestCSCI)) %>%
  mutate(CSCI_Class=case_when(CSCI >=0.92 ~"Class 1",
                        CSCI >=0.79 ~ "Class 2",
                        CSCI >=0.63~ "Class 3",
                        T~"Class 4") %>%
           factor(levels=c("Class 1","Class 2","Class 3", "Class 4")))


ggplot(data=annual_means_replicated_sites2 %>%
         # filter(smc_lu!="SMC_out") %>%
         arrange(masterid, Year), 
       aes(x=Year, y=CSCI-EarliestCSCI))+
  geom_path(alpha=.1, aes(group=masterid))+
  stat_summary(fun=mean, geom="line", color="red", size=1)+
  scale_color_brewer(palette="Set1")+
  theme_classic()+
  geom_hline(yintercept=0.79)+
  facet_wrap(~county)+
  coord_cartesian(xlim=c(2009, 2020))


ggplot(data=annual_means_replicated_sites2 %>%
         filter(smc_lu!="SMC_out") %>%
         arrange(masterid, Year), 
       aes(x=Year, y=CSCI-EarliestCSCI))+
  geom_path(alpha=.1, aes(group=masterid))+
  # stat_summary(fun=mean, geom="line", aes(color=smc_lu), size=1)+
  geom_smooth(aes(color=smc_lu))+
  scale_color_brewer(palette="Set1")+
  theme_classic()+
  # geom_hline(yintercept=0.79)+
  facet_wrap(~smc_lu)+
  coord_cartesian(xlim=c(2009, 2020))




#####

annual_means_replicated_sites_REGRESSIONS<-
  lapply(replicated_sites$masterid, function(site){
    xdf<-annual_means_replicated_sites %>%
      filter(masterid==site)
    lm(CSCI~Year, data=xdf)
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
      RangeCSCI <= (0.11*1.96)~"Stable",
      
      T~"Indeterminate"    )
    )

table(replicated_sites2$RangeClass, replicated_sites2$SlopeClass)

replicated_sites2 %>%
  filter(n_years>2) %>%
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

mysite<-"412WE0552"
ggplot(data=annual_means_replicated_sites %>%
         filter(masterid==mysite), aes(x=Year, y=CSCI))+
  geom_path() +
  geom_point()


ggplot(data=annual_means_replicated_sites2 %>%
         filter(masterid %in% replicated_sites2$masterid[replicated_sites2$RangeClass=="Decreasing"]), aes(x=Year, y=CSCI))+
  geom_path() +
  geom_point() + 
  facet_wrap(~masterid) + 
  geom_hline(yintercept=0.79, linetype="dashed")

ggplot(data=annual_means_replicated_sites2 %>%
         filter(masterid %in% replicated_sites2$masterid[replicated_sites2$RangeClass=="Increasing"]), aes(x=Year, y=CSCI))+
  geom_path() +
  geom_point() + 
  facet_wrap(~masterid) + 
  geom_hline(yintercept=0.79, linetype="dashed")


plot.dat2<-annual_means_replicated_sites2 %>%
  left_join(replicated_sites2 %>% select(masterid, Slope, RangeClass)) %>%
  filter(RangeClass %in% c("Increasing","Decreasing"))

ggplot(plot.dat2 , aes(x=Year, y=CSCI))+
  geom_path() +
  geom_point() + 
  facet_wrap(~masterid) + 
  geom_hline(yintercept=0.79, linetype="dashed")

changing_sites_plot<-ggplot(plot.dat2 , aes(x=Year, y=CSCI))+
  geom_path() +
  geom_point() + 
  facet_wrap(RangeClass~masterid, ncol=4) + 
  geom_hline(yintercept=0.79, linetype="dashed") + 
  theme_bw()
ggsave(changing_sites_plot, filename="figures/changing_sites_plot.jpg", dpi=300, height=6, width=12)


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

trends_plot<-ggplot(data=plot.dat, aes(x=Year, y=CSCI))+
  geom_path(alpha=.1, aes(group=masterid))+
  geom_path(data=plot.dat %>% filter(RangeClass %in% c("Increasing", "Decreasing","Stable")), aes(group=masterid, color=RangeClass), size=1)+
  geom_path(data=plot.dat %>% filter(RangeClass %in% c("Increasing", "Decreasing")), aes(group=masterid, color=RangeClass), size=1)+
  geom_path(data=plot.dat %>% filter(RangeClass %in% c("Increasing")), aes(group=masterid, color=RangeClass), size=1)+
  # stat_summary(fun=mean, geom="line", aes(color=smc_lu), size=1)+
  # scale_color_brewer(palette="Set1", name="Trajectory")+
  # scale_color_viridis_d(name="Land use")+
  scale_color_manual(values=c("#fc8d59","#74add1","#a1d99b"), name="Trajectory")+
  theme_classic()+
  geom_hline(yintercept=0.79,linetype="dashed")+
  facet_wrap(~smc_lu)+
  coord_cartesian(xlim=c(2009, 2020))+
  theme(legend.position = "bottom")+
  ylab("Mean CSCI score")+
  ggtitle("High-confidence trajectories at revisited sites")+
  scale_x_continuous(breaks=seq(from=2000, to=2020, by=5))
ggsave(trends_plot, filename="figures/trends_plot.jpg", dpi=300, height=6, width=12)

plot.dat %>%
  filter(smc_lu=="Open" & RangeClass=="Decreasing") %>%
  as.data.frame()

ggsave(meanscore_vs_year, filename="figures/meanscore_vs_year.jpg", dpi=300, width=6, height=5)
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

ggsave(map_effort, filename="figures/map_effort.jpg", dpi=300, width=12, height=6)

replicated_sites_sf2<-replicated_sites_sf %>%
  mutate(Class2 = case_when(RangeClass %in% c("Indeterminate","Indeterminate_low n") ~ "Indeterminate",
                            T~RangeClass))
replicated_sites_sf2$Class2<-factor(replicated_sites_sf2$Class2, levels=c("Indeterminate","Decreasing","Stable","Increasing"))
map_classification<-
  ggplot()+
  geom_sf(data=smc_sheds_sf)+
  # geom_sf(data=replicated_sites_sf, color="gray")+
  geom_sf(data=replicated_sites_sf2, aes(color=Class2), size=2)+
  scale_color_manual(values=c("gray", viridisLite::viridis(3, option="D") ), name="Change in\nCSCI scores")+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text=element_blank(),
        panel.grid = element_blank())
# scale_color_viridis_d(name="Classification")

ggsave(map_classification, filename="figures/map_classification.jpg", dpi=300, height=6, width=6)


changing_sites<-replicated_sites2 %>%
  filter(RangeClass %in% c("Increasing","Decreasing")) %>%
  select(masterid, latitude, longitude, smc_lu, county,
         Class=RangeClass,n_years,
         Slope, Slope_p, MaxCSCI, MinCSCI, MeanCSCI) %>%
  arrange(Class, masterid) 
  # write.table(file="clipboard", sep="\t", row.names=F)
write.csv(changing_sites, file="changing_sites.csv", row.names=F)


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
replicated_sites2%>%
  group_by(smc_lu) %>%
  tally()

stacked_dat$lu<-case_when(stacked_dat$smc_lu=="Agricultural"~"Agricultural (25)",
                          stacked_dat$smc_lu=="Open"~"Open (107)",
                          stacked_dat$smc_lu=="Urban"~"Urban (67)",
                          T~"x")

# 
# ggplot(data=stacked_dat, 
#        aes(x=smc_lu,
#            y=pct_total))+
#   geom_bar(aes(fill=RangeClass), stat="identity", position=position_stack())

stacked_plot<-ggplot(data=stacked_dat %>% 
         # filter(RangeClass!="Indeterminate_low n"), 
         filter(RangeClass %in% c("Increasing","Decreasing","Stable")),
       aes(x=lu,
           y=pct_total_highn))+
  geom_bar(aes(fill=RangeClass), stat="identity", position=position_stack(), color="gray25")+
  ggtitle("Trends in CSCI scores")+
  # scale_fill_viridis_d(name="CSCI trend is:")+
  scale_fill_brewer(palette="RdYlBu", name="CSCI trend:", direction= -1)+
  theme_classic(base_size = 6)+
  theme(legend.position="bottom")+
  xlab("")+
  scale_y_continuous(name="Proportion of sites visited\n3 or more times",
                     breaks=seq(from=0, to=1, by =0.25), limits=c(0,1))
ggsave(stacked_plot, filename="figures/stacked_plot.jpg", dpi=300, height=3, width=3)


#####
#Categorical analyses

#rank-change of sites
site_list_change<-replicated_sites2 %>%
  arrange(Slope) %>%
  select(masterid)

plot_dat_catchange<-annual_means_replicated_sites2 %>% 
  left_join(replicated_sites2 %>% 
              select(masterid, EarliestYear, LatestYear, MeanCSCI, LatestCSCI, DeltaCSCI, Slope,PctPassing)) %>%
  mutate(Class12 = case_when(CSCI>=0.79~"Above",T~"Below")) %>%
  arrange(PctPassing, LatestCSCI) %>%
  group_by(masterid) %>%
  mutate(RankYear=rank(Year)) %>%
  ungroup() %>%
  mutate(mid2 = factor(masterid, levels=masterid %>% unique()))
plot_dat_catchange$CSCI_fromPrevious<-sapply(1:nrow(plot_dat_catchange), function(i){
  year.i=plot_dat_catchange$RankYear[i]
  
  if(year.i==1)
    NA
  else
  {
   csci.now=plot_dat_catchange$CSCI[i]
   csci.prev=plot_dat_catchange$CSCI[i-1]
   ifelse(csci.now>csci.prev,"Higher","Lower")
   }
})

plot_dat_catchange$CSCI_DifffromPrevious<-sapply(1:nrow(plot_dat_catchange), function(i){
  year.i=plot_dat_catchange$RankYear[i]
  
  if(year.i==1)
    NA
  else
  {
    csci.now=plot_dat_catchange$CSCI[i]
    csci.prev=plot_dat_catchange$CSCI[i-1]
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
                     # labels=c("Above 0.79","Below 0.79"))+
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
ggsave(catchange_plot, filename="figures/catchange_plot.jpg", dpi=300, height=7, width=5)

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
                    # labels=c("Above 0.79","Below 0.79"))+
                    labels=c("Passing","Failing"))+
  theme_classic()+
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1),
        # panel.grid.major.y = element_line(color="gray90"),
        legend.position = "bottom"
        # panel.grid.major = element_line(color="gray90")
  ) +
  facet_wrap(~smc_lu, scales="free_y")+
  scale_y_continuous(breaks=c(2000,2010,2020))+
  coord_flip()
# ggsave(catchange_plot_pf, filename="figures/catchange_plot_pf.jpg", dpi=300, height=7, width=5)
ggsave(catchange_plot_pf, filename="figures/catchange_plot_pf.jpg", dpi=300, height=4, width=3)

plot_dat_catchange$smc_lu2<-factor(plot_dat_catchange$smc_lu, levels=c("Open","Agricultural","Urban"))
levels(plot_dat_catchange$smc_lu2)<-c("Open\nsites","Agricultural\nsites", "Urban\nsites")

blank_tiles<-crossing(plot_dat_catchange %>% select(mid2, smc_lu2),
                      Year=2000:2020)

class_plot_pf<-ggplot(data=plot_dat_catchange,
                          aes(x=mid2, y=Year))+
  # geom_point(aes(color=Class12), shape=15)+
  geom_tile(data=blank_tiles, fill="white", color="gray80")+
  geom_tile(aes(fill=CSCI_Class))+
  # scale_y_reverse("Sampling event", breaks=(1:9), labels=c("Most\nrecent",rep("",7), "Least\nrecent"))+
  # scale_y_discrete("Sampling event", breaks=(1:9))+
  xlab("")+ylab("")+
  # scale_color_viridis_d()+
  scale_fill_brewer(palette="RdYlBu", direction=-1, name="CSCI\nCondition Class",
                    labels=c("Likely\nintact","Possibly\naltered","Likely\naltered","Very likely\naltered"))+
  # theme_classic(base_size = 12)+
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        # axis.text.x = element_text(angle=45, hjust=1),
        # panel.grid.major.y = element_line(color="gray90"),
        legend.position = "bottom",
        legend.title = element_text(size=7),
        legend.text = element_text(size=7),
        strip.text = element_text(size=10),
        strip.background = element_blank(),
        panel.border = element_rect(color="black", fill=NA),
        panel.grid = element_blank(), 
        panel.background = element_blank(),
        strip.text.y.left = element_text(angle = 0)
        # panel.grid.major = element_line(color="gray90")
  ) +
  # facet_wrap(~smc_lu, scales="free_y")+
  facet_grid(smc_lu2~., scales="free_y", space="free", switch="y")+
  scale_y_continuous(breaks=c(2000,2010,2020))+
  coord_flip()
# ggsave(catchange_plot_pf, filename="figures/catchange_plot_pf.jpg", dpi=300, height=7, width=5)
ggsave(class_plot_pf, filename="figures/class_plot_pf.jpg", dpi=300, height=8, width=4.5)


#Plot of raw score
catchange_plot_raw<-ggplot(data=plot_dat_catchange,
       aes(x=mid2, y=Year))+
  # geom_point(aes(color=Class12), shape=15)+
  geom_tile(aes(fill=CSCI), color="white")+
  # scale_y_reverse("Sampling event", breaks=(1:9), labels=c("Most\nrecent",rep("",7), "Least\nrecent"))+
  # scale_y_discrete("Sampling event", breaks=(1:9))+
  xlab("site")+
  # scale_color_viridis_d()+
  scale_fill_gradient2(high="#2c7bb6",low="#d7191c", mid="#ffffbf", midpoint=0.79, name=""
                    # labels=c("Above 0.79","Below 0.79"))
                    )+
  theme_classic()+
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1),
        # panel.grid.major.y = element_line(color="gray90"),
        legend.position = "bottom"
        # panel.grid.major = element_line(color="gray90")
  ) +
  ggtitle("CSCI score")+
  facet_wrap(~smc_lu, scales="free_y")+
  scale_y_continuous(breaks=c(2000,2010,2020))+
  coord_flip()
# ggsave(catchange_plot_raw, filename="figures/catchange_plot_raw.jpg", dpi=300, height=7, width=5)
ggsave(catchange_plot_raw,
       filename="figures/catchange_plot_raw.jpg", dpi=300, height=4, width=3)


#Plot of raw chnage from previous year
catchange_plot_changefromprevious<-ggplot(data=plot_dat_catchange,
                           aes(x=mid2, y=Year))+
  # geom_point(aes(color=Class12), shape=15)+
  geom_tile(aes(fill=CSCI_DifffromPrevious), color="white")+
  # scale_y_reverse("Sampling event", breaks=(1:9), labels=c("Most\nrecent",rep("",7), "Least\nrecent"))+
  # scale_y_discrete("Sampling event", breaks=(1:9))+
  xlab("site")+
  # scale_color_viridis_d()+
  scale_fill_gradient2(high="#2c7bb6",low="#d7191c", mid="#ffffbf", midpoint=0, name="Change from\nprevious sample",
                       na.value = "gray75", breaks=c(-.5, 0,.5)
                       # labels=c("Above 0.79","Below 0.79"))
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
ggsave(catchange_plot_changefromprevious, filename="figures/catchange_plot_changefromprevious.jpg", dpi=300, height=7, width=5)


#Plot of class chnage from previous year
catchange_plot_changefromprevious_class<-ggplot(data=plot_dat_catchange,
                                          aes(x=mid2, y=Year))+
  # geom_point(aes(color=Class12), shape=15)+
  geom_tile(aes(fill=CSCI_fromPrevious), color="white")+
  # scale_y_reverse("Sampling event", breaks=(1:9), labels=c("Most\nrecent",rep("",7), "Least\nrecent"))+
  # scale_y_discrete("Sampling event", breaks=(1:9))+
  xlab("site")+
  # scale_color_viridis_d()+
  scale_fill_manual(values=c("#91bfdb","#fc8d59"), name="",na.value="gray75",
                    # labels=c("Above 0.79","Below 0.79"))+
                    labels=c("Higher","Lower","First event")
                    )+
  theme_classic()+
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1),
        # panel.grid.major.y = element_line(color="gray90"),
        legend.position = "bottom"
        # panel.grid.major = element_line(color="gray90")
  ) +
  facet_wrap(~smc_lu, scales="free_y")+
  scale_y_continuous(breaks=c(2000,2010,2020))+
  ggtitle("Change from previous sampling")+
  coord_flip()
# ggsave(catchange_plot_changefromprevious_class, filename="figures/catchange_plot_changefromprevious_class.jpg", dpi=300, height=7, width=5)
ggsave(catchange_plot_changefromprevious_class, filename="figures/catchange_plot_changefromprevious_class.jpg", dpi=300, height=4, width=3)

#######
#Channel engineering
con <- dbConnect(
  PostgreSQL(),
  host = "192.168.1.17",
  dbname = 'smc',
  user = 'smcread',
  password = '1969$Harbor' # if we post to github, we might want to do rstudioapi::askForPassword()
)

# chan.df<-read.csv("data/chan_eng_hardeneddistances_CF.csv", stringsAsFactors = F)
chan.df<- dbGetQuery(con, ' 
                      SELECT * FROM
                      sde.unified_channelengineering
                            ') 
chan.df$rightsideofstructure %>% unique()
chan.df2<-chan.df %>%
  select(stationcode, channeltype, bottom, leftsideofstructure, rightsideofstructure) %>%
  unique() %>%
  mutate(Class3=case_when(channeltype =="Engineered" & bottom %in% c("Concrete", "Grouted rock","Other", "Rock")~"Hard-bottom",
                          channeltype =="Engineered" & bottom %in% c("Soft/Natural")~"All or partially earthen",
                          channeltype =="Natural" ~ "Natural"),
         Class4=case_when(channeltype =="Engineered" & bottom %in% c("Concrete", "Grouted rock", "Rock","Other")~"Hard-bottom",
                          channeltype =="Engineered" & bottom %in% c("Soft/Natural") & leftsideofstructure %in% c("Concrete", "Grouted rock", "Rock","Grouted Rock","Other")~"Soft-bottom",
                          channeltype =="Engineered" & bottom %in% c("Soft/Natural") & rightsideofstructure %in% c("Concrete", "Grouted rock", "Rock","Grouted Rock", "ROck","Other")~"Soft-bottom",
                          channeltype =="Engineered" & bottom %in% c("Soft/Natural") & leftsideofstructure %in% c("Earthen", "Earthen bare", "Vegetative/Natural")~"Earthen",
                          channeltype =="Engineered" & bottom %in% c("Soft/Natural") & rightsideofstructure %in% c("Earthen", "Earthen bare", "Vegetative/Natural")~"Earthen",
                          channeltype =="Natural" ~ "Natural")) %>%
  inner_join(lu_station.df %>% select(masterid, stationcode=stationid)) %>%
  select(-stationcode) %>%
  unique() 

chan.df2$Class3<-factor(chan.df2$Class3, levels=c("Natural","All or partially earthen","Hard-bottom"))
chan.df2$Class4<-factor(chan.df2$Class4, levels=c("Natural","Earthen","Soft-bottom","Hard-bottom"))


plot_dat_catchange2<-plot_dat_catchange %>%
  inner_join(chan.df2) %>%
  filter(Class4!="Natural")
               

blank_tiles2<-crossing(plot_dat_catchange2 %>% select(mid2, Class4),
                      Year=2000:2020)

CHAN_ENG_class_plot_pf<-ggplot(data=plot_dat_catchange2,
                      aes(x=mid2, y=Year))+
  # geom_point(aes(color=Class12), shape=15)+
  geom_tile(data=blank_tiles2, fill="white", color="gray80")+
  geom_tile(aes(fill=CSCI_Class))+
  # scale_y_reverse("Sampling event", breaks=(1:9), labels=c("Most\nrecent",rep("",7), "Least\nrecent"))+
  # scale_y_discrete("Sampling event", breaks=(1:9))+
  xlab("")+ylab("")+
  # scale_color_viridis_d()+
  scale_fill_brewer(palette="RdYlBu", direction=-1, name="CSCI\nCondition Class",
                    labels=c("Likely\nintact","Possibly\naltered","Likely\naltered","Very likely\naltered"))+
  # theme_classic(base_size = 12)+
  theme(#axis.text.y = element_blank(),
    strip.placement = "outside",
        axis.ticks = element_blank(),
        # axis.text.x = element_text(angle=45, hjust=1),
        # panel.grid.major.y = element_line(color="gray90"),
        legend.position = "bottom",
        legend.title = element_text(size=7),
        legend.text = element_text(size=7),
        strip.text = element_text(size=10),
        strip.background = element_blank(),
        panel.border = element_rect(color="black", fill=NA),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.text.y.left = element_text(angle = 0)
        # panel.grid.major = element_line(color="gray90")
  ) +
  # facet_wrap(~smc_lu, scales="free_y")+
  facet_grid(Class4~., scales="free_y", space="free", switch="y")+
  scale_y_continuous(breaks=c(2000,2010,2020))+
  coord_flip()
# ggsave(catchange_plot_pf, filename="figures/catchange_plot_pf.jpg", dpi=300, height=7, width=5)
ggsave(CHAN_ENG_class_plot_pf, filename="figures/CHAN_ENG_class_plot_pf.jpg", dpi=300, height=8, width=5)



#
replicated_sites3<-replicated_sites2 %>%
  inner_join(chan.df2)
stacked_dat2<-crossing(smc_lu=replicated_sites3$Class4 %>% unique(),
                      RangeClass=replicated_sites3$RangeClass %>% unique())
stacked_dat2$n_total<-sapply(1:nrow(stacked_dat2),function(i){
  class.i<-stacked_dat2$RangeClass[i]
  lu.i<-stacked_dat2$smc_lu[i]
  xdf<-replicated_sites3 %>%
    filter(Class4==lu.i )
  nrow(xdf)
})

stacked_dat2$n_total_high<-  sapply(1:nrow(stacked_dat2),function(i){
  class.i<-stacked_dat2$RangeClass[i]
  lu.i<-stacked_dat2$smc_lu[i]
  xdf<-replicated_sites3 %>%
    filter(Class4==lu.i & RangeClass!="Indeterminate_low n")
  nrow(xdf)
})

stacked_dat2$n_sites<-sapply(1:nrow(stacked_dat2),function(i){
  class.i<-stacked_dat2$RangeClass[i]
  lu.i<-stacked_dat2$smc_lu[i]
  xdf<-replicated_sites3 %>%
    filter(Class4==lu.i & RangeClass==class.i)
  nrow(xdf)
})

stacked_dat2$pct_total<-stacked_dat2$n_sites/stacked_dat2$n_total
stacked_dat2$pct_total_highn<-stacked_dat2$n_sites/stacked_dat2$n_total_high

stacked_dat2$RangeClass<-factor(stacked_dat2$RangeClass,
                               levels= rev(c("Indeterminate_low n",
                                             "Indeterminate",
                                             "Decreasing",
                                             "Stable",
                                             "Increasing")))
replicated_sites3%>%
  group_by(Class4) %>%
  tally()

stacked_dat2$lu<-case_when(stacked_dat2$smc_lu=="Natural"~"Natural",
                          stacked_dat2$smc_lu=="Earthen"~"Earthen",
                          stacked_dat2$smc_lu=="Soft-bottom"~"Soft-bottom",
                          stacked_dat2$smc_lu=="Hard-bottom"~"Hard-bottom",
                          T~"x")

# 
# ggplot(data=stacked_dat, 
#        aes(x=smc_lu,
#            y=pct_total))+
#   geom_bar(aes(fill=RangeClass), stat="identity", position=position_stack())

CHANstacked_plot<-ggplot(data=stacked_dat2 %>% 
                       # filter(RangeClass!="Indeterminate_low n"), 
                       filter(RangeClass %in% c("Increasing","Decreasing","Stable")),
                     aes(x=lu,
                         y=pct_total_highn))+
  geom_bar(aes(fill=RangeClass), stat="identity", position=position_stack(), color="gray25")+
  ggtitle("Trends in CSCI scores")+
  # scale_fill_viridis_d(name="CSCI trend is:")+
  scale_fill_brewer(palette="RdYlBu", name="CSCI trend:", direction= -1)+
  theme_classic()+
  theme(legend.position="bottom")+
  xlab("")+
  scale_y_continuous(name="Proportion of sites visited\n3 or more times",
                     breaks=seq(from=0, to=1, by =0.25), limits=c(0,1))
ggsave(CHANstacked_plot, filename="figures/CHANstacked_plot.jpg", dpi=300, height=3, width=4)
