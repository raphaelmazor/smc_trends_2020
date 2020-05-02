library(tidyverse)
library(sf)
library(viridis)

pyrethroids<-read.csv("data/pyrethroid_data_042720.csv", stringsAsFactors = F)

mydf<- pyrethroids %>%
  select(masterid, latitude, longitude, result, OrgNorm, LEB, TEB, RLOC,
         analytename, SiteYear, smc_lu) %>%
  na.omit() %>%
  mutate(
    Assessment = case_when(
      OrgNorm > LEB ~ "Likely",
      RLOC > LEB ~ "RL too high",
      OrgNorm > TEB ~"Possible",
      RLOC > TEB ~ "Possible or Unlikely",
      OrgNorm< TEB & OrgNorm > 0 ~ "Unlikely (<TEB)",
      OrgNorm< TEB & OrgNorm == 0 ~ "Unlikely-ND",
      T~"XXXXXX"), 
    Assessment.f = factor(Assessment, levels=c(
      "Unlikely-ND",
      "Unlikely (<TEB)",
      "Possible or Unlikely",
      "Possible",
      "Likely",
      "RL too high")), 
      Assessment.n = as.numeric(Assessment.f)  )

assessment_xwalk<-mydf %>%
  select(Assessment, Assessment.f, Assessment.n) %>%
  unique()

mydf$Assessment.n[mydf$Assessment.n==6]<- -9

mydf_sitesummary<-mydf %>%
  group_by(masterid, latitude, longitude, smc_lu) %>%
  summarise(Assessment.n=max(Assessment.n, na.rm=T)) %>%
  ungroup() %>%
  left_join(assessment_xwalk)
  
mydf_sitesummary %>%
  group_by(Assessment.f) %>%
  tally()

mydf %>%
  filter(Assessment=="Likely") %>%
  select(masterid, Assessment) %>% unique() %>% nrow()



# 
#   
#   
#   na.omit() %>%
#   unique() %>%
#   mutate(Assessment2 = factor(Assessment,
#                               levels=c("Unlikely",
#                                        "Possible or Unlikely",
#                                        "Possible",
#                                        "Likely",
#                                        "RL too high")
#                               ),
#          Assessment3 = case_when(Assessment=="Unlikely" & OrgNorm == 0 ~"Unlikely-ND",
#                                  Assessment=="Unlikely" & OrgNorm != 0 ~"Unlikely (<TEB)",
#                                  T~Assessment) ) 
# plot.dat$Assessment3<-factor(plot.dat$Assessment3, levels=c("Unlikely-ND",
#                            "Unlikely (<TEB)",
#                            "Possible or Unlikely",
#                            "Possible",
#                            "Likely",
#                            "RL too high"))
# 
#          
# plot.dat %>% unique() %>% nrow()

plot.dat_sf<-mydf_sitesummary %>%
  st_as_sf(coords = c("longitude", "latitude"), # can use numbers here too
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326)

smc_sheds_sf<-st_read("data/SMCSheds2009/SMCSheds2009.shp") %>%
  st_transform(crs=4326)


mypal<-c(viridis_pal()(4),"gray")

# ggplot()+
#   geom_sf(data=smc_sheds_sf)+
#   geom_sf(data=plot.dat_sf, aes(color=Assessment2))+
#   geom_sf(data=plot.dat_sf %>% filter(Assessment2=="Unlikely"), aes(color=Assessment2))+
#   geom_sf(data=plot.dat_sf %>% filter(Assessment2=="Possible or Unlikely"), aes(color=Assessment2))+
#   geom_sf(data=plot.dat_sf %>% filter(Assessment2=="Possible"), aes(color=Assessment2))+
#   geom_sf(data=plot.dat_sf %>% filter(Assessment2=="Likely"), aes(color=Assessment2))+
#     scale_colour_manual(name="Adverse effects", values=mypal)+
#   ggtitle("Worst of any constituent, any replicate")

mypal2<-c(viridis_pal()(5),"gray")



ggplot()+
  geom_sf(data=smc_sheds_sf)+
  geom_sf(data=plot.dat_sf)

ggplot()+
  geom_sf(data=smc_sheds_sf)+
  geom_sf(data=plot.dat_sf, aes(color=Assessment.f))+
  geom_sf(data=plot.dat_sf %>% filter(Assessment.f=="Unlikely-ND"), aes(color=Assessment.f))+
  geom_sf(data=plot.dat_sf %>% filter(Assessment.f=="Unlikely (<TEB)"), aes(color=Assessment.f))+
  geom_sf(data=plot.dat_sf %>% filter(Assessment.f=="Possible or Unlikely"), aes(color=Assessment.f))+
  geom_sf(data=plot.dat_sf %>% filter(Assessment.f=="Possible"), aes(color=Assessment.f))+
  geom_sf(data=plot.dat_sf %>% filter(Assessment.f=="Likely"), aes(color=Assessment.f))+
  scale_colour_manual(name="Adverse effects", values=mypal2,
                      labels=c("Unlikely-ND (n=4)",
                               "Unlikely (<TEB) (n=9)",
                               "Possible or Unlikely (n=46)",
                               "Possible (n=18)",
                               "Likely (n=19)",
                               "RL too high (n=0)"
                               ))+
  ggtitle("Worst of any constituent, any replicate")

plot.dat %>% 
  select(masterid, Assessment3) %>%
  unique() %>%
  group_by(Assessment3) %>%
  tally()

pyrethroids %>%
  filter(Assessment=="Possible") %>%
  select(masterid) %>%
  unique()


sum_concentrations<-pyrethroids %>%
  filter(smc_lu!="SMC_out") %>%
  group_by(masterid, SiteYear, smc_lu) %>%
  summarise(tot_result=sum(result, na.rm=T),
            tot_orgnorm=sum(OrgNorm, na.rm=T))

ggplot(data=sum_concentrations, aes(x=smc_lu, y=tot_orgnorm))+
  geom_boxplot()+
  scale_y_sqrt()


csci.df<-read.csv("data/pyrethroid_csci.csv", stringsAsFactors = F) %>%
  select(masterid, SiteYear, csci, ASCI.hybrid, MeanSurvival ) %>%
  group_by(masterid, SiteYear) %>%
  summarize(CSCI=mean(csci, na.rm=T),
            ASCI=mean(ASCI.hybrid, na.rm=T),
            MeanSurvival=mean(MeanSurvival, na.rm=T))

relationship.df<-mydf %>%
  inner_join(csci.df)

ggplot(data=relationship.df, aes(x=))


sum(pyrethroids$RLOC > pyrethroids$LEB,na.rm=T)


####
pyrethroids<-read.csv("data/pyrethroid_data_050120.csv", stringsAsFactors = F) %>%
  mutate(
    Assessment = case_when(
      OrgNorm > LEB ~ "Likely",
      RLOC > LEB ~ "RL too high",
      OrgNorm > TEB ~"Possible",
      RLOC > TEB ~ "Possible or Unlikely",
      OrgNorm< TEB & OrgNorm > 0 ~ "Unlikely (<TEB)",
      OrgNorm< TEB & OrgNorm == 0 ~ "Unlikely-ND",
      T~"XXXXXX"), 
    Assessment.f = factor(Assessment, levels=c(
      "Unlikely-ND",
      "Unlikely (<TEB)",
      "Possible or Unlikely",
      "Possible",
      "Likely",
      "RL too high")), 
    Assessment.n = as.numeric(Assessment.f)  )

pyrethroids$Assessment.n[pyrethroids$Assessment.n==6]<- -1

assessment_xwalk<-mydf %>%
  select(Assessment, Assessment.f, Assessment.n) %>%
  unique()

result <- df %>% 
  group_by(A, B) %>%
  filter(value == max(value)) %>%
  arrange(A,B,C)

plot_dat<-pyrethroids %>%
  select(masterid, smc_lu,Assessment.n)  %>%
  unique() %>%
  group_by(masterid, smc_lu) %>%
  filter(Assessment.n==max(Assessment.n)) %>%
  ungroup() %>%
  inner_join(assessment_xwalk) %>%
  group_by(smc_lu,Assessment.f) %>%
  tally() 

plot_dat$total_lu<-sapply(1:nrow(plot_dat),function(i){
  lu.i<-plot_dat$smc_lu[i]
  # ass.i<-plot_dat$Assessment.f[i]
  xdf<-plot_dat %>% filter(smc_lu == lu.i)
  sum(xdf$n)
})  

plot_dat$Prop <-plot_dat$n/plot_dat$total_lu

pyreth_conc_lu_stack<-ggplot(data=plot_dat, aes(x=smc_lu, y=Prop))+
  geom_bar(aes(fill=Assessment.f), stat="identity", position=position_stack(), color="gray")+
  scale_fill_brewer(palette="Reds", name="Adverse effects")+
  xlab("")+ylab("Proportion of sites")+
  theme_classic(base_size=8)
ggsave(pyreth_conc_lu_stack, filename="figures/pyrethroids/pyreth_conc_lu_stack.jpg", dpi=300, height=3,width=4)

plot_dat2<-pyrethroids %>%
  select(masterid, smc_lu,result, OrgNorm)  %>%
  mutate(res2 =case_when(result< 0 ~ 0,
                         T~result)) %>%
  group_by(masterid, smc_lu) %>%
  summarise( Total=sum(res2),
            TotalOC=sum(OrgNorm)) %>%
  ungroup()

plot_dat2[which.max(plot_dat2$TotalOC),]

 
options(scipen = 999) #Turn off scientific notation
ggplot(data=plot_dat2, aes(x=smc_lu, y=TotalOC+.01))+
  geom_boxplot()+
  # geom_point(position=position_jitter(width=0.05, height=0))+
  scale_y_continuous(trans="log10", name="Total pyrethroids + 0.01 ug/g OC")
