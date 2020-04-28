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
