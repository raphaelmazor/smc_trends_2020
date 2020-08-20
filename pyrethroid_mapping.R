library(tidyverse)
library(sf)
library(viridis)

# pyrethroids<-read.csv("data/pyrethroid_data_042720.csv", stringsAsFactors = F)
pyrethroids<-read.csv("data/Pyrethroid_Sed_ThreshEvaluation_measurements_051120.csv", stringsAsFactors = F)

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


library(maps)
# Download STATE data and add projection
CA<-st_as_sf(maps::map("state", plot = FALSE, fill = TRUE, region="california"))

st_bbox(smc_sheds_sf)

pyrethroid_conc_map<-ggplot()+
  geom_sf(data=CA)+
  geom_sf(data=smc_sheds_sf, fill="white")+
  geom_sf(data=plot.dat_sf, aes(color=Assessment.f))+
  geom_sf(data=plot.dat_sf %>% filter(Assessment.f=="Unlikely-ND"), aes(color=Assessment.f))+
  geom_sf(data=plot.dat_sf %>% filter(Assessment.f=="Unlikely (<TEB)"), aes(color=Assessment.f))+
  geom_sf(data=plot.dat_sf %>% filter(Assessment.f=="Possible or Unlikely"), aes(color=Assessment.f))+
  geom_sf(data=plot.dat_sf %>% filter(Assessment.f=="Possible"), aes(color=Assessment.f))+
  geom_sf(data=plot.dat_sf %>% filter(Assessment.f=="Likely"), aes(color=Assessment.f))+
  scale_colour_manual(name="Adverse effects", values=mypal2,
                      labels=c("Unlikely-ND (n=7)",
                               "Unlikely (<TEB) (n=9)",
                               "Possible or Unlikely (RL>TEB) (n=55)",
                               "Possible (n=19)",
                               "Likely (n=22)",
                               "RL above LEB (n=0)"
                               ))+
  theme_minimal()+
  theme(axis.text=element_blank())+
  coord_sf(xlim=c(-119.47645,-116.28837),ylim=c(32.53426,34.82374))
  
ggsave(pyrethroid_conc_map, filename="figures/pyrethroids/pyrethroid_conc_map.jpg", dpi=300,
       width=6, height=4)

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
  summarise(tot_result=sum(result[result>0], na.rm=T),
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


plot_dat<-mydf %>%
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
  scale_fill_brewer(palette="Reds", name="Adverse effects",
                    labels=c("Unlikely (not detected)",
                             "Unlikely (detected below TEB)",
                             "Possible or unlikely (detected below LEB, but RL above TEB)",
                             "Possible (detected below LEB but above TEB)",
                             "Likely (detected above LEB)"))+
  xlab("")+ylab("Proportion of sites")+
  theme_classic()+
  theme(legend.text=element_text(size=7),
        legend.title = element_text(size=9))
ggsave(pyreth_conc_lu_stack, filename="figures/pyrethroids/pyreth_conc_lu_stack.jpg", dpi=300, height=3,width=6)

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




##

comb.df<-read.csv("data/CombDF.csv", stringsAsFactors = F)
head(comb.df)
nrow(comb.df)

plot_dat_CSCI<-comb.df %>%
  select(masterid, SiteYear, PyrethroidSum, PyrethroidSum_OrgNorm , Score=csci) %>%
  mutate(Index="CSCI")%>%
  na.omit()
plot_dat_ASCI<-comb.df %>%
  select(masterid, SiteYear, PyrethroidSum, PyrethroidSum_OrgNorm ,  Score=ASCI.hybrid) %>%
  mutate(Index="ASCI")%>%
  na.omit()

plot_dat_CSCI[which.max(plot_dat_CSCI$PyrethroidSum),]

plot_dat_index_pyr_refs=data.frame(Index=c("ASCI", "CSCI"),
                                   Score=c(0.85,0.79))

plot_dat_index_pyr<-bind_rows(plot_dat_CSCI, plot_dat_ASCI)

library(quantreg)
csci_qr<-plot_dat_index_pyr %>%
  filter(Index=="CSCI") %>%
  rq(formula=Score~PyrethroidSum, tau=0.9)
asci_qr<-plot_dat_index_pyr %>%
  filter(Index=="ASCI") %>%
  rq(formula=Score~PyrethroidSum, tau=0.9)
qr_preds<-data.frame()

pred.df<-data.frame(PyrethroidSum=seq(from=0, to=max(plot_dat_index_pyr$PyrethroidSum), length.out = 1000))
pred.df2<-pred.df %>%
  cbind(predict(csci_qr, interval="confidence", newdata = pred.df)) %>%
  mutate(Index="CSCI") %>%
  rename(Score=fit) %>%
  bind_rows(
    pred.df %>%
      cbind(predict(asci_qr, interval="confidence", newdata = pred.df)) %>%
      mutate(Index="ASCI") %>%
      rename(Score=fit) %>%
      filter(PyrethroidSum<0.16)
  )
  

scores_vs_pyrethroids_plot<-
  ggplot(data=plot_dat_index_pyr, aes(x=PyrethroidSum, y=Score))+
  geom_ribbon(data=pred.df2, aes(ymin=lower, ymax=higher), alpha=0.5, fill="gray80")+
  geom_point()+
  facet_wrap(~Index, scales="free_x")+
  xlab("Total pyrethroids (ug/g dry weight)")+
  geom_hline(data=plot_dat_index_pyr_refs, aes(yintercept=Score), linetype="dashed", color="red")+
  geom_quantile(quantiles=c(.9), color="gray")+
  
  theme_classic()+
  scale_y_continuous(breaks=c(0.5,0.75,1,1.25))+
  coord_cartesian(ylim=c(0,1.25))
ggsave(scores_vs_pyrethroids_plot, filename="figures/pyrethroids/scores_vs_pyrethroids_plot.jpg",
       dpi=300, height=4, width=6)


ggplot(data=plot_dat_CSCI, aes(x=PyrethroidSum, y=Score))+
  geom_point()+
  geom_quantile(quantiles=c(.5,.6,.7,.8, .9), color="gray")+
  theme_classic()

ggplot(data=plot_dat_ASCI, aes(x=PyrethroidSum, y=ASCI.hybrid))+
  geom_point()+
  geom_quantile(quantiles=c(.5,.6,.7,.8, .9), color="gray")+
  theme_classic()



ggplot(data=plot_dat_CSCI, aes(x=PyrethroidSum_OrgNorm, y=csci))+
  geom_point()+
  geom_quantile(quantiles=c(.5,.6,.7,.8, .9), color="gray")+
  theme_classic()


ggplot(data=plot_dat_ASCI, aes(x=PyrethroidSum_OrgNorm, y=ASCI.hybrid))+
  geom_point()+
  geom_quantile(quantiles=c(.5,.6,.7,.8, .9), color="gray")+
  theme_classic()
