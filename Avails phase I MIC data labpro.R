library(readr)
library(dplyr)
library(stringr)
library(tidyr)

setwd("C:/Users/alamsa/OneDrive - Danaher/R stuff/BB4 part 1 data avails phase 1")

LP_0519<-read_csv('20210519Bclin.csv')
LP_0520<-read_csv('20210520Bclin.csv')
LP_0521<-read_csv('20210521Bclin.csv')
LP_0525<-read_csv('20210525Aclin.csv')
LP_0526<-read_csv('20210526Aclin.csv')
LP_0527<-read_csv('20210527Aclin.csv')
LP_0528<-read_csv('20210528Cclin.csv')
LP_0601<-read_csv('20210601Cclin.csv')
LP_0602<-read_csv('20210602Bclin.csv')
LP_0603<-read_csv('20210603BCclin.csv')
LP_0604<-read_csv('20210604Bclin.csv')
LP_0607<-read_csv('20210607Cclin.csv')
LP_0608<-read_csv('20210608Cclin.csv')
LP_0609<-read_csv('20210609Cclin.csv')
LP_0610<-read_csv('20210610Cclin.csv')
LP_0611<-read_csv('20210611Cclin.csv')
LP_0614<-read_csv('20210614CDclin.csv')
LP_0615<-read_csv('20210615Cclin.csv')
LP_0616<-read_csv('20210616Cclin.csv')

LP_data_all<-bind_rows(list(LP_0519, LP_0520, LP_0521, LP_0525, LP_0526, LP_0527, LP_0528, LP_0601, LP_0602, LP_0603, LP_0604, LP_0607, LP_0608, LP_0609, LP_0610, LP_0611, LP_0614, LP_0615, LP_0616))

LP_data_PBC<-LP_data_all%>%
  filter(InocSource != 'P')%>%
  mutate(concat=str_c(ID, strains, anti), .before=LPDG_DSI)

LP_data_plate<-LP_data_all %>%
  filter(InocSource == 'P' )%>%
  mutate(concat=str_c(ID, strains, anti))

LP_data_plate_noreps<-LP_data_plate %>%
  distinct(concat, .keep_all = TRUE)

write_csv(LP_data_all, "AvailsPhase1_allLPMICs.csv")
write_csv(LP_data_PBC, "AvailsPhase1_LP_PBCMICs.csv")
write_csv(LP_data_plate, 'AvailsPhase1_LP_plateMICs.csv')

LP_data_plate_noreps_MIConly<-LP_data_plate_noreps%>%
  select(concat, LPDG_DSI, LPDG_MIC)%>%
  rename(plateDSI=LPDG_DSI, plateMIC=LPDG_MIC)

LP_PBCvsplate<-LP_data_PBC%>%
  left_join(LP_data_plate_noreps_MIConly)%>%
  mutate(PBCDSI_minus_PlateDSI=(LPDG_DSI-plateDSI))

write_csv(LP_PBCvsplate, 'AvailsPhase1_LP_PBCvsPlateMIC_noplaterepeats_realMIC.csv')

Pmirab_reprange<-LP_data_plate%>%
  filter(ID=='41784' |ID=='43977' | ID=='47004'| ID=='48021')%>%
  group_by(ID, anti)%>%
  summarize(min(LPDG_DSI), max(LPDG_DSI))

write_csv(Pmirab_reprange, "Pmirab_reprange_LP.csv")
