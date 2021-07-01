library(readr)
library(dplyr)
library(stringr)
library(tidyr)

setwd("C:/Users/alamsa/OneDrive - Danaher/R stuff/BB4 part 1 data avails phase 1")

part1_0519_0521<-read_csv("BB4Feasi_NEG_20210524_0519-0521.csv")

part1_0525_0601<-read_csv("BB4Feasi_NEG_20210604_0525-0601.csv")

part1_0602_0608<-read_csv("BB4_PBC_NEG_20210610_0602-0608.csv")

part1_0609_0610<-read_csv("BB4_PBC_NEG_20210614_0609-0610.csv")

part1_0611_0615<-read_csv("BB4_PBC_NEG_20210616_0611-0615.csv")

part1_0616<-read_csv("BB4_PBC_NEG_20210618_0616.csv")

cols_0519_0521<-colnames(part1_0519_0521)
cols_0525_0601<-colnames(part1_0525_0601)
cols_0602_0608<-colnames(part1_0602_0608)
cols_0609_0610<-colnames(part1_0609_0610)
cols_0611_0615<-colnames(part1_0611_0615)
cols_0616<-colnames(part1_0616)

length(cols_0519_0521)
length(cols_0525_0601)
length(cols_0602_0608)
length(cols_0609_0610)
length(cols_0611_0615)

cols_0519_0521==cols_0525_0601

cols_0519_0521[1:10]
cols_0525_0601[1:10]

head(part1_0525_0601)
head(part1_0519_0521)

part1_0519_0521_fixed<-part1_0519_0521%>%
  mutate(InocSource='B', .after=ID)%>%
  mutate(PBCInstrument=str_sub(SampleID, 9,9), .after=InocSource)%>%
  mutate(PBCTime=str_sub(SampleID, 10,10), .after=PBCInstrument)%>%
  mutate(PBCTime=str_pad(PBCTime, 2, side = "left", pad = "0"),)

head(part1_0519_0521_fixed)

cols_0519_0521_fixed<-colnames(part1_0519_0521_fixed)
length(cols_0519_0521_fixed)

sum(cols_0519_0521_fixed==cols_0525_0601)
sum(cols_0519_0521_fixed==cols_0602_0608)
sum(cols_0519_0521_fixed==cols_0609_0610)
sum(cols_0519_0521_fixed==cols_0611_0615)
sum(cols_0519_0521_fixed==cols_0616)

part1_all<-bind_rows(list(part1_0519_0521_fixed, part1_0525_0601, part1_0602_0608, part1_0609_0610, part1_0611_0615, part1_0616))

part1_all_PBC<-part1_all%>%
  filter(InocSource != 'P')

part1_all_plate<-part1_all%>%
  filter(InocSource == 'P')

write_csv(part1_all, 'AvailsPhase1_part1_all.csv')
write_csv(part1_all_PBC, "AvailsPhase1_part1_PBC.csv")
write_csv(part1_all_plate , 'AvailsPhase1_part1_plate.csv')

PBC_simplified<-part1_all_PBC%>%
  filter(conc_No==1)%>%
  select(testdate:LPDG_DSI_M)%>%
  mutate(concat=str_c(ID, strains, anti), .before= LPDG_DSI_M)

plate_simplified<-part1_all_plate%>%
  filter(conc_No==1)%>%
  mutate(concat=str_c(ID, strains, anti))%>%
  select(concat, LPDG_DSI_M)%>%
  rename(PlateDSI=LPDG_DSI_M)

plate_simplified_no_repeats<-plate_simplified%>%
  distinct(concat, .keep_all = TRUE)

PBCvsPlateMIC<-PBC_simplified%>%
  left_join(plate_simplified_no_repeats)%>%
  mutate(PBCDSI_minus_PlateDSI=(LPDG_DSI_M-PlateDSI))

write_csv(PBCvsPlateMIC, "AvailsPhase1_PBCvsPlateMIC_noplaterepeats.csv")

duplicate_plate_data<-part1_all_plate%>%
  filter(ID=='41784' |ID=='43977' | ID=='47004'| ID=='48021', conc_No==1)%>%
  group_by(ID, anti)%>%
  summarize(min(LPDG_DSI_M), max(LPDG_DSI_M))

write_csv(duplicate_plate_data, "Pmirabilis_fromplate_rangeMICs.csv")

