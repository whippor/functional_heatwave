#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                             ##
# Functional Heatwave Data QAQC                                               ##
# Script created 2025-02-03                                                   ##
# Data source: Gulf Watch Alaska                                              ##
# R code prepared by Ross Whippo - NOAA/NCCOS/KB                              ##
# Last updated 2025-02-03                                                     ##
#                                                                             ##
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:


# Required Files (check that script is loading latest version):
# FILE.csv

# Associated Scripts:
# FILE.R

# TO DO 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                         ####
#                                                                              +
# LOAD PACKAGES                                                                +
# READ IN AND PREPARE DATA                                                     +
# MANIPULATE DATA                                                              +
#                                                                              +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




### NOTES

# GET PELAGIC DATA AS WELL!









#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                             ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(viridis)
library(data.table)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sites <- read_csv("functional_heatwave/data/raw/Sites/NearshoreSystemsInGOA_IntertidalSiteLocations_MasterList21Dec2016.csv")

setwd("C:/Users/Ross.Whippo/Documents/git/functional_heatwave/data/raw/Temperature")
allTemps <- 
  list.files(pattern = "\\.csv$") %>% 
  map_df(~fread(.))
setwd("C:/Users/Ross.Whippo/Documents/git/functional_heatwave")

# calculated percent cover of sessile organisms
KATMKEFJWPWS_cover <- read_csv("data/raw/IntertidalCover/KATMKEFJWPWS_2006-2023_Rocky_Intertidal_Percent_Cover.csv")
# raw data, percent cover of sessile organisms
KBAY_cover_raw <- read_csv("data/raw/IntertidalCover/KBAY2012-2023_Rocky_Intertidal_Percent_Cover.csv", 
                           col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                                            Replicate = col_character(), `Quadrat(m2)` = col_character()))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                           ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# join sessile cover datasets together

# remove KBAY duplicate rows
KBAY_cover <- KBAY_cover_raw %>%
  distinct()

# simplify KBAY and standardize names
K1 <- KBAY_cover %>% 
  select(SiteName = Site, 
         SampleDate = Date, 
         Year, 
         Quadrat_Num = Replicate, 
         Elevation_Position_raw = Stratum, 
         Species_raw = `Original field ID`, 
         overstory = `%overstory`, 
         understory = `%understory`) %>%
  mutate(Percent_Cover_raw = understory + overstory, .keep = "unused") %>%
  mutate(Percent_Cover_100 = case_when(Percent_Cover_raw > 100 ~ 100,
                                       TRUE ~ Percent_Cover_raw),
         .keep = "unused") %>%
  mutate(Block_Name = "KBAY", .before = SiteName) %>%
  mutate(Elevation_Position = case_when(Elevation_Position_raw == "High" ~ "Upper",
                                        Elevation_Position_raw == "Mid" ~ "Mid",
                                        Elevation_Position_raw == "Low" ~ "Low",
                                        Elevation_Position_raw == "-1 m" ~ "Sub",
                                        Elevation_Position_raw == "-1" ~ "Sub",
                                        Elevation_Position_raw == "L" ~ "Low",
                                        Elevation_Position_raw == "M" ~ "Mid",
                                        Elevation_Position_raw == "H" ~ "Upper"),
         .keep = "unused", .before = Species_raw) %>%
  mutate(Species = case_when(Species_raw == "Bare substrate" ~ "bare substrate",
                             Species_raw == "sponge (encrusting)" ~ "unidentified sponge",
                             Species_raw == "Sponge (Ophlitospongia)" ~ "unidentified sponge",
                             Species_raw == "Balanidae" ~ "barnacle",
                             Species_raw == "Ascidiacea" ~ "unidentified tunicate",
                             Species_raw == "Hydroidea" ~ "unidentified hydroid",
                             Species_raw == "Devaleraea mollis" ~ "Palmaria hecatensis/Devaleraea mollis",
                             Species_raw == "Palmaria hecatensis" ~ "Palmaria hecatensis/Devaleraea mollis",
                             Species_raw == "Bryozoa" ~ "bryozoan",
                             Species_raw == "Rhodophyta" ~ "unidentified red alga",
                             Species_raw == "Mastocarpus papillatus (tetrasporophyte-Petrocelis)" ~ "Mastocarpus papillatus",
                             Species_raw == "Mazzaella/Mastocarpus complex" ~ "Mazzaella sp.",
                             Species_raw == "Neorhodomela oregonensis/Odonthalia floccosa complex" ~ "Neorhodomela sp.",
                             Species_raw == "upright coralline" ~ "Corallina sp.",
                             Species_raw == "Pyropia/Boreophyllum complex" ~ "Boreophyllum / Pyropia / Wildemania spp.",
                             TRUE ~ Species_raw), .keep = "unused", .after = Elevation_Position) %>%
  mutate(Quadrat_Num = as.numeric(Quadrat_Num))  %>%
  group_by(Block_Name, SiteName, SampleDate, Year, Quadrat_Num, Elevation_Position, Species) %>%
  summarise(Percent_Cover_sums = sum(Percent_Cover_100)) %>%
  mutate(Percent_Cover = Percent_Cover_sums, .keep = "unused") %>%
  ungroup() 

# add blocks to KATM dataset and harmonize species with KBAY
A1 <- KATMKEFJWPWS_cover %>%
  mutate(Block_Name = case_when(SiteName == "Observation Island" ~ "EPWS",
                                SiteName == "Simpson Bay" ~ "EPWS",
                                SiteName == "Olsen  Bay" ~ "EPWS",
                                SiteName == "Port Fidalgo" ~ "EPWS",
                                SiteName == "Galena Bay" ~ "EPWS",
                                SiteName == "Northwest Bay" ~ "WPWS",
                                SiteName == "Disk Island" ~ "WPWS",
                                SiteName == "Herring Bay (Bear Cove)" ~ "WPWS",
                                SiteName == "Herring Bay" ~ "WPWS",
                                SiteName == "Johnson Bay" ~ "WPWS",
                                SiteName == "Whale Bay" ~ "WPWS",
                                SiteName == "Iktua Bay" ~ "WPWS",
                                SiteName == "Hogan Bay" ~ "WPWS",
                                SiteName == "Unakwik Inlet" ~ "NPWS",
                                SiteName == "Perry Island" ~ "NPWS",
                                SiteName == "Bettles Bay" ~ "NPWS",
                                SiteName == "Esther Passage" ~ "NPWS",
                                SiteName == "Cedar Bay" ~ "NPWS",
                                SiteName == "Harris Bay" ~ "KEFJ",
                                SiteName == "Nuka Passage" ~ "KEFJ",
                                SiteName == "Nuka Bay" ~ "KEFJ",
                                SiteName == "McCarty Fjord" ~ "KEFJ",
                                SiteName == "Aialik Bay" ~ "KEFJ",
                                SiteName == "Ninagiak Island" ~ "KATM",
                                SiteName == "Takli Island" ~ "KATM",
                                SiteName == "Amalik Bay" ~ "KATM",
                                SiteName == "Kinak Bay" ~ "KATM",
                                SiteName == "Kaflia Bay" ~ "KATM",
                                SiteName == "Kukak Bay" ~ "KATM"),
         .before = SiteName) %>%
  mutate(SiteName = case_when(SiteName == "Olsen  Bay" ~ "Olsen Bay",
                              TRUE ~ SiteName),
         .keep = "unused", .before = SampleDate) %>%
  select(-SiteID) %>%
  mutate(Elevation_Position = case_when(Elevation_Position == "Mid (0.5 m MLLW)" ~ "Mid",
                                        Elevation_Position == "Upper (1.5 m MLLW)" ~ "Upper")) %>%
  mutate(Species_fixed = case_when(Species == "Blidingia minima var. minima" ~ "Blidingia minima",
                                   Species == "Phycodrys fimbriata" ~ "Phycodrys sp.",
                                   Species == "Constantinea subulifera" ~ "Constantinea spp.",
                                   Species == "Spirorbidae" ~ "Spirorbis sp.",
                                   Species == "foliose coralline algae" ~ "Corallina sp.",
                                   Species == "Ulothrix flacca" ~ "Ulothrix sp.",
                                   Species == "barnacle spat" ~ "barnacle",
                                   Species == "encrusting bryozoan" ~ "bryozoan",
                                   Species == "foliose bryozoan" ~ "bryozoan",
                                   Species == "unidentified red blade algae" ~ "unidentified red alga",
                                   Species == "unidentified filamentous red algae" ~ "unidentified red alga",
                                   TRUE ~ Species), .keep = "unused", .after = Elevation_Position) %>%
  mutate(Species = Species_fixed, .keep = "unused", .after = Elevation_Position) %>%
  group_by(Block_Name, SiteName, SampleDate, Year, Quadrat_Num, Elevation_Position, Species) %>%
  summarise(Percent_Cover_dupes = sum(Percent_Cover)) %>%
  mutate(Percent_Cover = Percent_Cover_dupes, .keep = "unused")


# join datasets together
allCover <- A1 %>%
  bind_rows(K1)
############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####