# The data for Figure 3 for the Mediterranean and Black Sea are directly taken
# from the latest STECF report on the CFP indicators update.
# Here we only aggregate the data for Figures 1 and 2, which refer to the 
# current status, as 2021, which is the latest year for which a higher number of
# assessments are available.

library(dplyr)
library(tidyr)
library(data.table)


# From Report STECF 25-01, adhoc e_annex04 has been downloaded from here:
# https://stecf.jrc.ec.europa.eu/documents/d/stecf/stecf-25-01-adhoc-annexes

## a single file is provided this year. GFCM data has 38 assessments, STECF file has 26, total of 64 assessments.
# In the med, even if one assessment can be run in a group of stocks, a stock is considered a species in one GSA, 
# unlike in the Atlantic side, so we have been told by JRC experts
# to treat joint assessments (ex. ANE11-12), as 2 stocks for our calculations.

med <-MED_GFCM_STECF_2025
names(med)
unique(med$Stock)
unique(med$Area)


med_split <- med %>%
  separate_rows(Area, sep = "_")

med$ID <- paste0(med$Stock,"_", med$Area)

unique(med$ID)
# 66
check <- unique(med[c("asses_year", "ID")])

check %>% group_by(asses_year)%>% summarise(count = n())
 

# 1       2022    8
# 2       2023    31
# 3       2024    27

df <- med

# take the terminal year of each stock

df2 <- df%>% group_by(ID) %>% top_n(1, Year)

df2$color_fig1 <- case_when(df2$F_Fref != "NA" & df2$B_Bref != "NA" ~ "GREEN",
                                is.na(df2$F_Fref) & is.na(df2$B_Bref) ~"RED",
                                TRUE ~ "ORANGE")


df2$catch[is.na(df2$catch)] <- 0
figure1 <- df2 %>%
        group_by(EcoRegion, color_fig1) %>% 
        summarise(Landings = sum(catch)) %>%
        ungroup() %>%
        spread(color_fig1, Landings, fill=0)


# This list of stocks all have ref points, but there are other assessments 
# without ref points, I have to include them in figure1 #DAVE?

# To be finished, with total number of assessed stocks and with GFCM landings
# http://www.fao.org/gfcm/data/capture-production


write.csv(figure1, file = "CSI032_figure1MED&BS_update2025_28oct.csv")


df2$color_fig2 <- case_when(df2$F_Fref < 1 & df2$B_Bref > 1 ~ "GREEN",
                               df2$F_Fref < 1 | df2$B_Bref > 1 ~ "ORANGE",
                               df2$F_Fref < 1 & is.na(df2$B_Bref) ~ "ORANGE",
                                is.na(df2$F_Fref) & df2$B_Bref > 1 ~ "ORANGE",
                               df2$F_Fref > 1 & df2$B_Bref < 1 ~ "RED",
                                is.na(df2$F_Fref) & is.na(df2$B_Bref) ~ "GREY",
                                TRUE ~ "RED")

figure2 <- df2 %>%
        group_by(EcoRegion, color_fig2) %>% 
        summarise(n= n()) %>%
        ungroup() %>%
        spread(color_fig2, n, fill=0)

write.csv(figure2, file = "CSI032_figure2MED&BS_update2025_28oct.csv")
