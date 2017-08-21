#2012 Wave Data Import and Preprocessing

library(foreign)
GPD2012 <- read.spss("GPD12FU.sav", to.data.frame = T, use.missings = T)
gpd12 <- GPD2012

str(gpd12)
head(gpd12)
summary(GPD2012)
summary(gpd12)
which(is.na(gpd12))



#subset
library(tidyverse)
gpd12 <- filter(gpd12, PANEL == 'SBA')
gpd12 <- sapply(gpd12[,], as.numeric)
gpd12 <- tbl_df(gpd12)

gpd4var <- gpd12 %>%
  select(Q1_0, Q1_1, Q1_2, Q1_2, Q1_3, Q1_4, Q1_5, Q7, Q8_GD, Q8_PC, Q9, Q10_0, Q10_1,
         Q11_0, Q11_1, Q11_2, Q12_0, Q12_1, Q12B, Q15, Q16_0, Q16_1, Q16_2, Q16_3, Q17_0, Q17_1, 
         Q17_2, Q17_3, Q18_0, Q18_1, Q18_2, Q18_3, Q18_4)

gpd2var <- gpd12 %>%
  select(Q2_0, Q2_1, Q2_2, Q2_3, Q2_4)

gpd2varmiss <- gpd12 %>%
  select(Q13, Q19_0, Q19_1, Q19_2, Q19_3, Q19_4, Q19_5,
         Q19_6, Q19_7, Q19_8, Q19_9, Q19_10, Q20_0, Q20_1, Q20_2, Q20_3)

gpddem <- gpd12[,79:104]

#flip and mark NA for 4 factor features
rev4ns <- function(x) {
  flip <- abs(x -5)
}

#flip and mark NA (if needed) on 2 factor features 
## w/Missing
rev2ns <- function(x){
  flip <- abs(x-3)
}

## No missing on Y/N
rev2 <- function(x){
  flip <- abs(x-2)
}

#apply

gpd4var <- sapply(gpd4var, rev4ns)
gpd4var <- tbl_df(gpd4var)

gpd2var <- sapply(gpd2var, rev2)
gpd2var <- tbl_df(gpd2var)

gpd2varmiss <- sapply(gpd2varmiss, rev2ns)
gpd2varmiss <- tbl_df(gpd2varmiss)

#Merge & Reconvert
gpd4var$RecordCount <- gpd12$RecordCount
gpd2var$RecordCount <- gpd12$RecordCount
gpd2varmiss$RecordCount <- gpd12$RecordCount
gpddem$RecordCount <- gpd12$RecordCount 

gpd12full <- full_join(gpd4var, gpd2var, by='RecordCount') %>%
  left_join(., gpd2varmiss, by='RecordCount') %>%
  left_join(., gpddem, by='RecordCount')

gpd12full.fac <- sapply(gpd12full[,], as.factor)

gpd12full.fac <- as.data.frame(gpd12full.fac)

#Missing Data
#table(x, y, useNA = 'always')

gpd12full <- gpd12full[,-c(70:78)]

gpd12nomiss <- na.omit(gpd12full)




###Factor demographics
gpd12nomiss$agesex.f <- as.factor(gpd12nomiss$AGESEX)

gpd12nomiss$age.f <- as.factor(gpd12nomiss$AGE)

gpd12nomiss$education_Level.f <- as.factor(gpd12nomiss$Education_Level)

gpd12nomiss$race <- ifelse(gpd12nomiss$Race_White == 1, 1,
                           ifelse(gpd12nomiss$Race_AmericanIndianorAlaskaNative == 1, 2,
                                  ifelse(gpd12nomiss$Race_Asian == 1, 3,
                                         ifelse(gpd12nomiss$Race_BlackorAfricanAmerican == 1, 4,
                                                ifelse(gpd12nomiss$Race_NativeHawaiianorOtherPacificIslander == 1, 5, 6)))))

gpd12nomiss$race.f <- as.factor(gpd12nomiss$race)





