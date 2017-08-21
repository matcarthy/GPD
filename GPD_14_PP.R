#2014 Wave Preprocessing
library(foreign)
GPD2014 <- read.spss("GPD2014FU.sav", to.data.frame = T, use.missings = T)
gpd14 <- GPD2014

str(gpd14)
head(gpd14)
summary(GPD2014)
summary(gpd14)
which(is.na(gpd14))



#subset
library(tidyverse)
gpd14 <- sapply(gpd14[,], as.numeric)
gpd14 <- tbl_df(gpd14)
gpd14$ID <- seq.int(nrow(gpd14))

gpd4var14 <- gpd14 %>%
  select(S3, S4, S5, S6, S7_0, S7_1, S7_2, S7_3, S7_4, S7_5, S7_6, S8, S9_0, S9_1, S9_2,
         S9_3, S9_4, S9_5, S9_6, S9_7, S10, S18, S19, S20)

gpd4varnotsure <- gpd14 %>%
  select(S1)

gpd2var14 <- gpd14 %>%
  select(S13_0, S13_1, S13_2, S13_3, S13_4, S13_5, S13_6, S13_7, S13_8, S13_9, S13_10, S13_11, S13_12, S13_13, S13_14, S16) 

gpd2notsure <- gpd14 %>%
  select(S14_0, S14_1, S14_2, S14_3, S14_4, S14_5, S14_6, S14_6, S14_7, S14_8, S14_9, S14_10, S15_0, S15_1, S15_2, S15_3, S15_4,
         S17)

gpd14dem <- gpd14[,c(1:14, 92:93)]


#flip and mark NA for 4 factor features
rev4ns <- function(x) {
  flip <- abs(x-5)
}

rev4 <- function(x){
  flip <- abs(x-4)
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

gpd4var14 <- sapply(gpd4var14, rev4)
gpd4var14 <- tbl_df(gpd4var14)

gpd4varnotsure <- sapply(gpd4varnotsure, rev4ns)
gpd4varnotsure <- tbl_df(gpd4varnotsure)

gpd2var14 <- sapply(gpd2var14, rev2)
gpd2var14 <- tbl_df(gpd2var14)

gpd2notsure <- sapply(gpd2notsure, rev2ns)
gpd2notsure <- tbl_df(gpd2notsure)


#Merge & Reconvert
gpd4var14$ID <- gpd14$ID
gpd4varnotsure$ID <- gpd14$ID
gpd2var14$ID <- gpd14$ID
gpd2notsure$ID <- gpd14$ID
gpd14dem$ID <- gpd14$ID

gpd14full <- full_join(gpd4var14, gpd2var14, by='ID') %>%
  left_join(., gpd4varnotsure, by='ID') %>%
  left_join(., gpd2notsure, by='ID') %>%
  left_join(., gpd14dem, by='ID')


#gpd14full.fac <- sapply(gpd14full[,], as.factor)

#gpd14full.fac <- as.data.frame(gpd14full.fac)

#Missing Data
#table(x, y, useNA = 'always')

gpd14nomiss <- na.omit(gpd14full)


#Demographic refactor

gpd14nomiss$gender.f <- as.factor(gpd14nomiss$Gender)

gpd14nomiss$age.f <- as.factor(gpd14nomiss$Age_Rollup_broad)

gpd14nomiss$ed.f <- as.factor(gpd14nomiss$Education_Level)

gpd14nomiss$income.f <- as.factor(gpd14nomiss$HH_Income_MERGED)

gpd14nomiss$kids.f <- as.factor(gpd14nomiss$children)

gpd14nomiss$married <- as.factor(gpd14nomiss$Marital_Status)

gpd14nomiss$race <- ifelse(gpd14nomiss$Race_White == 1, 1,
                           ifelse(gpd14nomiss$Race_AmericanIndianorAlaskaNative == 1, 2,
                                  ifelse(gpd14nomiss$Race_Asian == 1, 3, 
                                         ifelse(gpd14nomiss$Race_BlackorAfricanAmerican == 1, 4,
                                                ifelse(gpd14nomiss$Race_NativeHawaiianorOtherPacificIslander ==1, 5, 6)))))

gpd14nomiss$race.f <- as.factor(gpd14nomiss$race)




