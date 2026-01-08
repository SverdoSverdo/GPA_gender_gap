source("00_settings.R")


#### 1. GPA ####
data <- fread("N:/durable/data/registers/SSB/03_prepared/calculating_GPA/data_v6.0/grades_all_last_grs22_simple_new.csv", data.table = F)
  data <- data[c("w19_0634_lnr","mean_all","avgdato1","lnr_org1")]
  names(data) <- c("w19_0634_lnr","grades","avgdato","lnr_org")
  data$avgdato <- substr(data$avgdato, 1, 4) # removing month


#### 2.  MOTHER  AND FATHER IDs ####
slekt <-fread("N:/durable/data/registers/SSB/01_data/data_v5.0/CORE/csv/POPULATION_SLEKT.csv", data.table = F) 
  slekt <- slekt[slekt$w19_0634_lnr %in% data$w19_0634_lnr,]
  slekt <- slekt[c("w19_0634_lnr", "lopenr_mor","lopenr_far")]

data <- merge(data, slekt, by = "w19_0634_lnr", all.x = T)

rm(slekt)

#### 3. DEMOGRAPHIC VARIABLES ####
faste <- fread("N:/durable/data/registers/SSB/01_data/data_v6.0/CORE/csv/POPULATION_FASTE_OPPLYSNINGER_reduced.csv", data.table = F)


#gender and birth year/month
faste <- faste[c("w19_0634_lnr","kjoenn","foedsels_aar_mnd","lopenr_mor","lopenr_far")]
  faste$birth_year <- substr(faste$foedsels_aar_mnd, 1, 4) # removing month
  faste <- faste[c("w19_0634_lnr","kjoenn","birth_year","lopenr_mor","lopenr_far")]
  names(faste)[4:5] <- c("lopenr_mor2","lopenr_far2")

data <- merge(data,faste, by = "w19_0634_lnr", all.x = T)

#filling in a few additional missing mother and father IDs from this dataset
data <- data %>%
  mutate(
    lopenrmor = coalesce(lopenr_mor, lopenr_mor2),
    lopenr_far = coalesce(lopenr_far, lopenr_far2)
  )

data <- data %>%
  select(-lopenr_mor2, -lopenr_far2)

#recoding females to 0
data$kjoenn[data$kjoenn == 2] <- 0

        #### 4.  PGIS  ####
 
fam <- fread("N:/durable/data/moba/linkage/merged_IDs/MoBa_SSB_IDs_20250317.csv", data.table = F)
fam <- fam[c("PREG_ID_2601","ROLE","w19_0634_lnr","SENTRIX_ID")]  

noncog <- read.table("N:/durable/data/genetics/PGS/ldpred-207k/NonCog_Demange_2021.sscore")
noncog <- noncog[c(2,5)]
names(noncog) <- c("SENTRIX_ID","noncog")

cog <- read.table("N:/durable/data/genetics/PGS/ldpred-207k/Cog_Demange_2021.sscore")
cog <- cog[c(2,5)]
names(cog) <- c("SENTRIX_ID","cog")

cog_noncog <- merge(cog, noncog, by = "SENTRIX_ID")
cog_noncog <- merge(cog_noncog, fam, by = "SENTRIX_ID")

#some are part of multiple pregnancies, so they have duplicated rows
cog_noncog <- cog_noncog[!duplicated(cog_noncog),]

#genotyping covariates
pcs <- read.delim("N:/durable/data/genetics/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc-cov-noMoBaIDs.txt")
pcs<-pcs[,c("IID",
            "genotyping_center_num","Plate_id_num","genotyping_batch_num","imputation_batch_num",
            "PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10")]


cog_noncog <- merge(cog_noncog,pcs, by.x = "SENTRIX_ID", by.y = "IID")

##Residualizing cog PGIs on genotyping variables
res_model <- lm(cog ~ as.factor(genotyping_center_num)+
                  as.factor(imputation_batch_num)+
                  as.factor(genotyping_batch_num)+
                  as.factor(Plate_id_num),
                data = cog_noncog)

cog_res <- resid(res_model)

#changing the unresidualized PGI with the residualized one
cog_noncog$cog <- cog_res 

##Residualizing cog PGIs on genotyping variables
res_model <- lm(noncog ~as.factor(genotyping_center_num)+
                  as.factor(imputation_batch_num)+
                  as.factor(genotyping_batch_num)+
                  as.factor(Plate_id_num),
                data = cog_noncog)

noncog_res <- resid(res_model)

#changing the unresidualized PGI with the residualized one
cog_noncog$noncog <- noncog_res

#Separating mothers, fathers, and kids
mom_df <- cog_noncog[cog_noncog$ROLE == "Mother",]
mom_df <- subset(mom_df, select = c(cog,noncog,PREG_ID_2601))
names(mom_df)[1:2] <- paste0(names(mom_df)[1:2],"_mother")

dad_df <- cog_noncog[cog_noncog$ROLE == "Father",]
dad_df <- subset(dad_df, select = c(cog,noncog,PREG_ID_2601))
names(dad_df)[1:2] <- paste0(names(dad_df)[1:2],"_father")  

kid_df <- cog_noncog[cog_noncog$ROLE == "Child",]
kid_df <- subset(kid_df, select = c(w19_0634_lnr,cog,noncog,PREG_ID_2601))

#merging
pgi <- merge(kid_df,mom_df, by = "PREG_ID_2601", all = T)
  pgi <- merge(pgi, dad_df, by = "PREG_ID_2601", all = T)
  pgi <- subset(pgi, select = -c(PREG_ID_2601))

#merging back to main data
data <- merge(data, pgi, by = "w19_0634_lnr", all.x = T)

#average parental PGI when both parents have non-NA values
data$cog_parental <- rowMeans(data[c("cog_father","cog_mother")])
data$noncog_parental <- rowMeans(data[c("noncog_father","noncog_mother")])

        #### 5. MISC ####

#standardizing grades within years
data <- data %>%
  group_by(avgdato) %>%
  mutate(grades_std = scale(grades))

#creating school/year combination ID
data <- data %>%
  mutate(school_year = paste(lnr_org, avgdato, sep = "_"))


#write.csv(data, file = "N:/durable/projects/37323479_Sverre_GPA_gender_gap/temp.data/individ_data_cleaned.csv", row.names = F)
