# Load packages for data manipulation
library(tidyr)       # Data tidying
library(dplyr)       # Data manipulation
library(purrr)       # Functional programming
library(stringr)     # String manipulation
library(magrittr)    # Pipes (%>%)
library(lubridate)   # Date and time manipulation
library(readxl)      # Read Excel files
library(tibble)      # Enhanced data frames

# Load packages for statistical analysis
library(MASS)        # Statistical functions
library(mice)        # Multiple imputations
library(rms)         # Regression modeling
library(MuMIn)       # Model selection
library(boot)        # Bootstrap functions
library(AICcmodavg)  # AIC model comparison
library(caret)       # Machine learning
library(gam)         # Generalized additive models
library(rstatix)     # Statistical tests
library(metafor)

# Load packages for survival analysis
library(survival)        # Survival analysis
library(survivalAnalysis) # Advanced survival plots
library(ggsurvfit)       # ggplot2 extensions for survival plots
library(tidycmprsk)      # Competing risks analysis

# Load packages for visualization
library(ggplot2)         # Data visualization
library(ggpubr)          # Publication-ready plots
library(ggvenn)          # Venn diagrams
library(cowplot)         # Plot grid layouts
library(pheatmap)        # Heatmaps
#library(ComplexHeatmap)  # Advanced heatmaps
library(circlize)        # Circular plots
library(scales)          # Scaling functions
library(forplo)
library(patchwork)
library(gridExtra)

# Load packages for reporting and tables
library(gtsummary)       # Summary tables
library(tableone)        # Create Table 1
library(table1)          # Advanced Table 1
library(Gmisc, quietly = TRUE)  # Table summaries
library(glue)            # String interpolation
library(knitr)           # Dynamic reports

# Load packages for model selection
#library(glmulti)         # Multi-model inference
library(leaps)           # Subset regression

# Other utilities
#library(rJava)           # Java integration
library(writexl)         # Write to Excel
#======================================================================================================================================
# Define Calculator_OSI function
FEV1_intercept <- 1.726723
FEV1_beta1 <- -0.009392698
FEV1_beta2 <- 2.439627e-05
FEV1_beta3 <- -3.141919e-08
Tif_intercept <- 2.149472
Tif_beta1 <- -0.01628198
Tif_beta2 <- 5.707598e-05
Tif_beta3 <- -9.203903e-08
Calculator_OSI <- function(FEV1_PCT, Tif_PCT) {
  OSI <- 
    ((FEV1_intercept + 
        FEV1_beta1 * FEV1_PCT + 
        FEV1_beta2 * FEV1_PCT^2 + 
        FEV1_beta3 * FEV1_PCT^3) - 1) +
    ((Tif_intercept + 
        Tif_beta1 * Tif_PCT + 
        Tif_beta2 * Tif_PCT^2 + 
        Tif_beta3 * Tif_PCT^3) - 1) +
    1
  
  OSI <- round(OSI, 3)
  return(OSI)
}
#======================================================================================================================================
#======================================================================================================================================
#======================================================================================================================================
#PRELIMINARY PART
#Import the DATA from the file 

##Selecting the working directory
setwd("~/Med - PhD/ORACLE2 - Rstuff")

## Importing the original data (will be used to create table 1)
col_types <- c("guess", "guess", "text", "text", "numeric", "text", "numeric", "numeric", "numeric", "text", "numeric", "text", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "numeric", "text", "numeric", "text", "numeric", "numeric", "numeric", "text", "numeric", "text", "numeric", "numeric", "text", "text", "text", "text", "numeric", "numeric", "text", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", "text", "numeric", "text", "numeric", "numeric", "text", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric")
data_original_imported <- suppressWarnings(read_excel("data_ORACLE_original_20240429.xlsx", col_types = col_types))
data_original <-data_original_imported
## Importing the imputated data without the systematically missing data (d)= Only imputation of of all missing data
col_types <-c("guess", "text" ,"text","text", "text", "text", "numeric", "numeric", "numeric", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "text", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text" )
Data_imputated_all_imported <- suppressWarnings(read_excel("data_ORACLE_imp_20250102_joined.xlsx", col_types = col_types))
data_imputated<-Data_imputated_all_imported 
## Importing the imputated data with the systematically missing data not replaced (NR) = Only imputation of non-systematically missing
col_types <-c("guess","text","text","text","text","text","numeric","text","numeric","text","text","text","text","numeric","numeric","text","text","numeric","text","text","text","text","text","text","text","text","numeric","numeric","numeric","text","numeric","text","text","text","text","numeric","text","text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","text","numeric","text","numeric","numeric","text","numeric","numeric","text","text","text","text","text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","text")
Data_imputated_without_systematically_missing_imported<-suppressWarnings(read_excel("data_ORACLE_imp_sysREMOVED_20250102_joined.xlsx", col_types = col_types))
data_imputated_without_systematically_missing<- Data_imputated_without_systematically_missing_imported

#currentpath<-"~/Med - PhD/ORACLE2 - Rstuff"
#======================================================================================================================================
#TRANSFORMATION of Values

#Putting NA for the missing value
data_original[data_original == "NA"] <- NA
data_imputated[data_imputated== "NA"] <- NA
data_imputated_without_systematically_missing[data_imputated_without_systematically_missing == "NA"] <- NA

#Transform values to put them in the the good units (#please verify if these lines are still necessary)
#Modify the values of the stratos trials to put the reversibility in % for all the studies instead of decimal
data_original<- data_original %>% 
  mutate(FEV1_reversibility_percent_postBD_real = case_when(
    Enrolled_Trial_name=="STRATOS_1" ~ FEV1_PCT_reversibility_postBD*100,
    Enrolled_Trial_name=="STRATOS_2" ~ FEV1_PCT_reversibility_postBD*100,
    TRUE ~ FEV1_PCT_reversibility_postBD))
#Modify the values of the Captain study to put adherence in trial in percentage everywhere
data_original<- data_original %>% 
  mutate(Adherence_InTrial_quantity_real = case_when(
    Enrolled_Trial_name=="CAPTAIN" ~ Adherence_InTrial_quantity*100,
    TRUE ~ Adherence_InTrial_quantity))
#======================================================================================================================================

#Creation of a unique dataframe which contain original value, imputated and imputated_only_not_systematically_missing
data_original_main<-data_original[,c("Sequential_number","Enrolled_Trial_name","Treatment_arm","Age","Gender_0Female_1Male","BMI","Ethnicity","Country","Region","Treatment_step","Any_severe_attack_previous_12m_0no_1yes","Any_attack_or_hospitalization_previous_12_months","Number_severe_attack_previous_12m","Number_hospitalisations_for_asthma_previous_12_months","Number_hospitalizations_previous_12m_1Yes_0No","Previous_ICU_0no_1yes_9999notknown","Previous_Intubation_0no_1yes_9999notknown","Previous_ICU_or_intubation_0no_1yes","Smoking_0never_1ex_2current","Pack_years","Psychiatric_disease_0no_1yes_9999notknown","Atopy_history_0no_1yes_9999notknown","Eczema_0no_1yes_9999notknown","AllergicRhinitis__0no_1yes_9999notknown","Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown","Chronic_Rhinosinusitis_0no_1yes_9999notknown","Nasal_polyposis_0no_1yes_9999notknown","Previous_nasal_polypectomy_0no_1yes_9999notknown","ICS_DOSE_CLASS","LABA_prescribed_0no_1yes","LAMA_prescribed__0no_1yes","maintenance_OCS_prescribed__0no_1yes","Theophylline_prescribed__0no_1yes","Intranasal_seroid_prescribed__0no_1yes","FEV1_predicted_L","FVC_predicted_L","FEV1_preBD_L_Baseline","FEV1_preBD_PCT_Baseline","FVC_preBD_L_Baseline","FEV1_postBD_L_Baseline","FEV1_postBD_PCT_Baseline","FVC_postBD_L_Baseline","FVC_postBD_PCT_Baseline","FEV1_PCT_reversibility_postBD","FEV1_FVC_ratio","ACQ_baseline_score_mean","ACT_baseline_score","Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced","FeNO_baseline_ppb","Total_IgE","Follow_up_duration_days_nozero","Number_severe_asthma_attacks_during_followup","Time_to_First_attack","Time_to_2n_attack","Time_to_3n_attack","Time_to_4n_attack","Time_to_5n_attack","End_FollowUp_Reason","FEV1PREBD_L_52W","FEV1PREBD_PCT_52W","FEV1POSTBD_L_52W","FEV1POSTBD_PCT_52W","FEV1_reversibility_percent_postBD_real")]
data_imputated_all<-data_imputated[,c("Sequential_number",".imp","Age","Gender_0Female_1Male","BMI","Any_severe_attack_previous_12m_0no_1yes","Number_severe_attack_previous_12m_con","Number_hospitalisations_for_asthma_previous_12_months_con","Previous_ICU_or_intubation_0no_1yes","Smoking_0never_1ex_2current","Pack_years","Atopy_history_0no_1yes_9999notknown","Eczema_0no_1yes_9999notknown","AllergicRhinitis__0no_1yes_9999notknown","Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown","Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown","Chronic_Rhinosinusitis_0no_1yes_9999notknown","Nasal_polyposis_0no_1yes_9999notknown","Previous_nasal_polypectomy_0no_1yes_9999notknown","FEV1_preBD_L_Baseline","FEV1_preBD_PCT_Baseline","FVC_preBD_L_Baseline","FEV1_postBD_L_Baseline","FEV1_postBD_PCT_Baseline","FVC_postBD_L_Baseline","FEV1_PCT_reversibility_postBD","FEV1_FVC_ratio","ACQ_baseline_score_mean","Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced","FeNO_baseline_ppb","Total_IgE")]
colnames(data_imputated_all)[-c(1:2)] <- paste0(colnames(data_imputated_all)[-c(1:2)], "_imputated")
colnames(data_imputated_all) <- make.unique(colnames(data_imputated_all))

data_imputated_no_systematically_missing<-data_imputated_without_systematically_missing[,c("Sequential_number",".imp","Age","Gender_0Female_1Male","BMI","Any_severe_attack_previous_12m_0no_1yes","Number_severe_attack_previous_12m_con","Number_hospitalisations_for_asthma_previous_12_months_con","Previous_ICU_or_intubation_0no_1yes","Smoking_0never_1ex_2current","Pack_years","Atopy_history_0no_1yes_9999notknown","Eczema_0no_1yes_9999notknown","AllergicRhinitis__0no_1yes_9999notknown","Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown","Chronic_Rhinosinusitis_0no_1yes_9999notknown","Nasal_polyposis_0no_1yes_9999notknown","Previous_nasal_polypectomy_0no_1yes_9999notknown","FEV1_preBD_L_Baseline","FEV1_preBD_PCT_Baseline","FVC_preBD_L_Baseline","FEV1_postBD_L_Baseline","FEV1_postBD_PCT_Baseline","FVC_postBD_L_Baseline","FEV1_PCT_reversibility_postBD","FEV1_FVC_ratio","ACQ_baseline_score_mean","Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced","FeNO_baseline_ppb","Total_IgE")]
colnames(data_imputated_no_systematically_missing)[-c(1:2)] <- paste0(colnames(data_imputated_no_systematically_missing)[-c(1:2)], "_imputated_no_systematically_missing")

#combining the datasets
merged_data_imputated <- merge(data_imputated_all, data_imputated_no_systematically_missing, 
                               by = c("Sequential_number", ".imp"), 
                               all = TRUE)

All_data <- merge(data_original, merged_data_imputated, 
                  by = c("Sequential_number"), 
                  all = TRUE)
#======================================================================================================================================
#======================================================================================================================================
#======================================================================================================================================
#======================================================================================================================================
#PART A : CREATION of TABLE 1  

##Selecting the data_original which contain the value without any imputation
data_original_table<-All_data

###Labeling the differents variable with a clear name for table 1 and their respective units
####Age
label(data_original_table$Age) <- "Age"
####Sex
data_original_table$Gender_0Female_1Male <- factor(data_original_table$Gender_0Female_1Male, levels=c(0,1), labels=c("Female", "Male"))
label(data_original_table$Gender_0Female_1Male) <- "Sex, n(%)"
####Ethnicity
data_original_table$Ethnicity <- factor(data_original_table$Ethnicity, levels=c("American_Indian_or_Alaska_Native","Asian","Black_or_African_American","Maori","Multiple","Native_Hawaiian_or_other_Pacific_Islander","Other","White"))
label(data_original_table$Ethnicity) <- "Ethnicity, n(%)"
####Region
data_original_table$Region<-factor(data_original$Region,levels=c("Asia","Europe","North_America","Oceania","South_Africa","South_America"))
data_original_table$Region <- factor(data_original_table$Region, levels=c("Asia","Europe","North_America","Oceania","South_Africa","South_America"), labels=c("Asia","Europe","North_America","Oceania","South_Africa","South_America"))
label(data_original_table$Region) <- "Region, n(%)"
####BMI
label(data_original_table$BMI) <- "Body Mass Index"
units(data_original_table$BMI) <- "kg/m2"
####Treatment step
label(data_original_table$Treatment_step) <- "Treatment step, n(%)"
####Blood eosinophils (BEC)
label(data_original_table$Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced) <- "Blood eosinophils"
units(data_original_table$Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced) <- "x10^9 cells/L"
####FENO
label(data_original_table$FeNO_baseline_ppb) <- "FeNO"
units(data_original_table$FeNO_baseline_ppb) <- "ppb"
####IgE
label(data_original_table$Total_IgE) <- "Total IgE"
units(data_original_table$Total_IgE) <- "ng/mL"
####FEV1
label(data_original_table$FEV1_preBD_PCT_Baseline) <- "FEV1"
units(data_original_table$FEV1_preBD_PCT_Baseline) <- "% of predicted"
####FEV1/FVC
label(data_original_table$FEV1_FVC_ratio) <- "FEV1/FVC"
####Previous ICU or intubation
data_original_table$Previous_ICU_or_intubation_0no_1yes <- factor(data_original_table$Previous_ICU_or_intubation_0no_1yes, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Previous_ICU_or_intubation_0no_1yes) <- "Previous ICU or intubation, n(%)"
####Previous ICU
data_original_table$Previous_ICU_0no_1yes_9999notknown <- factor(data_original_table$Previous_ICU_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Previous_ICU_0no_1yes_9999notknown) <- "Previous ICU, n(%)"
####Previous intubation
data_original_table$Previous_Intubation_0no_1yes_9999notknown <- factor(data_original_table$Previous_Intubation_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Previous_Intubation_0no_1yes_9999notknown) <- "Previous intubation, n(%)"
####Atopy history
data_original_table$Atopy_history_0no_1yes_9999notknown <- factor(data_original_table$Atopy_history_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Atopy_history_0no_1yes_9999notknown) <- "Atopy history, n(%)"
####Allergy testing positive
data_original_table$Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown <- factor(data_original_table$Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown) <- "Allergy testing positive, n(%)"
####Eczema
data_original_table$Eczema_0no_1yes_9999notknown <- factor(data_original_table$Eczema_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Eczema_0no_1yes_9999notknown) <- "Eczema, n(%)"
####Allergic rhinitis
data_original_table$AllergicRhinitis__0no_1yes_9999notknown <- factor(data_original_table$AllergicRhinitis__0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$AllergicRhinitis__0no_1yes_9999notknown) <- "Allergic rhinitis, n(%)"
####Chronic rhinosinusitis
data_original_table$Chronic_Rhinosinusitis_0no_1yes_9999notknown <- factor(data_original_table$Chronic_Rhinosinusitis_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Chronic_Rhinosinusitis_0no_1yes_9999notknown) <- "Chronic rhinosinusitis, n(%)"
####Nasal polyposis
data_original_table$Nasal_polyposis_0no_1yes_9999notknown <- factor(data_original_table$Nasal_polyposis_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Nasal_polyposis_0no_1yes_9999notknown) <- "Nasal polyposis, n(%)"
####Previous nasal polypectomy
data_original_table$Previous_nasal_polypectomy_0no_1yes_9999notknown <- factor(data_original_table$Previous_nasal_polypectomy_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Previous_nasal_polypectomy_0no_1yes_9999notknown) <- "Previous nasal polypectomy, n(%)"
####ACQ-5
label(data_original_table$ACQ_baseline_score_mean) <- "ACQ-5"
####Psychiatric disease
data_original_table$Psychiatric_disease_0no_1yes_9999notknown <- factor(data_original_table$Psychiatric_disease_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Psychiatric_disease_0no_1yes_9999notknown) <- "Psychiatric disease, n(%)"
####Smoking history
data_original_table$Smoking_0never_1ex_2current <- factor(data_original_table$Smoking_0never_1ex_2current, levels=c(0,1,2), labels=c("Never smoked","Ex-smoker", "Current smoker"))
label(data_original_table$Smoking_0never_1ex_2current) <- "Smoking history, n(%)"
####On ICS
data_original_table$Any_ICS_prescribed_0no_1yes <- factor(data_original$Any_ICS_prescribed_0no_1yes, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Any_ICS_prescribed_0no_1yes) <- "On ICS, n(%)"
####ICS Dose
data_original_table$ICS_DOSE_CLASS <- factor(data_original_table$ICS_DOSE_CLASS, levels=c("0","Low","Medium","High"))
label(data_original_table$ICS_DOSE_CLASS) <- "ICS Dose, n(%)"
####On SABA
data_original_table$SABA_prescribed__0no_1yes <- factor(data_original_table$SABA_prescribed__0no_1yes, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$SABA_prescribed__0no_1yes) <- "On SABA, n(%)"
####On SABA actuation
label(data_original_table$SABA_actuations_per_day_average_PreTrial) <- "SABA actuations per day pre trial"
label(data_original_table$SABA_actuations_per_day_average_InTrial) <- "SABA actuations per day in trial"
####On mOCS
data_original_table$maintenance_OCS_prescribed__0no_1yes <- factor(data_original_table$maintenance_OCS_prescribed__0no_1yes, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$maintenance_OCS_prescribed__0no_1yes) <- "On mOCS, n(%)"
####On intranasal ICS
data_original_table$Intranasal_seroid_prescribed__0no_1yes <- factor(data_original_table$Intranasal_seroid_prescribed__0no_1yes, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Intranasal_seroid_prescribed__0no_1yes) <- "On intranasal ICS, n(%)"
####On LABA
data_original_table$LABA_prescribed_0no_1yes <- factor(data_original_table$LABA_prescribed_0no_1yes, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$LABA_prescribed_0no_1yes) <- "On LABA, n(%)"
####On Montelukast
data_original_table$Montelukast_prescribed__0no_1yes <- factor(data_original_table$Montelukast_prescribed__0no_1yes, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Montelukast_prescribed__0no_1yes) <- "On Montelukast, n(%)"
####On LAMA
data_original_table$LAMA_prescribed__0no_1yes <- factor(data_original_table$LAMA_prescribed__0no_1yes, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$LAMA_prescribed__0no_1yes) <- "On LAMA, n(%)"
####Adherence
label(data_original_table$Adherence_PreTrial_quantity) <- "Adherence pre-trial"
label(data_original_table$Adherence_InTrial_quantity) <- "Adherence in trial"
####Attack history: severe exacerbation or hospitalisation in past 12 months
data_original_table$Any_attack_or_hospitalization_previous_12_months <- factor(data_original_table$Any_attack_or_hospitalization_previous_12_months, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Any_attack_or_hospitalization_previous_12_months) <- "Attack history: severe exacerbation or hospitalisation in past 12 months, n(%)"
####Attack history: Number of severe exacerbations in past 12 months
label(data_original_table$Number_severe_attack_previous_12m) <- "Attack history: Number of severe exacerbations in past 12 months, n(%)"
####Attack history: Number of hospitalisation in past 12 months
label(data_original_table$Number_hospitalisations_for_asthma_previous_12_months) <- "Attack history: Number of hospitalisation in past 12 months"
####Follow-up duration (days)
label(data_original_table$Follow_up_duration_days_nozero) <- "Follow-up duration (days)"

##Identify the data which the only available data is in categories (for age and BMI)
data_original_table<- data_original_table %>% 
  mutate(Age_format = case_when(
    Age_cat=="NA" ~ NA,
    !is.na(Age) ~ "Continuous",
    TRUE ~ "In categories"))
data_original_table$Age_format <- factor(data_original_table$Age_format, levels=c("Continuous","In categories"))

data_original_table<- data_original_table %>% 
  mutate(BMI_format = case_when(
    BMI_cat=="NA" ~ NA,
    !is.na(BMI) ~ "Continuous",
    TRUE ~ "In categories"))
data_original_table$BMI_format <- factor(data_original_table$BMI_format, levels=c("Continuous","In categories"))

##Create the groups for the data analyzed by group (FEV1, ACQ-5, In trial severe exacerbations)
###Group of FEV1
data_original_table$FEV1_PCT_reversibility_postBD_by_group<-cut(data_original_table$FEV1_reversibility_percent_postBD_real,breaks = c(-1000,12,1000),labels=c("<12%","⩾12%"))
label(data_original_table$FEV1_PCT_reversibility_postBD_by_group) <- "FEV1 reversibility (by group)"
###Group of ACQ-5
data_original_table$ACQ_baseline_score_mean_by_group<-cut(data_original_table$ACQ_baseline_score_mean,breaks = c(-1000,1.5,1000),labels=c("<1.5","⩾1.5%"))
label(data_original_table$ACQ_baseline_score_mean_by_group) <- "ACQ-5 (by group)"

###Group of In trial severe exacerbations
data_original_table$Exacerbations_during_follow_up_by_group<-cut(data_original_table$Number_severe_asthma_attacks_during_followup,breaks = c(-1000,0.9,1000),labels=c("0","⩾1"))
label(data_original_table$Exacerbations_during_follow_up_by_group) <- "In trial severe exacerbations, n(%)"

#CREATING of CRSwNP
data_original_table<- data_original_table %>% 
  mutate(CRSwNP=case_when(
    #Identifying as "Yes" for patients with NP
    Nasal_polyposis_0no_1yes_9999notknown=="Yes"~ "Yes",
    #Identifying as "Yes" for patients with polypectomy history
    Previous_nasal_polypectomy_0no_1yes_9999notknown =="Yes"~ "Yes",
    #Identifying as "No" for patients without NP
    Nasal_polyposis_0no_1yes_9999notknown=="No" ~ "No",
    #Identifying as NA for patients that NP was not assessed
    is.na(Nasal_polyposis_0no_1yes_9999notknown) ~ NA,
    TRUE ~ NA
  ))
data_original_table$CRSwNP <- factor(data_original_table$CRSwNP, levels=c("Yes","No"), labels=c("Yes","No"))

##CRSsNP (No CRSwNP + No history of nasal polypectomy + CRS=="Yes")
data_original_table<- data_original_table %>% 
  mutate(CRSsNP=case_when(
    #Identifying as NA for patients that nasal polyposis was not assessed
    is.na(CRSwNP)~ NA,
    #Identifying as NA for patients that Chronic_Rhinosinusitis was not assessed
    is.na(Chronic_Rhinosinusitis_0no_1yes_9999notknown) ~ NA,
    #Identifying as "Yes" for patients with CRSwNP
    CRSwNP=="Yes"~ "No",
    #Identifying as "No" for patients without Chronic rhinosinusitis
    Chronic_Rhinosinusitis_0no_1yes_9999notknown=="No" ~ "No",
    #Identifying as "Yes" for patients with Chronic rhinosinusitis
    Chronic_Rhinosinusitis_0no_1yes_9999notknown=="Yes" ~ "Yes",
    TRUE ~ NA
  ))
data_original_table$CRSsNP <- factor(data_original_table$CRSsNP, levels=c("Yes","No"), labels=c("Yes","No"))


# Create function for continuous and categorical variables
my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x, ), digits = 3), c("", "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD), "Median (IQR)"=sprintf(paste("%s (%s - %s)"), MEDIAN, Q1, Q3),"Geo. mean (GSD)"=sprintf("%s (&plusmn; %s)", GMEAN, GSD),"Geo. mean (IQR)"=sprintf(paste("%s (%s - %s)"), GMEAN, Q1, Q3), "Range"=sprintf("%s - %s", MIN, MAX)))}

my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y, sprintf("%d (%0.0f%%)", FREQ, PCT))))}
colnames(data_original_table)
# Create the tables 1 by trial
Table1_by_trial <- table1(~ Age+ Age_format + Gender_0Female_1Male +BMI+BMI_imputated+BMI_imputated_no_systematically_missing+BMI_format +Smoking_0never_1ex_2current+ Ethnicity +Region +Atopy_history_0no_1yes_9999notknown+Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown+Eczema_0no_1yes_9999notknown+AllergicRhinitis__0no_1yes_9999notknown+Chronic_Rhinosinusitis_0no_1yes_9999notknown+Nasal_polyposis_0no_1yes_9999notknown+Previous_nasal_polypectomy_0no_1yes_9999notknown+CRSsNP+CRSwNP+Psychiatric_disease_0no_1yes_9999notknown+Any_ICS_prescribed_0no_1yes+ICS_DOSE_CLASS+SABA_prescribed__0no_1yes+SABA_actuations_per_day_average_PreTrial+SABA_actuations_per_day_average_InTrial+maintenance_OCS_prescribed__0no_1yes+Intranasal_seroid_prescribed__0no_1yes+LABA_prescribed_0no_1yes+LAMA_prescribed__0no_1yes+Montelukast_prescribed__0no_1yes+Adherence_PreTrial_quantity+Adherence_InTrial_quantity_real+ Treatment_step+ACQ_baseline_score_mean+ACQ_baseline_score_mean_by_group+Any_attack_or_hospitalization_previous_12_months+Number_severe_attack_previous_12m+Number_hospitalisations_for_asthma_previous_12_months+Previous_ICU_or_intubation_0no_1yes+Previous_ICU_0no_1yes_9999notknown+Previous_Intubation_0no_1yes_9999notknown+FEV1_preBD_PCT_Baseline+FEV1_FVC_ratio+FEV1_reversibility_percent_postBD_real+FEV1_PCT_reversibility_postBD_by_group+Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced+FeNO_baseline_ppb+Total_IgE+Total_IgE_imputated+Total_IgE_imputated_no_systematically_missing+Follow_up_duration_days_nozero+Exacerbations_during_follow_up_by_group  # add the variables you want
                          | Enrolled_Trial_name, # the | means  "by"
                          overall=c(left="Total"), # add overall= if you want the total number to be included
                          data=subset(data_original_table,.imp==1),
                          render.continuous=my.render.cont, 
                          render.categorical=my.render.cat)

# Other information for required Table 1:follow up duration and number asthma attack total and by study, 
## Total follow-up and number of severe asthma attack
table_total <- data.frame(c(sum(data_original_table$Follow_up_duration_years_nozero, na.rm=TRUE),sum(data_original_table$Number_severe_asthma_attacks_during_followup, na.rm=TRUE)))
rownames(table_total)<-c("Follow up duration (year)","Nb of asthma attack during follow-up")
colnames(table_total)<-"Total"

##Follow-up duration in year
sum(data_original$Follow_up_duration_years_nozero, na.rm=TRUE)
table_sum_follow_up<- aggregate(data_original_table$Follow_up_duration_years_nozero, by=list(Category=data_original_table$Enrolled_Trial_name), FUN=sum)
table_sum_follow_up<- t(table_sum_follow_up)
colnames(table_sum_follow_up)<-table_sum_follow_up[1,]
table_sum_follow_up<-table_sum_follow_up[2,]

##Number of severe asthma attack
table_sum_asthma_attack<- aggregate(data_original_table$Number_severe_asthma_attacks_during_followup, by=list(Category=data_original_table$Enrolled_Trial_name), FUN=sum)
table_sum_asthma_attack<- t(table_sum_asthma_attack)
colnames(table_sum_asthma_attack)<-table_sum_asthma_attack[1,]
table_sum_asthma_attack<-table_sum_asthma_attack[2,]
##Table with the total information
table_sum_follow_up_and_asthma_attack <- rbind(table_sum_follow_up,table_sum_asthma_attack)
rownames(table_sum_follow_up_and_asthma_attack)<- c("Follow up duration (year)","Nb of asthma attack during follow-up")
table_sum_follow_up_and_asthma_attack <- cbind(table_total,table_sum_follow_up_and_asthma_attack)
table_sum_follow_up_and_asthma_attack
#Export the table for follow-up duration and asthma attack sum
write_xlsx(table_sum_follow_up_and_asthma_attack,"table_sum_follow_up_and_attack.xlsx")
#======================================================================================================================================
#======================================================================================================================================
#======================================================================================================================================
#======================================================================================================================================
#PART C = PREPARING THE IMPUTATED DATASET (identification of variable, calculation of variables and creation of category)
## Identify the categorical data
Data_Oracle<-All_data %>% 
  #Gender
  mutate(Gender_0Female_1Male= case_when(Gender_0Female_1Male ==0 ~ "Female", Gender_0Female_1Male ==1 ~ "Male",TRUE ~ NA)) %>% 
  mutate(Gender_0Female_1Male_imputated= case_when(Gender_0Female_1Male_imputated ==0 ~ "Female", Gender_0Female_1Male_imputated ==1 ~ "Male",TRUE ~ NA)) %>% 
  mutate(Gender_0Female_1Male_imputated_no_systematically_missing= case_when(Gender_0Female_1Male_imputated_no_systematically_missing ==0 ~ "Female", Gender_0Female_1Male_imputated_no_systematically_missing ==1 ~ "Male",TRUE ~ NA)) %>% 
  #Smoking
  mutate(Smoking_0never_1ex_2current= case_when(Smoking_0never_1ex_2current ==0 ~ "Never", Smoking_0never_1ex_2current ==1 ~ "Yes (current or ex)",Smoking_0never_1ex_2current ==2 ~ "Yes (current or ex)",TRUE ~ NA)) %>% 
  mutate(Smoking_0never_1ex_2current_imputated= case_when(Smoking_0never_1ex_2current_imputated ==0 ~ "Never", Smoking_0never_1ex_2current_imputated ==1 ~ "Yes (current or ex)",Smoking_0never_1ex_2current_imputated ==2 ~ "Yes (current or ex)",TRUE ~ NA)) %>% 
  mutate(Smoking_0never_1ex_2current_imputated_no_systematically_missing= case_when(Smoking_0never_1ex_2current_imputated_no_systematically_missing ==0 ~ "Never", Smoking_0never_1ex_2current_imputated_no_systematically_missing ==1 ~ "Yes (current or ex)",Smoking_0never_1ex_2current_imputated_no_systematically_missing ==2 ~ "Yes (current or ex)",TRUE ~ NA)) %>% 
  #Atopy
  mutate(Atopy_history_0no_1yes_9999notknown= case_when(Atopy_history_0no_1yes_9999notknown ==0 ~ "No", Atopy_history_0no_1yes_9999notknown ==1 ~ "Yes",TRUE ~ NA)) %>%
  mutate(Atopy_history_0no_1yes_9999notknown_imputated= case_when(Atopy_history_0no_1yes_9999notknown_imputated ==0 ~ "No", Atopy_history_0no_1yes_9999notknown_imputated ==1 ~ "Yes",TRUE ~ NA)) %>%
  mutate(Atopy_history_0no_1yes_9999notknown_imputated_no_systematically_missing= case_when(Atopy_history_0no_1yes_9999notknown_imputated_no_systematically_missing ==0 ~ "No", Atopy_history_0no_1yes_9999notknown_imputated_no_systematically_missing ==1 ~ "Yes",TRUE ~ NA)) %>%
  #Airborne_allergen_sensitisation
  mutate(Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown= case_when(Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown ==0 ~ "No", Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown ==1 ~ "Yes",TRUE ~ NA)) %>% 
  mutate(Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown_imputated= case_when(Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown_imputated ==0 ~ "No", Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown_imputated ==1 ~ "Yes",TRUE ~ NA)) %>% 
  mutate(Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown_imputated_no_systematically_missing= case_when(Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown_imputated_no_systematically_missing ==0 ~ "No", Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown_imputated_no_systematically_missing ==1 ~ "Yes",TRUE ~ NA)) %>% 
  #Eczema
  mutate(Eczema_0no_1yes_9999notknown= case_when(Eczema_0no_1yes_9999notknown ==0 ~ "No", Eczema_0no_1yes_9999notknown ==1 ~ "Yes",TRUE ~ NA)) %>%
  mutate(Eczema_0no_1yes_9999notknown_imputated= case_when(Eczema_0no_1yes_9999notknown_imputated ==0 ~ "No", Eczema_0no_1yes_9999notknown_imputated ==1 ~ "Yes",TRUE ~ NA)) %>%
  mutate(Eczema_0no_1yes_9999notknown_imputated_no_systematically_missing= case_when(Eczema_0no_1yes_9999notknown_imputated_no_systematically_missing ==0 ~ "No", Eczema_0no_1yes_9999notknown_imputated_no_systematically_missing ==1 ~ "Yes",TRUE ~ NA)) %>%
  #Allergic rhinitis
  mutate(AllergicRhinitis__0no_1yes_9999notknown= case_when(AllergicRhinitis__0no_1yes_9999notknown ==0 ~ "No", AllergicRhinitis__0no_1yes_9999notknown ==1 ~ "Yes",TRUE ~ NA)) %>% 
  mutate(AllergicRhinitis__0no_1yes_9999notknown_imputated= case_when(AllergicRhinitis__0no_1yes_9999notknown_imputated ==0 ~ "No", AllergicRhinitis__0no_1yes_9999notknown_imputated ==1 ~ "Yes",TRUE ~ NA)) %>% 
  mutate(AllergicRhinitis__0no_1yes_9999notknown_imputated_no_systematically_missing= case_when(AllergicRhinitis__0no_1yes_9999notknown_imputated_no_systematically_missing ==0 ~ "No", AllergicRhinitis__0no_1yes_9999notknown_imputated_no_systematically_missing ==1 ~ "Yes",TRUE ~ NA)) %>% 
  #Chronic rhinosinusitis
  mutate(Chronic_Rhinosinusitis_0no_1yes_9999notknown= case_when(Chronic_Rhinosinusitis_0no_1yes_9999notknown ==0 ~ "No", Chronic_Rhinosinusitis_0no_1yes_9999notknown ==1 ~ "Yes",TRUE ~ NA)) %>% 
  mutate(Chronic_Rhinosinusitis_0no_1yes_9999notknown_imputated= case_when(Chronic_Rhinosinusitis_0no_1yes_9999notknown_imputated ==0 ~ "No", Chronic_Rhinosinusitis_0no_1yes_9999notknown_imputated ==1 ~ "Yes",TRUE ~ NA)) %>% 
  mutate(Chronic_Rhinosinusitis_0no_1yes_9999notknown_imputated_no_systematically_missing= case_when(Chronic_Rhinosinusitis_0no_1yes_9999notknown_imputated_no_systematically_missing ==0 ~ "No", Chronic_Rhinosinusitis_0no_1yes_9999notknown_imputated_no_systematically_missing ==1 ~ "Yes",TRUE ~ NA)) %>% 
  #Nasal polyposis
  mutate(Nasal_polyposis_0no_1yes_9999notknown= case_when(Nasal_polyposis_0no_1yes_9999notknown ==0 ~ "No", Nasal_polyposis_0no_1yes_9999notknown ==1 ~ "Yes",TRUE ~ NA)) %>% 
  mutate(Nasal_polyposis_0no_1yes_9999notknown_imputated= case_when(Nasal_polyposis_0no_1yes_9999notknown_imputated ==0 ~ "No", Nasal_polyposis_0no_1yes_9999notknown_imputated ==1 ~ "Yes",TRUE ~ NA)) %>% 
  mutate(Nasal_polyposis_0no_1yes_9999notknown_imputated_no_systematically_missing= case_when(Nasal_polyposis_0no_1yes_9999notknown_imputated_no_systematically_missing ==0 ~ "No", Nasal_polyposis_0no_1yes_9999notknown_imputated_no_systematically_missing ==1 ~ "Yes",TRUE ~ NA)) 


colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Gender_0Female_1Male" )]<- "Gender"
Data_Oracle$Gender<-as.factor(Data_Oracle$Gender)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Gender_0Female_1Male_imputated" )]<- "Gender_imputated"
Data_Oracle$Gender_imputated<-as.factor(Data_Oracle$Gender_imputated)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Gender_0Female_1Male_imputated_no_systematically_missing" )]<- "Gender_imputated_no_systematically_missing"
Data_Oracle$Gender_imputated_no_systematically_missing<-as.factor(Data_Oracle$Gender_imputated_no_systematically_missing)

colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Smoking_0never_1ex_2current" )]<- "Smoking_Statut"
Data_Oracle$Smoking_Statut<-as.factor(Data_Oracle$Smoking_Statut)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Smoking_0never_1ex_2current_imputated" )]<- "Smoking_Statut_imputated"
Data_Oracle$Smoking_Statut_imputated<-as.factor(Data_Oracle$Smoking_Statut_imputated)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Smoking_0never_1ex_2current_imputated_no_systematically_missing" )]<- "Smoking_Statut_imputated_no_systematically_missing"
Data_Oracle$Smoking_Statut_imputated_no_systematically_missing<-as.factor(Data_Oracle$Smoking_Statut_imputated_no_systematically_missing)

colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Atopy_history_0no_1yes_9999notknown" )]<- "Atopy_history"
Data_Oracle$Atopy_history<-as.factor(Data_Oracle$Atopy_history)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Atopy_history_0no_1yes_9999notknown_imputated" )]<- "Atopy_history_imputated"
Data_Oracle$Atopy_history_imputated<-as.factor(Data_Oracle$Atopy_history_imputated)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Atopy_history_0no_1yes_9999notknown_imputated_no_systematically_missing" )]<- "Atopy_history_imputated_no_systematically_missing"
Data_Oracle$Atopy_history_imputated_no_systematically_missing<-as.factor(Data_Oracle$Atopy_history_imputated_no_systematically_missing)

colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown" )]<- "Airborne_allergen_sensibilisation"
Data_Oracle$Airborne_allergen_sensibilisation<-as.factor(Data_Oracle$Airborne_allergen_sensibilisation)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown_imputated" )]<- "Airborne_allergen_sensibilisation_imputated"
Data_Oracle$Airborne_allergen_sensibilisation_imputated<-as.factor(Data_Oracle$Airborne_allergen_sensibilisation_imputated)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown_imputated_no_systematically_missing" )]<- "Airborne_allergen_sensibilisation_imputated_no_systematically_missing"
Data_Oracle$Airborne_allergen_sensibilisation_imputated_no_systematically_missing<-as.factor(Data_Oracle$Airborne_allergen_sensibilisation_imputated_no_systematically_missing)

colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Eczema_0no_1yes_9999notknown" )]<- "Eczema"
Data_Oracle$Eczema<-as.factor(Data_Oracle$Eczema)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Eczema_0no_1yes_9999notknown_imputated" )]<- "Eczema_imputated"
Data_Oracle$Eczema_imputated<-as.factor(Data_Oracle$Eczema_imputated)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Eczema_0no_1yes_9999notknown_imputated_no_systematically_missing" )]<- "Eczema_imputated_no_systematically_missing"
Data_Oracle$Eczema_imputated_no_systematically_missing<-as.factor(Data_Oracle$Eczema_imputated_no_systematically_missing)

colnames(Data_Oracle)[which(colnames(Data_Oracle)=="AllergicRhinitis__0no_1yes_9999notknown" )]<- "Allergic_rhinitis"
Data_Oracle$Allergic_rhinitis<-as.factor(Data_Oracle$Allergic_rhinitis)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="AllergicRhinitis__0no_1yes_9999notknown_imputated" )]<- "Allergic_rhinitis_imputated"
Data_Oracle$Allergic_rhinitis_imputated<-as.factor(Data_Oracle$Allergic_rhinitis_imputated)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="AllergicRhinitis__0no_1yes_9999notknown_imputated_no_systematically_missing" )]<- "Allergic_rhinitis_imputated_no_systematically_missing"
Data_Oracle$Allergic_rhinitis_imputated_no_systematically_missing<-as.factor(Data_Oracle$Allergic_rhinitis_imputated_no_systematically_missing)

colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Chronic_Rhinosinusitis_0no_1yes_9999notknown" )]<- "Chronic_rhinosinusitis"
Data_Oracle$Chronic_rhinosinusitis<-as.factor(Data_Oracle$Chronic_rhinosinusitis)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Chronic_Rhinosinusitis_0no_1yes_9999notknown_imputated" )]<- "Chronic_rhinosinusitis_imputated"
Data_Oracle$Chronic_rhinosinusitis_imputated<-as.factor(Data_Oracle$Chronic_rhinosinusitis_imputated)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Chronic_Rhinosinusitis_0no_1yes_9999notknown_imputated_no_systematically_missing" )]<- "Chronic_rhinosinusitis_imputated_no_systematically_missing"
Data_Oracle$Chronic_rhinosinusitis_imputated_no_systematically_missing<-as.factor(Data_Oracle$Chronic_rhinosinusitis_imputated_no_systematically_missing)

colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Nasal_polyposis_0no_1yes_9999notknown" )]<- "Nasal_polyposis"
Data_Oracle$Nasal_polyposis<-as.factor(Data_Oracle$Nasal_polyposis)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Nasal_polyposis_0no_1yes_9999notknown_imputated" )]<- "Nasal_polyposis_imputated"
Data_Oracle$Nasal_polyposis_imputated<-as.factor(Data_Oracle$Nasal_polyposis_imputated)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Nasal_polyposis_0no_1yes_9999notknown_imputated_no_systematically_missing" )]<- "Nasal_polyposis_imputated_no_systematically_missing"
Data_Oracle$Nasal_polyposis_imputated_no_systematically_missing<-as.factor(Data_Oracle$Nasal_polyposis_imputated_no_systematically_missing)

## Modify the name of columns for clear names
Data_Oracle<-Data_Oracle %>% 
  mutate(Eosinophils_Log=log10(Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced))
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced_imputated" )]<- "Eosinophils_Log_imputated"
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced_imputated_no_systematically_missing" )]<- "Eosinophils_Log_imputated_no_systematically_missing"

Data_Oracle<-Data_Oracle %>% 
  mutate(FeNO_Log=log10(FeNO_baseline_ppb))
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="FeNO_baseline_ppb_imputated" )]<- "FeNO_Log_imputated"
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="FeNO_baseline_ppb_imputated_no_systematically_missing" )]<- "FeNO_Log_imputated_no_systematically_missing"

Data_Oracle<-Data_Oracle %>% 
  mutate(IgE_Log=log10(Total_IgE))
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Total_IgE_imputated")]<- "IgE_Log_imputated"
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Total_IgE_imputated_no_systematically_missing")]<- "IgE_Log_imputated_no_systematically_missing"

colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Number_severe_attack_previous_12m_con"  )]<- "Attack_12mo_Nb"
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Number_severe_attack_previous_12m_con_imputated"  )]<- "Attack_12mo_Nb_imputated"
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Number_severe_attack_previous_12m_con_imputated_no_systematically_missing"  )]<- "Attack_12mo_Nb_imputated_no_systematically_missing"

colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Number_hospitalisations_for_asthma_previous_12_months_con"  )]<- "Hospitalisations_12mo_Nb"
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Number_hospitalisations_for_asthma_previous_12_months_con_imputated"  )]<- "Hospitalisations_12mo_Nb_imputated"
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Number_hospitalisations_for_asthma_previous_12_months_con_imputated_no_systematically_missing"  )]<- "Hospitalisations_12mo_Nb_imputated_no_systematically_missing"
#======================================================================================================================================
#Calculation of the continuous variable 
##Calculation of predicted spirometric parameters according to %FEV1 or %FVC when the FEV1_predicted_L was not given
Data_Oracle<-Data_Oracle%>% 
  mutate(FEV1_predicted_L= case_when(
    is.na(FEV1_predicted_L) & !is.na(FEV1_preBD_L_Baseline) & !is.na(FEV1_preBD_PCT_Baseline) ~ (100*FEV1_preBD_L_Baseline)/FEV1_preBD_PCT_Baseline,
    is.na(FEV1_predicted_L) & !is.na(FEV1_postBD_L_Baseline) & !is.na(FEV1_postBD_PCT_Baseline) ~ (100*FEV1_postBD_L_Baseline)/FEV1_postBD_PCT_Baseline,
    TRUE ~ FEV1_predicted_L
  )) %>% 
  mutate(FVC_predicted_L= case_when(
    is.na(FVC_predicted_L) & !is.na(FVC_preBD_L_Baseline) & !is.na(FVC_preBD_PCT_Baseline) ~ (100*FVC_preBD_L_Baseline)/FVC_preBD_PCT_Baseline,
    is.na(FVC_predicted_L) & !is.na(FVC_postBD_L_Baseline) & !is.na(FVC_preBD_PCT_Baseline) ~ (100*FVC_postBD_L_Baseline)/FVC_preBD_PCT_Baseline,
    TRUE ~ FVC_predicted_L
  )) 

## Calculation FEV1/FVC
Data_Oracle<-Data_Oracle%>% 
  mutate(Tiffeneau=FEV1_preBD_L_Baseline/FVC_preBD_L_Baseline)%>% 
  mutate(Tiffeneau_imputated=FEV1_preBD_L_Baseline_imputated/FVC_preBD_L_Baseline_imputated)%>% 
  mutate(Tiffeneau_imputated_no_systematically_missing=FEV1_preBD_L_Baseline_imputated_no_systematically_missing/FVC_preBD_L_Baseline_imputated_no_systematically_missing)

## Calculate the value in absolute for eosinophils, FeNO and IgE
Data_Oracle<-Data_Oracle %>% 
  mutate(Blood_Eos_baseline_x10_9_cells_per_L_imputated=10^Eosinophils_Log_imputated) %>% 
  mutate(Blood_Eos_baseline_x10_9_cells_per_L_imputated_no_systematically_missing=10^Eosinophils_Log_imputated_no_systematically_missing) %>% 
  
  mutate(FeNO_baseline_ppb_imputated=10^FeNO_Log_imputated)%>% 
  mutate(FeNO_baseline_ppb_imputated_no_systematically_missing=10^FeNO_Log_imputated_no_systematically_missing)%>% 
  
  mutate(Total_IgE_imputated=10^IgE_Log_imputated) %>% 
  mutate(Total_IgE_imputated_no_systematically_missing=10^IgE_Log_imputated_no_systematically_missing)

##Put the Follow up duration in days
Data_Oracle<-Data_Oracle %>% 
  mutate(Follow_up_duration_days=Data_Oracle$Follow_up_duration_days_nozero)
#======================================================================================================================================
## Calculation of the parameters by a definite change
Data_Oracle<-Data_Oracle%>% 
  #Age per 10 year increase
  mutate(Age_per_10=Age/10) %>%
  mutate(Age_per_10_imputated=Age_imputated/10) %>%
  mutate(Age_per_10_imputated_no_systematically_missing=Age_imputated_no_systematically_missing/10) %>%
  #BMI per 5 increase
  mutate(BMI_per_5=BMI/5) %>% 
  mutate(BMI_per_5_imputated=BMI_imputated/5) %>% 
  mutate(BMI_per_5_imputated_no_systematically_missing=BMI_imputated_no_systematically_missing/5) %>% 
  #FEV1 per 10% decrease
  mutate(FEV1_preBD_per10_Baseline=-FEV1_preBD_PCT_Baseline/10) %>%
  mutate(FEV1_preBD_per10_Baseline_imputated=-FEV1_preBD_PCT_Baseline_imputated/10) %>%
  mutate(FEV1_preBD_per10_Baseline_imputated_no_systematically_missing=-FEV1_preBD_PCT_Baseline_imputated_no_systematically_missing/10) %>%
  #Reversibility per 10%
  mutate(FEV1_per10_reversibilityBD= FEV1_PCT_reversibility_postBD/10) %>% 
  mutate(FEV1_per10_reversibilityBD_imputated= FEV1_PCT_reversibility_postBD_imputated/10) %>% 
  mutate(FEV1_per10_reversibilityBD_imputated_no_systematically_missing= FEV1_PCT_reversibility_postBD_imputated_no_systematically_missing/10)

### Calculate the value standardize by percentile 25 and 75 
FeNO_delta_75_25<- quantile(Data_Oracle$FeNO_Log_imputated)[4]-quantile(Data_Oracle$FeNO_Log_imputated)[2]   # Display quantiles for FeNO in the first dataset
BEC_delta_75_25<- quantile(Data_Oracle$Eosinophils_Log_imputated)[4]-quantile(Data_Oracle$Eosinophils_Log_imputated)[2]   # Display quantiles for FeNO in the first dataset

### Scaling FeNO based on the 25th and 75th percentiles (method by Prof. Frank Harrell)
Data_Oracle <- Data_Oracle %>%
  mutate(FeNO_p_imputated = FeNO_Log_imputated / FeNO_delta_75_25)  # Scale log FeNO by dividing by 0.47 (between the 25th and 75th percentiles)
Data_Oracle <- Data_Oracle %>%
  mutate(BEC_p_imputated = Eosinophils_Log_imputated / BEC_delta_75_25)  # Scale log FeNO by dividing by 0.47 (between the 25th and 75th percentiles)
#======================================================================================================================================
#Creation of new categoricals data
##Create the CRsNP factor (CRS without NP)

#CREATING of CRSwNP
Data_Oracle <- Data_Oracle  %>% 
  mutate(CRSwNP=case_when(
    #Identifying as "Yes" for patients with NP
    Nasal_polyposis=="Yes"~ "Yes",
    #Identifying as "Yes" for patients with polypectomy history
    Previous_nasal_polypectomy_0no_1yes_9999notknown =="Yes"~ "Yes",
    #Identifying as "No" for patients without NP
    Nasal_polyposis=="No" ~ "No",
    #Identifying as NA for patients that NP was not assessed
    is.na(Nasal_polyposis) ~ NA,
    TRUE ~ NA
  )) %>% 
  mutate(CRSwNP_imputated=case_when(
    #Identifying as "Yes" for patients with NP
    Nasal_polyposis_imputated=="Yes"~ "Yes",
    #Identifying as "Yes" for patients with polypectomy history
    Previous_nasal_polypectomy_0no_1yes_9999notknown_imputated =="Yes"~ "Yes",
    #Identifying as "No" for patients without NP
    Nasal_polyposis_imputated=="No" ~ "No",
    #Identifying as NA for patients that NP was not assessed
    is.na(Nasal_polyposis_imputated) ~ NA,
    TRUE ~ NA
  )) %>% 
  mutate(CRSwNP_imputated_no_systematically_missing=case_when(
    #Identifying as "Yes" for patients with NP
    Nasal_polyposis_imputated_no_systematically_missing=="Yes"~ "Yes",
    #Identifying as "Yes" for patients with polypectomy history
    Previous_nasal_polypectomy_0no_1yes_9999notknown_imputated_no_systematically_missing =="Yes"~ "Yes",
    #Identifying as "No" for patients without NP
    Nasal_polyposis_imputated_no_systematically_missing=="No" ~ "No",
    #Identifying as NA for patients that NP was not assessed
    is.na(Nasal_polyposis_imputated_no_systematically_missing) ~ NA,
    TRUE ~ NA
  ))

Data_Oracle$CRSwNP <- factor(Data_Oracle$CRSwNP, levels=c("No","Yes"), labels=c("No","Yes"))
Data_Oracle$CRSwNP_imputated <- factor(Data_Oracle$CRSwNP_imputated, levels=c("No","Yes"), labels=c("No","Yes"))
Data_Oracle$CRSwNP_imputated_no_systematically_missing <- factor(Data_Oracle$CRSwNP_imputated_no_systematically_missing, levels=c("No","Yes"), labels=c("No","Yes"))

##CRSsNP (No CRSwNP + No history of nasal polypectomy + CRS=="Yes")
Data_Oracle<- Data_Oracle %>% 
  mutate(CRSsNP=case_when(
    #Identifying as NA for patients that nasal polyposis was not assessed
    is.na(CRSwNP)~ NA,
    #Identifying as NA for patients that Chronic_Rhinosinusitis was not assessed
    is.na(Chronic_rhinosinusitis) ~ NA,
    #Identifying as "Yes" for patients with CRSwNP
    CRSwNP=="Yes"~ "No",
    #Identifying as "No" for patients without Chronic rhinosinusitis
    Chronic_rhinosinusitis=="No" ~ "No",
    #Identifying as "Yes" for patients with Chronic rhinosinusitis
    Chronic_rhinosinusitis=="Yes" ~ "Yes",
    TRUE ~ NA
  ))%>% 
  mutate(CRSsNP_imputated=case_when(
    #Identifying as NA for patients that nasal polyposis was not assessed
    is.na(CRSwNP_imputated)~ NA,
    #Identifying as NA for patients that Chronic_Rhinosinusitis was not assessed
    is.na(Chronic_rhinosinusitis_imputated) ~ NA,
    #Identifying as "Yes" for patients with CRSwNP
    CRSwNP_imputated=="Yes"~ "No",
    #Identifying as "No" for patients without Chronic rhinosinusitis
    Chronic_rhinosinusitis_imputated=="No" ~ "No",
    #Identifying as "Yes" for patients with Chronic rhinosinusitis
    Chronic_rhinosinusitis_imputated=="Yes" ~ "Yes",
    TRUE ~ NA
  ))%>% 
  mutate(CRSsNP_imputated_no_systematically_missing=case_when(
    #Identifying as NA for patients that nasal polyposis was not assessed
    is.na(CRSwNP_imputated_no_systematically_missing)~ NA,
    #Identifying as NA for patients that Chronic_Rhinosinusitis was not assessed
    is.na(Chronic_rhinosinusitis_imputated_no_systematically_missing) ~ NA,
    #Identifying as "Yes" for patients with CRSwNP
    CRSwNP_imputated_no_systematically_missing=="Yes"~ "No",
    #Identifying as "No" for patients without Chronic rhinosinusitis
    Chronic_rhinosinusitis_imputated_no_systematically_missing=="No" ~ "No",
    #Identifying as "Yes" for patients with Chronic rhinosinusitis
    Chronic_rhinosinusitis_imputated_no_systematically_missing=="Yes" ~ "Yes",
    TRUE ~ NA
  ))
Data_Oracle$CRSsNP <- factor(Data_Oracle$CRSsNP, levels=c("No","Yes"), labels=c("No","Yes"))
Data_Oracle$CRSsNP_imputated <- factor(Data_Oracle$CRSsNP_imputated, levels=c("No","Yes"), labels=c("No","Yes"))
Data_Oracle$CRSsNP_imputated_no_systematically_missing <- factor(Data_Oracle$CRSsNP_imputated_no_systematically_missing, levels=c("No","Yes"), labels=c("No","Yes"))

#================================================================================================================================================================================================================================================================================================================================================================================================
Table1_test_by_trial <- table1(~ Chronic_rhinosinusitis+Chronic_rhinosinusitis_imputated_no_systematically_missing+Nasal_polyposis+Nasal_polyposis_imputated_no_systematically_missing+CRSsNP+CRSsNP_imputated_no_systematically_missing+CRSwNP+CRSwNP_imputated_no_systematically_missing
                               | Enrolled_Trial_name, # the | means  "by"
                               overall=c(left="Total"), # add overall= if you want the total number to be included
                               data=subset(Data_Oracle,.imp==1),
                               render.continuous=my.render.cont, 
                               render.categorical=my.render.cat)

##Category by inflammatory marker
Data_Oracle$Eosinophils_by_group <- cut(
  Data_Oracle$Blood_Eos_baseline_x10_9_cells_per_L,
  breaks = c(0, 0.15, 0.3, Inf), # Use Inf for the last group to include all larger values
  labels = c('<0.15', '0.15-0.3', '>=0.3'),
  right = FALSE # Make intervals left-closed and right-open
)
Data_Oracle$Eosinophils_by_group_imputated <- cut(
  Data_Oracle$Blood_Eos_baseline_x10_9_cells_per_L_imputated,
  breaks = c(0, 0.15, 0.3, Inf), # Use Inf for the last group to include all larger values
  labels = c('<0.15', '0.15-0.3', '>=0.3'),
  right = FALSE # Make intervals left-closed and right-open
)

Data_Oracle$Eosinophils_by_group_imputated_no_systematically_missing <- cut(
  Data_Oracle$Blood_Eos_baseline_x10_9_cells_per_L_imputated_no_systematically_missing,
  breaks = c(0, 0.15, 0.3, Inf), # Use Inf for the last group to include all larger values
  labels = c('<0.15', '0.15-0.3', '>=0.3'),
  right = FALSE # Make intervals left-closed and right-open
)

Data_Oracle$FeNO_baseline_by_group <- cut(
  Data_Oracle$FeNO_baseline_ppb,
  breaks = c(0, 25, 50, Inf), # Replace 100000 with Inf to ensure >=50 includes 50 and beyond
  labels = c('<25', '25-<50', '>=50'),
  right = TRUE # Ensure intervals are right-closed
)
Data_Oracle$FeNO_baseline_by_group_imputated <- cut(
  Data_Oracle$FeNO_baseline_ppb_imputated,
  breaks = c(0, 25, 50, Inf), # Replace 100000 with Inf to ensure >=50 includes 50 and beyond
  labels = c('<25', '25-<50', '>=50'),
  right = TRUE # Ensure intervals are right-closed
)

Data_Oracle$FeNO_baseline_by_group_imputated_no_systematically_missing <- cut(
  Data_Oracle$FeNO_baseline_ppb_imputated_no_systematically_missing,
  breaks = c(0, 25, 50, Inf), # Replace 100000 with Inf to ensure >=50 includes 50 and beyond
  labels = c('<25', '25-<50', '>=50'),
  right = TRUE # Ensure intervals are right-closed
)


Data_Oracle$IgE_by_group<-cut(Data_Oracle$Total_IgE,breaks = c(0,150, 600,100000),labels=c('<150', '150-600', '>600'))
Data_Oracle$IgE_by_group_imputated<-cut(Data_Oracle$Total_IgE_imputated,breaks = c(0,150, 600,100000),labels=c('<150', '150-600', '>600'))
Data_Oracle$IgE_by_group_imputated_no_systematically_missing<-cut(Data_Oracle$Total_IgE_imputated_no_systematically_missing,breaks = c(0,150, 600,100000),labels=c('<150', '150-600', '>600'))


##Category by lung function
Data_Oracle$FEV1_preBD_Baseline_by_group<-cut(Data_Oracle$FEV1_preBD_PCT_Baseline,breaks = c(0,50,60,70,100000),right = FALSE,labels=c('<50%',"50-<60%",'60-<70%',">=70%"))

Data_Oracle$FEV1_preBD_Baseline_by_group_imputated<-cut(Data_Oracle$FEV1_preBD_PCT_Baseline_imputated,breaks = c(0,50,60,70,100000),labels=c('<50%',"50-<60%",'60-<70%',">=70%"))
Data_Oracle$FEV1_preBD_Baseline_by_group_imputated_no_systematically_missing<-cut(Data_Oracle$FEV1_preBD_PCT_Baseline_imputated_no_systematically_missing,breaks = c(0,50,60,70,100000),labels=c('<50%',"50-<60%",'60-<70%',">=70%"))

Data_Oracle$FEV1_preBD_Baseline_by_group <- factor(Data_Oracle$FEV1_preBD_Baseline_by_group, levels = c(">=70%","60-<70%","50-<60%","<50%"))
Data_Oracle$FEV1_preBD_Baseline_by_group_imputated <- factor(Data_Oracle$FEV1_preBD_Baseline_by_group_imputated, levels = c(">=70%","60-<70%","50-<60%","<50%"))
Data_Oracle$FEV1_preBD_Baseline_by_group_imputated_no_systematically_missing <- factor(Data_Oracle$FEV1_preBD_Baseline_by_group_imputated_no_systematically_missing, levels = c(">=70%","60-<70%","50-<60%","<50%"))

##Category by GINA_treatment_step
Data_Oracle<-Data_Oracle %>% 
  mutate(Treatment_step= case_when(Treatment_step=="1"~"Step 1",Treatment_step=="2"~"Step 2",Treatment_step=="3"~"Step 3",Treatment_step=="4"~"Step 4",Treatment_step=="5"~"Step 5",TRUE~Treatment_step))

Data_Oracle<-Data_Oracle %>% 
  mutate(Treatment_step_1and2= case_when(Treatment_step=="Step 1"~"Step 1-2",Treatment_step=="Step 2"~"Step 1-2",TRUE~Treatment_step))
Data_Oracle$Treatment_step_1and2<-as.factor(Data_Oracle$Treatment_step_1and2)

Data_Oracle$Treatment_step <- factor(Data_Oracle$Treatment_step, levels = c("Step 3","Step 1","Step 2","Step 4","Step 5"))
Data_Oracle$Treatment_step_1and2 <- factor(Data_Oracle$Treatment_step_1and2, levels = c("Step 4","Step 1-2","Step 3","Step 5"))

##Create a treatment step_1-2_vs_3-4-5
Data_Oracle <- Data_Oracle %>%
  mutate(Treatment_step_1_2vs3_5 = factor(case_when(
    Treatment_step %in% c("Step 1", "Step 2") ~ "Step 1-2",
    Treatment_step %in% c("Step 3", "Step 4", "Step 5") ~ "Step 3-5",
    TRUE ~ Treatment_step  # Keep other values as they are
  )))


##Category by ACQ-5
Data_Oracle$ACQ5_by_group<-cut(Data_Oracle$ACQ_baseline_score_mean,breaks = c(-10,1.5,3,100000),labels=c("<1.5","1.5-3",">3"))
Data_Oracle$ACQ5_by_group_imputated<-cut(Data_Oracle$ACQ_baseline_score_mean_imputated,breaks = c(-10,1.5,3,100000),labels=c("<1.5","1.5-3",">3"))
Data_Oracle$ACQ5_by_group_imputated_no_systematically_missing<-cut(Data_Oracle$ACQ_baseline_score_mean_imputated_no_systematically_missing,breaks = c(-10,1.5,3,100000),labels=c("<1.5","1.5-3",">3"))

##Category by BMI
Data_Oracle$BMI_by_group<-cut(Data_Oracle$BMI,breaks = c(-10,25,30,35,100000),labels=c("<25","25-30","30-35",">35"))
Data_Oracle$BMI_by_group_imputated<-cut(Data_Oracle$BMI_imputated,breaks = c(-10,25,30,35,100000),labels=c("<25","25-30","30-35",">35"))
Data_Oracle$BMI_by_group_imputated_no_systematically_missing<-cut(Data_Oracle$BMI_imputated_no_systematically_missing,breaks = c(-10,25,30,35,100000),labels=c("<25","25-30","30-35",">35"))


##Category by Age group
Data_Oracle$Age_by_group_imputated<-cut(Data_Oracle$Age_imputated,breaks = c(-10,40,50,60,100000),labels=c("<40","40-50","50-60",">60"))

#Category of ICS DOSE_CLASS
Data_Oracle$ICS_DOSE_CLASS <- factor(Data_Oracle$ICS_DOSE_CLASS, 
                                     levels = c("0", "Low", "Medium", "High"))
Data_Oracle$ICS_DOSE_CLASS <- relevel(Data_Oracle$ICS_DOSE_CLASS, ref = "High")
Data_Oracle$ICS_DOSE_NUMERIC <- as.numeric(factor(Data_Oracle$ICS_DOSE_CLASS, 
                                                  levels = c("0", "Low", "Medium", "High"))) - 1

Data_Oracle$ICS_DOSE_CLASS_0_or_Low_combine <- factor(ifelse(Data_Oracle$ICS_DOSE_CLASS %in% c("0", "Low"), 
                                                             "0 or Low", 
                                                             as.character(Data_Oracle$ICS_DOSE_CLASS)),
                                                      levels = c("0 or Low", "Medium", "High"))
#=================================================================================================================================================================================================================================================
#=================================================================================================================================================================================================================================================
#=================================================================================================================================================================================================================================================

#### SCREEN FAILURE ANALYSIS
# First, create a new variable to identify our specific group
Data_Oracle <- Data_Oracle %>%
  mutate(high_FEV1_low_rev_group = case_when(
    FEV1_preBD_PCT_Baseline_imputated >= 80 & 
      FEV1_reversibility_percent_postBD_real < 12 ~ "High FEV1, Low Rev",
    TRUE ~ "Other"
  ))

# Create additional groups for sensitivity analysis
Data_Oracle <- Data_Oracle %>%
  mutate(
    high_FEV1_group = case_when(
      FEV1_preBD_PCT_Baseline_imputated >= 80 ~ "High FEV1 Only",
      TRUE ~ "Not High FEV1"
    ),
    low_rev_group = case_when(
      FEV1_reversibility_percent_postBD_real < 12 ~ "Low Rev Only",
      TRUE ~ "Not Low Rev"
    )
  )

# Convert to factors
Data_Oracle$high_FEV1_low_rev_group <- factor(Data_Oracle$high_FEV1_low_rev_group)
Data_Oracle$high_FEV1_group <- factor(Data_Oracle$high_FEV1_group)
Data_Oracle$low_rev_group <- factor(Data_Oracle$low_rev_group)

# Add follow-up duration in years
Data_Oracle <- Data_Oracle %>%
  mutate(Follow_up_duration_years = Follow_up_duration_days / 365.25)

# Identify trials with at least one "High FEV1, Low Rev" patient
trials_with_high_fev1_low_rev <- Data_Oracle %>%
  filter(high_FEV1_low_rev_group == "High FEV1, Low Rev") %>%
  distinct(Enrolled_Trial_name) %>%
  pull(Enrolled_Trial_name)

# Filter the dataset to include only those trials
Data_Oracle_filtered <- Data_Oracle %>%
  filter(Enrolled_Trial_name %in% trials_with_high_fev1_low_rev)

# Print which trials are included
cat("Included trials:\n")
print(trials_with_high_fev1_low_rev)

# Print how many patients we have now vs. before
cat("\nBefore filtering:", nrow(Data_Oracle[Data_Oracle$.imp == 1,]), "patients\n")
cat("After filtering:", nrow(Data_Oracle_filtered[Data_Oracle_filtered$.imp == 1,]), "patients\n")

#########################################
# PRIMARY ANALYSIS - HIGH FEV1, LOW REV
#########################################

# Explicitly set reference level to "Other" to make interpretation clearer
Data_Oracle_filtered$high_FEV1_low_rev_group <- relevel(Data_Oracle_filtered$high_FEV1_low_rev_group, ref = "Other")

# Run models on all 10 imputations
res_comb <- NULL
for(i in 1:10){
  # Filter data for the current imputation
  data_imp_i <- Data_Oracle_filtered %>% filter(.imp == i)
  
  # Fit the model
  res_comb[[i]] <- glm.nb(Number_severe_asthma_attacks_during_followup ~ 
                            high_FEV1_low_rev_group +
                            ACQ_baseline_score_mean_imputated + 
                            Gender_imputated +
                            Age_imputated +
                            as.factor(Treatment_step) +
                            Number_severe_attack_previous_12m +
                            FEV1_preBD_PCT_Baseline_imputated + 
                            FEV1_reversibility_percent_postBD_real +
                            FEV1_FVC_ratio_imputated +
                            FeNO_Log_imputated + 
                            Eosinophils_Log_imputated +
                            offset(log(Follow_up_duration_days)) + 
                            as.factor(Enrolled_Trial_name), 
                          data = data_imp_i)
}

###############################################
# MANUALLY EXTRACT AND POOL THE COEFFICIENTS
###############################################

# First, check names of coefficients in the first model
coef_names_model1 <- names(coef(res_comb[[1]]))
cat("\nCoefficient names in model 1:\n")
print(coef_names_model1)

# Look for the high_FEV1_low_rev_group coefficient
# The format is typically "high_FEV1_low_rev_groupHigh FEV1, Low Rev"
group_coef_index <- grep("high_FEV1_low_rev_group", coef_names_model1)

if(length(group_coef_index) == 0) {
  stop("Could not find coefficient for high_FEV1_low_rev_group in model 1")
} else {
  group_coef_name <- coef_names_model1[group_coef_index]
  cat("\nFound coefficient:", group_coef_name, "\n")
}

# Extract coefficient values and standard errors from all 10 models
coefs <- numeric(10)
ses <- numeric(10)

for(i in 1:10) {
  # Get the coefficient summary
  coef_summary <- summary(res_comb[[i]])$coefficients
  
  # Extract the coefficient and standard error
  coefs[i] <- coef_summary[group_coef_name, "Estimate"]
  ses[i] <- coef_summary[group_coef_name, "Std. Error"]
}

# Pool the coefficients using Rubin's rules
pooled_coef <- mean(coefs)
between_var <- var(coefs)
within_var <- mean(ses^2)
total_var <- within_var + (1 + 1/10) * between_var
pooled_se <- sqrt(total_var)

# Calculate rate ratio and confidence interval
rate_ratio <- exp(pooled_coef)
rate_ratio_lower <- exp(pooled_coef - 1.96 * pooled_se)
rate_ratio_upper <- exp(pooled_coef + 1.96 * pooled_se)

# Print results for primary analysis
cat("\n=== PRIMARY ANALYSIS: HIGH FEV1 & LOW REV (Pooled Coefficients) ===\n")
cat(paste0("Rate Ratio (High FEV1, Low Rev vs. Other): ", 
           round(rate_ratio, 2), " (95% CI: ", 
           round(rate_ratio_lower, 2), "-", 
           round(rate_ratio_upper, 2), ")"))


# Add these lines after your existing code to print all coefficients except trial names

# First, identify which coefficients are related to the trial names
trial_indices <- grep("as.factor\\(Enrolled_Trial_name\\)", coef_names_model1)

# Get the names of all non-trial coefficients we want to extract
keep_coef_names <- coef_names_model1[-trial_indices]

# Create a data frame to store results
all_results <- data.frame(
  Coefficient = character(),
  Estimate = numeric(),
  StdError = numeric(),
  RateRatio = numeric(),
  LowerCI = numeric(),
  UpperCI = numeric(),
  stringsAsFactors = FALSE
)

# Process each coefficient
for(coef_name in keep_coef_names) {
  # Extract coefficient values and standard errors from all 10 models
  coefs <- numeric(10)
  ses <- numeric(10)
  
  for(i in 1:10) {
    # Get the coefficient summary
    coef_summary <- summary(res_comb[[i]])$coefficients
    
    # Extract the coefficient and standard error
    coefs[i] <- coef_summary[coef_name, "Estimate"]
    ses[i] <- coef_summary[coef_name, "Std. Error"]
  }
  
  # Pool using Rubin's rules
  pooled_coef <- mean(coefs)
  between_var <- var(coefs)
  within_var <- mean(ses^2)
  total_var <- within_var + (1 + 1/10) * between_var
  pooled_se <- sqrt(total_var)
  
  # Calculate rate ratio and confidence interval
  rate_ratio <- exp(pooled_coef)
  rate_ratio_lower <- exp(pooled_coef - 1.96 * pooled_se)
  rate_ratio_upper <- exp(pooled_coef + 1.96 * pooled_se)
  
  # Add to results data frame
  all_results <- rbind(all_results, data.frame(
    Coefficient = coef_name,
    Estimate = pooled_coef,
    StdError = pooled_se,
    RateRatio = rate_ratio,
    LowerCI = rate_ratio_lower,
    UpperCI = rate_ratio_upper,
    stringsAsFactors = FALSE
  ))
}

# Format the results for nicer display
all_results$CI <- paste0(round(all_results$LowerCI, 2), "-", round(all_results$UpperCI, 2))
all_results$RateRatio <- round(all_results$RateRatio, 2)
all_results$Estimate <- round(all_results$Estimate, 3)
all_results$StdError <- round(all_results$StdError, 3)

# Print the results - this will definitely show in the console
print("=== ALL COEFFICIENTS EXCEPT TRIAL NAMES (Pooled) ===")
print(all_results[, c("Coefficient", "RateRatio", "CI")])
print(all_results)



# Addressing editor's comment: Do BEC and FeNO predict asthma attacks in the High FEV1 Low Rev group?
cat("\n\n=== BIOMARKER PREDICTION ANALYSIS FOR HIGH FEV1 LOW REV GROUP ===\n")

# 1. First approach: Subgroup analysis within only High FEV1 Low Rev patients
res_subgroup <- NULL
for(i in 1:10){
  # Filter data for the current imputation AND only High FEV1 Low Rev patients
  data_imp_i <- Data_Oracle_filtered %>% 
    filter(.imp == i, high_FEV1_low_rev_group == "High FEV1, Low Rev")
  
  # Print sample size for the first imputation
  if(i == 1) {
    cat("Number of High FEV1 Low Rev patients:", nrow(data_imp_i), "\n\n")
  }
  
  # Fit model on just this subgroup to see if biomarkers predict attacks
  res_subgroup[[i]] <- try(glm.nb(Number_severe_asthma_attacks_during_followup ~ 
                                    FeNO_Log_imputated + 
                                    Eosinophils_Log_imputated +
                                    ACQ_baseline_score_mean_imputated + 
                                    Gender_imputated +
                                    Age_imputated +
                                    as.factor(Treatment_step) +
                                    Number_severe_attack_previous_12m +
                                    offset(log(Follow_up_duration_days)) + 
                                    as.factor(Enrolled_Trial_name), 
                                  data = data_imp_i), silent = TRUE)
}

# Check if any models failed to converge
model_failures <- sapply(res_subgroup, function(x) inherits(x, "try-error"))
if(any(model_failures)) {
  cat("Warning: Models failed to converge for imputations:", 
      paste(which(model_failures), collapse = ", "), 
      "\nThis may indicate insufficient data in the subgroup.\n")
  
  # Continue with only successful models
  res_subgroup <- res_subgroup[!model_failures]
}

if(length(res_subgroup) > 0) {
  # Extract results for FeNO
  feno_results <- data.frame(
    Estimate = numeric(length(res_subgroup)),
    SE = numeric(length(res_subgroup))
  )
  
  eos_results <- data.frame(
    Estimate = numeric(length(res_subgroup)),
    SE = numeric(length(res_subgroup))
  )
  
  for(i in 1:length(res_subgroup)) {
    coeffs <- summary(res_subgroup[[i]])$coefficients
    
    # Extract FeNO results
    feno_idx <- which(rownames(coeffs) == "FeNO_Log_imputated")
    feno_results$Estimate[i] <- coeffs[feno_idx, "Estimate"]
    feno_results$SE[i] <- coeffs[feno_idx, "Std. Error"]
    
    # Extract Eosinophils results
    eos_idx <- which(rownames(coeffs) == "Eosinophils_Log_imputated")
    eos_results$Estimate[i] <- coeffs[eos_idx, "Estimate"]
    eos_results$SE[i] <- coeffs[eos_idx, "Std. Error"]
  }
  
  # Pool FeNO results
  pooled_feno_coef <- mean(feno_results$Estimate)
  between_var_feno <- var(feno_results$Estimate)
  within_var_feno <- mean(feno_results$SE^2)
  total_var_feno <- within_var_feno + (1 + 1/length(res_subgroup)) * between_var_feno
  pooled_feno_se <- sqrt(total_var_feno)
  
  # Calculate rate ratio and CI for FeNO
  rr_feno <- exp(pooled_feno_coef)
  rr_feno_lower <- exp(pooled_feno_coef - 1.96 * pooled_feno_se)
  rr_feno_upper <- exp(pooled_feno_coef + 1.96 * pooled_feno_se)
  
  # Calculate p-value for FeNO
  z_feno <- pooled_feno_coef / pooled_feno_se
  p_feno <- 2 * (1 - pnorm(abs(z_feno)))
  
  # Pool Eosinophils results
  pooled_eos_coef <- mean(eos_results$Estimate)
  between_var_eos <- var(eos_results$Estimate)
  within_var_eos <- mean(eos_results$SE^2)
  total_var_eos <- within_var_eos + (1 + 1/length(res_subgroup)) * between_var_eos
  pooled_eos_se <- sqrt(total_var_eos)
  
  # Calculate rate ratio and CI for Eosinophils
  rr_eos <- exp(pooled_eos_coef)
  rr_eos_lower <- exp(pooled_eos_coef - 1.96 * pooled_eos_se)
  rr_eos_upper <- exp(pooled_eos_coef + 1.96 * pooled_eos_se)
  
  # Calculate p-value for Eosinophils
  z_eos <- pooled_eos_coef / pooled_eos_se
  p_eos <- 2 * (1 - pnorm(abs(z_eos)))
  
  # Print results
  cat("RESULTS FOR PATIENTS WITH HIGH FEV1 AND LOW REVERSIBILITY:\n")
  cat(paste0("FeNO (log): Rate Ratio = ", round(rr_feno, 2), 
             " (95% CI: ", round(rr_feno_lower, 2), "-", round(rr_feno_upper, 2), 
             "), p = ", round(p_feno, 3), "\n"))
  
  cat(paste0("Blood Eosinophils (log): Rate Ratio = ", round(rr_eos, 2), 
             " (95% CI: ", round(rr_eos_lower, 2), "-", round(rr_eos_upper, 2), 
             "), p = ", round(p_eos, 3), "\n\n"))
  
  # 2. Check for combined effect - add an interaction term between the biomarkers
  cat("COMBINED BIOMARKER EFFECTS:\n")
  
  # Run model with FeNO*Eosinophils interaction in High FEV1 Low Rev subgroup
  res_combined <- NULL
  for(i in 1:10){
    # Filter for High FEV1 Low Rev patients
    data_imp_i <- Data_Oracle_filtered %>% 
      filter(.imp == i, high_FEV1_low_rev_group == "High FEV1, Low Rev")
    
    # Fit model with interaction between biomarkers
    res_combined[[i]] <- try(glm.nb(Number_severe_asthma_attacks_during_followup ~ 
                                      FeNO_Log_imputated * Eosinophils_Log_imputated +
                                      ACQ_baseline_score_mean_imputated + 
                                      Gender_imputated +
                                      Age_imputated +
                                      as.factor(Treatment_step) +
                                      Number_severe_attack_previous_12m +
                                      offset(log(Follow_up_duration_days)) + 
                                      as.factor(Enrolled_Trial_name), 
                                    data = data_imp_i), silent = TRUE)
  }
  
  # Check for model convergence
  model_failures <- sapply(res_combined, function(x) inherits(x, "try-error"))
  if(any(model_failures)) {
    cat("Warning: Combined models failed to converge for some imputations\n")
    res_combined <- res_combined[!model_failures]
  }
  
  if(length(res_combined) > 0) {
    # Extract the interaction term
    int_name <- "FeNO_Log_imputated:Eosinophils_Log_imputated"
    int_results <- data.frame(
      Estimate = numeric(length(res_combined)),
      SE = numeric(length(res_combined))
    )
    
    for(i in 1:length(res_combined)) {
      coeffs <- summary(res_combined[[i]])$coefficients
      int_idx <- which(rownames(coeffs) == int_name)
      int_results$Estimate[i] <- coeffs[int_idx, "Estimate"]
      int_results$SE[i] <- coeffs[int_idx, "Std. Error"]
    }
    
    # Pool interaction results
    pooled_int_coef <- mean(int_results$Estimate)
    between_var_int <- var(int_results$Estimate)
    within_var_int <- mean(int_results$SE^2)
    total_var_int <- within_var_int + (1 + 1/length(res_combined)) * between_var_int
    pooled_int_se <- sqrt(total_var_int)
    
    # Calculate rate ratio and CI for interaction
    rr_int <- exp(pooled_int_coef)
    rr_int_lower <- exp(pooled_int_coef - 1.96 * pooled_int_se)
    rr_int_upper <- exp(pooled_int_coef + 1.96 * pooled_int_se)
    
    # Calculate p-value for interaction
    z_int <- pooled_int_coef / pooled_int_se
    p_int <- 2 * (1 - pnorm(abs(z_int)))
    
    cat(paste0("FeNO * Eosinophils interaction: Rate Ratio = ", round(rr_int, 2), 
               " (95% CI: ", round(rr_int_lower, 2), "-", round(rr_int_upper, 2), 
               "), p = ", round(p_int, 3), "\n"))
    
    # Interpretation guidance
    if(p_feno < 0.05 || p_eos < 0.05 || p_int < 0.05) {
      cat("\nINTERPRETATION: There is evidence that ")
      
      if(p_feno < 0.05) {
        cat("FeNO")
        if(p_eos < 0.05) cat(" and blood eosinophils") 
        cat(" predict asthma attacks in High FEV1 Low Rev patients")
      } else if(p_eos < 0.05) {
        cat("blood eosinophils predict asthma attacks in High FEV1 Low Rev patients")
      }
      
      if(p_int < 0.05) {
        if(p_feno < 0.05 || p_eos < 0.05) cat(", and there is ")
        cat("a significant interaction between the biomarkers, suggesting their combined effect differs from their individual effects")
      }
      cat(".\n")
    } else {
      cat("\nINTERPRETATION: There is no strong evidence that FeNO or blood eosinophils, either individually or in combination, predict asthma attacks in patients with High FEV1 and Low Reversibility.\n")
    }
  } else {
    cat("Could not evaluate combined biomarker effects due to model convergence failures.\n")
  }
  
  # NEW CODE: Extract all coefficients from the main model (without interaction)
  cat("\n\n=== ALL COEFFICIENTS FROM SUBGROUP MODEL ===\n")
  
  # Get the first successful model to determine all coefficient names
  first_model <- res_subgroup[[1]]
  coef_names <- names(coef(first_model))
  
  # Identify trial name coefficients to exclude
  trial_indices <- grep("as.factor\\(Enrolled_Trial_name\\)", coef_names)
  
  # Also exclude the intercept
  exclude_indices <- c(which(coef_names == "(Intercept)"), trial_indices)
  
  # Get the names of coefficients to include
  include_coef_names <- coef_names[-exclude_indices]
  
  # Create a data frame to store results for all coefficients
  all_results <- data.frame(
    Coefficient = character(),
    Estimate = numeric(),
    StdError = numeric(),
    RateRatio = numeric(),
    LowerCI = numeric(),
    UpperCI = numeric(),
    PValue = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Process each coefficient
  for(coef_name in include_coef_names) {
    # Extract coefficient values and standard errors from all models
    coef_results <- data.frame(
      Estimate = numeric(length(res_subgroup)),
      SE = numeric(length(res_subgroup))
    )
    
    for(i in 1:length(res_subgroup)) {
      coeffs <- summary(res_subgroup[[i]])$coefficients
      coef_idx <- which(rownames(coeffs) == coef_name)
      
      if(length(coef_idx) > 0) {
        coef_results$Estimate[i] <- coeffs[coef_idx, "Estimate"]
        coef_results$SE[i] <- coeffs[coef_idx, "Std. Error"]
      } else {
        # If coefficient is not present in this model, use NA
        coef_results$Estimate[i] <- NA
        coef_results$SE[i] <- NA
      }
    }
    
    # Remove any NAs
    coef_results <- coef_results[!is.na(coef_results$Estimate),]
    
    if(nrow(coef_results) > 0) {
      # Pool results
      pooled_coef <- mean(coef_results$Estimate)
      between_var <- var(coef_results$Estimate)
      within_var <- mean(coef_results$SE^2)
      total_var <- within_var + (1 + 1/nrow(coef_results)) * between_var
      pooled_se <- sqrt(total_var)
      
      # Calculate rate ratio and CI
      rr <- exp(pooled_coef)
      rr_lower <- exp(pooled_coef - 1.96 * pooled_se)
      rr_upper <- exp(pooled_coef + 1.96 * pooled_se)
      
      # Calculate p-value
      z <- pooled_coef / pooled_se
      p <- 2 * (1 - pnorm(abs(z)))
      
      # Add to results
      all_results <- rbind(all_results, data.frame(
        Coefficient = coef_name,
        Estimate = pooled_coef,
        StdError = pooled_se,
        RateRatio = rr,
        LowerCI = rr_lower,
        UpperCI = rr_upper,
        PValue = p,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Format the results and print
  all_results$FormattedResult <- paste0(round(all_results$RateRatio, 2), 
                                        " [", round(all_results$LowerCI, 2), 
                                        "-", round(all_results$UpperCI, 2), "]")
  
  # Create nicely formatted display names
  all_results$DisplayName <- gsub("as\\.factor\\(Treatment_step\\)", "Treatment Step ", all_results$Coefficient)
  all_results$DisplayName <- gsub("_imputated", "", all_results$DisplayName)
  all_results$DisplayName <- gsub("_Log", " (log)", all_results$DisplayName)
  all_results$DisplayName <- gsub("_", " ", all_results$DisplayName)
  
  # Print the final results
  result_table <- all_results[, c("DisplayName", "FormattedResult", "PValue")]
  names(result_table) <- c("Variable", "Adjusted RR [95% CI]", "P-value")
  result_table$`P-value` <- round(result_table$`P-value`, 3)
  
  print(result_table[order(result_table$`P-value`),], row.names = FALSE)
  
} else {
  cat("Could not evaluate biomarker effects due to model convergence failures.\n")
}





#################################################
# PREDICTED RATES FOR EACH GROUP FOR VISUALIZATION
#################################################

# Initialize arrays to store predictions for each imputation
all_predictions_high_low <- numeric(10)
all_predictions_other <- numeric(10)
all_se_link_high_low <- numeric(10)
all_se_link_other <- numeric(10)

# For each imputation
for(i in 1:10) {
  # Filter data for the current imputation
  data_imp_i <- Data_Oracle_filtered %>% filter(.imp == i)
  
  # Create prediction datasets specific to this imputation
  pred_data_high_low <- data.frame(
    high_FEV1_low_rev_group = "High FEV1, Low Rev",
    ACQ_baseline_score_mean_imputated = mean(data_imp_i$ACQ_baseline_score_mean_imputated, na.rm = TRUE),
    Gender_imputated = names(which.max(table(data_imp_i$Gender_imputated))),
    Age_imputated = mean(data_imp_i$Age_imputated, na.rm = TRUE),
    Treatment_step = names(which.max(table(data_imp_i$Treatment_step))),
    Number_severe_attack_previous_12m = mean(data_imp_i$Number_severe_attack_previous_12m, na.rm = TRUE),
    FEV1_preBD_PCT_Baseline_imputated = mean(data_imp_i$FEV1_preBD_PCT_Baseline_imputated, na.rm = TRUE),
    FEV1_reversibility_percent_postBD_real = mean(data_imp_i$FEV1_reversibility_percent_postBD_real, na.rm = TRUE),
    FEV1_FVC_ratio_imputated = mean(data_imp_i$FEV1_FVC_ratio_imputated, na.rm = TRUE),
    FeNO_Log_imputated = mean(data_imp_i$FeNO_Log_imputated, na.rm = TRUE),
    Eosinophils_Log_imputated = mean(data_imp_i$Eosinophils_Log_imputated, na.rm = TRUE),
    Follow_up_duration_days = 365,  # 1 year follow-up for rate calculation
    Enrolled_Trial_name = "NAVIGATOR" #trial with most high fev/low rev patients 
  )
  
  pred_data_other <- pred_data_high_low
  pred_data_other$high_FEV1_low_rev_group <- "Other"
  
  # Make predictions using this imputation's model
  pred_high_low <- predict(res_comb[[i]], newdata = pred_data_high_low, type = "link", se.fit = TRUE)
  pred_other <- predict(res_comb[[i]], newdata = pred_data_other, type = "link", se.fit = TRUE)
  
  # Store predictions (on the response scale) and standard errors (on link scale)
  all_predictions_high_low[i] <- exp(pred_high_low$fit)
  all_predictions_other[i] <- exp(pred_other$fit)
  all_se_link_high_low[i] <- pred_high_low$se.fit
  all_se_link_other[i] <- pred_other$se.fit
}

# Calculate mean rates across imputations (Rubin's rules)
final_rate_high_low <- mean(all_predictions_high_low)
final_rate_other <- mean(all_predictions_other)

# Calculate between-imputation variance
between_var_high_low <- var(all_predictions_high_low)
between_var_other <- var(all_predictions_other)

# For within-imputation variance
# Convert SEs on link scale to response scale using delta method approximation
within_var_high_low <- 0
within_var_other <- 0

for(i in 1:10) {
  # Convert SE on link scale to response scale using delta method
  # Var(exp(X)) ≈ [exp(E(X))]² * Var(X)
  within_var_high_low <- within_var_high_low + (all_predictions_high_low[i]^2) * (all_se_link_high_low[i]^2)
  within_var_other <- within_var_other + (all_predictions_other[i]^2) * (all_se_link_other[i]^2)
}
within_var_high_low <- within_var_high_low / 10
within_var_other <- within_var_other / 10

# Total variance using Rubin's formula
total_var_high_low <- within_var_high_low + (1 + 1/10) * between_var_high_low
total_var_other <- within_var_other + (1 + 1/10) * between_var_other

# Calculate confidence intervals
ci_high_low_lower <- final_rate_high_low - 1.96 * sqrt(total_var_high_low)
ci_high_low_upper <- final_rate_high_low + 1.96 * sqrt(total_var_high_low)
ci_other_lower <- final_rate_other - 1.96 * sqrt(total_var_other)
ci_other_upper <- final_rate_other + 1.96 * sqrt(total_var_other)

# Make sure lower bounds aren't negative
ci_high_low_lower <- max(0, ci_high_low_lower)
ci_other_lower <- max(0, ci_other_lower)

# Create a dataframe of predicted rates with confidence intervals
predicted_rates <- data.frame(
  Group = c("High FEV1, Low Rev", "Other"),
  Rate = c(final_rate_high_low, final_rate_other),
  Lower_CI = c(ci_high_low_lower, ci_other_lower),
  Upper_CI = c(ci_high_low_upper, ci_other_upper)
)

# Print predicted rates
cat("\n\nAdjusted Annual Asthma Attack Rates with Proper Multiple Imputation Pooling:\n")
print(predicted_rates)

# Calculate rate ratio from predicted rates (should be similar to coefficient-based method)
rate_ratio_from_rates <- final_rate_high_low / final_rate_other

# For rate ratio CI from rates, use log-normal approximation
log_rate_ratio_from_rates <- log(rate_ratio_from_rates)
var_log_rate_ratio_from_rates <- total_var_high_low/(final_rate_high_low^2) + 
  total_var_other/(final_rate_other^2)

rate_ratio_lower_from_rates <- exp(log_rate_ratio_from_rates - 1.96 * sqrt(var_log_rate_ratio_from_rates))
rate_ratio_upper_from_rates <- exp(log_rate_ratio_from_rates + 1.96 * sqrt(var_log_rate_ratio_from_rates))

# Print rate ratio from predicted rates (alternative calculation)
cat("\nRate Ratio from predicted rates (High FEV1, Low Rev vs. Other): ", 
    round(rate_ratio_from_rates, 2), " (95% CI: ", 
    round(rate_ratio_lower_from_rates, 2), "-", 
    round(rate_ratio_upper_from_rates, 2), ")\n")



### COMPARE BOTH GROUPS IN TABLE1
# Calculate FEV1 change variable (52 weeks - baseline)
Data_Oracle_filtered <- Data_Oracle_filtered %>%
  mutate(FEV1_preBD_change_L = FEV1PREBD_L_52W - FEV1_preBD_L_Baseline_imputated)

Data_Oracle_filtered <- Data_Oracle_filtered %>%
  mutate(FEV1_postBD_change_L = FEV1POSTBD_L_52W - FEV1_postBD_L_Baseline_imputated)


# Filter to imputation 1 only
data_imp1 <- subset(Data_Oracle_filtered, 
                    .imp == 1 & 
                      !is.na(high_FEV1_low_rev_group))

# Compute p-values using LOGGED values before transforming
p_values_table <- print(CreateTableOne(vars = c("FeNO_Log_imputated", "Eosinophils_Log_imputated"), 
                                       strata = "high_FEV1_low_rev_group", 
                                       data = data_imp1, 
                                       test = TRUE),
                        quote = FALSE, printToggle = FALSE, smd = FALSE)

# Extract p-values correctly
p_feno <- p_values_table[rownames(p_values_table) == "FeNO_Log_imputated", "p"]
p_eos <- p_values_table[rownames(p_values_table) == "Eosinophils_Log_imputated", "p"]

# Apply antilog transformation
data_imp1$FeNO_Antilog <- 10^(data_imp1$FeNO_Log_imputated)
data_imp1$Eosinophils_Antilog <- 10^(data_imp1$Eosinophils_Log_imputated)

# Calculate total followup years by group and add to the data frame
group_summary <- data_imp1 %>%
  group_by(high_FEV1_low_rev_group) %>%
  summarise(
    Group_Total_Attacks = sum(Number_severe_asthma_attacks_during_followup, na.rm = TRUE),
    total_followup_years = sum(Follow_up_duration_years, na.rm = TRUE),
    n_patients = n(),
    unadjusted_annual_rate = Group_Total_Attacks / total_followup_years
  )

# Print group summary with unadjusted annual rates
cat("\nUnadjusted Annualized Asthma Attack Rates by Group:\n")
print(group_summary)

# Add group-level variables to the data frame
data_imp1 <- data_imp1 %>%
  group_by(high_FEV1_low_rev_group) %>%
  mutate(
    Group_Total_Attacks = sum(Number_severe_asthma_attacks_during_followup, na.rm = TRUE),
    total_followup_years = sum(Follow_up_duration_years, na.rm = TRUE),
    Group_Unadjusted_Annual_Rate = Group_Total_Attacks / total_followup_years
  ) %>%
  ungroup()

# Create a categorical variable for Enrolled_Trial_name for Table 1
data_imp1$Enrolled_Trial_Category <- factor(data_imp1$Enrolled_Trial_name)

# Define variables for the table with the new order
vars_to_compare <- c("Age_imputated",
                     "Gender_imputated",
                     "Treatment_step",
                     "Number_severe_attack_previous_12m",
                     "FEV1_preBD_L_Baseline_imputated",
                     "FEV1_preBD_PCT_Baseline_imputated",
                     "FEV1_postBD_L_Baseline_imputated",
                     "FEV1_postBD_PCT_Baseline_imputated",
                     "FEV1_reversibility_percent_postBD_real",
                     "FEV1_FVC_ratio_imputated",
                     "ACQ_baseline_score_mean_imputated", 
                     "FeNO_Antilog", 
                     "Eosinophils_Antilog",
                     "Enrolled_Trial_Category",  
                     "Group_Total_Attacks",  
                     "total_followup_years",
                     "Group_Unadjusted_Annual_Rate",
                     "FEV1_preBD_change_L",
                     "FEV1_postBD_change_L")

# Create table without the overall column
table1 <- CreateTableOne(vars = vars_to_compare, 
                         strata = "high_FEV1_low_rev_group", 
                         data = data_imp1, 
                         addOverall = FALSE)  # Remove the Overall column

# Convert table to a modifiable format
table1_summary <- print(table1, showAllLevels = TRUE, quote = FALSE, pDigits = 3, printToggle = FALSE)

# Create table without p-values first
table1 <- CreateTableOne(vars = vars_to_compare, 
                         strata = "high_FEV1_low_rev_group", 
                         data = data_imp1, 
                         addOverall = FALSE)

# Convert table to a modifiable format
table1_summary <- print(table1, showAllLevels = TRUE, quote = FALSE, pDigits = 3, printToggle = FALSE)

# Manually insert the p-values for FeNO and Eosinophils
if ("FeNO_Antilog" %in% rownames(table1_summary)) {
  table1_summary["FeNO_Antilog", "p"] <- p_feno
}
if ("Eosinophils_Antilog" %in% rownames(table1_summary)) {
  table1_summary["Eosinophils_Antilog", "p"] <- p_eos
}

# Print markdown version
kable(table1_summary, format = "markdown", caption = "Table 1. Comparison of patient characteristics")

# Convert to HTML format
htmlTable(table1_summary)

# Save as an HTML file
writeLines(htmlTable(table1_summary), "table1_imputation1_filtered.html")

# Export as CSV for Excel
write.csv(table1_summary, "table1_imputation1_filtered.csv")

# Create a simpler chi-square test for treatment steps 3-5
treatment_data <- data_imp1 %>%
  filter(as.numeric(as.character(Treatment_step)) >= 3)

# Create contingency table and run chi-square test
if(nrow(treatment_data) > 0) {
  treatment_table <- table(treatment_data$high_FEV1_low_rev_group, treatment_data$Treatment_step)
  treatment_chi_square <- chisq.test(treatment_table)
  cat("\nChi-Square Test for Treatment Steps 3+ Distribution:\n")
  print(treatment_chi_square)
}

# Calculate FeNO_Antilog and Eosinophils_Antilog medians and IQRs by group
data_imp1 %>%
  # Group by high_FEV1_low_rev_group (or Overall)
  group_by(high_FEV1_low_rev_group) %>%
  # Summarize median and IQR
  summarise(
    FeNO_Antilog_median = median(FeNO_Antilog, na.rm = TRUE),
    FeNO_Antilog_IQR = IQR(FeNO_Antilog, na.rm = TRUE),
    Eosinophils_Antilog_median = median(Eosinophils_Antilog, na.rm = TRUE),
    Eosinophils_Antilog_IQR = IQR(Eosinophils_Antilog, na.rm = TRUE)
  ) %>%
  # Add a custom formatted column for output
  mutate(
    FeNO_Antilog = paste0(FeNO_Antilog_median, " [", round(FeNO_Antilog_median - FeNO_Antilog_IQR / 2, 2), " - ", round(FeNO_Antilog_median + FeNO_Antilog_IQR / 2, 2), "]"),
    Eosinophils_Antilog = paste0(Eosinophils_Antilog_median, " [", round(Eosinophils_Antilog_median - Eosinophils_Antilog_IQR / 2, 2), " - ", round(Eosinophils_Antilog_median + Eosinophils_Antilog_IQR / 2, 2), "]")
  ) %>%
  # Select relevant columns to print
  select(high_FEV1_low_rev_group, FeNO_Antilog, Eosinophils_Antilog)

# Compute the median and IQR for FeNO_Antilog and Eosinophils_Antilog in Overall group
overall_stats <- data_imp1 %>%
  summarise(
    FeNO_Antilog_median = median(FeNO_Antilog, na.rm = TRUE),
    FeNO_Antilog_IQR = IQR(FeNO_Antilog, na.rm = TRUE),
    Eosinophils_Antilog_median = median(Eosinophils_Antilog, na.rm = TRUE),
    Eosinophils_Antilog_IQR = IQR(Eosinophils_Antilog, na.rm = TRUE)
  )

# Print the results as median [Q1 - Q3]
FeNO_result <- paste0(overall_stats$FeNO_Antilog_median, " [", 
                      round(overall_stats$FeNO_Antilog_median - overall_stats$FeNO_Antilog_IQR / 2, 2), " - ", 
                      round(overall_stats$FeNO_Antilog_median + overall_stats$FeNO_Antilog_IQR / 2, 2), "]")

Eos_result <- paste0(overall_stats$Eosinophils_Antilog_median, " [", 
                     round(overall_stats$Eosinophils_Antilog_median - overall_stats$Eosinophils_Antilog_IQR / 2, 2), " - ", 
                     round(overall_stats$Eosinophils_Antilog_median + overall_stats$Eosinophils_Antilog_IQR / 2, 2), "]")

# Print the results
cat("FeNO_Antilog: ", FeNO_result, "\n")
cat("Eosinophils_Antilog: ", Eos_result, "\n")

# Tabulate the counts of Enrolled Trial for each group
enrolled_trial_table <- table(data_imp1$high_FEV1_low_rev_group, 
                              data_imp1$Enrolled_Trial_name)

# Convert the table to a dataframe for better formatting
enrolled_trial_df <- as.data.frame(enrolled_trial_table)

# Add proportions to the dataframe
enrolled_trial_df$Proportion <- with(enrolled_trial_df, Freq / ave(Freq, Var1, FUN = sum))

# Format the table using kable for basic display
kable(enrolled_trial_df, 
      format = "html", 
      col.names = c("Group", "Enrolled Trial", "Count", "Proportion"),
      caption = "Counts and proportions of enrolled trials by group.")

# Optionally, run a Chi-square test to compare the distributions between the groups
chi_square_result <- chisq.test(enrolled_trial_table)

# Print the Chi-square test results
cat("\nChi-Square Test Result:\n")
print(chi_square_result)



# Calculate missing counts and percentages for FEV1 change variables
missing_stats <- data_imp1 %>%
  group_by(high_FEV1_low_rev_group) %>%
  summarise(
    # Pre-bronchodilator FEV1 change
    n_preBD_missing = sum(is.na(FEV1_preBD_change_L)),
    pct_preBD_missing = round(sum(is.na(FEV1_preBD_change_L)) / n() * 100, 1),
    
    # Post-bronchodilator FEV1 change
    n_postBD_missing = sum(is.na(FEV1_postBD_change_L)),
    pct_postBD_missing = round(sum(is.na(FEV1_postBD_change_L)) / n() * 100, 1),
    
    # Total patients in group
    total_patients = n()
  )

# Display the results in a readable format
missing_stats %>%
  mutate(
    FEV1_preBD_change_missing = paste0(n_preBD_missing, " (", pct_preBD_missing, "%)"),
    FEV1_postBD_change_missing = paste0(n_postBD_missing, " (", pct_postBD_missing, "%)")
  ) %>%
  select(high_FEV1_low_rev_group, FEV1_preBD_change_missing, FEV1_postBD_change_missing)









###################################################
# SENSITIVITY ANALYSIS 1:
# High FEV1, Low Rev stratification without FEV1 and reversibility covariates
###################################################

# Run models on all 10 imputations without FEV1 and reversibility covariates
res_sens1 <- NULL
for(i in 1:10){
  # Filter data for the current imputation
  data_imp_i <- Data_Oracle_filtered %>% filter(.imp == i)
  
  # Explicitly set reference level
  data_imp_i$high_FEV1_low_rev_group <- relevel(data_imp_i$high_FEV1_low_rev_group, ref = "Other")
  
  # Fit the model without FEV1 and reversibility covariates
  res_sens1[[i]] <- glm.nb(Number_severe_asthma_attacks_during_followup ~ 
                             high_FEV1_low_rev_group +
                             ACQ_baseline_score_mean_imputated + 
                             Gender_imputated +
                             Age_imputated +
                             as.factor(Treatment_step) +
                             Number_severe_attack_previous_12m +
                             FeNO_Log_imputated + 
                             Eosinophils_Log_imputated +
                             offset(log(Follow_up_duration_days)) + 
                             as.factor(Enrolled_Trial_name), 
                           data = data_imp_i)
}

# Get coefficient index for the group variable
sens1_coef_names <- names(coef(res_sens1[[1]]))
sens1_group_coef_index <- grep("high_FEV1_low_rev_group", sens1_coef_names)
sens1_group_coef_name <- sens1_coef_names[sens1_group_coef_index]

# Extract coefficient values and standard errors from all 10 models
sens1_coefs <- numeric(10)
sens1_ses <- numeric(10)

for(i in 1:10) {
  # Get the coefficient summary
  coef_summary <- summary(res_sens1[[i]])$coefficients
  
  # Extract the coefficient and standard error
  sens1_coefs[i] <- coef_summary[sens1_group_coef_name, "Estimate"]
  sens1_ses[i] <- coef_summary[sens1_group_coef_name, "Std. Error"]
}

# Pool the coefficients using Rubin's rules
sens1_pooled_coef <- mean(sens1_coefs)
sens1_between_var <- var(sens1_coefs)
sens1_within_var <- mean(sens1_ses^2)
sens1_total_var <- sens1_within_var + (1 + 1/10) * sens1_between_var
sens1_pooled_se <- sqrt(sens1_total_var)

# Calculate rate ratio and confidence interval
sens1_rate_ratio <- exp(sens1_pooled_coef)
sens1_rate_ratio_lower <- exp(sens1_pooled_coef - 1.96 * sens1_pooled_se)
sens1_rate_ratio_upper <- exp(sens1_pooled_coef + 1.96 * sens1_pooled_se)

# Print results for sensitivity analysis 1
cat("\n\n=== SENSITIVITY ANALYSIS 1: HIGH FEV1 & LOW REV (WITHOUT FEV1/REVERSIBILITY COVARIATES) ===\n")
cat(paste0("Rate Ratio (High FEV1, Low Rev vs. Other): ", 
           round(sens1_rate_ratio, 2), " (95% CI: ", 
           round(sens1_rate_ratio_lower, 2), "-", 
           round(sens1_rate_ratio_upper, 2), ")"))


###################################################
# SENSITIVITY ANALYSIS 2:
# High FEV1 Only stratification without FEV1 covariate
###################################################

# Run models on all 10 imputations
res_sens2 <- NULL
for(i in 1:10){
  # Filter data for the current imputation
  data_imp_i <- Data_Oracle_filtered %>% filter(.imp == i)
  
  # Explicitly set reference level for high_FEV1_group
  data_imp_i$high_FEV1_group <- relevel(data_imp_i$high_FEV1_group, ref = "Not High FEV1")
  
  # Fit the model without FEV1 preBD percent covariate
  res_sens2[[i]] <- glm.nb(Number_severe_asthma_attacks_during_followup ~ 
                             high_FEV1_group +
                             ACQ_baseline_score_mean_imputated + 
                             Gender_imputated +
                             Age_imputated +
                             as.factor(Treatment_step) +
                             Number_severe_attack_previous_12m +
                             FEV1_reversibility_percent_postBD_real +
                             FEV1_FVC_ratio_imputated +
                             FeNO_Log_imputated + 
                             Eosinophils_Log_imputated +
                             offset(log(Follow_up_duration_days)) + 
                             as.factor(Enrolled_Trial_name), 
                           data = data_imp_i)
}

# Get coefficient index for the group variable
sens2_coef_names <- names(coef(res_sens2[[1]]))
sens2_group_coef_index <- grep("high_FEV1_group", sens2_coef_names)
sens2_group_coef_name <- sens2_coef_names[sens2_group_coef_index]

# Extract coefficient values and standard errors from all 10 models
sens2_coefs <- numeric(10)
sens2_ses <- numeric(10)

for(i in 1:10) {
  # Get the coefficient summary
  coef_summary <- summary(res_sens2[[i]])$coefficients
  
  # Extract the coefficient and standard error
  sens2_coefs[i] <- coef_summary[sens2_group_coef_name, "Estimate"]
  sens2_ses[i] <- coef_summary[sens2_group_coef_name, "Std. Error"]
}

# Pool the coefficients using Rubin's rules
sens2_pooled_coef <- mean(sens2_coefs)
sens2_between_var <- var(sens2_coefs)
sens2_within_var <- mean(sens2_ses^2)
sens2_total_var <- sens2_within_var + (1 + 1/10) * sens2_between_var
sens2_pooled_se <- sqrt(sens2_total_var)

# Calculate rate ratio and confidence interval
sens2_rate_ratio <- exp(sens2_pooled_coef)
sens2_rate_ratio_lower <- exp(sens2_pooled_coef - 1.96 * sens2_pooled_se)
sens2_rate_ratio_upper <- exp(sens2_pooled_coef + 1.96 * sens2_pooled_se)

# Print results for sensitivity analysis 2
cat("\n\n=== SENSITIVITY ANALYSIS 2: HIGH FEV1 ONLY (WITHOUT FEV1 COVARIATE) ===\n")
cat(paste0("Rate Ratio (High FEV1 Only vs. Not High FEV1): ", 
           round(sens2_rate_ratio, 2), " (95% CI: ", 
           round(sens2_rate_ratio_lower, 2), "-", 
           round(sens2_rate_ratio_upper, 2), ")"))


###################################################
# SENSITIVITY ANALYSIS 3:
# Low Rev Only stratification without reversibility covariate
###################################################

# Run models on all 10 imputations
res_sens3 <- NULL
for(i in 1:10){
  # Filter data for the current imputation
  data_imp_i <- Data_Oracle_filtered %>% filter(.imp == i)
  
  # Explicitly set reference level for low_rev_group
  data_imp_i$low_rev_group <- relevel(data_imp_i$low_rev_group, ref = "Not Low Rev")
  
  # Fit the model without reversibility covariate
  res_sens3[[i]] <- glm.nb(Number_severe_asthma_attacks_during_followup ~ 
                             low_rev_group +
                             ACQ_baseline_score_mean_imputated + 
                             Gender_imputated +
                             Age_imputated +
                             as.factor(Treatment_step) +
                             Number_severe_attack_previous_12m +
                             FEV1_preBD_PCT_Baseline_imputated + 
                             FEV1_FVC_ratio_imputated +
                             FeNO_Log_imputated + 
                             Eosinophils_Log_imputated +
                             offset(log(Follow_up_duration_days)) + 
                             as.factor(Enrolled_Trial_name), 
                           data = data_imp_i)
}

# Get coefficient index for the group variable
sens3_coef_names <- names(coef(res_sens3[[1]]))
sens3_group_coef_index <- grep("low_rev_group", sens3_coef_names)
sens3_group_coef_name <- sens3_coef_names[sens3_group_coef_index]

# Extract coefficient values and standard errors from all 10 models
sens3_coefs <- numeric(10)
sens3_ses <- numeric(10)

for(i in 1:10) {
  # Get the coefficient summary
  coef_summary <- summary(res_sens3[[i]])$coefficients
  
  # Extract the coefficient and standard error
  sens3_coefs[i] <- coef_summary[sens3_group_coef_name, "Estimate"]
  sens3_ses[i] <- coef_summary[sens3_group_coef_name, "Std. Error"]
}

# Pool the coefficients using Rubin's rules
sens3_pooled_coef <- mean(sens3_coefs)
sens3_between_var <- var(sens3_coefs)
sens3_within_var <- mean(sens3_ses^2)
sens3_total_var <- sens3_within_var + (1 + 1/10) * sens3_between_var
sens3_pooled_se <- sqrt(sens3_total_var)

# Calculate rate ratio and confidence interval
sens3_rate_ratio <- exp(sens3_pooled_coef)
sens3_rate_ratio_lower <- exp(sens3_pooled_coef - 1.96 * sens3_pooled_se)
sens3_rate_ratio_upper <- exp(sens3_pooled_coef + 1.96 * sens3_pooled_se)

# Print results for sensitivity analysis 3
cat("\n\n=== SENSITIVITY ANALYSIS 3: LOW REV ONLY (WITHOUT REVERSIBILITY COVARIATE) ===\n")
cat(paste0("Rate Ratio (Low Rev Only vs. Not Low Rev): ", 
           round(sens3_rate_ratio, 2), " (95% CI: ", 
           round(sens3_rate_ratio_lower, 2), "-", 
           round(sens3_rate_ratio_upper, 2), ")"))


##################################################
# SUMMARY TABLE OF ALL ANALYSES
##################################################

# Create a summary table of all results
results_summary <- data.frame(
  Analysis = c("Primary Analysis (Full Model)", 
               "Sens 1: High FEV1, Low Rev without FEV1/Rev covariates",
               "Sens 2: High FEV1 Only without FEV1 covariate",
               "Sens 3: Low Rev Only without Rev covariate"),
  Rate_Ratio = c(rate_ratio, sens1_rate_ratio, sens2_rate_ratio, sens3_rate_ratio),
  Lower_CI = c(rate_ratio_lower, sens1_rate_ratio_lower, sens2_rate_ratio_lower, sens3_rate_ratio_lower),
  Upper_CI = c(rate_ratio_upper, sens1_rate_ratio_upper, sens2_rate_ratio_upper, sens3_rate_ratio_upper)
)

# Format results to 2 decimal places
results_summary$Rate_Ratio_CI <- with(results_summary, 
                                      paste0(round(Rate_Ratio, 2), 
                                             " (", round(Lower_CI, 2), "-", round(Upper_CI, 2), ")"))

# Print formatted summary table
cat("\n\n=== SUMMARY OF ALL ANALYSES ===\n")
print(results_summary[, c("Analysis", "Rate_Ratio_CI")], row.names = FALSE)





#============================================================================================#
#LUNG FUNCTION





#########################################
# PRIMARY ANALYSIS - HIGH FEV1, LOW REV
# Linear regression for FEV1_preBD_change_L
#########################################

Data_Oracle_filtered <- Data_Oracle_filtered %>%
  mutate(FEV1_preBD_change_L = FEV1PREBD_L_52W - FEV1_preBD_L_Baseline_imputated)

Data_Oracle_filtered <- Data_Oracle_filtered %>%
  mutate(FEV1_postBD_change_L = FEV1POSTBD_L_52W - FEV1_postBD_L_Baseline_imputated)


# Explicitly set reference level to "Other" to make interpretation clearer
Data_Oracle_filtered$high_FEV1_low_rev_group <- relevel(Data_Oracle_filtered$high_FEV1_low_rev_group, ref = "Other")

# Run linear regression models on all 10 imputations
res_comb <- NULL
for(i in 1:10){
  # Filter data for the current imputation
  data_imp_i <- Data_Oracle_filtered %>% filter(.imp == i)
  
  # Fit the linear regression model for FEV1 change
  res_comb[[i]] <- lm(FEV1_preBD_change_L ~ 
                        high_FEV1_low_rev_group +
                        ACQ_baseline_score_mean_imputated + 
                        Gender_imputated +
                        Age_imputated +
                        as.factor(Treatment_step) +
                        Number_severe_attack_previous_12m +
                        FEV1_preBD_L_Baseline_imputated + 
                        FEV1_reversibility_percent_postBD_real +
                        FEV1_FVC_ratio_imputated +
                        FeNO_Log_imputated + 
                        Eosinophils_Log_imputated +
                        as.factor(Enrolled_Trial_name), 
                      data = data_imp_i)
}

###############################################
# MANUALLY EXTRACT AND POOL THE COEFFICIENTS
###############################################

# First, check names of coefficients in the first model
coef_names_model1 <- names(coef(res_comb[[1]]))
cat("\nCoefficient names in model 1:\n")
print(coef_names_model1)

# Look for the high_FEV1_low_rev_group coefficient
# The format is typically "high_FEV1_low_rev_groupHigh FEV1, Low Rev"
group_coef_index <- grep("high_FEV1_low_rev_group", coef_names_model1)

if(length(group_coef_index) == 0) {
  stop("Could not find coefficient for high_FEV1_low_rev_group in model 1")
} else {
  group_coef_name <- coef_names_model1[group_coef_index]
  cat("\nFound coefficient:", group_coef_name, "\n")
}

# Extract coefficient values and standard errors from all 10 models
coefs <- numeric(10)
ses <- numeric(10)

for(i in 1:10) {
  # Get the coefficient summary
  coef_summary <- summary(res_comb[[i]])$coefficients
  
  # Extract the coefficient and standard error
  coefs[i] <- coef_summary[group_coef_name, "Estimate"]
  ses[i] <- coef_summary[group_coef_name, "Std. Error"]
}

# Pool the coefficients using Rubin's rules
pooled_coef <- mean(coefs)
between_var <- var(coefs)
within_var <- mean(ses^2)
total_var <- within_var + (1 + 1/10) * between_var
pooled_se <- sqrt(total_var)

# Calculate confidence interval for the coefficient
ci_lower <- pooled_coef - 1.96 * pooled_se
ci_upper <- pooled_coef + 1.96 * pooled_se

# Print results for primary analysis
cat("\n=== PRIMARY ANALYSIS: HIGH FEV1 & LOW REV (Pooled Coefficients) ===\n")
cat(paste0("Coefficient (High FEV1, Low Rev vs. Other): ", 
           round(pooled_coef, 3), " L (95% CI: ", 
           round(ci_lower, 3), " to ", 
           round(ci_upper, 3), " L)"))

# Calculate p-value using normal distribution
z_score <- pooled_coef / pooled_se
p_value <- 2 * (1 - pnorm(abs(z_score)))
cat(paste0("\nP-value: ", format.pval(p_value, digits = 3)))


#################################################
# PREDICTED VALUES FOR EACH GROUP FOR VISUALIZATION
#################################################

# Initialize arrays to store predictions for each imputation
all_predictions_high_low <- numeric(10)
all_predictions_other <- numeric(10)
all_se_high_low <- numeric(10)
all_se_other <- numeric(10)

# For each imputation
for(i in 1:10) {
  # Filter data for the current imputation
  data_imp_i <- Data_Oracle_filtered %>% filter(.imp == i)
  
  # Find the trial with the largest number of non-missing FEV1_preBD_change_L values
  trial_counts <- data_imp_i %>%
    filter(!is.na(FEV1_preBD_change_L)) %>%
    count(Enrolled_Trial_name) %>%
    arrange(desc(n))
  
  # Use the trial with the most available data, or fall back to the first level if no valid trials
  best_trial <- if(nrow(trial_counts) > 0) {
    trial_counts$Enrolled_Trial_name[1]
  } else {
    levels(factor(data_imp_i$Enrolled_Trial_name))[1]
  }
  
  # Create prediction datasets specific to this imputation
  pred_data_high_low <- data.frame(
    high_FEV1_low_rev_group = "High FEV1, Low Rev",
    ACQ_baseline_score_mean_imputated = mean(data_imp_i$ACQ_baseline_score_mean_imputated, na.rm = TRUE),
    Gender_imputated = names(which.max(table(data_imp_i$Gender_imputated))),
    Age_imputated = mean(data_imp_i$Age_imputated, na.rm = TRUE),
    Treatment_step = names(which.max(table(data_imp_i$Treatment_step))),
    Number_severe_attack_previous_12m = mean(data_imp_i$Number_severe_attack_previous_12m, na.rm = TRUE),
    FEV1_preBD_L_Baseline_imputated = mean(data_imp_i$FEV1_preBD_L_Baseline_imputated, na.rm = TRUE),
    FEV1_reversibility_percent_postBD_real = mean(data_imp_i$FEV1_reversibility_percent_postBD_real, na.rm = TRUE),
    FEV1_FVC_ratio_imputated = mean(data_imp_i$FEV1_FVC_ratio_imputated, na.rm = TRUE),
    FeNO_Log_imputated = mean(data_imp_i$FeNO_Log_imputated, na.rm = TRUE),
    Eosinophils_Log_imputated = mean(data_imp_i$Eosinophils_Log_imputated, na.rm = TRUE),
    Enrolled_Trial_name = best_trial  # Use trial with most non-missing FEV1_preBD_change_L values
  )
  
  pred_data_other <- pred_data_high_low
  pred_data_other$high_FEV1_low_rev_group <- "Other"
  
  # Make predictions using this imputation's model
  pred_high_low <- predict(res_comb[[i]], newdata = pred_data_high_low, se.fit = TRUE)
  pred_other <- predict(res_comb[[i]], newdata = pred_data_other, se.fit = TRUE)
  
  # Store predictions and standard errors
  all_predictions_high_low[i] <- pred_high_low$fit
  all_predictions_other[i] <- pred_other$fit
  all_se_high_low[i] <- pred_high_low$se.fit
  all_se_other[i] <- pred_other$se.fit
}

# Calculate mean predictions across imputations (Rubin's rules)
final_pred_high_low <- mean(all_predictions_high_low)
final_pred_other <- mean(all_predictions_other)

# Calculate between-imputation variance
between_var_high_low <- var(all_predictions_high_low)
between_var_other <- var(all_predictions_other)

# Calculate within-imputation variance (average squared SE)
within_var_high_low <- mean(all_se_high_low^2)
within_var_other <- mean(all_se_other^2)

# Total variance using Rubin's formula
total_var_high_low <- within_var_high_low + (1 + 1/10) * between_var_high_low
total_var_other <- within_var_other + (1 + 1/10) * between_var_other

# Calculate confidence intervals
ci_high_low_lower <- final_pred_high_low - 1.96 * sqrt(total_var_high_low)
ci_high_low_upper <- final_pred_high_low + 1.96 * sqrt(total_var_high_low)
ci_other_lower <- final_pred_other - 1.96 * sqrt(total_var_other)
ci_other_upper <- final_pred_other + 1.96 * sqrt(total_var_other)

# Create a dataframe of predicted FEV1 changes with confidence intervals
predicted_values <- data.frame(
  Group = c("High FEV1, Low Rev", "Other"),
  FEV1_Change = c(final_pred_high_low, final_pred_other),
  Lower_CI = c(ci_high_low_lower, ci_other_lower),
  Upper_CI = c(ci_high_low_upper, ci_other_upper)
)

# Print predicted values
cat("\n\nAdjusted FEV1 Pre-Bronchodilator Change (L) with Proper Multiple Imputation Pooling:\n")
print(predicted_values)

# Calculate difference in predicted values (should match the coefficient)
difference_from_predictions <- final_pred_high_low - final_pred_other

# For difference CI, calculate variance of difference
var_difference <- total_var_high_low + total_var_other
difference_lower <- difference_from_predictions - 1.96 * sqrt(var_difference)
difference_upper <- difference_from_predictions + 1.96 * sqrt(var_difference)

# Print difference from predicted values (alternative calculation)
cat("\nDifference from predicted values (High FEV1, Low Rev vs. Other): ", 
    round(difference_from_predictions, 3), " L (95% CI: ", 
    round(difference_lower, 3), " to ", 
    round(difference_upper, 3), " L)\n")


###########EXTRA TABLE 1S FOR REVIEWER 2##################
# Additional Table 1 analyses for High FEV1 Only and Low Reversibility Only groups

# ===================================================================
# TABLE 1B: HIGH FEV1 ONLY GROUP (FEV1 ≥80%) vs NOT HIGH FEV1
# ===================================================================

cat("\n\n=== TABLE 1B: HIGH FEV1 ONLY GROUP ANALYSIS ===\n")

# Filter to imputation 1 only for High FEV1 analysis
data_imp1_fev1 <- subset(Data_Oracle_filtered, 
                         .imp == 1 & 
                           !is.na(high_FEV1_group))

# Calculate FEV1 change variable if not already done
data_imp1_fev1 <- data_imp1_fev1 %>%
  mutate(FEV1_preBD_change_L = FEV1PREBD_L_52W - FEV1_preBD_L_Baseline_imputated,
         FEV1_postBD_change_L = FEV1POSTBD_L_52W - FEV1_postBD_L_Baseline_imputated)

# Create adolescent variable (age < 18)
data_imp1_fev1 <- data_imp1_fev1 %>%
  mutate(Adolescent = factor(ifelse(Age_imputated < 18, "Yes", "No")))

# Apply antilog transformation for biomarkers
data_imp1_fev1$FeNO_Antilog <- 10^(data_imp1_fev1$FeNO_Log_imputated)
data_imp1_fev1$Eosinophils_Antilog <- 10^(data_imp1_fev1$Eosinophils_Log_imputated)

# Calculate group-level summary statistics
group_summary_fev1 <- data_imp1_fev1 %>%
  group_by(high_FEV1_group) %>%
  summarise(
    Group_Total_Attacks = sum(Number_severe_asthma_attacks_during_followup, na.rm = TRUE),
    total_followup_years = sum(Follow_up_duration_years, na.rm = TRUE),
    n_patients = n(),
    unadjusted_annual_rate = Group_Total_Attacks / total_followup_years
  )

cat("\nUnadjusted Annualized Asthma Attack Rates by High FEV1 Group:\n")
print(group_summary_fev1)

# Add group-level variables to the data frame
data_imp1_fev1 <- data_imp1_fev1 %>%
  group_by(high_FEV1_group) %>%
  mutate(
    Group_Total_Attacks = sum(Number_severe_asthma_attacks_during_followup, na.rm = TRUE),
    total_followup_years = sum(Follow_up_duration_years, na.rm = TRUE),
    Group_Unadjusted_Annual_Rate = Group_Total_Attacks / total_followup_years
  ) %>%
  ungroup()

# Create categorical variable for trial
data_imp1_fev1$Enrolled_Trial_Category <- factor(data_imp1_fev1$Enrolled_Trial_name)

# Define variables for the table
vars_to_compare_fev1 <- c("Age_imputated",
                          "Adolescent",
                          "Gender_imputated",
                          "Treatment_step",
                          "Number_severe_attack_previous_12m",
                          "FEV1_preBD_L_Baseline_imputated",
                          "FEV1_preBD_PCT_Baseline_imputated",
                          "FEV1_postBD_L_Baseline_imputated",
                          "FEV1_postBD_PCT_Baseline_imputated",
                          "FEV1_reversibility_percent_postBD_real",
                          "FEV1_FVC_ratio_imputated",
                          "ACQ_baseline_score_mean_imputated", 
                          "FeNO_Antilog", 
                          "Eosinophils_Antilog",
                          "Enrolled_Trial_Category",  
                          "Group_Total_Attacks",  
                          "total_followup_years",
                          "Group_Unadjusted_Annual_Rate",
                          "FEV1_preBD_change_L",
                          "FEV1_postBD_change_L")

# Create table for High FEV1 group
table1_fev1 <- CreateTableOne(vars = vars_to_compare_fev1, 
                              strata = "high_FEV1_group", 
                              data = data_imp1_fev1, 
                              addOverall = FALSE)

# Convert table to modifiable format
table1_fev1_summary <- print(table1_fev1, showAllLevels = TRUE, quote = FALSE, pDigits = 3, printToggle = FALSE)

# Print results
cat("\n=== TABLE 1B: HIGH FEV1 ONLY GROUP CHARACTERISTICS ===\n")
print(kable(table1_fev1_summary, format = "markdown", caption = "Table 1B. Comparison of High FEV1 Only vs Not High FEV1 patients"))

# Export as files
writeLines(htmlTable(table1_fev1_summary), "table1b_high_fev1_only.html")
write.csv(table1_fev1_summary, "table1b_high_fev1_only.csv")

# ===================================================================
# TABLE 1C: LOW REVERSIBILITY ONLY GROUP (REV <12%) vs NOT LOW REV
# ===================================================================

cat("\n\n=== TABLE 1C: LOW REVERSIBILITY ONLY GROUP ANALYSIS ===\n")

# Filter to imputation 1 only for Low Reversibility analysis
data_imp1_rev <- subset(Data_Oracle_filtered, 
                        .imp == 1 & 
                          !is.na(low_rev_group))

# Calculate FEV1 change variable if not already done
data_imp1_rev <- data_imp1_rev %>%
  mutate(FEV1_preBD_change_L = FEV1PREBD_L_52W - FEV1_preBD_L_Baseline_imputated,
         FEV1_postBD_change_L = FEV1POSTBD_L_52W - FEV1_postBD_L_Baseline_imputated)

# Apply antilog transformation for biomarkers
data_imp1_rev$FeNO_Antilog <- 10^(data_imp1_rev$FeNO_Log_imputated)
data_imp1_rev$Eosinophils_Antilog <- 10^(data_imp1_rev$Eosinophils_Log_imputated)

# Calculate group-level summary statistics
group_summary_rev <- data_imp1_rev %>%
  group_by(low_rev_group) %>%
  summarise(
    Group_Total_Attacks = sum(Number_severe_asthma_attacks_during_followup, na.rm = TRUE),
    total_followup_years = sum(Follow_up_duration_years, na.rm = TRUE),
    n_patients = n(),
    unadjusted_annual_rate = Group_Total_Attacks / total_followup_years
  )

cat("\nUnadjusted Annualized Asthma Attack Rates by Low Reversibility Group:\n")
print(group_summary_rev)

# Add group-level variables to the data frame
data_imp1_rev <- data_imp1_rev %>%
  group_by(low_rev_group) %>%
  mutate(
    Group_Total_Attacks = sum(Number_severe_asthma_attacks_during_followup, na.rm = TRUE),
    total_followup_years = sum(Follow_up_duration_years, na.rm = TRUE),
    Group_Unadjusted_Annual_Rate = Group_Total_Attacks / total_followup_years
  ) %>%
  ungroup()

# Create categorical variable for trial
data_imp1_rev$Enrolled_Trial_Category <- factor(data_imp1_rev$Enrolled_Trial_name)

# Define variables for the table (same as before)
vars_to_compare_rev <- c("Age_imputated",
                         "Gender_imputated",
                         "Treatment_step",
                         "Number_severe_attack_previous_12m",
                         "FEV1_preBD_L_Baseline_imputated",
                         "FEV1_preBD_PCT_Baseline_imputated",
                         "FEV1_postBD_L_Baseline_imputated",
                         "FEV1_postBD_PCT_Baseline_imputated",
                         "FEV1_reversibility_percent_postBD_real",
                         "FEV1_FVC_ratio_imputated",
                         "ACQ_baseline_score_mean_imputated", 
                         "FeNO_Antilog", 
                         "Eosinophils_Antilog",
                         "Enrolled_Trial_Category",  
                         "Group_Total_Attacks",  
                         "total_followup_years",
                         "Group_Unadjusted_Annual_Rate",
                         "FEV1_preBD_change_L",
                         "FEV1_postBD_change_L")

# Create table for Low Reversibility group
table1_rev <- CreateTableOne(vars = vars_to_compare_rev, 
                             strata = "low_rev_group", 
                             data = data_imp1_rev, 
                             addOverall = FALSE)

# Convert table to modifiable format
table1_rev_summary <- print(table1_rev, showAllLevels = TRUE, quote = FALSE, pDigits = 3, printToggle = FALSE)

# Print results
cat("\n=== TABLE 1C: LOW REVERSIBILITY ONLY GROUP CHARACTERISTICS ===\n")
print(kable(table1_rev_summary, format = "markdown", caption = "Table 1C. Comparison of Low Reversibility Only vs Not Low Reversibility patients"))

# Export as files
writeLines(htmlTable(table1_rev_summary), "table1c_low_reversibility_only.html")
write.csv(table1_rev_summary, "table1c_low_reversibility_only.csv")

# ===================================================================
# SUMMARY COMPARISON OF ALL THREE GROUPINGS
# ===================================================================

cat("\n\n=== SUMMARY COMPARISON OF GROUPING STRATEGIES ===\n")

# Create summary table of sample sizes and crude rates
summary_comparison <- data.frame(
  Grouping = c("High FEV1 + Low Rev", "High FEV1 Only", "Low Rev Only"),
  Group_N = c(
    sum(data_imp1$high_FEV1_low_rev_group == "High FEV1, Low Rev"),
    sum(data_imp1_fev1$high_FEV1_group == "High FEV1 Only"),
    sum(data_imp1_rev$low_rev_group == "Low Rev Only")
  ),
  Other_N = c(
    sum(data_imp1$high_FEV1_low_rev_group == "Other"),
    sum(data_imp1_fev1$high_FEV1_group == "Not High FEV1"),
    sum(data_imp1_rev$low_rev_group == "Not Low Rev")
  ),
  Group_Rate = c(
    group_summary$unadjusted_annual_rate[group_summary$high_FEV1_low_rev_group == "High FEV1, Low Rev"],
    group_summary_fev1$unadjusted_annual_rate[group_summary_fev1$high_FEV1_group == "High FEV1 Only"],
    group_summary_rev$unadjusted_annual_rate[group_summary_rev$low_rev_group == "Low Rev Only"]
  ),
  Other_Rate = c(
    group_summary$unadjusted_annual_rate[group_summary$high_FEV1_low_rev_group == "Other"],
    group_summary_fev1$unadjusted_annual_rate[group_summary_fev1$high_FEV1_group == "Not High FEV1"],
    group_summary_rev$unadjusted_annual_rate[group_summary_rev$low_rev_group == "Not Low Rev"]
  )
)

# Format rates to 2 decimal places
summary_comparison$Group_Rate <- round(summary_comparison$Group_Rate, 2)
summary_comparison$Other_Rate <- round(summary_comparison$Other_Rate, 2)

cat("Summary of all grouping strategies:\n")
print(summary_comparison)