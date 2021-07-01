library(icd)
library(tidyverse)
library(stringi)


first_3 <- function(x) {
  # retain first 3 characters of the ICD code
  substr(x, 1, 3) %>% unique()
}


# import mapping from icd package
icd10_comorb_map = icd10_map_quan_elix
icd9_comorb_map = icd9_map_quan_elix

# truncate all codes to the first 3 characters
icd10_comorb_map <- lapply(icd10_comorb_map, first_3)
icd9_comorb_map <- lapply(icd9_comorb_map, first_3)


comorb_formatted_list <- list()

# combine hypertension and diabetes
for (i in names(icd10_comorb_map)) {

  formatted_comorb = stri_join_list(icd10_comorb_map[i], sep = ", ") %>%
    as.data.frame() %>%
    mutate(Comorbidity = paste(i)) %>%
    rename("ICD-10 Codes" = ".") %>%
    select(Comorbidity, `ICD-10 Codes`)

  comorb_formatted_list[[i]] <- formatted_comorb

}


comorbidity_code_table <- bind_rows(comorb_formatted_list)


hypertension_combined <- comorbidity_code_table %>%
  filter(Comorbidity == "HTN" | Comorbidity == "HTNcx") %>%
  select(`ICD-10 Codes`) %>%
  as.list() %>%
  sapply(., toString) %>%
  as.data.frame() %>%
  rename("ICD-10 Codes" = ".") %>%
  mutate(Comorbidity = "HTN, combined") %>%
  select(Comorbidity, `ICD-10 Codes`)

diabetes_combined <- comorbidity_code_table %>%
  filter(Comorbidity == "DM" | Comorbidity == "DMcx") %>%
  select(`ICD-10 Codes`) %>%
  as.list() %>%
  sapply(., toString) %>%
  as.data.frame() %>%
  rename("ICD-10 Codes" = ".") %>%
  mutate(Comorbidity = "DM") %>%
  select(Comorbidity, `ICD-10 Codes`)

comorbidity_code_table <- comorbidity_code_table %>%
  filter(!Comorbidity %in% c("HTN", "HTNcx", "DM", "DMcx")) %>%
  rbind(., diabetes_combined, hypertension_combined)

# create a list of comorbs
# this should the order that the van_walraven_from_comorbid function applies weights
comorb_names <- names(icd10_comorb_map)

# add the weights
# note that hypertension will need to be in the righ oft order ( I manually moved the weights by hand to end for Hypertension and diabetes)
# note that each diabetes code has a weight (of 0), but I removed 1
van_walraven_weights <- c(7, 5, -1, 4, 2, 7, 6, 3, 0, 5, 11,
                          0, 0, 9, 12, 4, 0, 3, -4, 6, 5, -2, -2, 0, -7, 0, -3, 0, 0)

# should have 29 values
length(van_walraven_weights)


comorbidity_code_table <- comorbidity_code_table %>% cbind(van_walraven_weights)

# add full names for comorbidities to replace abbreviations

# this list was adaptaed from that in mappingNames.R provided by icd package
comorbidity_full <- c(
  "Congestive heart failure",
  "Cardiac arrhythmias",
  "Valvular disease",
  "Pulmonary circulation disorders",
  "Peripheral vascular disorders",
  "Paralysis",
  "Other neurological disorders",
  "Chronic pulmonary disease",
  "Hypothyroidism",
  "Renal failure",
  "Liver disease",
  "Peptic ulcer disease excluding bleeding",
  "HIV/AIDS",
  "Lymphoma",
  "Metastatic cancer",
  "Solid tumor without metastasis",
  "Rheumatoid arthritis/collagen vascular diseases",
  "Coagulopathy",
  "Obesity",
  "Weight loss",
  "Fluid and electrolye disorders",
  "Blood loss anemia",
  "Deficiency anemias",
  "Alcohol abuse",
  "Drug abuse",
  "Psychoses",
  "Depression",
  "Diabetes",
  "Hypertension"
)

comorbidity_code_table <- comorbidity_code_table %>%
  cbind(comorbidity_full) %>%
  arrange(comorbidity_full) %>%
  select(comorbidity_full, `ICD-10 Codes`, van_walraven_weights) %>%
  rename("Comorbidity" = comorbidity_full,
         "Van Walraven Weights" = van_walraven_weights)




write.csv(comorbidity_code_table, "tables/Table_Comorbidity_Mapping.csv", row.names = FALSE)
