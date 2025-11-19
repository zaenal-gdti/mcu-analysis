library(tidyverse)
library(readxl)
library(writexl)

df <- read_excel(
  'data_input.xlsx'
)

df <- df %>%
  rename_with(~ str_replace_all(
    str_trim(str_to_lower(.x)),
    "\\s+", "_"
  ))

df <- df %>%
  mutate(across(
    c(systolic_blood_pressure, diastolic_blood_pressure, bmi,
      uric_acid, total_cholesterol, fasting_blood_glucose),
    ~ as.numeric(readr::parse_number(.x))
  ))

df <- df %>%
  mutate(
    blood_pressure_category = case_when(
      is.na(systolic_blood_pressure) | is.na(diastolic_blood_pressure) ~ "Invalid",
      systolic_blood_pressure >= 180 | diastolic_blood_pressure >= 110 ~ "Derajat 3",
      systolic_blood_pressure >= 160 | diastolic_blood_pressure >= 100 ~ "Derajat 2",
      systolic_blood_pressure >= 140 | diastolic_blood_pressure >= 90  ~ "Derajat 1",
      systolic_blood_pressure >= 120 | diastolic_blood_pressure >= 80  ~ "Prehipertensi",
      TRUE ~ "Normal"
    )
  )


df <- df %>%
  mutate(
    bmi_category = case_when(
      is.na(bmi) ~ "Invalid",
      bmi < 18.5 ~ "Underweight",
      bmi >= 18.5 & bmi <= 25 ~ "Normal weight",
      bmi > 25 ~ "Overweight/Obese"
    ))


df <- df %>%
  mutate(
    uric_acid_status = case_when(
      is.na(gender) | is.na(uric_acid) ~ "Invalid",
      gender == "male"   & uric_acid > 7 ~ "Hyperuricemia",
      gender == "female" & uric_acid > 6 ~ "Hyperuricemia",
      TRUE                              ~ "Normal"
    )
  )



df <- df %>%
  mutate(
    cholesterol_category = case_when(
      is.na(total_cholesterol) ~ "Invalid",
      total_cholesterol < 150 ~ "Hijau",
      total_cholesterol >= 150 & total_cholesterol < 200 ~ "Kuning",
      total_cholesterol >= 200 ~ "Merah"
    )
  )

df <- df %>%
  mutate(
    glucose_category = case_when(
      is.na(fasting_blood_glucose) ~ "Invalid",
      fasting_blood_glucose < 90 ~ "Hijau",
      fasting_blood_glucose >= 90 & fasting_blood_glucose <= 126 ~ "Kuning",
      fasting_blood_glucose > 126 ~ "Merah"
    )
  )

df <- df %>%
  mutate(
    egfr_category = case_when(
      is.na(egfr) ~ "Invalid",
      egfr > 90 ~ "Hijau",
      egfr <= 90 & egfr >= 60 ~ "Kuning",
      egfr < 60 ~ "Merah"
    )
  )

df <- df %>% rename(sex = gender, 
                    parameter_kolesterol = cholesterol_category,
                    parameter_asam_urat  = uric_acid_status,
                    parameter_bmi    = bmi_category   ,
                    parameter_gula_darah  = glucose_category,
                    parameter_tekanan_darah   = blood_pressure_category,
                    parameter_fungsi_ginjal = egfr_category,
                    diagnoses = diagnose  
                    )  %>%
    select(sex, usia, parameter_kolesterol, parameter_asam_urat,
           parameter_tekanan_darah, parameter_gula_darah, parameter_bmi,
           parameter_fungsi_ginjal, diagnoses )

    
    
df %>% write_xlsx('data.xlsx')


