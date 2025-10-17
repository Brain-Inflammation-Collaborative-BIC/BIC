# Install if needed
install.packages(c("readxl", "dplyr", "writexl"))

# Load libraries
library(readxl)
library(dplyr)
library(writexl)

# Read the correct tab from your Excel file
file_path <- "/Users/jameshunt/Desktop/BIC Data/Vassar_Request_10_7.xlsx"

df <- read_excel(file_path, sheet = "Vassar_Request")

df <- df %>%
  mutate(
    # ---- Physical Functioning (reverse 1–3)
    sf36_vig_act_r = 4 - sf36_vig_act,
    sf36_mod_act_r = 4 - sf36_mod_act,
    sf36_groceries_r = 4 - sf36_groceries,
    sf36_stairs_sev_r = 4 - sf36_stairs_sev,
    sf36_stairs_one_r = 4 - sf36_stairs_one,
    sf36_bending_r = 4 - sf36_bending,
    sf36_walk_miles_r = 4 - sf36_walk_miles,
    sf36_walk_blocks_r = 4 - sf36_walk_blocks,
    sf36_walk_block_r = 4 - sf36_walk_block,
    sf36_bath_r = 4 - sf36_bath,
    
    # ---- Role Physical (reverse 1–2)
    sf36_phys_act_r = 3 - sf36_phys_act,
    sf36_phys_less_r = 3 - sf36_phys_less,
    sf36_phys_act_limit_r = 3 - sf36_phys_act_limit,
    sf36_phys_act_diff_r = 3 - sf36_phys_act_diff,
    
    # ---- Bodily Pain
    sf36_pain_r = 7 - sf36_pain,
    sf36_pain_work_r = 6 - sf36_pain_work,
    
    # ---- General Health (some reversed)
    sf36_gen_health_r = 6 - sf36_gen_health,
    sf36_health_excel_r = 6 - sf36_health_excel,
    sf36_healthy_r = 6 - sf36_healthy,
    sf36_sick_r = 6 - sf36_sick,
    sf36_health_worse_r = sf36_health_worse,  # not reversed
    
    # ---- Vitality
    sf36_pep_r = 7 - sf36_pep,
    sf36_tired_r = sf36_tired,
    sf36_energy_r = 7 - sf36_energy,
    sf36_worn_out_r = sf36_worn_out,
    
    # ---- Social Functioning
    sf36_social_interfere_r = 6 - sf36_social_interfere,
    sf36_social_act_r = 6 - sf36_social_act,
    
    # ---- Role Emotional
    sf36_emot_act_r = 3 - sf36_emot_act,
    sf36_emot_less_r = 3 - sf36_emot_less,
    sf36_careful_r = 3 - sf36_careful,
    
    # ---- Mental Health
    sf36_happy_r = 7 - sf36_happy,
    sf36_nervous_r = sf36_nervous,
    sf36_down_dumps_r = sf36_down_dumps,
    sf36_calm_r = 7 - sf36_calm,
    sf36_downhearted_r = sf36_downhearted
  )

df <- df %>%
  mutate(
    PF = ((rowMeans(select(., ends_with("_r"))[
      , c("sf36_vig_act_r","sf36_mod_act_r","sf36_groceries_r","sf36_stairs_sev_r",
          "sf36_stairs_one_r","sf36_bending_r","sf36_walk_miles_r","sf36_walk_blocks_r",
          "sf36_walk_block_r","sf36_bath_r")], na.rm = TRUE) - 1) / (3 - 1)) * 100,
    
    RP = ((rowMeans(select(., sf36_phys_act_r, sf36_phys_less_r, sf36_phys_act_limit_r, sf36_phys_act_diff_r),
                    na.rm = TRUE) - 1) / (2 - 1)) * 100,
    
    BP = ((rowMeans(select(., sf36_pain_r, sf36_pain_work_r), na.rm = TRUE) - 1) / (5)) * 100,
    
    GH = ((rowMeans(select(., sf36_gen_health_r, sf36_health_worse_r, sf36_health_excel_r, sf36_healthy_r, sf36_sick_r),
                    na.rm = TRUE) - 1) / (4)) * 100,
    
    VT = ((rowMeans(select(., sf36_pep_r, sf36_tired_r, sf36_energy_r, sf36_worn_out_r),
                    na.rm = TRUE) - 1) / (5)) * 100,
    
    SF = ((rowMeans(select(., sf36_social_interfere_r, sf36_social_act_r),
                    na.rm = TRUE) - 1) / (4)) * 100,
    
    RE = ((rowMeans(select(., sf36_emot_act_r, sf36_emot_less_r, sf36_careful_r),
                    na.rm = TRUE) - 1) / (1)) * 100,
    
    MH = ((rowMeans(select(., sf36_happy_r, sf36_nervous_r, sf36_down_dumps_r, sf36_calm_r, sf36_downhearted_r),
                    na.rm = TRUE) - 1) / (5)) * 100
  )

# Domain weights
pcs_weights <- c(PF=0.424, RP=0.351, BP=0.317, GH=0.249, VT=0.028, SF=-0.007, RE=-0.192, MH=-0.220)
mcs_weights <- c(PF=-0.229, RP=-0.123, BP=-0.097, GH=-0.015, VT=0.235, SF=0.268, RE=0.434, MH=0.485)

# Z-standardize domain scores using US 1998 norms
sf36_means <- c(PF=84.2, RP=81.2, BP=75.2, GH=72.0, VT=61.1, SF=83.3, RE=81.3, MH=74.7)
sf36_sds   <- c(PF=23.8, RP=33.0, BP=23.7, GH=20.9, VT=20.9, SF=22.7, RE=33.0, MH=18.0)

df <- df %>%
  mutate(across(c(PF, RP, BP, GH, VT, SF, RE, MH),
                ~ (. - sf36_means[cur_column()]) / sf36_sds[cur_column()],
                .names = "z_{.col}")) %>%
  mutate(
    PCS = (z_PF*pcs_weights["PF"] + z_RP*pcs_weights["RP"] + z_BP*pcs_weights["BP"] +
             z_GH*pcs_weights["GH"] + z_VT*pcs_weights["VT"] + z_SF*pcs_weights["SF"] +
             z_RE*pcs_weights["RE"] + z_MH*pcs_weights["MH"]) * 10 + 50,
    
    MCS = (z_PF*mcs_weights["PF"] + z_RP*mcs_weights["RP"] + z_BP*mcs_weights["BP"] +
             z_GH*mcs_weights["GH"] + z_VT*mcs_weights["VT"] + z_SF*mcs_weights["SF"] +
             z_RE*mcs_weights["RE"] + z_MH*mcs_weights["MH"]) * 10 + 50
  )

write_xlsx(df, "/Users/jameshunt/Desktop/BIC Data/Vassar_Request_10_17_scored.xlsx")
