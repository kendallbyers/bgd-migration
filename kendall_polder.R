rm(list=ls())

pacman::p_load(dplyr,
               data.table,
               stargazer,
               systemfit,
               aod,
               jtools,
               texreg,
               mfx,
               margins,
               corrplot,
               gvlma)

###############################################################################

#Load original polder data file
dat <- read.csv("C:/Users/kenda/Documents/R/bgd-migration/HHdata_cleanNEW.csv")
dat <- data.table(dat)
View(dat)

dat <- dat %>%
  select(POL_NAME, gender_hh, age_hh, religion_hh, edu_hh, #Sec1
         papers_plot, area_plot_1, area_plot_2, status_plot_1, status_plot_2, #Sec2
         crop_submerged_plot_1, crop_submerged_plot_2, total_land, #Sec2
         freq_flood, freq_drought, freq_salinity, freq_insects, loss_prod_flood, #Sec4
         loss_prod_drought, loss_prod_salinity, loss_prod_insect, #Sec4
         memb_wmg, # Sec5
         num_migrants, nature_migrants1, gender_1, age_1, edu_1, place_mig_1, #Sec6
         name_place_1, mig_job_1, loan_mig_1, remitences, women_involve_increase,#Sec6
         agi_group, water_use_group, credit_group, lg_group, womens_group, #Sec7
         development_gp, #Sec7
         num_male_agri_lobor, wage_male_lobor, num_female_agri_lobor, #Sec8
         wage_female_agri_lobor, num_male_nonagri_lobor, wage_male_nonagri_lobor, #Sec8
         num_female_nonagri_lobor, wage_female_nonagri_lobor, income_poultry, #Sec8
         income_fish, income_vege, income_assests, income_wage, #Sec8
         income_wage_nonagri, salary_pension, income_remi, income_rent, #Sec8
         income_bussiness, income_transport, income_caste_occu, income_other, #Sec8
         annual_income_poultry, annual_income_fish, annual_income_vege, #Sec8
         annual_income_assests, annual_income_wage, annual_income_wage_non_agri, #Sec8
         annual_salary_pension, annual_income_remi, annual_income_ren, #Sec8
         annual_income_bussi, annual_income_transport, annual_income_caste_occu, #Sec8
         annual_income_others, #Sec8
         food_short_boi, food_short_jios, food_short_ash, food_short_sra, #Sec9
         food_short_bhadro, food_short_ashshin, food_short_kartik, food_short_ograon, #Sec9
         food_short_poush, food_short_magh, food_short_falgun, food_short_choitro, #Sec9
         reduce_quantity, reduce_time_eat, without_food, borrow_food, loan_food, #Sec9
         loan_micro, exchange_things, morgate_land, morgate_non_land, #Sec9
         rice, wheat, maize, potato, vegetable, fruits, pulses, egges, #Sec9
         meat, meat_chi, fish, oils, dairy, quantity_rice, #Sec9
         num_adult_male, num_adult_female, num_child_male, num_child_female, #Sec10
         literate_male, literate_female, literate_child_male, literate_child_female, #Sec10
         working_adult_male, working_adult_female, working_child_male, working_child_female, #Sec10
         latrine, elctricity, month_stock, paddy_boi, paddy_jois, paddy_asharh, #Sec10
         paddy_srabon, paddy_bhadro, paddy_ashshin, paddy_katrik, paddy_ogra, #Sec10
         paddy_poush, paddy_magh, paddy_falgun, paddy_choitro, income_1, #Sec10
         khat, chair, table, sofa, almira, wooden_box, radio, television, sew_machine, #Sec11
         stove, mobile, bicycle, rickshaw, motorcycle, plough, power_tiller, #Sec11
         thresing_machine, spray_machine, husking_machine, tredle_pump, #Sec11
         manual_pump, solar_panel, battery, bank_acc, #Sec11
         bullock, cow, colf, buffalo, goat, sheep, pigeon, chiken, duck, goose) #Sec11

dat <- data.table(dat)
dat %>% count(place_mig_1)

dat$place_mig_1 <- as.factor(dat$place_mig_1)

dat$age_hh=as.numeric(dat$age_hh)
dat$papers_plot=as.numeric(dat$papers_plot)
dat$total_land=as.integer(dat$total_land)
dat$freq_flood=as.numeric(dat$freq_flood)
dat$freq_drought=as.numeric(dat$freq_drought)
dat$freq_salinity=as.numeric(dat$freq_salinity)
dat$freq_insects=as.numeric(dat$freq_insects)
dat$loss_prod_flood=as.numeric(dat$loss_prod_flood)
dat$loss_prod_drought=as.numeric(dat$loss_prod_drought)
dat$loss_pord_salinity=as.numeric(dat$loss_pord_salinity)
dat$loss_prod_insect=as.numeric(dat$loss_prod_insect)
dat$num_migrants=as.numeric(dat$num_migrants)
dat$age_1=as.numeric(dat$age_1)
dat$edu_1=as.numeric(dat$edu_1)
dat$num_male_agri_lobor=as.numeric(dat$num_male_agri_lobor)
dat$num_female_agri_lobor=as.numeric(dat$num_female_agri_lobor)
dat$num_male_nonagri_lobor=as.numeric(dat$num_male_nonagri_lobor)
dat$num_female_nonagri_lobor=as.numeric(dat$num_female_nonagri_lobor)
dat$month_stock=as.numeric(dat$month_stock)

#attributes(dat)
#str(dat)

# Our Dependent Variable: At Least One Household Migrant
dat$migration <- ifelse(dat$num_migrants > 0, 1, 0)
dat$migration = as.factor(dat$migration)

summary(dat$migration)

dat$edu_hh_code <- ifelse(dat$edu_hh == "no school", "Illiterate", "Literate")
dat$edu_hh_code=as.factor(dat$edu_hh_code)

summary(dat$edu_hh_code)

dat$farm_types <- ifelse(dat$papers_plot > 0, "Have Plot Papers", "No Papers")
dat$farm_types=as.factor(dat$farm_types)

summary(dat$farm_types)

dat <- dat %>%
  select(-papers_plot)

dat <- dat %>%
  rowwise() %>%
  mutate(total_plot_area = sum(area_plot_1, area_plot_2, na.rm = TRUE)) %>%
  mutate(total_plot_area_ha = total_plot_area * 0.00404686) %>% # Convert decimal to hectare
  select(-total_plot_area) # Deleted "total_plot_area" decimal column

summary(dat$total_plot_area_ha)

dat <- dat %>%
  select(-area_plot_1)

dat <- dat %>%
  select(-area_plot_2)

#We can use the above one (farm types)
dat$plot1_status <- ifelse(dat$status_plot_1 == "Have papers and used by the HH", "Owned", "Lease & Shared")
dat$plot1_status=as.factor(dat$plot1_status)
dat <- dat %>%
  select(-status_plot_1)

#We can use the above one (farm types)
dat$plot2_status <- ifelse(dat$status_plot_2 == "Have papers and used by the HH", "Owned", "Lease & Shared")
dat$plot2_status=as.factor(dat$plot2_status)
dat <- dat %>%
  select(-status_plot_2)

summary(dat$plot1_status)
summary(dat$plot2_status)

#There are some data problem with Sec 4:
#freq_flood, freq_drought, freq_salinity, freq_insects
#loss_prod_flood, loss_prod_drought, loss_prod_salinity, loss_prod_insects
## I have corrected the data in excel and just copy here.

dat$migrant_edu_code <- ifelse(dat$edu_1 > 1, "Literate", "Illiterate")
dat$migrant_edu_code=as.factor(dat$migrant_edu_code)
dat <- dat %>%
  select(-edu_1)

summary(dat$migrant_edu_code)

dat$migration_place <- ifelse(dat$place_mig_1 == "abroad", "Abroad",
                           ifelse(dat$place_mig_1 == "rural","Rural",
                           ifelse(dat$place_mig_1 == "urban","Urban","Multiple Locations")))

dat$migration_place=as.factor(dat$migration_place)
summary(dat$migration_place)
dat <- dat %>%
  select(-place_mig_1)

#Annual income from Agriculture Sources
dat <- dat %>%
  rowwise() %>%
  mutate(Annual_income_Agriculture_BDT = sum(annual_income_poultry, annual_income_fish, annual_income_vege,
                                                  annual_income_assests, annual_income_wage, annual_income_ren,
                                                  na.rm = TRUE)) %>%
  mutate(Annual_income_Agriculture_combined_USD = Annual_income_Agriculture_BDT / 84.75) %>% # One USD = 84.75 BDT
  select(-Annual_income_Agriculture_BDT)
dat$Annual_income_Agriculture_combined_USD=as.integer(dat$Annual_income_Agriculture_combined_USD)
summary(dat$Annual_income_Agriculture_combined_USD)

#Annual income from Non-Agriculture Sources
dat <- dat %>%
  rowwise() %>%
  mutate(Annual_income_Non_Agriculture_BDT = sum(annual_income_wage_non_agri, annual_salary_pension, annual_income_bussi,
                                                 annual_income_transport, annual_income_caste_occu, annual_income_others,
                                                 na.rm = TRUE)) %>%
  mutate(Annual_income_Non_Agriculture_combined_USD = Annual_income_Non_Agriculture_BDT / 84.75) %>% # One USD = 84.75 BDT
  select(-Annual_income_Non_Agriculture_BDT)
dat$Annual_income_Non_Agriculture_combined_USD=as.integer(dat$Annual_income_Non_Agriculture_combined_USD)
summary(dat$Annual_income_Non_Agriculture_combined_USD)

#Annual income from Remittance
dat <- dat %>%
  mutate(Annual_income_Remittance_USD = annual_income_remi / 84.75) %>% # One BDT = 84.75 USD
  select(-annual_income_remi)
dat$Annual_income_Remittance_USD=as.integer(dat$Annual_income_Remittance_USD)
summary(dat$Annual_income_Remittance_USD)

#Coding Food Shortages in Any Month as variable
dat$food_short <- ifelse(dat$food_short_boi == "food shortage" |
                               dat$food_short_jios == "food shortage" |
                               dat$food_short_ash == "food shortage" |
                               dat$food_short_sra == "food shortage" |
                               dat$food_short_bhadro == "food shortage" |
                               dat$food_short_ashshin == "food shortage" |
                               dat$food_short_kartik == "food shortage" |
                               dat$food_short_ograon == "food shortage" |
                               dat$food_short_poush == "food shortage" |
                               dat$food_short_magh == "food shortage" |
                               dat$food_short_falgun == "food shortage" |
                               dat$food_short_choitro == "food shortage", "Food Shortage", "No Food Shortage")
dat$food_short=as.factor(dat$food_short)
summary(dat$food_short)

#Of the hungry, only 51/236 (or 21.6%) migrated
ofhungry_movers <- dat %>%
  filter(food_short == "Food Shortage") %>%
  select(migration)
summary(ofhungry_movers)

#Of the migrants, about 51/165 (or 30.9%) had a food shortage during part of the year
ofmovers_hungry <- dat %>%
  filter(migration == 1) %>%
  select(food_short)
summary(ofmovers_hungry)

#Food Security: coding shortage-induced food restriction and debt

dat$food_restriction <- ifelse(dat$reduce_quantity == "Yes" |
                                      dat$reduce_time_eat == "Yes" |
                                      dat$without_food == "Yes", "Forced Food Restriction", "No Forced Food Restriction")
dat$food_debt <- ifelse(dat$borrow_food == "Yes" |
                               dat$loan_food == "Yes" |
                               dat$loan_micro == "Yes" |
                               dat$exchange_things == "Yes" |
                               dat$morgate_land == "Yes" |
                               dat$morgate_non_land == "Yes", "Food Debt", "No Food Debt")

dat %>% count(food_restriction)
dat %>% count(food_debt)
#Food Consumption Score according to World Food Programme

#FCS thresholds: 0-21 = Poor, 21.5-35 = Borderline, > 35 = Healthy

# #FCS <- dat %>% select(rice, wheat, maize, potato,
#               pulses,
#               fruits,
#               vegetable,
#               egges, meat, meat_chi, fish,
#               dairy,
#               oils)
#FCS formula = (Main staples)*2 + (Pulses)*3 + (Veg)*1 + (Fruit)*1 + (Meat + Fish)*4 + (Milk)*4 + (Sugar)*0.5 + (Oil)*0.5
#Round down to 7 if any food group exceeds 7, prior to multiplying
# FCS$staples <- FCS %>% (rice + wheat + maize + potato
#                         %>% >= 7, then 7
#                         %>% staples*2
#
# FCS$dal <- (if FCS$pulses >7, then 7)*3
# FCS$phal <- FCS$fruits
# FCS$sabzi <- FCS$vegetable
# FCS$protein <- FCS %>% (egges + meat + meat_chi + fish) #round down to 7, then multiply by 4
# FCS$dood <- FCS %>% dairy*4
# FCS$tel <- FCS %>% oil/2
# FCS$rawscore = FCS %>% sum (staples + dal + phal + sabzi + protein + dood + tel)
# if rawscore =< 21 then "Poor", if rawscore =>21.5 <=35 then "Borderline", if rawscore > 35 then "Healthy"

# Sec 10 - General Social Capital and Housing Assets

# "num_adult_male" "num_adult_female" "num_child_male" "num_child_female"
#"literate_male" "literate_female" "literate_child_male" "literate_child_female" "working_adult_male" "working_adult_female" "working_child_male" "working_child_female"

# I will develop Wealth Index from Sec 11

#Models

reg_var <- dat %>%
  select(farm_types, total_plot_area_ha, migrant_edu_code, Annual_income_Agriculture_combined_USD,
         Annual_income_Non_Agriculture_combined_USD, food_short)

# Base model - demographics etc - significant or not, have to be
base_reg <- formula("migration ~ farm_types + memb_wmg + total_plot_area_ha + edu_hh_code")

glm(base_reg, family = binomial(link="probit"), data=dat)

Mod1 <- glm(migration ~ farm_types + memb_wmg + total_plot_area_ha + edu_hh_code + loss_prod_insect + loss_prod_drought +
              loss_prod_salinity + loss_prod_flood + num_adult_male + literate_male +
              Annual_income_Agriculture_combined_USD,
              family = binomial(link = "probit"), data = dat)

summ(Mod1)

# Specific test #1 - income on migration

# Specific test #2
