rm(list=ls())

pacman::p_load(corrplot,
               data.table,
               stargazer,
               systemfit,
               aod,
               jtools,
               texreg,
               mfx,
               margins,
               gvlma,
               dplyr,
               summarytools,
               gtsummary)

#TO DO FOR 8/23 :
#output stargazer iterative model in markdown or pdf form
#post pdf to bgd slack channel
#Run through tbl_regression() tutorial to print markdown/pdf of

# Refer to TEAMS if you get muddled on your next steps.
###############################################################################

#Load original polder data file
dat <- read.csv("C:/Users/Kendall Byers/Documents/R/bgd-migration/data/HHdata_cleanNEW.csv")

dat <- data.table(dat)
View(dat)

#Load preanalyzed excel file (temporary for experiment - remove before publication)
# mig <- read.csv("C:/Users/kenda/Documents/R/bgd-migration/HHdata_cleanNEW.csv")
# View(mig)

dat$age_hh=as.numeric(dat$age_hh)
dat$religion_hh=as.factor(dat$religion_hh)
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

# Our Dependent Variable: At Least One Household Migrant
dat$migration <- ifelse(dat$num_migrants > 0, 1, 0)
dat$migration = as.factor(dat$migration)

#to temporary file 'mig'
# mig$migration <- dat$migration

#165 households with at least one migrant, 860 with none
summary(dat$migration)

# Separating permanent vs seasonal labor
dat$nature_migrants1 <- as.factor(dat$nature_migrants1)
summary(dat$nature_migrants1) # perm = 94, seasonal = 71
dat$nature_migrants2 <- as.factor(dat$nature_migrants2)
summary(dat$nature_migrants2) #perm = 17, seasonal = 5

#Coding literacy
dat$edu_hh_code <- ifelse(dat$edu_hh == "no school", "Illiterate", "Literate")
dat$edu_hh_code=as.factor(dat$edu_hh_code)

dat$edu_hh_spouse_code <- ifelse(dat$edu_hh_spouse == "no school", "Illiterate", "Literate")
dat$edu_hh_spouse_code=as.factor(dat$edu_hh_spouse_code)

summary(dat$edu_hh_code) #Illiterate = 126, Literate = 899
summary(dat$edu_hh_spouse_code) #Illiterate = 145, literate = 829

dat$both_literate <- ifelse(dat$edu_hh_code == "Literate" &
                            dat$edu_hh_spouse_code == "Literate", "Literate Parents", "An Illiterate Parent")
dat$both_literate  <- as.factor(dat$both_literate)
summary(dat$both_literate) #An Illiterate Parent = 208, Both Literate = 773, NA = 44

#papers or no papers for worked agricultural fields
dat$farm_types <- ifelse(dat$papers_plot > 0, "Have Plot Papers", "No Papers")
dat$farm_types=as.factor(dat$farm_types)

#tenancy
dat$sharecropping <- ifelse(dat$cultivate_without_papers > dat$papers_plot, "Tenant farmer", "Landowner")
dat$sharecropping=as.factor(dat$sharecropping)
summary(dat$sharecropping) # Landowner = 692, Tenant Farmer = 333

dat <- dat %>%
  dplyr::select(-papers_plot)

dat <- dat %>%
  rowwise() %>%
  mutate(total_plot_area = sum(area_plot_1, area_plot_2, na.rm = TRUE)) %>%
  mutate(total_plot_area_ha = total_plot_area * 0.00404686) %>% # Convert decimal to hectare
  select(-total_plot_area) # Deleted "total_plot_area" decimal column

#Median farm size is 1 acre (.4 ha), mean = .56 ha
summary(dat$total_plot_area_ha)
# ggplot(data = dat, mapping = aes(x = total_plot_area_ha, colour = total_plot_area_ha)) +
  geom_freqpoly(binwidth = 0.1)

dat <- dat %>%
  select(-area_plot_1)

dat <- dat %>%
  select(-area_plot_2)

#status of plot 1 and plot 2 - "farm types" is the preferred variable, however
dat$plot1_status <- ifelse(dat$status_plot_1 == "Have papers and used by the HH", "Owned", "Lease & Shared")
dat$plot1_status=as.factor(dat$plot1_status)
dat <- dat %>%
  select(-status_plot_1)

dat$plot2_status <- ifelse(dat$status_plot_2 == "Have papers and used by the HH", "Owned", "Lease & Shared")
dat$plot2_status=as.factor(dat$plot2_status)
dat <- dat %>%
  select(-status_plot_2)

summary(dat$plot1_status) # Lease/Shared: 436, Owned: 565, NA:24
summary(dat$plot2_status) # Lease/Shared: 331, Owned: 479, NA: 24

#Coding for sunk crops and low elevation land:
dat$crop_sunk <- ifelse(dat$crop_submerged_plot_1 == "yes" |
                          dat$crop_submerged_plot_2 == "yes", "Crops Sunk", "Not Sunk")
dat$crop_sunk <- as.factor(dat$crop_sunk)
summary(dat$crop_sunk) #257 respondents had submerged crops

dat$low_land <- ifelse(dat$elevation_plot_1 == "Low" |
                         dat$elevation_plot_2 == "Low", 1, 0)
dat$low_land <- as.factor(dat$low_land)
summary(dat$low_land) #242 respondents said that they cropped in lowlying land

#alternate measurement for lowland - percentage
dat$percent_abovesea <- ((dat$high_agri_land + dat$medium_agri_land)/dat$total_land)*100
summary(dat$percent_abovesea) #55%
hist(dat$percent_abovesea)

dat$percent_lowland <- (dat$low_agri_land/dat$total_land)*100
summary(dat$percent_lowland) #14% is mean lowland percent of total land
hist(dat$percent_lowland)

#coding poor infrastructure
dat$bad_canals <- ifelse(dat$condition_canals <= 5, "Poor Canal Condition", "OK Canals")
dat$bad_canals <- as.factor(dat$bad_canals)
summary(dat$bad_canals) # OK Canals - 539, Poor Canal Condition - 485, NA - 1

dat$bad_embankments <- ifelse(dat$condi_embankment <= 5, "Poor seawall condition", "OK seawalls")
dat$bad_embankments <- as.factor(dat$bad_embankments)
summary(dat$bad_embankments) # OK Seawalls: 796, Poor Seawalls: 229

dat$bad_gates <- ifelse(dat$cond_gate <= 5, "Poor sluice gates", "OK sluice gates")
dat$bad_gates <- as.factor(dat$bad_gates)
summary(dat$bad_gates) #OK sluices: 712, Poor sluices: 307, NA: 6

#Coding bad water management groups
summary(dat$memb_wmg) # there are 232 HHs who have a member in a WMG, 792 who do not.

dat$memb_wmg <- ifelse(dat$memb_wmg == "yes", 1, 0)
dat$bad_transparency <- ifelse(dat$transparency <= 5, 1, 0)
dat$bad_financial <- ifelse(dat$finantial <= 5, 1, 0)
dat$bad_participation <- ifelse(dat$participation <= 5, 1, 0)
dat$bad_rules <- ifelse(dat$rules <= 5, 1, 0)
dat$bad_gate_operation <- ifelse(dat$gate_operation <= 5, 1, 0)
dat$maintenance <- ifelse(dat$maintenance <= 5, 1, 0)

# Coding Education as migrant characteristic

dat$migrant_edu_code <- ifelse(dat$edu_1 > 1, "Literate", "Illiterate")
dat$migrant_edu_code=as.factor(dat$migrant_edu_code)
dat <- dat %>%
  select(-edu_1)

#150/165 of migrants had at least 1 year of education (can read), while 14 did not
summary(dat$migrant_edu_code)

#18/165 (11%) headed abroad, 35 (21%) migrated to other rural areas, 97 (59%) migrated to urban areas, and 12 (7%) to multiple places
dat$migration_place <- ifelse(dat$place_mig_1 == "abroad", "Abroad",
                           ifelse(dat$place_mig_1 == "rural","Rural",
                           ifelse(dat$place_mig_1 == "urban","Urban","Multiple Locations")))

dat$migration_place=as.factor(dat$migration_place)
summary(dat$migration_place)

dat <- dat %>%
  select(-place_mig_1)

#42 (25%) of first migrants took loans to travel, 16 of those (38%) from institutions and 26 from social networks (relatives, social credit groups)
dat$loan_mig_1 <- as.factor(dat$loan_mig_1)
summary(dat$loan_mig_1)

dat$migrant_debt <- ifelse(dat$loan_mig_1 != "no loan"|
                             dat$loan_mig_2 !="no loan", "Loan Taken", "No Loan")
dat$migrant_debt <- as.factor(dat$migrant_debt)
summary(dat$migrant_debt) #Of migrants, 46 loans taken, 17 no loan

#Annual income from Agriculture Sources -

dat <- dat %>%
  rowwise() %>%
  mutate(Annual_income_Agriculture_BDT = sum(annual_income_poultry, annual_income_fish, annual_income_vege,
                                                  annual_income_assests, annual_income_wage, annual_income_ren,
                                                  na.rm = TRUE)) %>%
  mutate(Annual_income_Agriculture_combined_USD = Annual_income_Agriculture_BDT / 84.75) %>% # One USD = 84.75 BDT
  select(-Annual_income_Agriculture_BDT)
dat$Annual_income_Agriculture_combined_USD=as.integer(dat$Annual_income_Agriculture_combined_USD)

summary(dat$Annual_income_Agriculture_combined_USD) #Median = $294/yr, Mean = $426/year

#Annual income from Non-Agriculture Sources - Median = $117/yr, Mean = $352/yr
dat <- dat %>%
  rowwise() %>%
  mutate(Annual_income_Non_Agriculture_BDT = sum(annual_income_wage_non_agri, annual_salary_pension, annual_income_bussi,
                                                 annual_income_transport, annual_income_caste_occu, annual_income_others,
                                                 na.rm = TRUE)) %>%
  mutate(Annual_income_Non_Agriculture_combined_USD = Annual_income_Non_Agriculture_BDT / 84.75) %>% # One USD = 84.75 BDT
  select(-Annual_income_Non_Agriculture_BDT)
dat$Annual_income_Non_Agriculture_combined_USD=as.integer(dat$Annual_income_Non_Agriculture_combined_USD)
summary(dat$Annual_income_Non_Agriculture_combined_USD)

#Annual income from Remittance - Median = $707, Mean = $640/yr, although wider distribution than either local work
dat <- dat %>%
  mutate(Annual_income_Remittance_USD = annual_income_remi / 84.75) %>% # One BDT = 84.75 USD
  select(-annual_income_remi)
dat$Annual_income_Remittance_USD=as.integer(dat$Annual_income_Remittance_USD)
summary(dat$Annual_income_Remittance_USD)

# ggplot(data = dat, mapping = aes(x = Annual_income_Remittance_USD, colour = Annual_income_Remittance_USD)) +
#   geom_freqpoly(binwidth = 0.1)

#There's room here for computing x = length of migration and y = annual income remittance USD

#How to compare agriculture, non-agriculture, and remittance household income?

#What are the average total savings, and is that a potential regressor? - Median is 300 (bdt?), Mean is 634 bdt
summary(dat$total_savings) #Median is 300, Mean is 634, Max is 12,000


#remittances priorities: if loan or food
#TO DO: subset analysis using filter and select of migrant's 1st or 2nd priorities (push/pull determination)


#Coding Food Shortages in Any Month as variable - 236 as "1" for
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
                               dat$food_short_choitro == "food shortage", 1, 0)

sum(dat$food_short) #indeed, there are 236 households reporting food shortage in any year.

#Here is the food_short generalized linear model, showing significance for migration

nofood <- formula("migration ~ food_short")
nofoodmod <- glm(nofood, family = binomial(link="probit"), data=dat)
summary(nofoodmod) #In short, "No Food Shortage" is highly correlated with migration (99%)

#owever, of the hungry, only 51/236 (or 21.6%) migrated
ofhungry_movers <- dat %>%
  filter(food_short == 1) %>%
  select(migration)
summary(ofhungry_movers) #51 movers of the hungry

ofhungry_permmove <- dat %>%
  filter(food_short == 1) %>%
  select(nature_migrants1)
summary(ofhungry_permmove) #Permanent migrants were 17, Seasonal was 34, NAs were 185

#Of the migrants, about 51/165 (or 30.9%) had a food shortage during part of the year

ofmovers_hungry <- dat %>%
  filter(migration == 1) %>%
  select(food_short)
sum(ofmovers_hungry) #indeed, 51 migrants had a food shortage

# Age, gender of First Migrants and Second Migrants
summary(dat$age_1) #median is 32, mean is 34
dat$gender_1 <- as.factor(dat$gender_1)
summary(dat$gender_1) #4.8 percent female, 95% male

summary(dat$age_2) #median is 30, mean is 31
dat$gender_2 <- as.factor(dat$gender_2)
summary(dat$gender_2) # 82% male, 18% female (only 22 migrants)

dat$gender_3 <- as.factor(dat$gender_3)
summary(dat$gender_3) # 2 males, one is 25 and the other is 28

#Food Security: coding shortage-induced food restriction and debt

dat$food_restriction <- ifelse(dat$reduce_quantity == "Yes" |
                                      dat$reduce_time_eat == "Yes" |
                                      dat$without_food == "Yes", 1, 0)
dat$food_restriction <- as.factor(dat$food_restriction)
summary(dat$food_restriction)
dat$food_debt <- ifelse(dat$borrow_food == "Yes" |
                               dat$loan_food == "Yes" |
                               dat$loan_micro == "Yes" |
                               dat$exchange_things == "Yes" |
                               dat$morgate_land == "Yes" |
                               dat$morgate_non_land == "Yes", 1, 0)
dat$food_debt <- as.factor(dat$food_debt)

#93 restricted food quantity, number of meals, and went without food. 132 did not. 800 NAs
summary(dat$food_restriction)

#211 took on debt for food, 17 did not - 797 NAs
summary(dat$food_debt)

#Food Consumption Score according to World Food Programme

#FCS thresholds: 0-21 = Poor, 21.5-35 = Borderline, > 35 = Healthy

FCS <- dat %>% select(rice, wheat, maize, potato,
               pulses,
               fruits,
               vegetable,
               egges, meat, meat_chi, fish,
               dairy,
               oils)
#FCS formula = (Main staples)*2 + (Pulses)*3 + (Veg)*1 + (Fruit)*1 + (Meat + Fish)*4 + (Milk)*4 + (Sugar)*0.5 + (Oil)*0.5
#Round down to 7 if any food group exceeds 7, prior to multiplying

FCS$staples <- (dat$rice + dat$wheat + dat$maize + dat$potato)
FCS$staples[dat$rice + dat$wheat + dat$maize + dat$potato > 7] <- 7
FCS$staples <- FCS$staples*2

FCS$pulses <- dat$pulses
FCS$pulses[FCS$pulses > 7] <-7
FCS$pulses <- FCS$pulses*3

FCS$veg <- dat$vegetable
FCS$veg[FCS$veg >7] <- 7
hist(FCS$veg)

FCS$fruit <- dat$fruits
summary(FCS$fruit)
FCS$fruit[FCS$fruit >7] <- 7

FCS$protein <- (dat$egges + dat$meat + dat$meat_chi + dat$fish)
FCS$protein[dat$egges + dat$meat + dat$meat_chi + dat$fish > 7] <- 7
FCS$protein <- FCS$protein*4

FCS$dairy <- dat$dairy
FCS$dairy[FCS$dairy > 7] <- 7
FCS$dairy <- FCS$dairy*4

FCS$oils <- dat$oils
FCS$oils[FCS$oils > 7] <- 7
FCS$oils <- FCS$oils/2

FCS$total <- FCS$staples + FCS$pulses + FCS$veg + FCS$fruit + FCS$protein + FCS$dairy + FCS$oils
hist(FCS$total)

FCS$Food_Consumption_Score[FCS$total < 21] = "Poor"
FCS$Food_Consumption_Score[FCS$total > 21.5 & FCS$total < 35] = "Borderline"
FCS$Food_Consumption_Score[FCS$total > 35] = "Healthy"
FCS$Food_Consumption_Score <- as.factor(FCS$Food_Consumption_Score)
summary(FCS$Food_Consumption_Score)

dat$FCS <- FCS$Food_Consumption_Score

# if rawscore =< 21 then "Poor", if rawscore =>21.5 <=35 then "Borderline", if rawscore > 35 then "Healthy"

# Sec 10 - General Social Capital and Housing Assets

dat$num_adults <- dat$num_adult_male + dat$num_adult_female
hist(dat$num_adults)

dat$kids <- dat$num_child_male + dat$num_child_female
hist(dat$kids)

dat$workingkids <- ifelse(dat$working_child_female > 0 |
                            dat$working_child_male > 0, 1, 0)
hist(dat$workingkids)
#incidence of child labor is very low

dat$hh_size <- dat$num_adults + dat$kids
mean(dat$hh_size, na.rm = TRUE)
hist(dat$hh_size)
summary(dat$hh_size) #mean HH size is 5 people, median is 4

#calculating rice (kg) per capita

dat$rice_per_capita <- (dat$quantity_rice)/dat$hh_size
hist(dat$rice_per_capita)
summary(dat$rice_per_capita) #Mean is 3.2kg/wk, Median is 3.0/wk

#what about dependent adults? Does increased dependents further increase migration?
dat$indigent <- (dat$num_adults - (dat$working_adult_female + dat$working_adult_male))
dat$indigent <- dat$indigent + dat$kids - dat$workingkids
hist(dat$indigent)
summary(dat$indigent)
sum(dat$indigent, na.rm = TRUE) #579 dependent adults in the dataset.

# Loose ends before running model regressions
dat$income_bussiness <- as.factor(dat$income_bussiness)

#Isolate explanatory data descriptors to generate a summary table:

dat$POL_NAME <- as.factor(dat$POL_NAME)
dat$VILLAGE_NAME <- as.factor(dat$VILLAGE_NAME)
dat$gender_hh <- as.factor(dat$gender_hh)

toplines <- dat %>%
  select(POL_NAME, religion_hh, gender_hh, age_hh, edu_hh_code, hh_size, farm_types, migration)
toplines <- as.data.frame(toplines)

view(dfSummary(toplines))

#Models

# Base model - demographics etc - significant or not, have to be there
colnames(dat)

base<- formula("migration ~ farm_types + edu_hh_code + total_plot_area_ha + age_hh +
                    working_adult_male + hh_size + low_land")

mod1 <- glm(base, family = binomial(link="probit"), data=dat)

summary(mod1)

#basemod significance: 90% religion_hh = Islam,
# 100% num_male_agri_lobor, 99% age_hh, 90% working_adult_male, 90% num_rooms, 95% not low_land
#why is Islam influencing migration? Minority status?

# Specific test #1 - Environmental Stress -
# both flood frequency in past 5 years (100%), drought (90%) and insect attack (90%) were significant,
# while production losses were insignificant as well as sunk crops

clim_freq <- formula("migration ~ freq_flood + loss_prod_flood + freq_drought + loss_prod_drought +
                      freq_salinity + loss_prod_salinity + freq_insects + loss_prod_insect + crop_sunk")
freqmod <- glm(clim_freq, family = binomial(link="probit"), data=dat)
summary(freqmod)

#retrying production loss (environmental perception) as a factor
# clim_loss <- formula("migration ~ low_land + crop_sunk + percent_abovesea + loss_prod_flood + loss_prod_drought + loss_prod_salinity + loss_prod_insect")
# lossmod <- glm(clim_loss, family = binomial(link="probit"), data=dat)
# summary(lossmod)

baseandclim <- formula("migration ~ farm_types + edu_hh_code + total_plot_area_ha + age_hh +
                  working_adult_male + hh_size + low_land + freq_flood + freq_drought + freq_insects")
mod2 <- glm(baseandclim, family = binomial(link="probit"), data=dat)
summary(mod2)

# Which polders had the most migrants? Which ones had the most environmental stresses?

dat2 <- dat %>% select(POL_NAME, hh_size, migration, freq_drought,
                       freq_flood, freq_salinity, freq_insects)
dat2 %>% tbl_summary()
dat2 %>% tbl_summary(by = POL_NAME,
                     statistic = list(all_continuous() ~ "{mean} ({sd})",
                                      all_categorical() ~ "{n} / {N} ({p}%)"),
                     digits = all_continuous() ~ 3,
                     list(freq_drought ~ "Drought Frequency, Out of 5 Years",
                          freq_flood ~ "Flood Frequency, Out of 5 Years",
                          freq_salinity ~ "Oversalinity Frequency, Out of 5 Years",
                          freq_insects ~ "Insect Infestation Frequency, Out of 5 Years"),
                     missing_text = "(Missing)"
)

# tbl_summary(toplines)

#corplot on climate pressures: let's see how frequency or loss perception affects migration
# clim <- dat %>%
#   select(migration, freq_flood, loss_prod_flood, freq_drought, loss_prod_drought,
#          freq_salinity, loss_prod_salinity, freq_insects, loss_prod_insect, crop_sunk)
#
# clim$migration <- as.numeric(dat$migration)
# clim$crop_sunk <- as.numeric(dat$crop_sunk)
# str(clim)
#
# climat <- cor(clim, use = "complete.obs")
#
# corrplot(climat, order = "AOE", method = "number", type = "lower")

#Specific Test 2 - poor infrastructure and water management - Poor Canal Condition is 95% predictive for migration,
# poor sluice gates is 90% predictive, and Having Member of a Water Management Group is 95% predictive

dat$memb_wmg <- as.numeric(dat$memb_wmg)

bad_infra <- formula("migration ~ bad_transparency + bad_financial + bad_participation +
                     bad_rules + bad_gate_operation + maintenance + bad_canals +
                     bad_embankments + bad_gates + memb_wmg")
bad_infra_mod <- glm(bad_infra, family = binomial(link="probit"), data=dat)


summary(bad_infra_mod)

basecliminfra <- formula("migration ~ farm_types + edu_hh_code + total_plot_area_ha + age_hh +
                  working_adult_male + hh_size + low_land + freq_flood + freq_drought + freq_insects +
                  bad_canals + bad_gates + memb_wmg")

mod3 <- glm(basecliminfra, family = binomial(link="probit"), data=dat)
summary(mod3)

# Specific test #3 - income and debt load on migration -
# income from trade and business strongly predicts migration (100%), with number of kids (90%) and Non-Ag income (90%)

money <- formula("migration ~ Annual_income_Agriculture_combined_USD + Annual_income_Non_Agriculture_combined_USD +
                 farm_types + kids + num_rooms + income_rent + income_caste_occu +
                 income_fish + income_poultry + income_bussiness + income_transport + total_plot_area_ha")
moneymod <- glm(money, family = binomial(link="probit"), data=dat)
summary(moneymod)

basecliminframoney <- formula("migration ~ farm_types + edu_hh_code + total_plot_area_ha + age_hh +
                  working_adult_male + hh_size + low_land + freq_flood + freq_drought + freq_insects +
                  bad_canals + bad_gates + memb_wmg + Annual_income_Non_Agriculture_combined_USD +
                  kids + income_bussiness")

mod4 <-glm(basecliminframoney, family = binomial(link="probit"), data=dat)
summary(mod4)

# Specific test #4 - food insecurity - food shortage, food restriction, food debt, food consumption score class
# food_short is highly predictive of migration (no food shortage), but breaks this model due to unknown reason

hunger_reg <- formula("migration ~ food_restriction + rice_per_capita + food_debt + FCS")
hungermod <- glm(hunger_reg, family = binomial(link="probit"), data=dat)
summary(hungermod) #No Food Shortage (99% correlated), No Forced Food Restriction (90% correlated)

#Monthly hunger correlation - Having adequate food in months of Poush (90%) and Magh (95%) indicate migration
monga <- formula("migration ~ food_short_boi + food_short_jios + food_short_ash + food_short_sra +
                 food_short_bhadro + food_short_ashshin + food_short_kartik + food_short_ograon +
                 food_short_poush + food_short_magh + food_short_falgun + food_short_choitro")
monga_mod <- glm(monga, family = binomial(link="probit"), data=dat)
summary(monga_mod)

basecliminframoneyhunger <- formula("migration ~ farm_types + edu_hh_code + total_plot_area_ha + age_hh +
                  working_adult_male + hh_size + low_land + freq_flood + freq_drought + freq_insects +
                  bad_canals + bad_gates + memb_wmg + Annual_income_Non_Agriculture_combined_USD +
                  kids + income_bussiness + food_restriction + food_short_poush + food_short_magh")

mod5 <- glm(basecliminframoneyhunger, family = binomial(link="probit"), data=dat)
summary(mod5)

stargazer(mod1, mod2, mod3, mod4, mod5,
          out = "C:/Users/Kendall Byers/Documents/R/bgd-migration/output/allthemarbles_0823_1330.htm")


bottom_line <- formula("migration ~ religion_hh + age_hh + num_male_agri_lobor + working_adult_male + num_rooms +
                       freq_flood + freq_insects + low_land + memb_wmg +
                       bad_gates + bad_canals + income_bussiness + kids +
                       Annual_income_Non_Agriculture_combined_USD + food_restriction +
                       food_short_poush + food_short_magh")
bottom_line_mod <- glm(bottom_line, family = binomial(link="probit"), data=dat)
summary(bottom_line_mod)
summ(bottom_line_mod)

#throw them all into a bucket and corrplot them
bucket <- dat %>%
  select(migration, religion_hh, farm_types, age_hh,
         num_male_agri_lobor, working_adult_female, working_adult_male,
         num_rooms, freq_flood, freq_insects, low_land, memb_wmg,
         bad_gates, bad_canals, income_bussiness, kids,
         Annual_income_Non_Agriculture_combined_USD, food_restriction,
         food_short_poush, food_short_magh)

#Correlation matrix must be numeric, and have short names to fit on graph
bucket$migration <- bucket$migration <- as.integer(dat$migration)
bucket$religion <- bucket$religion_hh <- as.numeric(dat$religion_hh)
bucket$age <- bucket$age_hh <- as.numeric(dat$age_hh)
bucket$agworkmen <- bucket$num_male_agri_lobor <- as.numeric(dat$num_male_agri_lobor)
bucket$workingwomen <- bucket$working_adult_female <- as.numeric(dat$working_adult_female)
bucket$workingmen <- bucket$working_adult_male <- as.numeric(dat$working_adult_male)
bucket$rooms <- bucket$num_rooms <- as.numeric(dat$num_rooms)
bucket$floods <- bucket$freq_flood <- as.numeric(dat$freq_flood)
bucket$insects <- bucket$freq_insects <- as.numeric(dat$freq_insects)
bucket$low_land <- as.numeric(dat$low_land)
bucket$memb_wmg <- as.numeric(dat$memb_wmg)
bucket$bad_gates <- bucket$bad_gates <- as.numeric(dat$bad_gates)
bucket$bad_canals <- bucket$bad_canals <- as.numeric(dat$bad_canals)
bucket$kids <- as.numeric(dat$kids)
bucket$NonAgIncome <- bucket$Annual_income_Non_Agriculture_combined_USD <- as.numeric(dat$Annual_income_Non_Agriculture_combined_USD)
bucket$lessfood <- bucket$food_restriction <- as.numeric(dat$food_restriction)
bucket$NoLandDeeds <- ifelse(dat$farm_types == "No Papers", 1, 0)
bucket$trade <- bucket$income_bussiness <- ifelse(dat$income_bussiness == "Yes", 1, 0)
# bucket$poush <- bucket$food_short_poush <- ifelse(dat$food_short_poush == "food shortage", 1, 0)
# bucket$magh <- bucket$food_short_magh <- ifelse(dat$food_short_magh == "food shortage", 1, 0)
bucket$food_short <- ifelse(dat$food_short == "Food Shortage", 1, 0)
bucket$food_short <- as.integer(bucket$food_short)
#Remove unabbreviated names
bucket <- bucket %>%
  select(-religion_hh,
         -age_hh,
         -num_male_agri_lobor,
         -working_adult_male,
         -working_adult_female,
         -num_rooms,
         -freq_flood,
         -freq_insects,
         -Annual_income_Non_Agriculture_combined_USD,
         -income_bussiness,
         -food_restriction)

#correlation matrix for all significant factors
mat <- cor(bucket, use = "complete.obs")

#names of corrplot
r = rbind(c("Bad Floodgates", "Food Shortage in Magh", "Number of Males working in Agriculture",
            "Member of Water Management Group", "Women Working", "Bad Canals",
            "Seasonal Food Restriction", "Owns Farm", "Has Trade Income", "Non-Ag Income",
            "Lowland", "Religion", "Number of Children", "Number of Rooms", "Men Working", "Insect Atk Frequency",
            "Food Shortage in Poush", "House Head's Age", "Flood Frequency", "Migrant"))
corrplot(mat, order = "AOE", method = "number", type = "lower") %>%
         # method = "color" addCoef.col = "gray")
  corrRect(nameMat = r)

# use namesMat parameter EXAMPLE
# r = rbind(c('eggs', 'catsize', 'airborne', 'milk'),
#           c('catsize', 'eggs', 'milk', 'airborne'))
# corrplot(Z, order = 'hclust') %>% corrRect(namesMat = r)


# corr_simple <- function(data=bucket, sig = 0.1){
#   #convert data to numeric in order to run correlations
#   #convert to factor first to keep the integrity of the data -
#   #each value will become a number rather than turn into NA
#   df_cor <- bucket %>% mutate_if(is.character, as.factor)
#   df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
#
#   #run a correlation and drop the insignificant ones
#   corr <- cor(df_cor)
#   #prepare to drop duplicates and correlations of 1
#   # corr[lower.tri(corr,diag=TRUE)] <- NA
#   #drop perfect correlations
#   corr[corr == 1] <- NA
#
#   #turn into a 3-column table
#   corr <- as.data.frame(as.table(corr))
#   #remove the NA values from above
#   corr <- na.omit(corr)
#
#   #select significant values
#   corr <- subset(corr, abs(Freq) > sig)
#   #sort by highest correlation
#   corr <- corr[order(-abs(corr$Freq)),]
#
#   #print table
#   print(corr)
#
#   #turn corr back into matrix in order to plot with corrplot
#   # mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
#
#   #plot correlations visually
#   # corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ")
# }
# corr_simple()

# corrplot(mtx_corr)

#Change these to reflect bottom lines, but this is your basic model
# stargazer(bottom_line_mod,
#           title = "Fig. 1: Basic Demographics of Study Area",
#           # dep.var.caption = "Migration Decision by Resident, 1 = Yes, 0 = No",
#           covariate.labels = c("Polder Number", "Village Name", "Household Religion",
#                                "Head of Household's Gender", "Head of Household's Age",
#                                "Head of Household's Literacy", "Spouse of Household's Literacy",
#                                "Has Papers for their Farm", "Migration Decision by Resident, 1 = Yes, 0 = No"),
#           notes.label = "Significance Levels",
#           type = "html",
#           out = "bgd-migration/output/bottomlineAug08_2021_noon.htm")

#Factors from "Mig"

# sel_var <- mig %>%
#   select(adoption_combined, power_tiller, thresing_machine, spray_machine, husking_machine,
#          tredle_pump, solar_panel, battery, sew_machine, hh_size, average_yield_loss_combined,
#          risk_perception_flood_withNA, risk_perception_drought_withNA, risk_perception_salinity_withNA,
#          rice_kg_eaten_7days_kg_per_capita, primary_decision_remi, women_remi)
#
# mig_formula <- formula("migration ~ power_tiller +thresing_machine + spray_machine + husking_machine +
#          tredle_pump + solar_panel + battery + sew_machine + hh_size + average_yield_loss_combined +
#          risk_perception_flood_withNA + risk_perception_drought_withNA + risk_perception_salinity_withNA +
#          rice_kg_eaten_7days_kg_per_capita + month_stock + adoption_combined")
# migmod <- glm(mig_formula, family = binomial(link="probit"), data=mig)
# summary(migmod) #husking_machine yes (99%), tredle_pump yes (99%), sew_machine yes (95%), hh_size (99.9%)

# Learning tbl_summary() and gtsummary()
head(trial)

trial2 <- trial %>% select(trt,age,grade)
trial2 %>% tbl_summary()
trial2 %>% tbl_summary(by = trt) %>% add_p()
