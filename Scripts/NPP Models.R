# load libraries ----------------------------------------------------------
library(dplyr)
library(magrittr)
library(MASS)
library(DHARMa)
library(performance)
library(car)
library(effectsize)
library(partR2)
library(rsq)
library(piecewiseSEM)

# Load Data ---------------------------------------------------------------

npp_variables_allsites <- read.csv("Derived Data/NPP Variables.csv") %>%
  dplyr::mutate(REC_1000 = KELP_RECRUITS_M2 * 1000) %>% 
  dplyr::mutate(REC_round = round(REC_1000, digits = 0))

npp_variables_SEM <- read.csv("Derived Data/NPP Variables.csv") %>%
  dplyr::mutate(REC_1000 = KELP_RECRUITS_M2 * 1000) %>% 
  dplyr::mutate(REC_round = round(REC_1000, digits = 0)) %>% 
  dplyr::select(Site, kelp.year, NPP_dry_per.year, APRIL_BIOMASS, REC_round,
                KELP_GROWTH, l_lagged, LARGE_WAVES_MAX, SAND_COVER, URCHIN_GRAZING_AVE,
                TEMP_THRESHOLD_DAY, NITRATE_THRESHOLD_RECRUIT) %>% na.omit()

# Model 1- NPP ------------------------------------------------------------


npp_m1 <- lm(NPP_dry_per.year ~ APRIL_BIOMASS + KELP_RECRUITS_M2 +
               KELP_GROWTH + Site, data = npp_variables_allsites, na.action = na.omit) 


resid_lm <- residuals(npp_m1)

acf_test <- Box.test(resid_lm, lag = 1, type = "Ljung-Box")

print(acf_test)
#x-sqaured = 0.0028 and p-value = 0.96


summary(npp_m1)
simulationOutput <- simulateResiduals(fittedModel = npp_m1) #shows normality and variance
plot(simulationOutput)
performance::check_model(npp_m1)
partial_r2(npp_m1)
performance::check_model(npp_m2)
car::Anova(npp_m1)
check_collinearity(npp_m1)
effectsize(npp_m1)
car::vif(npp_m1)

#added paths for SEM
npp_sem <- lm(NPP_dry_per.year ~ APRIL_BIOMASS + REC_round +
                KELP_GROWTH + LARGE_WAVES_MAX + Site, data = npp_variables_SEM)

summary(npp_sem)
performance::check_model(npp_sem)
car::Anova(npp_sem)
effectsize(npp_sem)

# Model 2- Biomass -------------------------------------------------

#Model
biomass_m2 <-lm(APRIL_BIOMASS ~ l_lagged + 
                  URCHIN_GRAZING_AVE + Site, 
                data =npp_variables_allsites, na.action = na.omit)

resid_lm <- residuals(biomass_m2)

acf_test <- Box.test(resid_lm, lag = 1, type = "Ljung-Box")

print(acf_test)
#x-squared = 3.80 and p-value = 0.05


summary(biomass_m2)
simulationOutput <- simulateResiduals(fittedModel = biomass_m2) #shows normality and variance
plot(simulationOutput)
performance::check_model(biomass_glm)
check_collinearity(biomass_m2)
car::vif(biomass_m2)
rsq.partial(biomass_m2)


#sem
biomass_sem <-lm(APRIL_BIOMASS ~l_lagged + LARGE_WAVES_MAX +
                   URCHIN_GRAZING_AVE + Site, 
                 data =npp_variables_SEM)

summary(biomass_sem)
performance::check_model(biomass_sem)
effectsize(biomass_m2)
partial_r2(biomass_m2)
car::Anova(biomass_m2)
anova(biomass_m2)


#Model 3- Loss --------------------------------------------------------------



loss_m3 <-lm(l_lagged ~ LARGE_WAVES_MAX + Site,
             data =npp_variables_allsites, na.action = na.omit)

resid_lm <- residuals(loss_m3)

acf_test <- Box.test(resid_lm, lag = 1, type = "Ljung-Box")

print(acf_test)
#x-squared = 1.84 and p-value = 0.18


summary(loss_m3)
simulationOutput <- simulateResiduals(fittedModel = loss_m3) #shows normality and variance
plot(simulationOutput)
performance::check_model(loss_m3)
effectsize(loss_m3)
car::Anova(loss_m3)
check_collinearity(loss_m3)
partial_r2(loss_m3)
car::vif(loss_m3)
rsq.partial(loss_m3)

#model with SEM dataset
loss_sem <- lm(l_lagged ~ LARGE_WAVES_MAX + Site,
               data =npp_variables_SEM)

summary(loss_sem)
performance::check_model(loss_sem)
effectsize(loss_sem)
check_collinearity(loss_m3)
partial_r2(loss_m3)


# Model 4- Recruitment ----------------------------------------------------


recruit_m4 <- glm.nb(REC_round  ~ SAND_COVER + URCHIN_GRAZING_AVE +
                       TEMP_THRESHOLD_DAY + NITRATE_THRESHOLD_RECRUIT + 
                       APRIL_BIOMASS + 
                       Site, data = npp_variables_allsites, na.action = na.omit)

resid_glm <- residuals(recruit_m4)

acf_test <- Box.test(resid_glm, lag = 1, type = "Ljung-Box")

print(acf_test)
#x-squared = 1.41 and p-value = 0.2357


summary(recruit_m4)
simulationOutput <- simulateResiduals(fittedModel = recruit_m4) #shows normality and variance
plot(simulationOutput)
car::Anova(recruit_m4)
effectsize(recruit_m4)
performance::r2(recruit_m4)
performance::check_model(recruit_m4)
rsq.partial(recruit_m4)
car::Anova(recruit_m4)
check_collinearity(recruit_m4)
plot(x)
car::vif(recruit_m4)

#sem
recruit_sem <- glm.nb(REC_round  ~ SAND_COVER + URCHIN_GRAZING_AVE +
                        TEMP_THRESHOLD_DAY + NITRATE_THRESHOLD_RECRUIT + 
                        APRIL_BIOMASS + l_lagged +
                        Site, data = npp_variables_SEM)

summary(recruit_sem)
simulationOutput <- simulateResiduals(fittedModel = recruit_sem) #shows normality and variance
plot(simulationOutput)
car::Anova(recruit_sem)
effectsize(recruit_sem)


# Model 5- Growth Model ---------------------------------------------------

growth_m1 <- lm(KELP_GROWTH ~ PLANT_DENSITY_M2 + PLANT_SIZE +
                  NITRATE_THRESHOLD_ANNUAL + TEMP_THRESHOLD_DAY_annual +
                  Site, data = npp_variables_allsites, na.action = na.omit)

resid_lm <- residuals(growth_m1)

acf_test <- Box.test(resid_lm, lag = 1, type = "Ljung-Box")

print(acf_test)
#x-squared = 2.58 and p-value = 0.11

summary(growth_m1)
partial_r2(growth_m1)
simulationOutput <- simulateResiduals(fittedModel = growth_m1) #shows normality and variance
plot(simulationOutput)
car::Anova(growth_m1)
check_collinearity(growth_m1)
car::vif(growth_m1)
effectsize(growth_m1)


# FINAL SEM ---------------------------------------------------------------
#without added paths
sem <- psem(lm(l_lagged ~ LARGE_WAVES_MAX + Site,
               data =npp_variables_SEM),
            lm(APRIL_BIOMASS ~l_lagged + 
                 URCHIN_GRAZING_AVE + Site, 
               data =npp_variables_SEM),
            glm.nb(REC_round  ~ SAND_COVER + URCHIN_GRAZING_AVE +
                     TEMP_THRESHOLD_DAY + NITRATE_THRESHOLD_RECRUIT + 
                     APRIL_BIOMASS +  
                     Site, data = npp_variables_SEM),
            lm(NPP_dry_per.year ~ APRIL_BIOMASS + REC_round +
                 KELP_GROWTH + Site, data = npp_variables_SEM)) 

summary(sem)

#missing paths: WAVES-->BMASS, WAVES-->NPP, 
#LOSS-->KELP_GROWTH, LOSS-->RECRUIT


#with paths
sem <- psem(lm(l_lagged ~ LARGE_WAVES_MAX + Site,
               data =npp_variables_SEM),
            lm(APRIL_BIOMASS ~ LARGE_WAVES_MAX + l_lagged +
                 URCHIN_GRAZING_AVE + Site, 
               data =npp_variables_SEM),
            glm.nb(REC_round  ~ SAND_COVER + URCHIN_GRAZING_AVE +
                     TEMP_THRESHOLD_DAY + NITRATE_THRESHOLD_RECRUIT + 
                     APRIL_BIOMASS + l_lagged +
                     Site, data = npp_variables_SEM),
            lm(NPP_dry_per.year ~ APRIL_BIOMASS + REC_round +
                 KELP_GROWTH + LARGE_WAVES_MAX + Site, data = npp_variables_SEM)
) 


summary(sem)


#omit paths that do not make ecological sense
sem<-update(sem,l_lagged%~~%KELP_GROWTH)
# Extract R² values
r2_submodels <- rsquared(sem)

summary(sem)
