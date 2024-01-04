rm(list=ls())

# packages
require(DHARMa)
require(performance)
require(MASS)
require(glmmTMB)
require(emmeans)
require(multcompView)
require(multcomp)
# reading the data and making necessary adjustments
data = read.csv('C:/Users/joaop/Desktop/Corpora/C-Oral-Brasil/dissertation_codes/text_output_files/metadataWithTranscription_csv_2023-09-26.csv')
data$age[data$age=="M"] = "A"
data$age[data$age=="D"] = "C"
names(data) # dataframe columns

# setting up predictors
sex = as.factor(data$sex)
table(sex) # quantifying

age = as.factor(data$age)
table(age) # quantifying

schooling = as.factor(data$schooling)
table(schooling) # quantifying

# setting up response variable
response = data$count_foreign_words

# exploring the data
## separately
boxplot(response ~ sex)
boxplot(response ~ age)
boxplot(response ~ schooling)

## 'model-like' function
boxplot(response ~ sex + age + schooling)
boxplot(response ~ sex * age * schooling)

## no interaction formula
formula_no_interaction = " response ~ sex + age + schooling"

## with interaction formula
formula_with_interaction = " response ~ sex * age * schooling"

# ====================================

## model with residual distribution modeled by POISSON
model_no_interaction_poisson = glm(formula_no_interaction, family = poisson(link = "log"))

### variance
par(mfrow = c(2, 2))
plot(model_no_interaction_poisson) 
par(mfrow = c(1, 1))

### normality
simulationOutput <- simulateResiduals(fittedModel = model_no_interaction_poisson, plot = TRUE)

### Dispersion
#### p-value < 5%, indicates problems
#### red line on the right indicates overdispersion
#### red line on the left indicates underdispersion
#### p-value >=5% passed the test
testDispersion(model_no_interaction_poisson)
check_overdispersion(model_no_interaction_poisson)

#### dispersion parameter, ideal < 1.5
analisis = summary(model_no_interaction_poisson)
analisis$deviance/analisis$df.residual

### Zero inflation - same idea of dispersion
testZeroInflation(model_no_interaction_poisson)
check_zeroinflation(model_no_interaction_poisson)

### R2 - not so relevant for count data but we did it anyway
r2(model_no_interaction_poisson)

### collinearity
check_collinearity(model_no_interaction_poisson)

# ===============================

## model with residual distribution modeled by POISSON
model_with_interaction_poisson = glm(formula_with_interaction, family = poisson(link = "log"))

### variance
par(mfrow = c(2, 2))
plot(model_with_interaction_poisson) 
par(mfrow = c(1, 1))

### normality
simulationOutput <- simulateResiduals(fittedModel = model_with_interaction_poisson, plot = TRUE)


### Dispersion
#### p-value < 5%, indicates problems
#### red line on the right indicates overdispersion
#### red line on the left indicates underdispersion
#### p-value >=5% passed the test
testDispersion(model_with_interaction_poisson)
check_overdispersion(model_with_interaction_poisson)

#### dispersion parameter, ideal < 1.5
analisis = summary(model_with_interaction_poisson)
analisis$deviance/analisis$df.residual

### Zero inflation - same idea of dispersion
testZeroInflation(model_with_interaction_poisson)
check_zeroinflation(model_with_interaction_poisson)

### R2 - not so relevant for count data but we did it anyway
r2(model_with_interaction_poisson)

### collinearity
check_collinearity(model_with_interaction_poisson)

# ===============================

## model with residual distribution modeled by NEGATIVE BINOMIAL
model_no_interaction_nb = glm.nb(formula_no_interaction)

### variance
par(mfrow = c(2, 2))
plot(model_no_interaction_nb) 
par(mfrow = c(1, 1))

### normality
simulationOutput <- simulateResiduals(fittedModel = model_no_interaction_nb, plot = TRUE)


### Dispersion
testDispersion(model_no_interaction_nb)
check_overdispersion(model_no_interaction_nb)

#### dispersion parameter, ideal < 1.5
analisis = summary(model_no_interaction_nb)
analisis$deviance/analisis$df.residual

### Zero inflation - same idea of dispersion
testZeroInflation(model_no_interaction_nb)
check_zeroinflation(model_no_interaction_nb)

### R2 - not so relevant for count data but we did it anyway
r2(model_no_interaction_nb)

### collinearity
check_collinearity(model_no_interaction_nb)

# ===============================

## model with residual distribution modeled by NEGATIVE BINOMIAL
model_with_interaction_nb = glm.nb(formula_with_interaction)

### variance
par(mfrow = c(2, 2))
plot(model_with_interaction_nb) 
par(mfrow = c(1, 1))

### normality
simulationOutput <- simulateResiduals(fittedModel = model_with_interaction_nb, plot = TRUE)


### Dispersion
testDispersion(model_with_interaction_nb)
check_overdispersion(model_with_interaction_nb)

#### dispersion parameter, ideal < 1.5
analisis = summary(model_with_interaction_nb)
analisis$deviance/analisis$df.residual

### Zero inflation - same idea of dispersion
testZeroInflation(model_with_interaction_nb)
check_zeroinflation(model_with_interaction_nb)

### R2 - not so relevant for count data but we did it anyway
r2(model_with_interaction_nb)

### collinearity
check_collinearity(model_with_interaction_nb)

# =============================================
# comparing models
summary(model_no_interaction_nb)
summary(model_with_interaction_nb)

# anova testing - check if models are equal (choose the less complex)
anova(model_no_interaction_nb, model_with_interaction_nb, test = "Chisq")

# ========================

# NO INTERACTION
### analysing the levels in SEX
m_means <- emmeans(model_no_interaction_nb, ~ sex)
cld(m_means, Letters = letters)
#### BASIC PLOT
emmip(m_means, ~ sex) 
####BASIC PLOT WITH CONFIDENCE LIMITS
emmip(m_means, ~ sex, CIs=T) 
#### BASIC PLOT WITH CONFIDENCE LIMITS ON THE RESPONSE SCALE
emmip(m_means, ~ sex, CIs=T, type="response")

### analysing the levels in AGE
m_means <- emmeans(model_no_interaction_nb, ~ age)
cld(m_means, Letters = letters)
#### BASIC PLOT
emmip(m_means, ~ age) 
####BASIC PLOT WITH CONFIDENCE LIMITS
emmip(m_means, ~ age, CIs=T) 
#### BASIC PLOT WITH CONFIDENCE LIMITS ON THE RESPONSE SCALE
emmip(m_means, ~ age, CIs=T, type="response")

### analysing the levels in SCHOOLING
m_means <- emmeans(model_no_interaction_nb, ~ schooling)
cld(m_means, Letters = letters)

m_means_response <- emmeans(model_no_interaction_nb, ~ schooling, type="response")
cld(m_means_response, Letters = letters)
#### BASIC PLOT
emmip(m_means, ~ schooling) 
####BASIC PLOT WITH CONFIDENCE LIMITS
emmip(m_means, ~ schooling, CIs=T) 
#### BASIC PLOT WITH CONFIDENCE LIMITS ON THE RESPONSE SCALE
emmip(m_means, ~ schooling, CIs=T, type="response")





