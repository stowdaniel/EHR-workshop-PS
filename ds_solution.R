# Exercise 1 Read in the analysis dataset (cohort.dta) and explore the data.


# install.packages(c("MatchIt","survey", "cobalt", "tableone","haven"))

library(tidyverse) 
library(MatchIt) 
library(survey) 
library(cobalt) 
library(tableone) 
library(haven)
library(broom)
library(ggplot2)

data <- read_dta(file = "PS_practical_data.dta")

data<-data[complete.cases(data),]

# Exercise 2 Estimate the effect of treatment on the outcome

model <- glm(outcome ~ trt, family=binomial(), data=data)
summary(model)
model%>%
  tidy(exponentiate = TRUE, conf.int = TRUE)


adjusted_model <-
  glm(
    outcome ~trt + age + female + ses + smoke + alc + bmicat + nsaid_rx + cancer + hyper,
    family = binomial(),
    data = data
  )

adjusted_model%>%
  tidy(conf.int = TRUE, exponentiate = TRUE)

# Exercise 3 Estimate the propensity score

PS_model <-
  glm(
    trt ~ age + female + ses + smoke + alc + bmicat + nsaid_rx + cancer + hyper,
    family = binomial(),
    data = data
  )

PS_model%>%
  tidy(conf.int = TRUE, exponentiate = TRUE)


data$PS <- PS_model %>% predict(data, type = "response")

# Exercise 4 Check distribution

hist(data$PS)

hist(
  data$PS,
  xlab = "Propensity score",
  ylab = "Number of individuals",
  main = "Propensity score distribution",
  col = "darkgrey",
  breaks = 50
)

# Exercise 5 Check distribution cont.

boxplot(PS ~ trt, data= data, ylab="Propensity score", xlab="Treatment")

ggplot(data, aes(
  x = PS,
  color = factor(trt),
  fill = factor(trt)
)) + geom_histogram(position = "identity",
                    bins = 50,
                    alpha = 0.5) + labs(x = "Propensity score", y = "Number of individuals")


data %>%
  group_by(trt) %>%
  summarize(
    n = n(),
    min = min(PS),
    mean_PS = mean(PS),
    median_PS = median(PS),
    max = max(PS)
  )

# Exercise 6 Assessing covariate balance

data %>%
  group_by(trt) %>% summarize(mean=mean(age), sd=sd(age))


StD<-(mean(data$age[data$trt==1]) - mean(data$age[data$trt==0]))/sd(data$age)
StD
data %>%
  group_by(trt, female) %>% 
  summarize(n = n()) %>% 
  mutate(perc = prop.table(n))


vars <-
  c("age",
    "female",
    "ses",
    "alc",
    "bmicat",
    "nsaid_rx",
    "cancer",
    "hyper") 

SD_crude <-
  CreateTableOne(vars,
                 data = data,
                 strata = "trt",
                 test = FALSE)

print(SD_crude, smd = TRUE)

covs <- subset(data, select=c(age, female, ses, alc, bmicat, nsaid_rx, cancer, hyper)) 
bal.tab(covs, treat=data$trt, binary="std", continuous="std", s.d.denom="pooled")

# Exercise 7 Adjust on propensity score

model <- glm(outcome ~ trt + PS, data = data, family = binomial) 

summary(model)

# Exercise 8 Generate inverse probability treatment weights
data$wt <- ifelse(data$trt == 1, 1 / data$PS, 1 / (1 - data$PS))

data%>%
  ggplot(aes(x = wt, fill=factor(trt)))+
  geom_histogram()

# EXERCISE 9
data_svy <-
  svydesign(
    id =  ~ patid,
    weights =  ~ wt,
    data = data
  )


weight_glm<-svyglm(outcome~trt, design=data_svy)

summary(weight_glm)

weight_glm%>%tidy(exponentiate = TRUE)

# Exercise 10 Perform matching

match_data <-
  matchit(
    trt ~ outcome + age + female + ses + smoke + alc + bmicat + nsaid_rx + cancer + hyper,
    method = "nearest",
    ratio = 1,
    replace = TRUE,
    caliper = 0.2 * sd(data$PS),
    data = data
  )
summary(match_data)


hist(
  match_data$weights,
  xlab = "Weights",
  ylab = "Number of individuals",
  main = "Distribution of matching weights",
  col = "darkgrey",
  breaks = 50
)

matched <- match.data(match_data) 
head(matched)

boxplot(PS ~ trt,
        data = matched,
        ylab = "Propensity score",
        xlab = "Treatment")

ggplot(matched, aes(
  x = PS,
  color = factor(trt),
  fill = factor(trt)
)) + geom_histogram(position = "identity",
                    bins = 50,
                    alpha = 0.5) + labs(x = "Propensity score", y = "Number of individuals")

SD_matched <- CreateTableOne(vars, data=matched, strata="trt", test=FALSE) 
print(SD_matched, smd=TRUE)


matched_logit <- glm(outcome ~ trt, weights=weights, family=binomial(), data=matched)
summary(matched_logit)


# Exercise 11 Complete the table using the estimates generate from earlier questions. How do the methods compare?

data.frame(
  "Method" = c("standard" ,"prop_adjustment", "prop_weighting", "matching"),
  "N" = c(nobs(adjusted_model), nobs(model),nobs(weight_glm), nobs(matched_logit)),
  "Estimate" = c(
    adjusted_model$coefficients["trt"],
    model$coefficients["trt"],
    weight_glm$coefficients["trt"],
    matched_logit$coefficients["trt"]
  ),
  "Estimate_exp" = c(
    exp(adjusted_model$coefficients["trt"]),
    exp(model$coefficients["trt"]),
    exp(weight_glm$coefficients["trt"]),
    exp(matched_logit$coefficients["trt"])
  ),
  "SE" = c(
    summary(adjusted_model)$coefficients["trt", 2],
    summary(model)$coefficients["trt", 2],
    summary(weight_glm)$coefficients["trt", 2],
    summary(matched_logit)$coefficients["trt", 2]
  )
)
a