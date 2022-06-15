#fast food

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lm.beta))
suppressPackageStartupMessages(library(openintro))

Q1<-cor(fastfood[fastfood$restaurant %in% c("Sonic", "Subway", "Taco Bell"),
                 c('calories', 'total_fat', 'sugar', 'calcium')], 
        use="pairwise.complete.obs", method="pearson")
Q1

Q02 <- openintro::fastfood %>% 
  filter(restaurant %in% c("Mcdonalds", "Subway")) %>%
  mutate(is_mcdonalds = restaurant == "Mcdonalds")
fit_Q02 <- glm(is_mcdonalds ~ calories + sodium + protein, 
               family = "binomial", data = fastfood)
summary(fit_Q02)

Q2 <- round(coef(fit_Q02), 2)
Q2

fit_Q03 <- glm(is_mcdonalds ~ calories + protein, 
               family = "binomial", data = fastfood)
summary(fit_Q03)

Q3 <- fit_Q03$aic
Q3

Q05 <- openintro::fastfood %>%
  group_by(restaurant) %>%
  filter(n() >= 50 & n() <= 60)
Q05
fit_Q05 <- lm(total_fat ~ cholesterol + vit_a + total_carb + restaurant,
              data = fastfood)


fit_Q05 <- update(fit_Q05, . ~ . - vit_a, - restaurant)
coefs_Q05 <- round(coef(lm.beta::lm.beta(fit_Q05)), 2)

Q5 <- coefs_Q05[which.max(coefs_Q05)]
Q5
