library(dplyr)
library(ggplot2)

test_lm <- lm(formula = Weight ~ factor(Country) + Height + Length + HP + Gear.Ratio, 
              data = car90)

summary(test_lm)


beautify_summary <- function(model) {

  coefficient_data_frame <- summary(model)$coefficients %>%
    as.data.frame() %>%
    select(Estimate) %>%
    rename(Cofficient = Estimate)
  
  r_squared <- summary(model)$r.squared
  adjusted_r_squared <- summary(model)$adj.r.squared
  
  return(list(paste("r^2 =", r_squared), 
              paste("Adjusted r^2 =", adjusted_r_squared),  
              knitr::kable(coefficient_data_frame)))
} 

p <- ggplot(car90, aes(x = Weight, y = Height)) + 
  geom_point()
setwd("C:/Users/dudey/Desktop")
?ggsave(filename = "hi.png", path = "C:/Users/dudey/Downloads", plot = p, device = "png")
