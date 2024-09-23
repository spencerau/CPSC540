# install.packages("dagitty")
# install.packages("ggdag")
library(dagitty)
library(ggdag)


my_first_dag <- ggdag::dagify(
    x ~ y,
    y ~ z + w
)

coordinates(my_first_dag) <- list(
    x = c(x = 0.5,y = 0,z = -0.5,w = -0.5),
    y = c(x = 0,y = 0,z = -0.25,w = 0.25)
)
ggdag(my_first_dag) + 
    theme_dag()


smoking <- dagify(
    weight ~ defect + smoking,
    mortality ~ defect + smoking + weight,
    outcome = "weight",
    exposure = "smoking"
)


ggdag(smoking, layout = "time_ordered",
      node_size = 20) + theme_dag()



gender <- dagify(
    occ ~ discrim + abil,
    income ~ discrim + abil + occ,
    discrim ~ gend,
    outcome = "discrim",
    exposure = "income"
)


ggdag(gender, layout = "time_ordered",
      node_size = 20) + theme_dag()


# from: https://mixtape.scunning.com/03-directed_acyclical_graphs
library(tidyverse)

tb <- tibble(
    female = ifelse(runif(10000)>=0.5,1,0),
    ability = rnorm(10000),
    discrimination = female,
    occupation = 1 + 2*ability + 0*female - 2*discrimination + rnorm(10000),
    wage = 1 - 1*discrimination + 1*occupation + 2*ability + rnorm(10000) 
)

# open path d -> o -> i
lm_1 <- lm(wage ~ female, tb)
# open path d -> o <- a -> y
lm_2 <- lm(wage ~ female + occupation, tb)
# woo!
lm_3 <- lm(wage ~ female + occupation + ability, tb)

summary(lm_1)
