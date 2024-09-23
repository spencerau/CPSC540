# install necessary packages
# install.packages("remotes")
# remotes::install_github("LucyMcGowan/touringplans")

# import libraries
library(broom)
library(touringplans)

# debugging libs
library(dplyr)


# seven_dwarfs_9 <- seven_dwarfs_train_2018 |> filter(wait_hour == 9)
# adjusted with dplyer due to some weird issue with piping
seven_dwarfs_9 <- seven_dwarfs_train_2018 %>% filter(wait_hour == 9)

seven_dwarfs_9_with_ps <-
    glm(
        park_extra_magic_morning ~ park_ticket_season + park_close + park_temperature_high,
        data = seven_dwarfs_9,
        family = binomial()
    ) |>
    augment(type.predict = "response", data = seven_dwarfs_9)


seven_dwarfs_9_with_ps |>
    select(
        .fitted,
        park_date,
        park_extra_magic_morning,
        park_ticket_season,
        park_close,
        park_temperature_high
    ) |>
    head() |>
    knitr::kable()

# install.packages("halfmoon")
library(halfmoon)
library(ggplot2)
ggplot(
    seven_dwarfs_9_with_ps,
    aes(.fitted, fill = factor(park_extra_magic_morning))
) +
    geom_mirror_histogram(bins = 50) +
    scale_y_continuous(labels = abs) +
    labs(x = "propensity score", fill = "extra magic morning")


library(ggdag)
ggdag(butterfly_bias()) +
    theme_dag()

ggdag_adjustment_set(butterfly_bias()) +
    theme_dag() +
    theme(legend.position = "bottom")

