library(broom)
library(touringplans)

seven_dwarfs_9 <- seven_dwarfs_train_2018 |> filter(wait_hour == 9)


# install.packages("MatchIt")
library(MatchIt)
m <- matchit(
    park_extra_magic_morning ~ park_ticket_season + park_close + park_temperature_high,
    data = seven_dwarfs_9
)
m


matched_data <- get_matches(m)
glimpse(matched_data)


# install propensity lib
# remotes::install_github("r-causal/propensity")
# propensity weighting
library(propensity)

seven_dwarfs_9_with_ps <-
    glm(
        park_extra_magic_morning ~ park_ticket_season + park_close + park_temperature_high,
        data = seven_dwarfs_9,
        family = binomial()
    ) |>
    augment(type.predict = "response", data = seven_dwarfs_9)
seven_dwarfs_9_with_wt <- seven_dwarfs_9_with_ps |>
    mutate(w_ate = wt_ate(.fitted, park_extra_magic_morning))

# show weights
seven_dwarfs_9_with_wt |>
    select(
        w_ate,
        .fitted,
        park_date,
        park_extra_magic_morning,
        park_ticket_season,
        park_close,
        park_temperature_high
    ) |>
    head() |>
    knitr::kable()