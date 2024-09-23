library(broom)
library(touringplans)
library(propensity)
library(ggplot2)

seven_dwarfs_9 <- seven_dwarfs_train_2018 |> filter(wait_hour == 9)

seven_dwarfs_9_with_ps <-
    glm(
        park_extra_magic_morning ~ park_ticket_season + park_close + park_temperature_high,
        data = seven_dwarfs_9,
        family = binomial()
    ) |>
    augment(type.predict = "response", data = seven_dwarfs_9)
seven_dwarfs_9_with_wt <- seven_dwarfs_9_with_ps |>
    mutate(w_ate = wt_ate(.fitted, park_extra_magic_morning))

# Now, using the tidy_smd function, we can examine the standardized mean difference 
# before and after weighting.
library(halfmoon)
smds <-
    seven_dwarfs_9_with_wt |>
    mutate(park_close = as.numeric(park_close)) |>
    tidy_smd(
        .vars = c(park_ticket_season, park_close, park_temperature_high),
        .group = park_extra_magic_morning,
        .wts = w_ate
    )
smds


ggplot(
    data = smds,
    aes(
        x = abs(smd),
        y = variable,
        group = method,
        color = method
    )
) +
    geom_love()

# unweighted
ggplot(
    seven_dwarfs_9_with_wt,
    aes(
        x = factor(park_extra_magic_morning),
        y = park_temperature_high,
        group = park_extra_magic_morning
    )
) +
    geom_boxplot(outlier.color = NA) +
    geom_jitter() +
    labs(
        x = "Extra magic morning",
        y = "Temperature high"
    )

# weighted
ggplot(
    seven_dwarfs_9_with_wt,
    aes(
        x = factor(park_extra_magic_morning),
        y = park_temperature_high,
        group = park_extra_magic_morning,
        weight = w_ate
    )
) +
    geom_boxplot(outlier.color = NA) +
    geom_jitter() +
    labs(
        x = "Extra magic morning",
        y = "Historic temperature high"
    )

# Similarly, we can also examine the empirical cumulative distribution function 
# (eCDF) for the confounder stratified by each exposure group. 
# The unweighted eCDF can be visualized using geom_ecdf
ggplot(
    seven_dwarfs_9_with_wt,
    aes(
        x = park_temperature_high,
        color = factor(park_extra_magic_morning)
    )
) +
    geom_ecdf() +
    scale_color_manual(
        "Extra Magic Morning",
        values = c("#5154B8", "#5DB854"),
        labels = c("Yes", "No")
    ) +
    labs(
        x = "Historic temperature high",
        y = "Proportion <= x"
    )

# The halfmoon package allows for the additional weight argument to be passed 
# to geom_ecdf to display a weighted eCDF plot.
ggplot(
    seven_dwarfs_9_with_wt,
    aes(
        x = park_temperature_high,
        color = factor(park_extra_magic_morning)
    )
) +
    geom_ecdf(aes(weights = w_ate)) +
    scale_color_manual(
        "Extra Magic Morning",
        values = c("#5154B8", "#5DB854"),
        labels = c("Yes", "No")
    ) +
    labs(
        x = "Historic temperature high",
        y = "Proportion <= x"
    )


# Let’s try refitting our propensity score model using a natural spline. 
# We can use the function splines::ns for this.
seven_dwarfs_9_with_ps <-
    glm(
        park_extra_magic_morning ~ park_ticket_season + park_close +
            splines::ns(park_temperature_high, df = 5), # refit model with a spline
        data = seven_dwarfs_9,
        family = binomial()
    ) |>
    augment(type.predict = "response", data = seven_dwarfs_9)
seven_dwarfs_9_with_wt <- seven_dwarfs_9_with_ps |>
    mutate(w_ate = wt_ate(.fitted, park_extra_magic_morning))

# Now let’s see how that impacts the weighted eCDF plot
ggplot(
    seven_dwarfs_9_with_wt,
    aes(
        x = park_temperature_high,
        color = factor(park_extra_magic_morning)
    )
) +
    geom_ecdf(aes(weights = w_ate)) +
    scale_color_manual(
        "Extra Magic Morning",
        values = c("#5154B8", "#5DB854"),
        labels = c("Yes", "No")
    ) +
    labs(
        x = "Historic temperature high",
        y = "Proportion <= x"
    )