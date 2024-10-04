library(lme4)
#> Loading required package: Matrix
library(dplyr)
library(tibble)


# Convert to tibble for better printing. Convert factors to strings
sleepstudy <- sleepstudy %>% 
    as_tibble() %>% 
    mutate(Subject = as.character(Subject))

# Add two fake participants
df_sleep <- bind_rows(
    sleepstudy,
    tibble(Reaction = c(286, 288), Days = 0:1, Subject = "374"),
    tibble(Reaction = 245, Days = 0, Subject = "373"))

df_sleep


# We can visualize all the data in ggplot2 by using facet_wrap() to create 
# subplots for each participant and stat_smooth() to create a regression line in 
# each subplot.
library(ggplot2)

xlab <- "Days of sleep deprivation"
ylab <- "Average reaction time (ms)"

ggplot(df_sleep) + 
    aes(x = Days, y = Reaction) + 
    stat_smooth(method = "lm", se = FALSE) +
    # Put the points on top of lines
    geom_point() +
    facet_wrap("Subject") +
    labs(x = xlab, y = ylab) + 
    # We also need to help the x-axis, so it doesn't 
    # create gridlines/ticks on 2.5 days
    scale_x_continuous(breaks = 0:4 * 2)


df_no_pooling <- lmList(Reaction ~ Days | Subject, df_sleep) %>% 
    coef() %>% 
    # Subject IDs are stored as row-names. Make them an explicit column
    rownames_to_column("Subject") %>% 
    rename(Intercept = `(Intercept)`, Slope_Days = Days) %>% 
    add_column(Model = "No pooling") %>% 
    # Remove the participant who only had one data-point
    filter(Subject != "373")

head(df_no_pooling)


# Fit a model on all the data pooled together
m_pooled <- lm(Reaction ~ Days, df_sleep) 

# Repeat the intercept and slope terms for each participant
df_pooled <- tibble(
    Model = "Complete pooling",
    Subject = unique(df_sleep$Subject),
    Intercept = coef(m_pooled)[1], 
    Slope_Days = coef(m_pooled)[2]
)

head(df_pooled)


# We can compare these two approaches. Instead of calculating the regression lines
# with stat_smooth(), we can use geom_abline() to draw the lines from our 
# dataframe of intercept and slope parameters.
# Join the raw data so we can use plot the points and the lines.
df_models <- bind_rows(df_pooled, df_no_pooling) %>% 
    left_join(df_sleep, by = "Subject")

p_model_comparison <- ggplot(df_models) + 
    aes(x = Days, y = Reaction) + 
    # Set the color mapping in this layer so the points don't get a color
    geom_abline(
        aes(intercept = Intercept, slope = Slope_Days, color = Model),
        size = .75
    ) + 
    geom_point() +
    facet_wrap("Subject") +
    labs(x = xlab, y = ylab) + 
    scale_x_continuous(breaks = 0:4 * 2) + 
    # Fix the color palette 
    scale_color_brewer(palette = "Dark2") + 
    theme(legend.position = "top", legend.justification = "left")

p_model_comparison


# partial pooling
# install.packages("arm")
library(arm)
m <- lmer(Reaction ~ 1 + Days + (1 + Days | Subject), df_sleep)
arm::display(m)

# Make a dataframe with the fitted effects
df_partial_pooling <- coef(m)[["Subject"]] %>% 
    rownames_to_column("Subject") %>% 
    as_tibble() %>% 
    rename(Intercept = `(Intercept)`, Slope_Days = Days) %>% 
    add_column(Model = "Partial pooling")

head(df_partial_pooling)

df_models <- bind_rows(df_pooled, df_no_pooling, df_partial_pooling) %>% 
    left_join(df_sleep, by = "Subject")

# Replace the data-set of the last plot
p_model_comparison %+% df_models


df_zoom <- df_models %>% 
    filter(Subject %in% c("335", "350", "373", "374"))

p_model_comparison %+% df_zoom


# Also visualize the point for the fixed effects
df_fixef <- tibble(
    Model = "Partial pooling (average)",
    Intercept = fixef(m)[1],
    Slope_Days = fixef(m)[2]
)

# Complete pooling / fixed effects are center of gravity in the plot
df_gravity <- df_pooled %>% 
    distinct(Model, Intercept, Slope_Days) %>% 
    bind_rows(df_fixef)
df_gravity
#> # A tibble: 2 × 3
#>   Model                     Intercept Slope_Days
#>   <chr>                         <dbl>      <dbl>
#> 1 Complete pooling               252.       10.3
#> 2 Partial pooling (average)      253.       10.5

df_pulled <- bind_rows(df_no_pooling, df_partial_pooling)

ggplot(df_pulled) + 
    aes(x = Intercept, y = Slope_Days, color = Model, shape = Model) + 
    geom_point(size = 2) + 
    geom_point(
        data = df_gravity, 
        size = 5,
        # Prevent size-5 point from showing in legend keys
        show.legend = FALSE
    ) + 
    # Draw an arrow connecting the observations between models
    geom_path(
        aes(group = Subject, color = NULL), 
        arrow = arrow(length = unit(.02, "npc")),
        show.legend = FALSE
    ) + 
    # Use ggrepel to jitter the labels away from the points
    ggrepel::geom_text_repel(
        aes(label = Subject, color = NULL), 
        data = df_no_pooling,
        show.legend = FALSE
    ) + 
    # Don't forget 373
    ggrepel::geom_text_repel(
        aes(label = Subject, color = NULL), 
        data = filter(df_partial_pooling, Subject == "373"),
        show.legend = FALSE
    ) + 
    theme(
        legend.position = "bottom", 
        legend.justification = "right"
    ) + 
    ggtitle("Pooling of regression parameters") + 
    xlab("Intercept estimate") + 
    ylab("Slope estimate") + 
    scale_shape_manual(values = c(15:18)) +
    scale_color_brewer(palette = "Dark2") 


# extract the covariance matrix estimated by the model.
cov_mat <- VarCorr(m)[["Subject"]]

# Strip off some details so that just the useful part is printed
attr(cov_mat, "stddev") <- NULL
attr(cov_mat, "correlation") <- NULL
cov_mat

# We create five ellipses for different quantile levels.
#install.packages("ellipse")
library(ellipse)
#> 
#> Attaching package: 'ellipse'
#> The following object is masked from 'package:graphics':
#> 
#>     pairs

# Helper function to make a data-frame of ellipse points that 
# includes the level as a column
make_ellipse <- function(cov_mat, center, level) {
    ellipse(cov_mat, centre = center, level = level) %>%
        as.data.frame() %>%
        add_column(level = level) %>% 
        as_tibble()
}

center <- fixef(m)
levels <- c(.1, .3, .5, .7, .9)

# Create an ellipse dataframe for each of the levels defined 
# above and combine them
df_ellipse <- levels %>%
    lapply(
        function(x) make_ellipse(cov_mat, center, level = x)
    ) %>% 
    bind_rows() %>% 
    rename(Intercept = `(Intercept)`, Slope_Days = Days)

df_ellipse

# Then we add them onto our previous plot
ggplot(df_pulled) + 
    aes(x = Intercept, y = Slope_Days, color = Model, shape = Model) + 
    # Draw contour lines from the distribution of effects
    geom_path(
        aes(group = level, color = NULL, shape = NULL), 
        data = df_ellipse, 
        linetype = "dashed", 
        color = "grey40"
    ) + 
    geom_point(
        aes(shape = Model),
        data = df_gravity, 
        size = 5,
        show.legend = FALSE
    ) + 
    geom_point(size = 2) + 
    geom_path(
        aes(group = Subject, color = NULL), 
        arrow = arrow(length = unit(.02, "npc")),
        show.legend = FALSE
    ) + 
    theme(
        legend.position = "bottom", 
        legend.justification = "right"
    ) + 
    ggtitle("Topographic map of regression parameters") + 
    xlab("Intercept estimate") + 
    ylab("Slope estimate") + 
    scale_color_brewer(palette = "Dark2") +
    scale_shape_manual(values = c(15:18))

# slight tweaks to plot
last_plot() +
    coord_cartesian(
        xlim = range(df_pulled$Intercept), 
        ylim = range(df_pulled$Slope_Days),
        expand = TRUE
    ) 

# label the contours with the confidence levels
# Euclidean distance
contour_dist <- function(xs, ys, center_x, center_y) {
    x_diff <- (center_x - xs) ^ 2
    y_diff <- (center_y - ys) ^ 2
    sqrt(x_diff + y_diff)
}

# Find the point to label in each ellipse.
df_label_locations <- df_ellipse %>% 
    group_by(level) %>%
    filter(
        Intercept < quantile(Intercept, .25), 
        Slope_Days < quantile(Slope_Days, .25)
    ) %>% 
    # Compute distance from center.
    mutate(
        dist = contour_dist(Intercept, Slope_Days, fixef(m)[1], fixef(m)[2])
    ) %>% 
    # Keep smallest values.
    top_n(-1, wt = dist) %>% 
    ungroup()

# Tweak the last plot one more time!
last_plot() +
    geom_text(
        aes(label = level, color = NULL, shape = NULL), 
        data = df_label_locations, 
        nudge_x = .5, 
        nudge_y = .8, 
        size = 3.5, 
        color = "grey40"
    )


