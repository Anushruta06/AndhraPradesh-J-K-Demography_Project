library(survey)
library(haven)
library(dplyr)
library(ineq)
library(ggplot2)
library(tidyr)
library(readxl)
library(tidyverse)

data1 <- read_dta("Merged_NSSO_80th round L1_L2.dta")

andhra_pradesh <- subset(data1, st_numeric == 28)
jammu_kashmir <- subset(data1, st_numeric == 1)


#===============================================================================
#Population Pyramid
#===============================================================================
#Pyramid
make_pyramid <- function(data, state_name) {
  
  data %>%
    filter(
      !is.na(b3c5),
      !is.na(b3c4),
      !is.na(final_weight),
      final_weight > 0,
      b3c4 %in% c(1, 2)
    ) %>%
    mutate(
      age_group = cut(
        b3c5,
        breaks = seq(0, 110, by = 5),
        right = FALSE,
        include.lowest = TRUE
      ),
      gender = factor(
        b3c4,
        levels = c(1, 2),
        labels = c("Male", "Female")
      )
    ) %>%
    group_by(age_group, gender) %>%
    summarise(
      population = sum(final_weight),
      .groups = "drop"
    ) %>%
    group_by(gender) %>%
    ungroup() %>%
    mutate(
      state = state_name
    )
}

ap_pyramid <- make_pyramid(
  andhra_pradesh, "Andhra Pradesh"
)

jk_pyramid <- make_pyramid(
  jammu_kashmir, "Jammu & Kashmir"
)

pyramid_data <- bind_rows(ap_pyramid, jk_pyramid) %>%
  group_by(state) %>%
  mutate(
    population_percentage = population /
      sum(population) * 100,
    population_percentage = if_else(
      gender == "Male",
      -population_percentage,
      population_percentage
    )
  ) %>%
  ungroup()

ggplot(
  pyramid_data,
  aes(
    x = population_percentage,
    y = age_group,
    fill = gender
  )
) +
  geom_col(width = 0.85) +
  facet_wrap(~state) +
  scale_x_continuous(
    labels = function(x) paste0(abs(x), "%"),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  scale_fill_manual(
    values = c("Male" = "steelblue", "Female" = "indianred")
  ) +
  labs(
    title = "Population Pyramid: Andhra Pradesh and Jammu & Kashmir",
    x = "Percentage of total population",
    y = "Age group (years)",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

#Dependency Ratio
dependency_ratio <- function(data, state_name) {
  
  data %>%
    filter(
      !is.na(b3c5),
      !is.na(final_weight),
      final_weight > 0,
      b3c5 >= 0
    ) %>%
    summarise(
      children = sum(final_weight[b3c5 <= 14]),
      working_age = sum(final_weight[b3c5 >= 15 & b3c5 <= 64]),
      older_people = sum(final_weight[b3c5 >= 65])
    ) %>%
    mutate(
      child_dependency_ratio =
        children / working_age * 100,
      
      old_age_dependency_ratio =
        older_people / working_age * 100,
      
      total_dependency_ratio =
        (children + older_people) / working_age * 100,
      
      state = state_name
    ) %>%
    select(
      state,
      child_dependency_ratio,
      old_age_dependency_ratio,
      total_dependency_ratio
    )
}

dependency_results <- bind_rows(
  dependency_ratio(andhra_pradesh, "Andhra Pradesh"),
  dependency_ratio(jammu_kashmir, "Jammu & Kashmir")
)

print(dependency_results)

#===============================================================================
#GSDP
#===============================================================================
GSDP_India <- read_xlsx("Statewise-GSDP.XLSX")

GSDP_selected <- GSDP_India %>%
  filter(`State/Union Teritory` %in%
           c("Andhra Pradesh", "Jammu & Kashmir"))

GSDP_long <- GSDP_selected %>%
  mutate(across(-`State/Union Teritory`, as.character)) %>%
  pivot_longer(
    cols = -`State/Union Teritory`,
    names_to = "Year",
    values_to = "GSDP"
  ) %>%
  mutate(
    GSDP = as.numeric(GSDP),
    Year = as.numeric(substr(Year, 1, 4))
  )

ggplot(GSDP_long,
       aes(x = Year, y = GSDP,
           colour = `State/Union Teritory`,
           group = `State/Union Teritory`)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Year-wise GSDP: Andhra Pradesh vs Jammu & Kashmir",
    x = "Financial Year",
    y = "GSDP",
    colour = "State"
  ) +
  theme_minimal()
#===============================================================================
#Inequality Analysis
#===============================================================================
#Households

households <- data1 %>%
  distinct(fsu, samp, hhd, .keep_all = TRUE)

andhra_hh <- households %>%
  filter(st_numeric == 28)

jk_hh <- households %>%
  filter(st_numeric == 1)
#Lorenz Curve

prepare_lorenz <- function(data) {
  df <- data %>%
    filter(!is.na(umce), !is.na(final_weight),
           umce >= 0, final_weight > 0) %>%
    arrange(umce)
  
  total_exp <- sum(df$umce * df$final_weight)
  stopifnot(total_exp > 0)
  
  data.frame(
    households  = c(0, cumsum(df$final_weight) / sum(df$final_weight)),
    expenditure = c(0, cumsum(df$umce * df$final_weight) / total_exp)
  )
}

gini_from_lorenz <- function(lz) {
  x <- lz$households
  y <- lz$expenditure
  1 - sum((y[-1] + y[-length(y)]) * diff(x))
}

lorenz_ap <- prepare_lorenz(andhra_hh)
lorenz_jk <- prepare_lorenz(jk_hh)

gini_andhra <- gini_from_lorenz(lorenz_ap)
gini_jk     <- gini_from_lorenz(lorenz_jk)

op <- par(pty = "s")   # square plotting region

ticks <- seq(0, 1, by = 0.1)   # same spacing on both axes

plot(lorenz_ap$households, lorenz_ap$expenditure,
     type = "n",                          # set up the empty plot first
     xlim = c(0, 1), ylim = c(0, 1),
     xaxs = "i", yaxs = "i",              # axes run exactly 0 to 1
     axes = FALSE,
     xlab = "Cumulative share of households",
     ylab = "Cumulative share of monthly expenditure",
     main = "Lorenz Curves of Household Expenditure")

# Equal grid: same positions in both directions
abline(h = ticks, v = ticks, col = "grey80", lty = "solid")

axis(1, at = ticks)
axis(2, at = ticks, las = 1)
box()

# Curves
lines(lorenz_ap$households, lorenz_ap$expenditure, col = "blue", lwd = 2)
lines(lorenz_jk$households, lorenz_jk$expenditure, col = "orange", lwd = 2)
abline(0, 1, col = "red", lty = 2, lwd = 2)

legend("topleft",
       legend = c(sprintf("Andhra Pradesh", gini_andhra),
                  sprintf("Jammu & Kashmir", gini_jk),
                  "Line of equality"),
       col = c("blue", "orange", "red"),
       lty = c(1, 1, 2), lwd = 2, bty = "n")

par(op)   # restore settings

#===============================================================================
#Health Indicator Comparisons Within/Between Diff Gender
#===============================================================================
# Household identifiers
HH_ID <- c("st_numeric", "sec", "fsu", "samp", "sss", "hhd")

# Choose expenditure measure:
# TRUE  = per-capita expenditure (umce / hhsz)
# FALSE = total household expenditure (umce)
PER_CAPITA <- TRUE

q_labels <- c(
  "Q1: Lowest",
  "Q2: Low",
  "Q3: Middle",
  "Q4: High",
  "Q5: Highest"
)

# ============================================================
# 1. Create UNWEIGHTED expenditure quintiles at household level
# ============================================================

add_quintiles_unweighted <- function(data) {
  
  hh <- data %>%
    mutate(hhsz = as.numeric(hhsz)) %>%
    filter(
      !is.na(umce),
      !is.na(hhsz),
      umce >= 0,
      hhsz > 0
    ) %>%
    distinct(across(all_of(HH_ID)), .keep_all = TRUE) %>%
    mutate(
      rank_exp = if (PER_CAPITA) umce / hhsz else umce,
      expenditure_group = ntile(rank_exp, 5)
    ) %>%
    select(all_of(HH_ID), expenditure_group)
  
  data %>%
    inner_join(hh, by = HH_ID)
}

# ============================================================
# 2. Calculate UNWEIGHTED gender-specific prevalence
# ============================================================

gender_expenditure_unweighted <- function(data, state_name) {
  
  add_quintiles_unweighted(data) %>%
    filter(
      b3c4 %in% c(1, 2),
      b3c9 %in% c(1, 2)
    ) %>%
    mutate(
      gender = factor(
        b3c4,
        levels = c(1, 2),
        labels = c("Male", "Female")
      ),
      hospitalised_binary = as.integer(b3c9 == 1)
    ) %>%
    group_by(expenditure_group, gender) %>%
    summarise(
      people_sample = n(),
      hospitalised_sample = sum(hospitalised_binary),
      prevalence = mean(hospitalised_binary) * 100,
      .groups = "drop"
    ) %>%
    mutate(
      expenditure_group = factor(
        expenditure_group,
        levels = 1:5,
        labels = q_labels
      ),
      state = state_name
    )
}

# ============================================================
# 3. Run analysis for both states
# ============================================================

ap_gender <- gender_expenditure_unweighted(
  andhra_pradesh,
  "Andhra Pradesh"
)

jk_gender <- gender_expenditure_unweighted(
  jammu_kashmir,
  "Jammu & Kashmir"
)

gender_results <- bind_rows(ap_gender, jk_gender)

# Display prevalence and sample counts
print(
  gender_results %>%
    mutate(
      prevalence = round(prevalence, 2)
    ),
  n = Inf
)

# ============================================================
# 4. Plot: Gender vs expenditure group
# ============================================================


p_gender <- ggplot(
  gender_results,
  aes(
    x = expenditure_group,
    y = prevalence,
    colour = gender,
    group = gender
  )
) +
  geom_line(linewidth = 1) +
  geom_point(
    aes(size = hospitalised_sample),
    alpha = 0.9
  ) +
  facet_wrap(~state) +
  scale_x_discrete(
    guide = guide_axis(n.dodge = 2)
  ) +
  scale_colour_manual(
    values = c(
      "Male" = "#00BFC4",
      "Female" = "#F8766D"
    )
  ) +
  scale_size_continuous(
    range = c(2, 6),
    name = "Hospitalised\n(sample count)"
  ) +
  labs(
    x = "Household expenditure group",
    y = "Hospitalisation prevalence (%)",
    colour = "Gender"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

print(p_gender)

# ============================================================
# 5. Female - Male prevalence gap (percentage points)
# ============================================================

gender_gap <- gender_results %>%
  select(
    state,
    expenditure_group,
    gender,
    prevalence
  ) %>%
  pivot_wider(
    names_from = gender,
    values_from = prevalence
  ) %>%
  mutate(
    gender_gap = Female - Male,
    prevalence_ratio = Female / Male
  )

print(
  gender_gap %>%
    mutate(
      across(
        c(Male, Female, gender_gap, prevalence_ratio),
        ~ round(.x, 2)
      )
    ),
  n = Inf
)

# ============================================================
# 6. Plot the female - male prevalence gap
# ============================================================

p_gap <- ggplot(
  gender_gap,
  aes(
    x = expenditure_group,
    y = gender_gap,
    fill = gender_gap >= 0
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_col(
    width = 0.6
  ) +
  geom_text(
    aes(
      label = sprintf("%.1f", gender_gap),
      vjust = ifelse(gender_gap >= 0, -0.4, 1.2)
    ),
    size = 3.5
  ) +
  facet_wrap(~state) +
  scale_fill_manual(
    values = c("TRUE" = "#F8766D", "FALSE" = "#00BFC4"),
    guide = "none"
  ) +
  scale_x_discrete(
    guide = guide_axis(n.dodge = 2)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.10, 0.15))
  ) +
  labs(
    title = "Female - Male Hospitalisation Prevalence Gap",
    subtitle = "Unweighted estimates",
    x = "Household expenditure group",
    y = "Gap (percentage points)"
  ) +
  theme_minimal()

print(p_gap)

#===============================================================================
#Health Indicator Comparisons Within/Between Diff Castes
#===============================================================================

# ---------------- Social group across expenditure groups ----------------
social_expenditure <- function(data, state_name) {
  add_quintiles(data) %>%
    mutate(social_group = factor(as.numeric(as.character(b5i3)),
                                 levels = c(1, 2, 3, 9),
                                 labels = c("ST", "SC", "OBC", "Others"))) %>%
    filter(!is.na(social_group), !is.na(b3c9), final_weight > 0) %>%
    group_by(expenditure_group, social_group) %>%
    summarise(
      people_sample       = n(),                       # unweighted people
      hospitalised_sample = sum(b3c9 == 1),            # unweighted hospitalised
      population          = sum(final_weight),
      hospitalised        = sum(final_weight * (b3c9 == 1)),
      prevalence          = hospitalised / population * 100,
      .groups = "drop"
    ) %>%
    mutate(
      expenditure_group = factor(expenditure_group, levels = 1:5, labels = q_labels),
      state = state_name
    )
}

ap_social <- social_expenditure(andhra_pradesh, "Andhra Pradesh")
jk_social <- social_expenditure(jammu_kashmir,  "Jammu & Kashmir")
social_results <- bind_rows(ap_social, jk_social)
print(social_results %>% mutate(across(where(is.numeric), ~ round(.x, 2))))

# ---------------- Plot: prevalence by expenditure group and social group ----------------
p1 <- ggplot(social_results,
             aes(x = expenditure_group, y = prevalence,
                 colour = social_group, group = social_group)) +
  geom_line(linewidth = 1) +
  geom_point(aes(size = hospitalised_sample), alpha = 0.9) +
  facet_wrap(~state) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_size_continuous(range = c(1.5, 6), name = "Hospitalised\n(sample count)") +
  labs(title = "Hospitalisation Prevalence by Expenditure Group and Social Group",
       subtitle = "Survey-weighted; point size shows how many hospitalised people the estimate rests on",
       x = "Expenditure group (household level)",
       y = "Hospitalisation prevalence (%)",
       colour = "Social group") +
  theme_minimal()
print(p1)

# ---------------- Gap and ratio relative to "Others" ----------------
social_gap <- social_results %>%
  select(state, expenditure_group, social_group, prevalence) %>%
  pivot_wider(names_from = social_group, values_from = prevalence) %>%
  mutate(across(any_of(c("ST", "SC", "OBC")),
                list(gap = ~ .x - Others, ratio = ~ .x / Others),
                .names = "{.col}_{.fn}"))
print(social_gap %>% mutate(across(where(is.numeric), ~ round(.x, 2))))

# ---------------- Plot: gap versus "Others" ----------------
gap_long <- social_gap %>%
  select(state, expenditure_group, ends_with("_gap")) %>%
  pivot_longer(ends_with("_gap"), names_to = "social_group", values_to = "gap") %>%
  mutate(social_group = sub("_gap$", "", social_group))

p2 <- ggplot(gap_long,
             aes(x = expenditure_group, y = gap,
                 colour = social_group, group = social_group)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  facet_wrap(~state) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(title = "Hospitalisation Prevalence Gap Relative to 'Others'",
       subtitle = "Percentage points; above zero means higher than the 'Others' group",
       x = "Expenditure group (household level)",
       y = "Gap (percentage points)",
       colour = "Social group") +
  theme_minimal()
print(p2)
#===============================================================================
#Logistic Regression and Testing
#===============================================================================
#Gender Basedl
# Keep valid observations from both states
gender_data <- subset(
  data1,
  st_numeric %in% c(1, 28) &
    b3c9 %in% c(1, 2) &
    b3c4 %in% c(1, 2, 3) &
    !is.na(umce) &
    umce > 0
)

# Outcome: 1 = hospitalised, 0 = not hospitalised
gender_data$hospitalised <- ifelse(
  gender_data$b3c9 == 1, 1, 0
)

# State: Jammu & Kashmir is the reference
gender_data$State <- factor(
  gender_data$st_numeric,
  levels = c(1, 28),
  labels = c("Jammu_Kashmir", "Andhra_Pradesh")
)

# Gender: Gender 1 is the reference
gender_data$Gender <- factor(
  gender_data$b3c4,
  levels = c(1, 2, 3),
  labels = c("Gender1", "Gender2", "Gender3")
)

# Expenditure in thousands of rupees
gender_data$umce_1000 <- gender_data$umce / 1000

# Model with State × Gender interaction
model_interaction <- glm(
  hospitalised ~ State * Gender + umce_1000,
  data = gender_data,
  family = binomial(link = "logit")
)

summary(model_interaction)

#Caste Based
# Keep the two states and valid observations
social_data <- subset(
  data1,
  st_numeric %in% c(1, 28) &
    b3c9 %in% c(1, 2) &
    b5i3 %in% c(1, 2, 3, 9) &
    !is.na(umce) &
    umce > 0
)

# Hospitalisation: 1 = Yes, 0 = No
social_data$hospitalised <- ifelse(
  social_data$b3c9 == 1, 1, 0
)

# State: Jammu & Kashmir is the reference
social_data$State <- factor(
  social_data$st_numeric,
  levels = c(1, 28),
  labels = c("Jammu_Kashmir", "Andhra_Pradesh")
)

# Social group: ST is the reference
social_data$SocialGroup <- factor(
  social_data$b5i3,
  levels = c(1, 2, 3, 9),
  labels = c("ST", "SC", "OBC", "Others")
)

# Expenditure measured in thousands of rupees
social_data$umce_1000 <- social_data$umce / 1000

# Fit logistic regression
model_social <- glm(
  hospitalised ~ State + SocialGroup + umce_1000,
  data = social_data,
  family = binomial(link = "logit")
)

# Model summary
summary(model_social)

# Coefficients, odds ratios, Z-statistics and p-values
results <- summary(model_social)$coefficients

results_table <- data.frame(
  Variable = rownames(results),
  Coefficient = results[, 1],
  Odds_Ratio = exp(results[, 1]),
  Z_statistic = results[, 3],
  P_value = results[, 4],
  row.names = NULL
)

print(results_table)

#===============================================================================
#Chronic/Recent Illness
#===============================================================================

# Household identification variables
HH_ID <- c("st_numeric", "sec", "fsu", "samp", "sss", "hhd")

# Create unweighted expenditure quintiles
add_quintiles_unweighted <- function(data) {
  
  hh <- data %>%
    mutate(
      umce = as.numeric(umce),
      hhsz = as.numeric(hhsz)
    ) %>%
    filter(
      !is.na(umce),
      !is.na(hhsz),
      umce >= 0,
      hhsz > 0
    ) %>%
    distinct(across(all_of(HH_ID)), .keep_all = TRUE) %>%
    mutate(
      expenditure_pc = umce / hhsz,
      expenditure_group = ntile(expenditure_pc, 5)
    ) %>%
    select(all_of(HH_ID), expenditure_group)
  
  data %>%
    select(-any_of("expenditure_group")) %>%
    inner_join(hh, by = HH_ID) %>%
    mutate(
      expenditure_group = factor(
        expenditure_group,
        levels = 1:5,
        labels = c("Q1: Lowest", "Q2: Low", "Q3: Middle",
                   "Q4: High", "Q5: Highest")
      )
    )
}

# Apply separately to each state
andhra_q <- add_quintiles_unweighted(andhra_pradesh)
jk_q <- add_quintiles_unweighted(jammu_kashmir)

# Prepare illness and gender variables
prepare_illness_data <- function(data, state_name) {
  data %>%
    mutate(
      state = state_name,
      gender = factor(
        as.numeric(b3c4),
        levels = c(1, 2),
        labels = c("Male", "Female")
      ),
      chronic_illness = case_when(
        as.numeric(b3c14) == 1 ~ 1,
        as.numeric(b3c14) == 2 ~ 0,
        TRUE ~ NA_real_
      ),
      recent_illness = case_when(
        as.numeric(b3c15) == 1 ~ 1,
        as.numeric(b3c15) == 2 ~ 0,
        TRUE ~ NA_real_
      )
    )
}

combined <- bind_rows(
  prepare_illness_data(andhra_q, "Andhra Pradesh"),
  prepare_illness_data(jk_q, "Jammu & Kashmir")
)

# Calculate prevalence by state, expenditure quintile and gender
illness_summary <- combined %>%
  filter(!is.na(expenditure_group), !is.na(gender)) %>%
  group_by(state, expenditure_group, gender) %>%
  summarise(
    total_people = n(),
    
    chronic_cases = sum(chronic_illness == 1, na.rm = TRUE),
    chronic_valid_n = sum(!is.na(chronic_illness)),
    chronic_prevalence = ifelse(
      chronic_valid_n > 0,
      100 * chronic_cases / chronic_valid_n,
      NA_real_
    ),
    
    recent_cases = sum(recent_illness == 1, na.rm = TRUE),
    recent_valid_n = sum(!is.na(recent_illness)),
    recent_prevalence = ifelse(
      recent_valid_n > 0,
      100 * recent_cases / recent_valid_n,
      NA_real_
    ),
    
    .groups = "drop"
  )

# Display the results
print(illness_summary, n = Inf)

# Chronic illness graph
p_chronic <- ggplot(
  illness_summary,
  aes(x = expenditure_group, y = chronic_prevalence,
      colour = gender, group = gender)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~state) +
  scale_colour_manual(
    values = c("Male" = "#00BFC4", "Female" = "#F8766D")
  ) +
  labs(
    title = "Chronic Illness Prevalence by Gender and Expenditure",
    x = "Household Expenditure Group",
    y = "Chronic Illness Prevalence (%)",
    colour = "Gender"
  ) +
  theme_minimal()

print(p_chronic)

# Recent illness graph
p_recent <- ggplot(
  illness_summary,
  aes(x = expenditure_group, y = recent_prevalence,
      colour = gender, group = gender)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~state) +
  scale_colour_manual(
    values = c("Male" = "#00BFC4", "Female" = "#F8766D")
  ) +
  labs(
    title = "Recent Illness Prevalence by Gender and Expenditure",
    x = "Per-Capita Expenditure Quintile",
    y = "Recent Illness Prevalence (%)",
    colour = "Gender"
  ) +
  theme_minimal()

print(p_recent)
#===============================================================================
#Chronic Diseased People Hospitalised
#===============================================================================
health_data <- bind_rows(
  andhra_pradesh %>% mutate(State = "Andhra Pradesh"),
  jammu_kashmir %>% mutate(State = "Jammu & Kashmir")
)

# Create expenditure quintiles separately within each state
health_data <- health_data %>%
  group_by(State) %>%
  mutate(expenditure_group = ntile(umce, 5)) %>%
  ungroup()

# Select people with chronic ailments
chronic_data <- health_data %>%
  filter(
    b3c14 == 1,
    !is.na(umce),
    !is.na(b3c10),
    !is.na(expenditure_group)
  )

# Summarise hospitalisation frequency
hospitalisation_summary <- chronic_data %>%
  group_by(State, expenditure_group) %>%
  summarise(
    people = n(),
    mean_hospitalisations = mean(b3c10),
    median_hospitalisations = median(b3c10),
    .groups = "drop"
  )

print(hospitalisation_summary)

# Compare mean number of hospitalisations
ggplot(
  hospitalisation_summary,
  aes(
    x = expenditure_group,
    y = mean_hospitalisations,
    colour = State,
    group = State
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 1:5) +
  labs(
    title = "Hospitalisation Frequency Among People with Chronic Ailments",
    x = "Expenditure Quintile (1 = Lowest, 5 = Highest)",
    y = "Mean Number of Hospitalisations",
    colour = "State"
  ) +
  theme_minimal()


ggplot(
  hospitalisation_summary,
  aes(
    x = expenditure_group,
    y = median_hospitalisations,
    colour = State,
    group = State
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 1:5) +
  labs(
    title = "Hospitalisation Frequency Among People with Chronic Ailments",
    x = "Expenditure Quintile (1 = Lowest, 5 = Highest)",
    y = "Mean Number of Hospitalisations",
    colour = "State"
  ) +
  theme_minimal()
#===============================================================================
#Logistic Regression
#===============================================================================

# All individuals, irrespective of chronic illness
analysis_data <- health_data %>%
  filter(
    !is.na(b3c9),
    !is.na(b3c4),
    !is.na(b3c5),
    !is.na(umce)
  ) %>%
  mutate(
    hospitalised = case_when(
      b3c9 == 1 ~ 1,
      b3c9 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    State = factor(State),
    Gender = factor(b3c4)
  ) %>%
  filter(!is.na(hospitalised)) %>%
  group_by(State) %>%
  mutate(
    expenditure_group = factor(ntile(umce, 5))
  ) %>%
  ungroup()

# Model 1: Main effects
model1 <- glm(
  hospitalised ~ State + Gender +
    umce + b3c5,
  data = analysis_data,
  family = binomial(link = "logit")
)

# Model 2: State × Gender interaction
model2 <- glm(
  hospitalised ~ State * Gender +
    umce + b3c5,
  data = analysis_data,
  family = binomial(link = "logit")
)

summary(model2)

# Test whether the interaction improves model fit
anova(model1, model2, test = "Chisq")

# Odds ratios and 95% confidence intervals
exp(cbind(
  OR = coef(model2),
  confint(model2)
))
