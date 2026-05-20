library(readr)
indicators <- read_csv("C:/Users/lisap/Downloads/qlfs_key_indicators.csv")
View(indicators)

library(readr)
province <- read_csv("C:/Users/lisap/Downloads/qlfs_by_province.csv")
View(province)

library(readr)
age <- read_csv("C:/Users/lisap/Downloads/qlfs_by_age.csv")
View(age)

library(readr)
sex <- read_csv("C:/Users/lisap/Downloads/qlfs_by_sex.csv")
View(sex)



# ============================================
# SA Unemployment Analysis
# Script 1: Load & Explore Data
# Source: Stats SA QLFS Q4 2024
# ============================================

# Load libraries
library(tidyverse)

# ── Load datasets ────────────────────────────
indicators <- read_csv("data/qlfs_key_indicators.csv")
province   <- read_csv("data/qlfs_by_province.csv")
age        <- read_csv("data/qlfs_by_age.csv")
sex        <- read_csv("data/qlfs_by_sex.csv")

# ── First look ───────────────────────────────
glimpse(indicators)
glimpse(province)
glimpse(age)
glimpse(sex)

# ── Quick summaries ──────────────────────────
summary(indicators$unemployment_rate)
summary(province$unemp_rate_Q42024)

# ── Check for missing values ─────────────────
colSums(is.na(indicators))
colSums(is.na(province))



# ============================================
# SA Unemployment Analysis
# Script 2: Wrangle & Analyse
# Source: Stats SA QLFS Q4 2024
# ============================================

# ── Create a proper date column ───────────────
indicators <- indicators %>%
  mutate(date = as.Date(paste0(year, "-",
                               case_when(
                                 quarter == "Q1" ~ "03",
                                 quarter == "Q2" ~ "06",
                                 quarter == "Q3" ~ "09",
                                 quarter == "Q4" ~ "12"
                               ), "-01")))

# ── Key summary stats ─────────────────────────
cat("=== Unemployment Rate Summary ===\n")
cat("Highest:", max(indicators$unemployment_rate), "%\n")
cat("Lowest: ", min(indicators$unemployment_rate), "%\n")
cat("Average:", round(mean(indicators$unemployment_rate), 1), "%\n")
cat("Latest (Q4 2024):", 
    indicators$unemployment_rate[nrow(indicators)], "%\n")

# ── Which province has it worst? ──────────────
cat("\n=== Province Rankings (Q4 2024) ===\n")
province %>%
  arrange(desc(unemp_rate_Q42024)) %>%
  select(province, unemp_rate_Q42024) %>%
  print()

# ── Youth vs adults ───────────────────────────
cat("\n=== Unemployment by Age Group ===\n")
age %>%
  select(age_group, unemployment_rate) %>%
  arrange(desc(unemployment_rate)) %>%
  print()

# ── Gender gap ────────────────────────────────
cat("\n=== Gender Gap (Q4 2024) ===\n")
sex_latest <- sex %>% filter(year == 2024)
cat("Male unemployment rate:  ", sex_latest$unemp_rate_male, "%\n")
cat("Female unemployment rate:", sex_latest$unemp_rate_female, "%\n")
cat("Gap:", sex_latest$unemp_rate_female - sex_latest$unemp_rate_male, 
    "percentage points\n")




# ============================================
# SA Unemployment Analysis
# Script 3: Visualisations
# Source: Stats SA QLFS Q4 2024
# ============================================


indicators <- indicators %>%
  mutate(date = as.Date(paste0(year, "-",
                               case_when(
                                 quarter == "Q1" ~ "03",
                                 quarter == "Q2" ~ "06",
                                 quarter == "Q3" ~ "09",
                                 quarter == "Q4" ~ "12"
                               ), "-01")))

# Create output folder
dir.create("output", showWarnings = FALSE)

# ── PLOT 1: Unemployment rate over time ───────
p1 <- ggplot(indicators, aes(x = date, y = unemployment_rate)) +
  geom_line(colour = "#c0392b", linewidth = 1.2) +
  geom_point(colour = "#c0392b", size = 2) +
  geom_hline(yintercept = mean(indicators$unemployment_rate),
             linetype = "dashed", colour = "grey50") +
  annotate("text", x = min(indicators$date),
           y = mean(indicators$unemployment_rate) + 0.5,
           label = paste0("Average: ",
                          round(mean(indicators$unemployment_rate), 1), "%"),
           hjust = 0, size = 3.5, colour = "grey40") +
  labs(
    title = "South Africa Unemployment Rate: Q4 2019 – Q4 2024",
    subtitle = "Official unemployment rate among persons aged 15–64",
    x = NULL, y = "Unemployment Rate (%)",
    caption = "Source: Stats SA QLFS Q4 2024"
  ) +
  theme_minimal(base_size = 13)

ggsave("output/plot1_unemployment_trend.png", p1, 
       width = 10, height = 5, dpi = 300)

# ── PLOT 2: Unemployment by province ─────────
p2 <- province %>%
  arrange(unemp_rate_Q42024) %>%
  mutate(province = fct_inorder(province)) %>%
  ggplot(aes(x = unemp_rate_Q42024, y = province, 
             fill = unemp_rate_Q42024)) +
  geom_col() +
  geom_text(aes(label = paste0(unemp_rate_Q42024, "%")),
            hjust = -0.1, size = 3.5) +
  scale_fill_gradient(low = "#f9ca24", high = "#c0392b", guide = "none") +
  xlim(0, 55) +
  labs(
    title = "Unemployment Rate by Province",
    subtitle = "Q4 2024 — Limpopo and Eastern Cape hit hardest",
    x = "Unemployment Rate (%)", y = NULL,
    caption = "Source: Stats SA QLFS Q4 2024"
  ) +
  theme_minimal(base_size = 13)

ggsave("output/plot2_province.png", p2, 
       width = 9, height = 6, dpi = 300)

# ── PLOT 3: Youth vs adult unemployment ───────
p3 <- ggplot(age, aes(x = age_group, y = unemployment_rate,
                      fill = unemployment_rate)) +
  geom_col() +
  geom_text(aes(label = paste0(unemployment_rate, "%")),
            vjust = -0.5, size = 4) +
  scale_fill_gradient(low = "#f9ca24", high = "#c0392b", guide = "none") +
  ylim(0, 80) +
  labs(
    title = "Unemployment Rate by Age Group",
    subtitle = "Q4 2024 — 70% of youth aged 15–24 are unemployed",
    x = "Age Group", y = "Unemployment Rate (%)",
    caption = "Source: Stats SA QLFS Q4 2024"
  ) +
  theme_minimal(base_size = 13)

ggsave("output/plot3_age.png", p3, 
       width = 9, height = 6, dpi = 300)

# ── PLOT 4: Gender gap over time ──────────────
p4 <- sex %>%
  mutate(date = as.Date(paste0(year, "-12-01"))) %>%
  pivot_longer(cols = c(unemp_rate_male, unemp_rate_female),
               names_to = "sex",
               values_to = "rate") %>%
  mutate(sex = recode(sex,
                      "unemp_rate_male"   = "Male",
                      "unemp_rate_female" = "Female")) %>%
  ggplot(aes(x = date, y = rate, colour = sex, group = sex)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = c("Male" = "#2980b9", 
                                 "Female" = "#c0392b")) +
  labs(
    title = "Unemployment Rate by Gender: Q4 2016 – Q4 2024",
    subtitle = "Women consistently face higher unemployment than men",
    x = NULL, y = "Unemployment Rate (%)", colour = NULL,
    caption = "Source: Stats SA QLFS Q4 2024"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

ggsave("output/plot4_gender.png", p4, 
       width = 10, height = 5, dpi = 300)

cat("✅ All 4 plots saved to output/ folder!\n")

