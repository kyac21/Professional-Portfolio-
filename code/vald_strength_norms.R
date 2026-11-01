# =============================================================================
# VALD Strength Norms: Hip Adduction (ForceFrame) & Quad Torque (DynaMo)
# Author: Kya Carroll | Overtime Elite Sports Science
#
# Pulls VALD strength testing from the Smartabase API, builds a bilateral
# baseline for each athlete (mean of top 3 left + mean of top 3 right), and
# produces position-specific and squad-wide percentile norms with descriptors.
#
# Note: credentials are read from environment variables and the roster is
# read from a local file. No athlete data is included in this sample.
# =============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, smartabaseR, janitor)

sb_user <- Sys.getenv("SMARTABASE_USER")
sb_pass <- Sys.getenv("SMARTABASE_PASS")
sb_url  <- Sys.getenv("SMARTABASE_URL")

roster  <- read_csv("roster.csv", show_col_types = FALSE)   # player, position_cluster
pos_map <- roster %>% distinct(player, position_cluster) %>% mutate(player = str_to_lower(player))

season_start <- "01/08/2025"
season_end   <- "01/01/2027"

# Generic Smartabase pull for any form ----
pull_form <- function(form_name) {
  out <- list()
  for (p in unique(roster$player)) {
    res <- try(smartabaseR::sb_get_event(
      form = form_name, date_range = c(season_start, season_end),
      url = sb_url, username = sb_user, password = sb_pass,
      filter = smartabaseR::sb_get_event_filter(user_key = "about", user_value = p)
    ), silent = TRUE)
    if (!inherits(res, "try-error") && !is.null(res) && nrow(res) > 0)
      out[[length(out) + 1]] <- mutate(res, player = p, across(where(is.factor), as.character))
    Sys.sleep(0.3)
  }
  bind_rows(lapply(out, \(df) mutate(df, across(where(is.character), as.character))))
}

# Bilateral baseline: mean(top 3 L) and mean(top 3 R), averaged ----
top3 <- function(x) mean(head(sort(x, decreasing = TRUE), 3), na.rm = TRUE)

bilateral_baseline <- function(df, left, right) {
  df %>%
    group_by(player) %>%
    summarise(position_cluster = first(position_cluster),
              top3_left  = top3({{ left }}),
              top3_right = top3({{ right }}),
              .groups = "drop") %>%
    mutate(baseline = (top3_left + top3_right) / 2,
           asymmetry_pct = 100 * (top3_left - top3_right) / pmax(top3_left, top3_right))
}

# Percentile norms with descriptors ----
probs <- c(0.97, 0.95, seq(0.90, 0.05, by = -0.05), 0.03)
make_norms <- function(baselines, by_position = TRUE) {
  baselines %>%
    { if (by_position) group_by(., position_cluster) else . } %>%
    reframe(pth = probs * 100, baseline = quantile(baseline, probs = probs, na.rm = TRUE)) %>%
    mutate(descriptor = case_when(pth >= 90 ~ "EXCELLENT", pth >= 70 ~ "ABOVE",
                                  pth >= 40 ~ "AVERAGE",   pth >= 15 ~ "BELOW",
                                  TRUE ~ "POOR"))
}

# Hip adduction (ForceFrame) ----
hip <- pull_form("VALD Performance Test") %>%
  filter(`Test Type` == "Hip Ab/Ad") %>%
  clean_names() %>%
  mutate(player = str_to_lower(player)) %>%
  left_join(pos_map, by = "player")
hip_base <- bilateral_baseline(hip, l_add_torque, r_add_torque)

# Quad torque (DynaMo knee extension) ----
quad <- pull_form("VALD Dynamo") %>%
  filter(`Body Region` == "Knee", Movement == "Extension") %>%
  clean_names() %>%
  mutate(player = str_to_lower(player)) %>%
  left_join(pos_map, by = "player")
quad_base <- bilateral_baseline(quad, quad_strength_l_tourque, quad_strength_r_tourque)

write_csv(make_norms(hip_base,  TRUE),  "hip_add_norms_by_position.csv")
write_csv(make_norms(hip_base,  FALSE), "hip_add_norms_squad.csv")
write_csv(make_norms(quad_base, TRUE),  "quad_torque_norms_by_position.csv")
write_csv(make_norms(quad_base, FALSE), "quad_torque_norms_squad.csv")
