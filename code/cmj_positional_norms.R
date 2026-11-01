# =============================================================================
# CMJ Positional Norms & T-Score Pipeline
# Author: Kya Carroll | Overtime Elite Sports Science
#
# Pulls Hawkin Dynamics countermovement jump (CMJ) data from the Smartabase
# API across multiple seasons, builds each athlete's best-effort baseline,
# then generates position-specific percentile norms and T-scores for nine
# force-time metrics.
#
# Note: credentials are read from environment variables and the roster is
# read from a local file. No athlete data is included in this sample.
# =============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, smartabaseR, janitor, lubridate)

# Credentials (set in .Renviron, never hard-coded) ----
sb_user <- Sys.getenv("SMARTABASE_USER")
sb_pass <- Sys.getenv("SMARTABASE_PASS")
sb_url  <- Sys.getenv("SMARTABASE_URL")

# Roster and positions ----
# roster.csv columns: player, position_cluster (GUARD / WING / BIG), season
roster  <- read_csv("roster.csv", show_col_types = FALSE)
players <- unique(roster$player)
pos_map <- roster %>%
  distinct(player, position_cluster) %>%
  mutate(player = str_to_lower(player))

season_start <- "01/01/2023"   # dd/mm/YYYY
season_end   <- "01/01/2027"

# API pull ----
fetch_player <- function(player_name) {
  res <- smartabaseR::sb_get_event(
    form       = "Hawkin CMJ",
    date_range = c(season_start, season_end),
    url        = sb_url,
    username   = sb_user,
    password   = sb_pass,
    filter     = smartabaseR::sb_get_event_filter(user_key = "about",
                                                  user_value = player_name)
  )
  if (is.null(res) || nrow(res) == 0) return(NULL)
  mutate(res, player = player_name)
}

all_events <- list()
for (p in players) {
  out <- try(fetch_player(p), silent = TRUE)      # one failure won't stop the loop
  if (!inherits(out, "try-error") && !is.null(out)) all_events[[length(all_events) + 1]] <- out
  Sys.sleep(0.3)                                  # respect API rate limits
}

# Harmonise column types across pulls, then bind ----
char_cols <- unique(unlist(lapply(all_events, function(df)
  names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))])))
cmj_raw <- all_events %>%
  lapply(function(df) mutate(df, across(any_of(char_cols), as.character))) %>%
  bind_rows()

# Clean and join positions ----
cmj <- cmj_raw %>%
  filter(Type == "Countermovement Jump") %>%
  clean_names() %>%
  mutate(player = str_to_lower(player)) %>%
  left_join(pos_map, by = "player")

metrics <- c("max_jump_height_in", "m_rsi", "jump_momentum_kg_m_s",
             "braking_net_impulse_n_s", "peak_relative_braking_force_percent",
             "propulsive_net_impulse_n_s", "peak_relative_propulsive_force_percent",
             "impulse_ratio", "relative_force_at_min_displacement_percent")

# Athlete baseline: top 3 jumps per session, averaged across sessions ----
athlete_baseline <- cmj %>%
  group_by(player, date) %>%
  slice_max(order_by = max_jump_height_in, n = 3, with_ties = FALSE) %>%
  group_by(player) %>%
  summarise(position_cluster = first(position_cluster),
            across(all_of(metrics), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")

# Position-specific percentile norms (3rd to 97th) ----
probs <- c(0.97, 0.95, seq(0.90, 0.05, by = -0.05), 0.03)

norms <- athlete_baseline %>%
  group_by(position_cluster) %>%
  reframe(pth = probs * 100,
          across(all_of(metrics), ~ quantile(.x, probs = probs, na.rm = TRUE))) %>%
  mutate(descriptor = case_when(pth >= 90 ~ "GOOD",
                                pth >= 70 ~ "ABOVE",
                                pth >= 40 ~ "AVERAGE",
                                pth >= 15 ~ "BELOW",
                                TRUE      ~ "POOR"))

# Positional reference stats and T-scores (mean 50, SD 10) ----
ref <- athlete_baseline %>%
  group_by(position_cluster) %>%
  summarise(n_athletes = n(),
            across(all_of(metrics), list(mean = ~ mean(.x, na.rm = TRUE),
                                         sd   = ~ sd(.x, na.rm = TRUE))),
            .groups = "drop")

t_scores <- athlete_baseline %>%
  pivot_longer(all_of(metrics), names_to = "metric", values_to = "value") %>%
  left_join(ref %>%
              pivot_longer(-c(position_cluster, n_athletes),
                           names_to = c("metric", ".value"),
                           names_pattern = "(.*)_(mean|sd)$"),
            by = c("position_cluster", "metric")) %>%
  mutate(t_score = round((value - mean) / sd * 10 + 50, 1)) %>%
  select(player, position_cluster, metric, t_score) %>%
  pivot_wider(names_from = metric, values_from = t_score, names_prefix = "t_")

write_csv(norms,    "cmj_positional_norms.csv")
write_csv(t_scores, "cmj_athlete_t_scores.csv")
