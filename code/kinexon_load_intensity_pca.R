# =============================================================================
# Kinexon Load & Intensity: Correlation and PCA Weighting
# Authors: Kya Carroll with OTE sport science staff
#
# Explores relationships among Kinexon LPS metrics, runs a principal
# component analysis, and converts PC1 loadings into weights for a single
# composite load / intensity score used in daily reporting.
#
# Load metrics:      THIL, AAL, Decel Load, FCT, Exertions
# Intensity metrics: THIL/AAL, high accelerations (>6.43 ft/s^2) / total,
#                    high decelerations (< -6.43 ft/s^2) / total,
#                    high-intensity exertions / total
#
# Note: input is a de-identified session-level export (no names or dates).
# =============================================================================

library(tidyverse)
library(janitor)
library(ggcorrplot)

kinexon <- read_csv("kinexon_sessions_deidentified.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  select(where(is.numeric)) %>%
  select(where(~ !all(is.na(.)))) %>%          # drop empty metrics
  select(where(~ !all(. == 0, na.rm = TRUE))) %>%  # drop all-zero metrics
  drop_na()

# Correlation heatmap (hierarchically clustered) ----
ggcorrplot(cor(kinexon), hc.order = TRUE, lab = TRUE, lab_size = 3,
           colors = c("#6D94B8", "white", "#E18485"),
           title = "Correlation of Kinexon Metrics",
           ggtheme = theme_minimal)

# PCA on standardised metrics ----
pca <- prcomp(kinexon, scale. = TRUE)
summary(pca)

scree <- tibble(PC = seq_along(pca$sdev),
                PVE = pca$sdev^2 / sum(pca$sdev^2)) %>%
  mutate(cumulative = cumsum(PVE))

ggplot(scree, aes(PC)) +
  geom_col(aes(y = PVE), fill = "lightblue", colour = "black") +
  geom_line(aes(y = cumulative), colour = "red", linewidth = 1) +
  geom_point(aes(y = cumulative), colour = "red", size = 2) +
  scale_y_continuous(labels = scales::percent,
                     sec.axis = sec_axis(~., name = "Cumulative", labels = scales::percent)) +
  scale_x_continuous(breaks = scree$PC) +
  labs(title = "Scree Plot: Kinexon PCA", x = "Principal component",
       y = "Proportion of variance explained") +
  theme_minimal()

# Loadings heatmap (first 5 PCs) ----
as_tibble(pca$rotation[, 1:5], rownames = "metric") %>%
  pivot_longer(-metric, names_to = "PC", values_to = "loading") %>%
  ggplot(aes(PC, metric, fill = loading)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(loading, 2)), size = 3) +
  scale_fill_gradient2(low = "#6D94B8", mid = "white", high = "#E18485") +
  labs(title = "Metric Loadings on First 5 Principal Components", x = NULL, y = NULL) +
  theme_minimal()

# PC1 loadings -> composite weights (normalised absolute loadings, sum to 1) ----
pc1 <- pca$rotation[, "PC1"]
weights <- tibble(metric = names(pc1),
                  loading_pc1 = pc1,
                  weight = abs(pc1) / sum(abs(pc1))) %>%
  mutate(weight_pct = round(100 * weight, 1)) %>%
  arrange(desc(weight))

print(weights)
write_csv(weights, "kinexon_pc1_weights.csv")
