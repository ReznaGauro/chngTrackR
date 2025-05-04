library(terra)
library(tidyterra)
library(ggplot2)

# 1. Load Data ------------------------------------------------------------
output_dir <- "D:/MSc_EAGLE/RPackage/TestPackage/Plots_Final/"
class_trans <- rast(paste0(output_dir, "class_transitioned.tif"))

# 2. Define Colors --------------------------------------------------------
class_colors <- c(
  "Broadleaved" = "#4D9E2B",  # Original green
  "Coniferous" = "#006400",    # Original dark green
  "Others" = "#D3D3D3",        # Original light grey
  "Water" = "#00688B"          # Original blue
)

transition_colors <- c(
  "Others → Broadleaved" = "#8FA87F",    # Grey-green mix
  "Broadleaved → Coniferous" = "#D95F02", # Distinct orange
  "Water → Coniferous" = "#1B9E77",      # Teal
  "Broadleaved → Others" = "#636363",   # Darkgrey
  "No Change" = "#F0F0F0",               # Off-white
  "Broadleaved → Water" = "#40E0D0"      # Blue-green
)

# 3. Reclassify to Factor -------------------------------------------------
reclass_mat <- matrix(c(
  -2, 1,   # Others → Broadleaved
  1, 2,    # Broadleaved → Coniferous
  -1, 3,   # Water → Coniferous
  2, 4,    # Broadleaved → Others
  0, 5,    # No Change
  3, 6     # Broadleaved → Water
), ncol = 2, byrow = TRUE)

class_trans_fac <- classify(class_trans, reclass_mat) %>%
  as.factor()

levels(class_trans_fac) <- data.frame(
  ID = 1:6,
  label = names(transition_colors)
)

# 4. Create Plot ----------------------------------------------------------
ggplot() +
  geom_spatraster(data = class_trans_fac) +
  scale_fill_manual(
    values = transition_colors,
    name = "Class Transitions",
    na.value = "transparent",
    guide = guide_legend(
      ncol = 2,
      title.position = "top",
      keywidth = unit(1.5, "cm")
    )  # Fixed by removing the trailing comma here
  ) +  # Close scale_fill_manual
  ggtitle("Forest Cover Transitions (2010-2020)") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank()
  ) +
  coord_sf() +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr")

# 5. Save Output ----------------------------------------------------------
ggsave(
  paste0(output_dir, "Final_Transition_Map.png"),
  width = 12,
  height = 8,
  dpi = 300
)

print("Final Transition Map")

