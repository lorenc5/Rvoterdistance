# Generate hex sticker for Rvoterdistance
# Run: Rscript data-raw/make_logo.R

if (!requireNamespace("hexSticker", quietly = TRUE)) {
  install.packages("hexSticker", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("showtext", quietly = TRUE)) {
  install.packages("showtext", repos = "https://cloud.r-project.org")
}

library(hexSticker)
library(ggplot2)
library(showtext)

font_add_google("Roboto Condensed", "rc")
showtext_auto()

# Subplot: voter at center, distance lines radiating to polling locations
set.seed(42)
n <- 8
angles <- seq(0, 2 * pi, length.out = n + 1)[1:n]
df_lines <- data.frame(
  x = 0, y = 0,
  xend = 1.2 * cos(angles),
  yend = 1.2 * sin(angles)
)
df_points <- data.frame(
  x = c(0, 1.2 * cos(angles)),
  y = c(0, 1.2 * sin(angles)),
  type = c("voter", rep("location", n))
)

p <- ggplot() +
  geom_segment(
    data = df_lines,
    aes(x = x, y = y, xend = xend, yend = yend),
    colour = "white", linewidth = 0.3, alpha = 0.6
  ) +
  geom_point(
    data = df_points[df_points$type == "location", ],
    aes(x = x, y = y),
    colour = "#FFD700", size = 1.5, shape = 17  # triangles for locations
  ) +
  geom_point(
    data = df_points[df_points$type == "voter", ],
    aes(x = x, y = y),
    colour = "white", size = 2.5                # circle for voter
  ) +
  theme_void() +
  theme(legend.position = "none")

dir.create("man/figures", showWarnings = FALSE, recursive = TRUE)

sticker(
  p,
  package   = "Rvoterdistance",
  p_size    = 14,
  p_y       = 1.45,
  p_color   = "white",
  p_family  = "rc",
  s_x       = 1,
  s_y       = 0.8,
  s_width   = 1.4,
  s_height  = 1.0,
  h_fill    = "#2C3E50",
  h_color   = "#E74C3C",
  h_size    = 1.5,
  url       = "lorenc5/Rvoterdistance",
  u_color   = "white",
  u_size    = 3.5,
  filename  = "man/figures/logo.png",
  dpi       = 300
)

cat("Logo saved to man/figures/logo.png\n")
