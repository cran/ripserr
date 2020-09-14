## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ripserr)

## ----load-data----------------------------------------------------------------
# create reproducible dataset
set.seed(42)
unif_angles <- runif(25, 0, 2 * pi)
circle2d <- data.frame(x = cos(unif_angles),
                       y = sin(unif_angles))

# take a peek at first 6 rows
head(circle2d)

## ----plot-circle2d, fig.width = 4, fig.height = 4.5---------------------------
# scatterplot of circle2d
plot(circle2d, xlab = "x", ylab = "y", main = "2-d circle point cloud")

## ----calc-hom-----------------------------------------------------------------
# calculate persistent homology
circle.phom <- vietoris_rips(circle2d)

# print first 6 features (ordered by dimension and birth)
head(circle.phom)

# print last 6 features (ordered by dimension and birth)
tail(circle.phom)

## ----phom-vis, fig.height = 3, fig.width = 7----------------------------------
# plot topological barcode
TDAstats::plot_barcode(as.matrix(circle.phom))

