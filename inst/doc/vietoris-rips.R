## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ripserr)

## ----load-data----------------------------------------------------------------
# create reproducible dataset
set.seed(42)
angles <- runif(25, 0, 2 * pi)
circle_df <- data.frame(x = cos(angles),
                        y = sin(angles))

# take a peek at first 6 rows
head(circle_df)

## ----plot-circle, fig.width = 4, fig.height = 4.5-----------------------------
# scatterplot of circle2d
plot(circle_df, xlab = "x", ylab = "y", main = "2-d circle point cloud")

## ----calc-hom-----------------------------------------------------------------
# calculate persistent homology
circ_phom <- vietoris_rips(circle_df)

# print first 6 features (ordered by dimension and birth)
head(circ_phom)

# print last 6 features (ordered by dimension and birth)
tail(circ_phom)

## ----formats------------------------------------------------------------------
# matrix format
circ_mat <- as.matrix(circle_df)
head(circ_mat)

# dist object
circ_dist <- dist(circ_mat)
head(circ_dist)

# calculate persistent homology of each
mat_phom <- vietoris_rips(circ_mat)
dist_phom<- vietoris_rips(circ_dist)

# compare equality
all.equal(circ_phom, mat_phom)
all.equal(circ_phom, dist_phom)

## ----prime-fields-------------------------------------------------------------
# prime field Z/2Z (default)
circ_phom2 <- vietoris_rips(circle_df)

# prime field Z/5Z
circ_phom5 <- vietoris_rips(circle_df, p = 5L)

# confirm equal outputs
all.equal(circ_phom2, circ_phom5)

