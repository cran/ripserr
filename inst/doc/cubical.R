## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ripserr)

## ----load-data,fig.width=6,fig.height=4---------------------------------------
# create dataset
sample_image <- matrix(0, nrow = 10, ncol = 10)
i <- 2:9
j <- c(2, 9)
sample_image[i, j] <- 1
sample_image[j, i] <- 1

# view as matrix
sample_image

# view as image
graphics::image(sample_image, useRaster = TRUE, axes = FALSE)

## -----------------------------------------------------------------------------
# calculate persistent homology
image_phom <- cubical(sample_image)

# print `cubical` output
image_phom

# print features
head(image_phom)

