context("cubical 3-dim")
library("ripserr")

test_that("basic 3-dim cubical works", {
  # reproducibility
  set.seed(42)
  
  # create data
  test_data <- rnorm(10 ^ 3)
  dim(test_data) <- rep(10, 3)
  
  # create cubical complex
  cub_comp <- ripserr::cubical(test_data)
  
  # test cubical complex frequency/counts
  expect_equal(ncol(cub_comp), 3)
  expect_true(nrow(cub_comp) > 0)
  
  counts <- base::table(cub_comp[, 1])
  names(counts) <- NULL
  counts <- as.numeric(counts)
  
  # at least 1 feature from each dimension
  expect_true(counts[1] > 0)
  expect_true(counts[2] > 0)
  expect_true(counts[3] > 0)
  expect_true(counts[4] > 0)
  
  # make sure no births after deaths
  expect_equal(0, sum(cub_comp[, 2] > cub_comp[, 3]))
})

# these tests use example data + original code from Github: CubicalRipser/Cubical_2dim
#   to validate accuracy
test_that("3-dim calculation returns same values as validated tests", {
  # read validated input and output data
  input_data <- readRDS("input_3dim.rds")
  output_data <- readRDS("output_3dim.rds")
  
  # re-calculate output w/ ripserr
  THRESH <- 9999
  test_output <- ripserr::cubical(input_data, threshold = THRESH)
  
  # filter out threshold value features to avoid spurious differences in equality
  output_data <- subset(output_data, death < THRESH)
  test_output <- subset(test_output, death < THRESH)
  
  # ensure no NAs
  expect_equal(0, sum(is.na(output_data)))
  expect_equal(0, sum(is.na(test_output)))
  
  # make sure # of features is close enough
  expect_equal(nrow(test_output), nrow(output_data))
  
  # check means of births and deaths to ensure close enough
  expect_equal(mean(test_output$birth), mean(output_data$birth))
  expect_equal(mean(test_output$death), mean(output_data$death))
})