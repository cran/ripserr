## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
library(ripserr)

## -----------------------------------------------------------------------------
data(aegypti)
data(case_predictors)

stateAbbSort <- sort(aegypti$state_code[!duplicated(aegypti$state_code)]) 

## -----------------------------------------------------------------------------
caseModel <- data.frame()

## ----fig.width = 4, fig.height = 4--------------------------------------------
AC_coord <- aegypti[aegypti$state_code == "AC", c("x", "y"), drop = FALSE]
AC_rips <- vietoris_rips(AC_coord) ##filtration

plot.new()
plot.window(
  xlim = c(0, max(AC_rips$death)),
  ylim = c(0, max(AC_rips$death)),
  asp = 1
)
axis(1L)
axis(2L)
abline(a = 0, b = 1)
points(AC_rips[AC_rips$dimension == 0L, c("birth", "death")], pch = 16L)
points(AC_rips[AC_rips$dimension == 1L, c("birth", "death")], pch = 17L)

## ----fig.width = 4, fig.height = 4--------------------------------------------
plot(x = AC_coord$x, y = AC_coord$y, asp = 1, xlab = "X", ylab = "Y",
     main = "Aedes Aegypti Occurrences in AC")

## -----------------------------------------------------------------------------
topologicalFeatures <- function(state_rips){
  numH0 <- length(which(state_rips$dimension == 0))
  numH1 <- length(which(state_rips$dimension == 1))
  if (numH1 != 0) {
    maxPerst <- max(state_rips$persistence[(numH0 + 1) : (numH0 + numH1)])
    medianPerst <- median(state_rips$persistence[(numH0 + 1) : (numH0 + numH1)])
    
  } else {
    maxPerst <- 0
    medianPerst <- 0
  }
  return(c(numH0, numH1, medianPerst, maxPerst))
}

## -----------------------------------------------------------------------------
for(val in stateAbbSort) {
  state_coord <- aegypti[aegypti$state_code == val, c("x", "y"), drop = FALSE]
  state_rips <- vietoris_rips(state_coord)
  state_rips$persistence<-(state_rips$death - state_rips$birth)
  
  features <- topologicalFeatures(state_rips)
  caseModel <- rbind(
    caseModel,
    c(features[1], features[2], features[3], features[4])
  )
}
colnames(caseModel) <-  c("H0", "H1", "H1MD","H1ML")
rownames(caseModel) <- stateAbbSort

## -----------------------------------------------------------------------------
caseModel <- merge(caseModel, case_predictors, by = "row.names")[, -1]
rownames(caseModel) <- stateAbbSort
head(caseModel)

## ----fig.width = 6, fig.height = 6--------------------------------------------
par(mfrow = c(2L, 1L))

hist(caseModel$CASE, main = "Distribution of Case Counts")

hist(log(caseModel$CASE), 
     main = "Distribution of Log-Transformed Case Counts")

## ----fig.width = 5, fig.height = 5--------------------------------------------
pairs(caseModel, pch = 16L)

## -----------------------------------------------------------------------------
fit.1 <- lm(log(CASE) ~ POP + TEMP + PRECIP + H0, data = caseModel)
summary(fit.1)

## ----fig.width = 8, fig.height = 4--------------------------------------------
par(mfrow = c(1L, 2L))
plot(fit.1, which = 1)
plot(fit.1, which = 2)

## -----------------------------------------------------------------------------
fit.nested <- lm(log(CASE) ~ POP + TEMP + H0, data = caseModel)
lmtest::lrtest(fit.nested, fit.1)
fit.1 <- lm(log(CASE) ~ POP + TEMP + H0, data = caseModel)
summary(fit.1)

## -----------------------------------------------------------------------------
fit.2 <- lm(log(CASE) ~ POP + TEMP + H0 + H1 + H1ML + H1MD,
            data = caseModel)
summary(fit.2)

## ----fig.width = 8, fig.height = 4--------------------------------------------
par(mfrow = c(1L, 2L))
plot(fit.2, which = 1)
plot(fit.2, which = 2)

## -----------------------------------------------------------------------------
fit.test0 <- lm(log(CASE) ~ POP + TEMP + H0 + H1 + H1ML,
                data = caseModel)
summary(fit.test0)
lmtest::lrtest(fit.test0, fit.2)

## -----------------------------------------------------------------------------
fit.test1 <- lm(log(CASE) ~ POP + TEMP + H0 + H1ML, data = caseModel)
summary(fit.test1)
lmtest::lrtest(fit.test1, fit.test0)

## -----------------------------------------------------------------------------
fit.2 <- lm(log(CASE) ~ POP + TEMP + H0 + H1ML, data = caseModel)

## -----------------------------------------------------------------------------
summary(fit.1)
summary(fit.2)

## -----------------------------------------------------------------------------
Example_States <- c("PE", "CE", "GO")
Coord_Example <- data.frame()
for(val in Example_States){
  targetRow <- caseModel[rownames(caseModel) == val,]
  Coord_Example <- rbind(Coord_Example,
                         c(targetRow$H0, targetRow$H1ML, targetRow$CASE))
}
rownames(Coord_Example) <- Example_States
colnames(Coord_Example) <- c("H0", "H1ML", "CASE");Coord_Example

## ----fig.width = 9, fig.height = 3--------------------------------------------
par(mfrow = c(1L, 3L))

PE_coord <- aegypti[aegypti$state_code == "PE", c("x", "y"), drop = FALSE]
PE_rips <- vietoris_rips(PE_coord)
plot(x = PE_coord$x, y = PE_coord$y, asp = 1, col = "green",
     xlim = c(-42, -32), xlab = "X", ylab = "Y", main = "Pernambuco")

CE_coord <- aegypti[aegypti$state_code == "CE", c("x", "y"), drop = FALSE]
CE_rips <- vietoris_rips(CE_coord)
plot(x = CE_coord$x, y = CE_coord$y, asp = 1, col = "blue",
     xlim = c(-43, -33), xlab = "X", ylab = "Y", main = "Ceará")

GO_coord <- aegypti[aegypti$state_code == "GO", c("x", "y"), drop = FALSE]
GO_rips <- vietoris_rips(GO_coord) 
plot(x = GO_coord$x, y = GO_coord$y, asp = 1, col = "red",
     xlim = c(-54, - 44), xlab = "X", ylab = "Y", main = "Goiás")

par(mfrow = c(1L, 1L))

