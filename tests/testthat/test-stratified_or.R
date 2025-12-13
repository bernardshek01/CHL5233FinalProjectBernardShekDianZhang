test_that("stratified_or returns expected structure", {

  set.seed(2025)
  exposure   <- rbinom(100, 1, 0.4)
  outcome    <- rbinom(100, 1, 0.3)
  confounder <- sample(1:3, 100, replace = TRUE)

  res <- stratified_or(exposure, outcome, confounder)

  expect_type(res, "list")
  expect_named(res, c("Crude", "Stratified"))
  expect_named(res$Crude, c("table", "Odds_Ratio"))
  expect_true(is.matrix(res$Crude$table))
  expect_true(is.numeric(res$Crude$Odds_Ratio))
  expect_type(res$Stratified, "list")

})
