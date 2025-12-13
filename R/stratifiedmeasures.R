#' @title X
#' @description X
#' @param X
#' @param X
#' @return X
#' @examples X
#' @export

library(epitools)

stratified_or <- function(exposure, outcome, confounder) {

  # DATA CHECKS
  # Length Check
  if(length(unique(c(length(exposure), length(outcome), length(confounder)))) != 1) {stop("Exposure, Outcome, and Confounder must all be the same length.")}
  # Numeric Check
  # Outcome Check
  n_levels_outcomes <- unique(outcome[!is.na(outcome)])
  if(length(n_levels_outcomes) != 2) {stop("Outcome must have exactly two levels.")}
  if(!(is.numeric(outcome) | is.factor(outcome))) {stop("Outcome must be a factor or numeric.")}
  else if(is.factor(outcome)) {message("Outcome is a factor. Using factor level order as the numeric coding: ", paste(levels(outcome), collapse = " < "))}
  n_missing_outcomes <- sum(is.na(outcome))
  if(n_missing_outcomes > 0) {message("Outcome contains ", n_missing_outcomes, " missing observation(s) excluded from calculation.")}
  # Exposure Check
  n_levels_exposure <- unique(exposure[!is.na(exposure)])
  if(length(n_levels_exposure) > 5) {warning("Exposure may be continuous. Please double check if the exposure is categorical or ordinal.")}
  else if(length(n_levels_exposure) < 2) {warning("Exposure contains no variation. Odds Ratio may be infinite or undefined.")}
  if(!(is.numeric(exposure) | is.factor(exposure))) {stop("Exposure must be a factor or numeric.")}
  else if (is.factor(exposure)) {message("Exposure is a factor. Using factor level order as the numeric coding: ", paste(levels(exposure), collapse = " < "))}
  n_miss_exposure <- sum(is.na(exposure))
  if(n_miss_exposure > 0) {message("Exposure contains ", n_miss_exposure, " missing observation(s) excluded from calculation.")}
  # Confounder Check
  n_missing_confounder <- sum(is.na(confounder))
  if(n_missing_confounder > 0) {message("Note: Confounder has ", n_missing_confounder, " missing value(s). NA will be treated as an unprinted stratum.")}
  n_levels_confounder <- unique(confounder[!is.na(confounder)])
  if(length(n_levels_confounder) > 5) {warning("Confounder may be continuous. Please double check the data type.")}
  else if(length(n_levels_confounder) < 2) {warning("Confounder contains no variation. Only crude Odds Ratio is presented.")}

  # CRUDE 2x2 TABLE AND ODDS RATIO (OR)
  crude_dataframe <- data.frame(exposure = exposure, outcome = outcome, confounder = confounder)
  crude_or <- oddsratio(crude_dataframe$exposure, crude_dataframe$outcome)
  crude_2x2_table <- crude_or[["data"]]
  crude_or_values <- crude_or[["measure"]]

  # STRATIFIED 2x2 TABLE AND ODDS RATIO (OR)
  dataframe_list <- split(crude_dataframe, crude_dataframe$confounder)
  stratified_results <- list()
  for(i in seq_along(dataframe_list)){
    stratified_dataframe <- dataframe_list[[i]]
    stratified_or <- oddsratio(stratified_dataframe$exposure, stratified_dataframe$outcome)
    stratified_results[[i]] <- list(table = stratified_or[["data"]], Odds_Ratio = stratified_or[["measure"]][-1, ])
  }
  names(stratified_results) <- names(dataframe_list)

  # RETURN FINAL RESULTS
  result <- list(Crude = list(table = crude_2x2_table, Odds_Ratio = crude_or_values[-1, ]), Stratified = stratified_results)
  return(result)

}

stratified_rr <- function(exposure, outcome, confounder) {

  # DATA CHECKS
  # Length Check
  if(length(unique(c(length(exposure), length(outcome), length(confounder)))) != 1) {stop("Exposure, Outcome, and Confounder must all be the same length.")}
  # Numeric Check
  # Outcome Check
  n_levels_outcomes <- unique(outcome[!is.na(outcome)])
  if(length(n_levels_outcomes) != 2) {stop("Outcome must have exactly two levels.")}
  if(!(is.numeric(outcome) | is.factor(outcome))) {stop("Outcome must be a factor or numeric.")}
  else if(is.factor(outcome)) {message("Outcome is a factor. Using factor level order as the numeric coding: ", paste(levels(outcome), collapse = " < "))}
  n_missing_outcomes <- sum(is.na(outcome))
  if(n_missing_outcomes > 0) {message("Outcome contains ", n_missing_outcomes, " missing observation(s) excluded from calculation.")}
  # Exposure Check
  n_levels_exposure <- unique(exposure[!is.na(exposure)])
  if(length(n_levels_exposure) > 5) {warning("Exposure may be continuous. Please double check if the exposure is categorical or ordinal.")}
  else if(length(n_levels_exposure) < 2) {warning("Exposure contains no variation. Risk Ratio may be infinite or undefined.")}
  if(!(is.numeric(exposure) | is.factor(exposure))) {stop("Exposure must be a factor or numeric.")}
  else if (is.factor(exposure)) {message("Exposure is a factor. Using factor level order as the numeric coding: ", paste(levels(exposure), collapse = " < "))}
  n_miss_exposure <- sum(is.na(exposure))
  if(n_miss_exposure > 0) {message("Exposure contains ", n_miss_exposure, " missing observation(s) excluded from calculation.")}
  # Confounder Check
  n_missing_confounder <- sum(is.na(confounder))
  if(n_missing_confounder > 0) {message("Note: Confounder has ", n_missing_confounder, " missing value(s). NA will be treated as an unprinted stratum.")}
  n_levels_confounder <- unique(confounder[!is.na(confounder)])
  if(length(n_levels_confounder) > 5) {warning("Confounder may be continuous. Please double check the data type.")}
  else if(length(n_levels_confounder) < 2) {warning("Confounder contains no variation. Only crude Risk Ratio is presented.")}

  # RENAME OUTPUT ROW AND COLUMN FUNCTION
  rename_sum_to_total <- function(tab) {
    rownames(tab)[rownames(tab) == "Sum"] <- "Total"
    colnames(tab)[colnames(tab) == "Sum"] <- "Total"
    tab
    }

  # CRUDE 2x2 TABLE AND RISK RATIO (RR)
  crude_dataframe <- data.frame(exposure = exposure, outcome = outcome, confounder = confounder)
  crude_2x2_table <- table(crude_dataframe$exposure, crude_dataframe$outcome)
  crude_rr <- riskratio(crude_2x2_table)
  crude_2x2_table_total <- addmargins(crude_2x2_table)
  crude_2x2_table_total <- rename_sum_to_total(crude_2x2_table_total)
  crude_rr_values <- crude_rr[["measure"]][-1, , drop = FALSE]

  # STRATIFIED 2x2 TABLE AND RISK RATIO (RR)
  dataframe_list <- split(crude_dataframe, crude_dataframe$confounder)
  stratified_results <- list()
  for(i in seq_along(dataframe_list)){
    stratified_dataframe <- dataframe_list[[i]]
    stratified_table <- table(stratified_dataframe$exposure, stratified_dataframe$outcome)
    stratified_rr <- riskratio(stratified_table)
    stratified_table_total <- addmargins(stratified_table)
    stratified_table_total <- rename_sum_to_total(stratified_table_total)
    stratified_results[[i]] <- list(table = stratified_table_total, Risk_Ratio = stratified_rr[["measure"]][-1, , drop = FALSE])
  }
  names(stratified_results) <- names(dataframe_list)

  # RETURN FINAL RESULTS
  result <- list(Crude = list(table = crude_2x2_table_total, Risk_Ratio = crude_rr_values), Stratified = stratified_results)
  return(result)

}
