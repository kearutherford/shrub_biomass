
################################################################################
################################################################################
# Top-level function
################################################################################
################################################################################

#' @title CompileShrubs
#'
#' @description
#' Compiles shrub data beyond the plot level. Specifically designed to further summarize outputs from the ShrubBiomass function. Recognizes simple random sampling and stratified random sampling designs. Also recognizes the design of the Fire and Fire Surrogate. See \href{https://github.com/kearutherford/BerkeleyForestsAnalytics/tree/main}{README} for details.
#'
#' @param shrub_data A dataframe or tibble. Shrub biomass and cover must already be calculated at the plot-level using the ShrubBiomass function. Required columns depend on the sampling design:
#' \itemize{
#' \item Simple random sampling: must have time, site, plot, total_ag_Mg_ha, cover_perc, and sc_tran_length.
#' \item Stratified random sampling: must have time, site, stratum, plot, total_ag_Mg_ha, cover_perc, and sc_tran_length.
#' \item Fire and Fire Surrogate: must have time, trt_type, site, plot, total_ag_Mg_ha, cover_perc, and sc_tran_length.
#' }
#' @param design Specifies the sampling design. Must be set to "SRS" (simple random sample), "STRS" (stratified ransom sample), or "FFS" (Fire and Fire Surrogate). There is no default.
#' @param wt_data Only required for stratified random sampling designs. A dataframe or tibble with the following columns: time (optional), site, stratum, and wh (stratum weight). The default is set to "not_needed", and should be left as such for design = "SRS" or design = "FFS".
#' @param fpc_data  An optional dataframe or tibble. Incorporates the finite population correction factor (FPC) when samples were taken without replacement. The default is set to "not_needed". Required columns depend on the sampling design:
#' \itemize{
#' \item Simple random sampling: must have site, N, and n columns. A time column is optional.
#' \item Stratified random sampling: must have site, stratum, N, and n columns. A time column is optional.
#' \item Fire and Fire Surrogate: must have trt_type, site, N and n columns. A time column in optional.
#' }
#'
#' @return Depends on the sampling design:
#' \itemize{
#' \item Simple random sampling: a dataframe with site-level summaries.
#' \item Stratified random sampling: a list with two components: (1) a dataframe with stratum-level summaries and (2) a dataframe with site-level summaries.
#' \item Fire and Fire Surrogate: a list with two components: (1) a dataframe with site-level (i.e., compartment-level) summaries and (2) a dataframe with treatment-level summaries.
#' }
#'
#' @export

CompileShrubs <- function(shrub_data, design, wt_data = "not_needed", fpc_data = "not_needed") {

  # coerce tibble inputs into data.frame
  shrub_data <- as.data.frame(shrub_data)

  if(all(wt_data != "not_needed", na.rm = TRUE)) {
    wt_data <- as.data.frame(wt_data)
  }

  if(all(fpc_data != "not_needed", na.rm = TRUE)) {
    fpc_data <- as.data.frame(fpc_data)
  }

  # check and prep input data
  ValidateShrubData(shrub_data_check = shrub_data,
                    design_check = design,
                    wt_data_check = wt_data,
                    fpc_data_check = fpc_data)

  # get finite population correction factor data prepped
  # function defined in compilation_general.R
  if(all(fpc_data != "not_needed")) {
    fpc_step1 <- FPC(fpc_data, design)
  } else {
    fpc_step1 <- fpc_data
  }

  # summarize data
  if(design == "SRS") {

    step1 <- SRS_CalcsSh(data = shrub_data,
                         fpc = fpc_step1)

  } else if (design == "STRS") {

    step1 <- STRS_CalcsSh(data = shrub_data,
                          wh_data = wt_data,
                          fpc = fpc_step1)

  } else if (design == "FFS") {

    step1 <- FFS_CalcsSh(data = shrub_data,
                         fpc = fpc_step1)

  }

  return(step1)

}


################################################################################
################################################################################
# Data validation functions
################################################################################
################################################################################

################################################################
# overarching data validation function
################################################################

ValidateShrubData <- function(shrub_data_check, design_check, wt_data_check, fpc_data_check) {

  # check that options are set appropriately
  # function defined in compilation_general.R
  ValidateOptions(design_val = design_check, wt_data_val = wt_data_check)

  # check that all necessary columns are present and formatted as expected
  ValidateShrubColumns(shrub_data_val = shrub_data_check, design_val = design_check)

  # check that each observation is a unique plot
  # functions defined in compilation_general.R
  ValidateObs(data_val = shrub_data_check, design_val = design_check, data_name = "shrub_data")

  # check weights dataframe
  # function defined in compilation_general.R
  if(design_check == "STRS") {
    ValidateWeights(data_val = shrub_data_check, wt_data_val = wt_data_check, data_name = "shrub_data")
  }

  # check fpc dataframe
  # function defined in compilation_general.R
  if(all(fpc_data_check != "not_needed", na.rm = TRUE)) {
    ValidateFPC(data_val = shrub_data_check, fpc_data_val = fpc_data_check, design_val = design_check, data_name = "shrub_data")
  }

}

###################################################################
# function to check that all columns are in the provided dataframe
# and are formatted as expected
###################################################################

ValidateShrubColumns <- function(shrub_data_val, design_val) {

  # check that necessary columns exist
  if(design_val == "SRS") {

    necessary_columns = c("time", "site", "plot",
                          "total_ag_Mg_ha", "cover_perc", "sc_tran_length")

    if(!all(is.element(necessary_columns, names(shrub_data_val)))) {

      stop('shrub_data is missing necessary columns! For a SRS design, shrub_data must include:\n',
           'time, site, plot, total_ag_Mg_ha, cover_perc, sc_tran_length')

    }

  } else if (design_val == "STRS") {

    necessary_columns = c("time", "site", "stratum", "plot",
                          "total_ag_Mg_ha", "cover_perc", "sc_tran_length")

    if(!all(is.element(necessary_columns, names(shrub_data_val)))) {

      stop('shrub_data is missing necessary columns! For a STRS design, shrub_data must include:\n',
           'time, site, stratum, plot, total_ag_Mg_ha, cover_perc, sc_tran_length')

    }

  } else if (design_val == "FFS") {

    necessary_columns = c("time", "trt_type", "site", "plot",
                          "total_ag_Mg_ha", "cover_perc", "sc_tran_length")

    if(!all(is.element(necessary_columns, names(shrub_data_val)))) {

      stop('shrub_data is missing necessary columns! For a FFS design, shrub_data must include:\n',
           'time, trt_type, site, plot, total_ag_Mg_ha, cover_perc, sc_tran_length')

    }

  }

  # Check for missing values
  if('TRUE' %in% is.na(shrub_data_val$time)) {
    stop('For shrub_data, there are missing values in the time column.')
  }

  if('TRUE' %in% is.na(shrub_data_val$site)) {
    stop('For shrub_data, there are missing values in the site column.')
  }

  if('TRUE' %in% is.na(shrub_data_val$plot)) {
    stop('For shrub_data, there are missing values in the plot column.')
  }

  if(design_val == "STRS" && 'TRUE' %in% is.na(shrub_data_val$stratum)) {
    stop('For shrub_data, there are missing values in the stratum column.')
  }

  if(design_val == "FFS" && 'TRUE' %in% is.na(shrub_data_val$trt_type)) {
    stop('For shrub_data, there are missing values in the trt_type column.')
  }

  if('TRUE' %in% is.na(shrub_data_val$total_ag_Mg_ha)) {
    stop('For shrub_data, there are missing values in the total_ag_Mg_ha column.')
  }

  if('TRUE' %in% is.na(shrub_data_val$cover_perc)) {
    stop('For shrub_data, there are missing values in the cover_perc column.')
  }

  if('TRUE' %in% is.na(shrub_data_val$sc_tran_length)) {
    stop('For shrub_data, there are missing values in the sc_tran_length column.')
  }

  # check column classes
  if(!is.character(shrub_data_val$time)) {
    stop('For shrub_data, time must be a character variable.\n',
         'The time column is currently class: ', class(shrub_data_val$time))
  }

  if(!is.character(shrub_data_val$site)) {
    stop('For shrub_data, site must be a character variable.\n',
         'The site column is currently class: ', class(shrub_data_val$site))
  }

  if(!is.character(shrub_data_val$plot)) {
    stop('For shrub_data, plot must be a character variable.\n',
         'The plot column is currently class: ', class(shrub_data_val$plot))
  }

  if(design_val == "STRS" && !is.character(shrub_data_val$stratum)) {
    stop('For shrub_data, stratum must be a character variable.\n',
         'The stratum column is currently class: ', class(shrub_data_val$stratum))
  }

  if(design_val == "FFS" && !is.character(shrub_data_val$trt_type)) {
    stop('For shrub_data, trt_type must be a character variable.\n',
         'The trt_type column is currently class: ', class(shrub_data_val$trt_type))
  }

  if(!is.numeric(shrub_data_val$total_ag_Mg_ha)) {
    stop('For shrub_data, total_ag_Mg_ha must be a numeric variable.\n',
         'The total_ag_Mg_ha column is currently class: ', class(shrub_data_val$total_ag_Mg_ha))
  }

  if(!is.numeric(shrub_data_val$cover_perc)) {
    stop('For shrub_data, cover_perc must be a numeric variable.\n',
         'The cover_perc column is currently class: ', class(shrub_data_val$cover_perc))
  }

  if(!is.numeric(shrub_data_val$sc_tran_length)) {
    stop('For shrub_data, sc_tran_length must be a numeric variable.\n',
         'The sc_tran_length column is currently class: ', class(shrub_data_val$sc_tran_length))
  }

}


################################################################################
################################################################################
# Summary functions
################################################################################
################################################################################

################################################################
# function for simple random sampling
################################################################

SRS_CalcsSh <- function(data, fpc) {

  # create column to reduce looping
  data$ts <- paste0(data$time,'_',data$site)

  # create an empty dataframe to fill
  compiled_df <- data.frame(matrix(nrow = 0, ncol = 6))
  colnames(compiled_df) <- c("time", "site", "avg_ag_Mg_ha", "se_ag_Mg_ha", "avg_cover_perc", "se_cover_perc")

  # loop through each time:site
  sites <- unique(data$ts)

  for(m in sites) {

    all_plots <- subset(data, ts == m)

    compiled_df[nrow(compiled_df) + 1, ] <- NA
    l <- nrow(compiled_df)

    compiled_df$time[l] <- all_plots$time[1]
    compiled_df$site[l] <- all_plots$site[1]

    # function defined in compilation_surface_fuels.R
    ag <- WeightedValues(all_plots, "total_ag_Mg_ha", "sc_tran_length", fpc, "SRS")
    pc <- WeightedValues(all_plots, "cover_perc", "sc_tran_length", fpc, "SRS")

    compiled_df$avg_ag_Mg_ha[l] <- ag[1]
    compiled_df$se_ag_Mg_ha[l] <- ag[2]
    compiled_df$avg_cover_perc[l] <- pc[1]
    compiled_df$se_cover_perc[l] <- pc[2]

  }

  return(compiled_df)

}

################################################################
# function for stratified random sampling
################################################################

STRS_CalcsSh <- function(data, wh_data, fpc) {

  # create columns to reduce looping
  data$tss <- paste0(data$time,'_',data$site,'_',data$stratum)
  data$ts <- paste0(data$time,'_',data$site)

  # stratum values --------------------------------------------
  # create an empty dataframe to fill
  str_df <- data.frame(matrix(nrow = 0, ncol = 8))
  colnames(str_df) <- c("ts", "time", "site", "stratum", "avg_ag_Mg_ha", "se_ag_Mg_ha", "avg_cover_perc", "se_cover_perc")

  # loop through each time:site:stratum
  strats <- unique(data$tss)

  for(k in strats) {

    all_plots <- subset(data, tss == k)

    str_df[nrow(str_df) + 1, ] <- NA
    j <- nrow(str_df)

    str_df$ts[j] <- all_plots$ts[1]
    str_df$time[j] <- all_plots$time[1]
    str_df$site[j] <- all_plots$site[1]
    str_df$stratum[j] <- all_plots$stratum[1]

    # function defined in compilation_surface_fuels.R
    ag <- WeightedValues(all_plots, "total_ag_Mg_ha", "sc_tran_length", fpc, "STRS")
    pc <- WeightedValues(all_plots, "cover_perc", "sc_tran_length", fpc, "STRS")

    str_df$avg_ag_Mg_ha[j] <- ag[1]
    str_df$se_ag_Mg_ha[j] <- ag[2]
    str_df$avg_cover_perc[j] <- pc[1]
    str_df$se_cover_perc[j] <- pc[2]

  }

  # add column with stratum wights
  # function defined in compilation_general.R
  str_df_wh <- StratumWeights(str_df, wh_data)

  # overall values ----------------------------------------
  # create an empty dataframe to fill
  overall_df <- data.frame(matrix(nrow = 0, ncol = 6))
  colnames(overall_df) <- c("time", "site", "avg_ag_Mg_ha", "se_ag_Mg_ha", "avg_cover_perc", "se_cover_perc")

  # loop through each time:site
  sites <- unique(str_df_wh$ts)

  for(m in sites) {

    all_strats <- subset(str_df_wh, ts == m)

    overall_df[nrow(overall_df) + 1, ] <- NA
    l <- nrow(overall_df)

    overall_df$time[l] <- all_strats$time[1]
    overall_df$site[l] <- all_strats$site[1]

    # function defined in compilation_general.R
    ag_st <- OverallValues(all_strats, "avg_ag_Mg_ha", "se_ag_Mg_ha")
    pc_st <- OverallValues(all_strats, "avg_cover_perc", "se_cover_perc")

    overall_df$avg_ag_Mg_ha[l] <- ag_st[1]
    overall_df$se_ag_Mg_ha[l] <- ag_st[2]
    overall_df$avg_cover_perc[l] <- pc_st[1]
    overall_df$se_cover_perc[l] <- pc_st[2]

  }

  # create output dataframe with correct columns
  str_return_df <- subset(str_df, select = -c(ts))

  # create return list & name list items
  return_list <- list(str_return_df, overall_df)
  names(return_list) <- c("stratum", "site")
  return(return_list)

}

################################################################
# function for FFS
################################################################

FFS_CalcsSh <- function(data, fpc) {

  # create columns to reduce looping
  data$tts <- paste0(data$time,'_',data$trt_type,'_',data$site)
  data$tt <- paste0(data$time,'_',data$trt_type)

  # compartment values --------------------------------------------
  # create an empty dataframe to fill
  comp_df <- data.frame(matrix(nrow = 0, ncol = 8))
  colnames(comp_df) <- c("tt", "time", "trt_type", "site", "avg_ag_Mg_ha", "se_ag_Mg_ha", "avg_cover_perc", "se_cover_perc")

  # loop through each time:treatment:compartment
  comps <- unique(data$tts)

  for(k in comps) {

    all_plots <- subset(data, tts == k)

    comp_df[nrow(comp_df) + 1, ] <- NA
    j <- nrow(comp_df)

    comp_df$tt[j] <- all_plots$tt[1]
    comp_df$time[j] <- all_plots$time[1]
    comp_df$trt_type[j] <- all_plots$trt_type[1]
    comp_df$site[j] <- all_plots$site[1]

    # function defined in compilation_surface_fuels.R
    ag <- WeightedValues(all_plots, "total_ag_Mg_ha", "sc_tran_length", fpc, "FFS")
    pc <- WeightedValues(all_plots, "cover_perc", "sc_tran_length", fpc, "FFS")

    comp_df$avg_ag_Mg_ha[j] <- ag[1]
    comp_df$se_ag_Mg_ha[j] <- ag[2]
    comp_df$avg_cover_perc[j] <- pc[1]
    comp_df$se_cover_perc[j] <- pc[2]

  }

  # treatment type values -------------------------------------
  # create an empty dataframe to fill
  type_df <- data.frame(matrix(nrow = 0, ncol = 6))
  colnames(type_df) <- c("time", "trt_type", "avg_ag_Mg_ha", "se_ag_Mg_ha", "avg_cover_perc", "se_cover_perc")

  # loop through each time:treatment
  treats <- unique(comp_df$tt)

  for(m in treats) {

    all_comps <- subset(comp_df, tt == m)

    type_df[nrow(type_df) + 1, ] <- NA
    l <- nrow(type_df)

    type_df$time[l] <- all_comps$time[1]
    type_df$trt_type[l] <- all_comps$trt_type[1]

    # function defined in compilation_general.R
    one_cp <- StratumValues(all_comps, "avg_ag_Mg_ha", "not_needed")
    ten_cp <- StratumValues(all_comps, "avg_cover_perc", "not_needed")

    type_df$avg_ag_Mg_ha[l] <- one_cp[1]
    type_df$se_ag_Mg_ha[l] <- one_cp[2]
    type_df$avg_cover_perc[l] <- ten_cp[1]
    type_df$se_cover_perc[l] <- ten_cp[2]

  }

  # create output dataframe with correct columns
  comp_return_df <- subset(comp_df, select = -c(tt))

  # create return list & name list items
  return_list <- list(comp_return_df, type_df)
  names(return_list) <- c("site", "trt_type")
  return(return_list)

}
