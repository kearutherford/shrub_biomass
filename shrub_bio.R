
shrub_data <- data.frame(
  site = c("SEKI","SEKI","SEKI","SEKI","SEKI","SEKI","SEKI","SEKI","YOMI","YOMI","YOMI","YOMI","YOMI"),
  plot = c("1","1","1","1","2","2","2","2","1","1","1","1","2"),
  transect = c("1","1","2","2","1","1","2","2","1","1","2","2","1"),
  tran_length = c(20,20,20,20,20,20,20,20,20,20,20,20,20),
  species = c("AMAL","AMAL","CEIN","QUSP","AMAL","ARVI","COCO","QUSP","ARVI","ARVI","NODE","NODE","NONE"),
  ht = c(1,0.5,1.5,2,1,0.5,1.5,2,1,0.5,1.5,2,NA),
  crown_axis_1 = c(0.5,1,0.8,0.2,0.5,1,0.8,0.2,0.5,1,0.8,0.2,NA),
  crown_axis_2 = c(1,1.5,0.9,0.5,1,1.5,0.9,0.5,1,1.5,0.9,0.5,NA)
)

################################################################################
################################################################################
# Top-level function
################################################################################
################################################################################

#' @title ShrubBiomass
#'
#' @description
#' Work on....
#'
#' @param data A dataframe or tibble with the following columns: site, plot, transect, tran_length, species, ht, crown_axis_1, and crown_axis_2. Each row must be an observation of an individual shrub.
#' @param results Not a variable (column) in the provided dataframe or tibble. Specifies whether the results will be summarized by shrub or by plot. Must be set to either "by_shrub" or "by_plot". The default is set to "by_plot".
#'
#' @return Depends on the results setting:
#' \itemize{
  #' \item by_shrub:
  #' \item by_plot:
  #' }
#'
#' @examples
#' Work on...
#'
#' @export

ShrubBiomass <- function(data, results = "by_plot") {

  step0 <- as.data.frame(data)

  # check input data
  ValidateShrubs(data = step0, results_val = results)

  # calculate shrub-level biomass
  step1 <- PredictShrubBio(step0)

  if(results == "by_shrub") {

    return(step1)

  } else if (results == "by_plot") {

    # calculate plot-level shrub biomass (on a per area basis)
    step2 <- SumShrubs(step1)
    return(step2)

  }

}


################################################################################
################################################################################
# ValidateShrubs function
################################################################################
################################################################################

ValidateShrubs <- function(data, results_val) {

  ###########################################################
  # check that results options are set appropriately
  ###########################################################

  if(results_val == "by_shrub" || results_val == "by_plot") {
    # do nothing
  } else {
    stop('The "results" parameter must be set to either "by_shrub" or "by_plot".')
  }

  ###########################################################
  # Check that all columns are in the provided dataframe
  ###########################################################

  if(!("site" %in% colnames(data))) {
    stop('Input data is missing the necessary "site" column.')
  }

  if(!("plot" %in% colnames(data))) {
    stop('Input data is missing the necessary "plot" column.')
  }

  if(!("transect" %in% colnames(data))) {
    stop('Input data is missing the necessary "transect" column.')
  }

  if(!("tran_length" %in% colnames(data))) {
    stop('Input data is missing the necessary "tran_length" column.')
  }

  if(!("species" %in% colnames(data))) {
    stop('Input data is missing the necessary "species" column.')
  }

  if(!("ht" %in% colnames(data))) {
    stop('Input data is missing the necessary "ht" column.')
  }

  if(!("crown_axis_1" %in% colnames(data))) {
    stop('Input data is missing the necessary "crown_axis_1" column.')
  }

  if(!("crown_axis_2" %in% colnames(data))) {
    stop('Input data is missing the necessary "crown_axis_2" column.')
  }

  ###########################################################
  # Check that column classes are as expected
  ###########################################################

  # Categorical variables ------------------------------------------------------
  if(!is.character(data$site)) {
    stop('"site" must be a character variable.\n',
         'You have input a variable of class: ', class(data$site))
  }

  if(!is.character(data$plot)) {
    stop('"plot" must be a character variable.\n',
         'You have input a variable of class: ', class(data$plot))
  }

  if(!is.character(data$transect)) {
    stop('"transect" must be a character variable.\n',
         'You have input a variable of class: ', class(data$transect))
  }

  if(!is.character(data$species)) {
    stop('"species" must be a character variable.\n',
         'You have input a variable of class: ', class(data$species))
  }

  # Numerical variables --------------------------------------------------------
  if(!is.numeric(data$tran_length)) {
    stop('"tran_length" must be a numerical variable.\n',
         'You have input a variable of class: ', class(data$tran_length))
  }

  if(!is.numeric(data$ht)) {
    stop('"ht" must be a numerical variable.\n',
         'You have input a variable of class: ', class(data$ht))
  }

  if(!is.numeric(data$crown_axis_1)) {
    stop('"crown_axis_1" must be a numerical variable.\n',
         'You have input a variable of class: ', class(data$crown_axis_1))
  }

  if(!is.numeric(data$crown_axis_2)) {
    stop('"crown_axis_2" must be a numerical variable.\n',
         'You have input a variable of class: ', class(data$crown_axis_2))
  }

  #########################################################
  # Check that site, plot, and transect are as expected
  #########################################################

  if ('TRUE' %in% is.na(data$site)) {
    stop('There are missing values in the site column in the provided dataframe.')
  }

  if ('TRUE' %in% is.na(data$plot)) {
    stop('There are missing values in the plot column in the provided dataframe.')
  }

  if ('TRUE' %in% is.na(data$transect)) {
    stop('There are missing values in the transect column in the provided dataframe.')
  }

  ###########################################################
  # Check transect length
  ###########################################################

  # Check for NA ---------------------------------------------------------------
  if ('TRUE' %in% is.na(data$tran_length)) {
    stop('There are missing transect lengths in the provided dataframe.')
  }

  # Check for negative values ---------------------------------------------------
  if (min(data$tran_length, na.rm = TRUE) <= 0) {
    stop('There are transect lengths <= 0 in the provided dataframe. All tran_length values must be > 0.')
  }

  # Check for matching values --------------------------------------------------
  data$site_plot_tran <- paste(data$site, data$plot, data$transect, sep = "_")
  unq_ids <- unique(data$site_plot_tran)

  tl_check_vec <- c()

  for(u in unq_ids) {

    all_shrubs <- subset(data, site_plot_tran == u)

    if(length(unique(all_shrubs$tran_length)) != 1) {
      tl_check_vec <- c(tl_check_vec, u)
    }

  }

  if(length(tl_check_vec) > 0) {

    tl_check <- paste0(tl_check_vec, sep = "   ")

    stop('Each site:plot:transect should have the same transect length recorded.\n',
         'The following site:plot:transect combinations have multiple tran_length values: ', tl_check)

  }

  ###########################################################
  # Check that species codes are as expected
  ###########################################################

  # Check for unrecognized species codes ---------------------------------------
  sp_code_names <- c("AMAL", "ARPA", "ARVI", "CESP", "CECO", "CEIN", "CEVE", "CHSP", "CHSE",
                     "COSP", "COCO", "LEDA", "NODE", "QUSP", "RISP", "OTHER", "NONE")

  if(!all(is.element(data$species, sp_code_names))) {

    unrecognized_sp <- sort(paste0(unique(data[!is.element(data$species, sp_code_names), "species"]), sep = " "))

    stop('Not all species codes were recognized!\n',
         'Unrecognized codes: ', unrecognized_sp)

  }

  # Check for NA ---------------------------------------------------------------
  if ('TRUE' %in% is.na(data$species)) {
    stop('There are missing species codes in the provided dataframe. Consider using OTHER or NONE for these shrubs.')
  }

  # Check for proper use of NONE species ---------------------------------------
  for(u in unq_ids) {

    all_shrubs <- subset(data, site_plot_tran == u)

    if('TRUE' %in% is.element(all_shrubs$species, "NONE")) {

      n <- nrow(all_shrubs)

      if(n > 1) {

        stop('There are transects with a recorded species code of NONE, but with more than one row.\n',
             'Transects with no shrubs should be represented by a single row with site, plot, transect, and tran_length filled in as appropriate and a species code of NONE.')

      }

    }

  }

  trans_wo_shrubs <- subset(data, species == "NONE",
                            select = c(ht, crown_axis_1, crown_axis_2))

  if('FALSE' %in% is.na(trans_wo_shrubs)) {

    warning('There are transects with a recorded species code of NONE, but with non-NA ht, crown_axis_1 and/or crown_axis_2.\n',
            'Transects with no shrubs should be represented by a single row with site, plot, transect, and tran_length filled in as appropriate,\n',
            'a species code of NONE, and NA ht, crown_axis_1, and/or crown_axis_2. Consider investigating these entries.')

  }

  ###########################################################
  # Check height
  ###########################################################

  trans_w_shrubs <- subset(data, species != "NONE")

  # Check for NA ---------------------------------------------------------------
  if ('TRUE' %in% is.na(trans_w_shrubs$ht)) {
    stop('There are missing shrub heights in the provided dataframe.')
  }

  # Check for negative values ---------------------------------------------------
  if (min(trans_w_shrubs$ht) <= 0) {
    stop('There are shrub heights <= 0 in the provided dataframe. All heights must be > 0.')
  }

  ###########################################################
  # Check crown axis
  ###########################################################

  # Check for NA ---------------------------------------------------------------
  if ('TRUE' %in% is.na(trans_w_shrubs$crown_axis_1)) {
    stop('There are missing crown axis 1 values in the provided dataframe.')
  }

  if ('TRUE' %in% is.na(trans_w_shrubs$crown_axis_2)) {
    stop('There are missing crown axis 2 values in the provided dataframe.')
  }

  # Check for negative values ---------------------------------------------------
  if (min(trans_w_shrubs$crown_axis_1) <= 0) {
    stop('There are crown axis 1 values <= 0 in the provided dataframe. All values must be > 0.')
  }

  if (min(trans_w_shrubs$crown_axis_2) <= 0) {
    stop('There are crown axis 2 values <= 0 in the provided dataframe. All values must be > 0.')
  }

}


################################################################################
################################################################################
# PredictShrubBio function
################################################################################
################################################################################

PredictShrubBio <- function(data) {

  # calculate crown area (using equation for an ellipse)
  data$ca_m2 <- pi*data$crown_axis_1*data$crown_axis_2

  shrub_coefs <- read.csv("agl_coefs.csv") # remove once data has been internalized!!!!!
  data2 <- merge(data, shrub_coefs, by = "species", all.x = TRUE, all.y = FALSE)

  # estimate biomass (in g)
  data2$total_ag_g <- ifelse(data2$eqn == "M1", (data2$b1*(data2$ca_m2^data2$b2)*(data2$ht^data2$b3)),
                             ifelse(data2$eqn == "M2", (data2$b1*(data2$ca_m2^data2$b2)), 0))

  # convert biomass from g to kg
  data2$total_ag_kg <- data2$total_ag_g/1000

  return_df <- subset(data2, select = -c(eqn,b1,b2,b3,ca_m2,total_ag_g))
  return(return_df)

}


################################################################################
################################################################################
# SumShrubs function
################################################################################
################################################################################

SumShrubs <- function(data) {

  # calculate average crown width
  data$avg_cw_m <- (data$crown_axis_1 + data$crown_axis_2)/2

  # discount biomass based on size of shrub
  data$dis_bio <- ifelse(data$species == "NONE", 0, data$total_ag_kg/data$avg_cw_m)

  # create empty dataframe to fill
  fill_df <- data.frame(matrix(nrow = 0, ncol = 3))
  colnames(fill_df) <- c("site", "plot", "total_ag_Mg_ha")

  # loop through each site/plot
  data$site_plot <- paste(data$site, data$plot, sep = "_")
  unq_ids <- unique(data$site_plot)

  for(u in unq_ids) {

    all_shrubs <- subset(data, site_plot == u)
    unq_trans <- unique(all_shrubs$transect)
    L <- 0

    for(t in unq_trans) {

      tran_t <- subset(all_shrubs, transect == t)
      L <- L + tran_t$tran_length[1]

    }

      fill_df[nrow(fill_df) + 1, ] <- NA
      n <- nrow(fill_df)

      fill_df$site[n] <- all_shrubs$site[1]
      fill_df$plot[n] <- all_shrubs$plot[1]
      # Note: also unit converting here from kg/m2 to Mg/ha
      fill_df$total_ag_Mg_ha[n] <- (10/L)*sum(all_shrubs$dis_bio)

  }

  return_df <- fill_df[order(fill_df$site, fill_df$plot), ]
  return(return_df)

}
