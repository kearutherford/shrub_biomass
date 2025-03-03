
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
  # ValidateShrubs(data = step0)

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

ValidateShrubs <- function(data) {






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
