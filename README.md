
# Shrub biomass estimates

The `ShrubBiomass` function uses allometric equations to estimate above-ground shrub biomass. See "Background information for shrub biomass estimations" below for further details.

## :eight_spoked_asterisk: `ShrubBiomass( )`

### Inputs

1. `shrub_data` A dataframe or tibble. Each row must be an observation of an individual transect at a specific site/plot. Must have at least these columns (column names exact):

    * **site:** Describes the broader location or forest where the data were collected. The class of this variable must be character.
    * **plot:** Identifies the plot in which the individual shrub transect was measured. The class of this variable must be character.
    * **transect:** Identifies the transect on which the specific shrub data were collected. The transect ID Will often be an azimuth from plot center. The class of this variable must be character.
    * **tran_length:** The length of the sampling transect in meters. The class of this variable must be numeric. 
    * **species:** Specifies the species of the individual shrub. Must follow our four-letter species code conventions (see "Species code table" in "Background information for shrub biomass estimations"). The class of this variable must be character.
    * **ht:** Provides the height of the individual shrub in meters. The class of this variable must be numeric. 
    * **crown_axis_1:** Provides the major diameter of the individual shrub in meters (see "Data collection requirements" in "Background information for shrub biomass estimations"). The class of this variable must be numeric. 
    * **crown_axis_2:** Provides the minor diameter of the individual shrub in meters (see "Data collection requirements" in "Background information for shrub biomass estimations"). The class of this variable must be numeric. 
    * **slope:** The slope of the transect in percent (not the slope of the plot). If transect slope was not taken, set the slope to be 0 (which assumes no slope). 

2. `results` Not a variable (column) in the provided dataframe or tibble. Specifies whether the results will be summarized by shrub or by plot. Must be set to either "by_shrub" or "by_plot". The default is set to "by_plot".

*Note: there must be a one-to-one match between time:site:plot identities of tree and fuel data.*

### Outputs

Depends on the results setting: 

* by_shrub: The original dataframe, with a new column:

    * `total_ag_kg`: total above-ground live shrub biomass in kilograms. Includes wood, bark, and leaves. Excludes flowers, seeds, litter, and dead wood. 

* by_plot: A dataframe with plot-level values, with the following columns:
    
    * `site`: as described above
    * `plot`: as described above
    * `total_ag_Mg_ha`: total above-ground live shrub biomass in megagrams per hectare. Includes wood, bark, and leaves. Excludes flowers, seeds, litter, and dead wood.
    * `cover_perc`: total shrub cover in percent
    * `sc_tran_length`: slope-corrected transect length (i.e., horizontal transect length) in meters. This is the total horizontal length of transect sampled for shrubs at the specific site:plot.

### Demonstrations

shrub_demo_data <- data.frame(
  site = c("SEKI","SEKI","SEKI","SEKI","SEKI","SEKI","SEKI","SEKI","YOMI","YOMI","YOMI","YOMI","YOMI","YOMI"),
  plot = c("1","1","1","1","2","2","2","2","1","1","1","1","2","2"),
  transect = c("1","1","2","2","1","1","2","2","1","1","2","2","1","2"),
  tran_length = c(20,20,20,20,20,20,20,20,20,20,20,20,20,20),
  species = c("AMAL","AMAL","CEIN","QUSP","AMAL","ARVI","COCO","QUSP","ARVI","ARVI","NODE","NODE","NONE","NONE"),
  ht = c(0.5,0.3,1.5,1.5,1,1.5,1.5,2,1,0.5,1.5,2,NA,NA),
  crown_axis_1 = c(0.5,1,1.5,0.8,1,1.5,0.9,1.5,1,0.8,0.9,1.5,NA,NA),
  crown_axis_2 = c(0.3,0.5,0.8,0.7,0.5,1,0.8,0.8,0.5,0.6,0.8,1.2,NA,NA),
  slope = c(5,5,10,10,0,0,20,20,15,15,5,5,10,10)
)

```{r}
# investigate input dataframe
shrub_demo_data
```

*Notice that site = YOMI, plot = 2 is a plot without shrubs. For all plot-level summaries below, this plot without shrubs will have 0 biomass and cover estimates.*

<br>

**Results by shrub:**

```{r}
# call the ShrubBiomass() function in the BerkeleyForestsAnalytics package
shrub_demo1 <- ShrubBiomass(shrub_data = shrub_demo_data,
                            results = "by_shrub")

shrub_demo1
```
<br>

**Results summarized by plot:**

```{r}
# call the ShrubBiomass() function in the BerkeleyForestsAnalytics package
# keep default results (= "by_plot")
shrub_demo2 <- ShrubBiomass(shrub_data = shrub_demo_data)

shrub_demo2
```



# Further data summarization   

The three functions (`CompilePlots`, `CompileSurfaceFuels`, and `CompileShrubs`) summarize data beyond the plot level. These functions are specifically designed to further summarize the outputs from other `BerkeleyForestsAnalytics` functions. The functions recognize simple random sampling and stratified random sampling designs. They also recognize the design of the Fire and Fire Surrogate study. See "Background information for further data summarization" below for further details. 

## :eight_spoked_asterisk: `CompileShrubs( )`

### Inputs

1. `shrub_data` A dataframe or tibble. Each row must be an observation of an individual transect at a specific site/plot. Must have at least these columns (column names exact):

    * **site:** Describes the broader location or forest where the data were collected. The class of this variable must be character.
    * **plot:** Identifies the plot in which the individual shrub transect was measured. The class of this variable must be character.
    * **transect:** Identifies the transect on which the specific shrub data were collected. The transect ID Will often be an azimuth from plot center. The class of this variable must be character.
    * **tran_length:** The length of the sampling transect in meters. The class of this variable must be numeric. 
    * **species:** Specifies the species of the individual shrub. Must follow our four-letter species code conventions (see "Species code table" in "Background information for shrub biomass estimations"). The class of this variable must be character.
    * **ht:** Provides the height of the individual shrub in meters. The class of this variable must be numeric. 
    * **crown_axis_1:** Provides the major diameter of the individual shrub in meters (see "Data collection requirements" in "Background information for shrub biomass estimations"). The class of this variable must be numeric. 
    * **crown_axis_2:** Provides the minor diameter of the individual shrub in meters (see "Data collection requirements" in "Background information for shrub biomass estimations"). The class of this variable must be numeric. 
    * **slope:** The slope of the transect in percent (not the slope of the plot). If transect slope was not taken, set the slope to be 0 (which assumes no slope). 

2. `results` Not a variable (column) in the provided dataframe or tibble. Specifies whether the results will be summarized by shrub or by plot. Must be set to either "by_shrub" or "by_plot". The default is set to "by_plot".

*Note: there must be a one-to-one match between time:site:plot identities of tree and fuel data.*

### Outputs

Depends on the results setting: 

* by_shrub: The original dataframe, with a new column:

    * `total_ag_kg`: total above-ground live shrub biomass in kilograms. Includes wood, bark, and leaves. Excludes flowers, seeds, litter, and dead wood. 

* by_plot: A dataframe with plot-level values, with the following columns:
    
    * `site`: as described above
    * `plot`: as described above
    * `total_ag_Mg_ha`: total above-ground live shrub biomass in megagrams per hectare. Includes wood, bark, and leaves. Excludes flowers, seeds, litter, and dead wood.
    * `cover_perc`: total shrub cover in percent
    * `sc_tran_length`: slope-corrected transect length (i.e., horizontal transect length) in meters. This is the total horizontal length of transect sampled for shrubs at the specific site:plot.

### Demonstrations

shrub_demo_data <- data.frame(
  site = c("SEKI","SEKI","SEKI","SEKI","SEKI","SEKI","SEKI","SEKI","YOMI","YOMI","YOMI","YOMI","YOMI","YOMI"),
  plot = c("1","1","1","1","2","2","2","2","1","1","1","1","2","2"),
  transect = c("1","1","2","2","1","1","2","2","1","1","2","2","1","2"),
  tran_length = c(20,20,20,20,20,20,20,20,20,20,20,20,20,20),
  species = c("AMAL","AMAL","CEIN","QUSP","AMAL","ARVI","COCO","QUSP","ARVI","ARVI","NODE","NODE","NONE","NONE"),
  ht = c(0.5,0.3,1.5,1.5,1,1.5,1.5,2,1,0.5,1.5,2,NA,NA),
  crown_axis_1 = c(0.5,1,1.5,0.8,1,1.5,0.9,1.5,1,0.8,0.9,1.5,NA,NA),
  crown_axis_2 = c(0.3,0.5,0.8,0.7,0.5,1,0.8,0.8,0.5,0.6,0.8,1.2,NA,NA),
  slope = c(5,5,10,10,0,0,20,20,15,15,5,5,10,10)
)

```{r}
# investigate input dataframe
shrub_demo_data
```

*Notice that site = YOMI, plot = 2 is a plot without shrubs. For all plot-level summaries below, this plot without shrubs will have 0 biomass and cover estimates.*

<br>

**Results by shrub:**

```{r}
# call the ShrubBiomass() function in the BerkeleyForestsAnalytics package
shrub_demo1 <- ShrubBiomass(shrub_data = shrub_demo_data,
                            results = "by_shrub")

shrub_demo1
```
<br>

**Results summarized by plot:**

```{r}
# call the ShrubBiomass() function in the BerkeleyForestsAnalytics package
# keep default results (= "by_plot")
shrub_demo2 <- ShrubBiomass(shrub_data = shrub_demo_data)

shrub_demo2
```

