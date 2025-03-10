
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

* by_shrub: A dataframe with tree-level biomass and carbon estimates.

* by_plot: A dataframe with plot-level biomass and carbon estimates.


A dataframe with the following columns: 

1. `time`: as described above

2. `site`: as described above 

3. `plot`: as described above 

4. `load_1h_Mg_ha` (or `load_1h_ton_ac`): fuel load of 1-hour fuels in megagrams per hectare (or US tons per acre)

### Demonstration

```{r}
# investigate input tree_data
overstory_demo
```

```{r}
# invesigate input fuel_data 
fwd_demo
```

<br>

```{r}
# call the FineFuels() function in the BerkeleyForestsAnalytics package
# keep default sp_codes (= "4letter") and units (= "metric")
fine_demo <- FineFuels(tree_data = overstory_demo,
                       fuel_data = fwd_demo)

fine_demo
```
