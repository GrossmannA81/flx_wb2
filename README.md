# Actual Evapotranspiration in Arid and Semi-arid Regions  

This project uses Eddy Covariance FluxDataKit, Condensation, Soil Thickness Data from Pelletier and 
Compound Topographic Index (CTI) Data to explain deviations of high aridity in 
combination with high actual evapotranspiration from the theoretical Budyko framework.

### Scenarios: in order to compare different impacts of condensation (with or without) and 
letant energy (corrected or not), 4 scenarios are distinguished:

- without condensation + gap filled latent energy
- wihtout condensation + corrected latent energy
- with condensation + gap filled latent energy
- with condensation + corrected latent energy


## Data 

### FluxDataKit
The FLuxDataKit (version 3.4) provides flux tower records from [Zenodo](https://doi.org/10.5281/zenodo.10885933) and delivers half hourly and 
daily data from PLUMBER2 (Ukkola, 2021), Ameriflux (Ameriflux, 2023; Pastorello, 2020), ICOS
Drought2018 (ICOS, 2018), ICOS WarmWinter2020 (ICOS, 2020).
The data is available for each site.

path to files:
```bash
- ~/data_2/FluxDataKit/v3.4/zenodo_upload/fdk_site_info.csv
```
```bash
- ~/data_2/FluxDataKit/v3.4/zenodo_upload/fdk_site_fullyearsequence.csv
```

### Topography from Marthews (2015)
This NetCDF dataset is at grid layer which was upscaled to a resolution of 15 arcsec. 
Each grid cell has a geographical location holding a specific value for CTI. 


path to file:
```bash
- ~/data/archive/gti_marthews_2015/data/ga2.nc
```


### Soil Thickness from Pelletier (2016)
This additional variable is an estimated thickness from bedrock to land surface. 
It comes with a spatial resolution of 1km grid, also provided in the NetCDF format.
With exact coordinates, thickness is extracted and assigned to the  sitenames of the FluxDataKit.

path to file:
```bash
- ~/data/archive/soil_pelletier_2016/data/Global_Soil_Regolith_Sediment_1304/data/average_soil_and_sedimentary-deposit_thickness.tif
```

### Condensation
The P-Model calculates condensation by converting night time total net radiation
into the equivalent mass of water. In order to check robustness of condensation which
is added to precipitation, this additional factor helps to distinguish different scenarios.

path to file:
```bash
- ~/data_2/FluxDataKit/v3.4/zenodo_upload/rsofun_driver_data_v3.4.2.rds
```



## Structure
This project is structured as followed and shall be run in sequenced order:

```
data-raw/
   ├─ 00_Download_Data.R
   ├─ 01_P_Model_RSOFUN.R
data/
   ├─ 01_Mean_Table.R
   ├─ 02_Budyko.R

analysis/
   ├─ 00_Budyko_Plots.R
   ├─ 01_Aridity_Plots.R
   ├─ 02_Site-Characteristics.R
   ├─ 03_Statistics.R
   ├─ 04_RandomForest_Model.R
   ├─ pics/     #folder for outputs
   ├─ tables/   #folder for outputs
```




### /data-raw

- 00_Downolad_Data.R:
This script gatters site infos and fullyearsequences from FluxDataKit.
In a next step, needed Value from CTI is extracted and finally merged and written into 
the df_sites.csv file which will be stored in the /data folder.

- 01_P_Model_RSOFUN.R:
Values of condensation are extracted and written into df_cond_mean_ann_2.csv.
finally, these values are joined into the df_sites.csv file.

### /data

- 01_Mean-Table.R:
The daily FluxDataKit data is loaded. With the function `fun_read_onesite()`entries matching 
`FLX_<sitename>_FLUXDATAKIT_FULLSET_DD*` are read from the CSV and tagged with the sitename.
looping over all sitenames in `df_sites$sitename`, purr::map then combines them into the nex dataframe 
`daily_data`.

functions for converting latent energy to evapotranspiration are execuded.

having mass flux in mm day-1, final values of precipitation `prec`, actual evapotranspiration `aet`,
corrected aet `aet_corr`, each with and without condensation, and potential evapotranspration `pet`
are joined to the main dataframe df_sites.csv


- 02_Budyko.R:
Here, the Budyko by Fu (1981) equations are introduced. For each scenario, the new dataframe `df_budyko`
transmutes the variables of precipitation and actual aswell as potential evapotranspiration.

finally, the residuals of the actual evapotranspiration from the theoretical (potential)
evapotraspiration in the Budyko framework can be calculated, which delivers a value for each scenario.




### /analysis

- 00_Budyko-Plots.R:
The aim of this script is the visualization of the scenarios. 
In this sort, the plotting of the residuals in the in the Budyko framework and
according histogram are the output of this script.

see: 
```
analysis/
   ├─ pics/  
      ├─ Budyko_Comparison_ALL.png
      ├─ Budyko_Residuals_ALL.png
```


- 01_Aridity-Plots.R:
In order to filter out arid and semi-arid as well as high cti, the data is plit into quintiles.

Based on the PET/P ratio, following splitting is defined for aridity:
`H (Humid): PET/P < 1`
`SH (Semi-humid): 1 ≤ PET/P < 2`
`SA (Semi-arid): 2 ≤ PET/P < 4`
`A (Arid): PET/P ≥ 4`

CTI is plit into quintiles as followed:
`Very Low`
`Low`
`Medium`
`High`
`Very High`

In this sort, `aridity_class_nocond`, `aridity_class`, and `cti_class` are the objects for the use of 
the extraction of the Fluxnet sites with high aridity and high CTI.

The outputs are:
```
analysis/
   ├─ pics/  
      ├─ AI_Boxplot_ALL.png
      ├─ Heatmap_ALL.png
```


- 02_Site-Characteristics.R:
This script includes high CTI and aridity in a diagram showing the degree of deviations from Budyko.
Also, site information (coordinates) with soil thickness data from 
Pelletier et al. (2016) is linked and integrated to result in two excel tables. Here, in addition 
to high CTI and aridity, soil thickness expands the dataset. 

The data is merged into `df_budyko`





```
analysis/
   ├─ pics/  
      ├─ AI_CTI_Diagram_ALL.png
      ├─ Pelletier_LE_F_MDS.png
      ├─ Pelletier_LE_CORR.png
   ├─ tables/       
      ├─ table_topsites.xlsx
      ├─ table_topsites_corr.xlsx
```


- 03_Statistics.R:
Here, the linear model statistical analysis is executed for the estimation of coefficients with gap-filled
and corrected latent energy. 



```
analysis/
   ├─ pics/  
      ├─ stat_lm_coefficients.png
      ├─ stat_lm_coefficients_corr.png
```      
      
- 04_RandomForest_Model.R:

To visualize the distribution of R² and RMSE, bowplots show the behavior of the different predictors (CTI,
thickness and landcovertype). With the help of random forest outputs, 


```
analysis/
   ├─ pics/  
      ├─ stat_rf_vip.png
      ├─ stat_rf_vip_corr.png
      ├─ stat_rf_comparison_ALL.png
```   



## Bibliography

- Budyko, M.I. Climate and Life, New York, Academic Press,1974.

- Fan, Y., Clark, M., Lawrence, D. M., Swenson, S., Band, L. E., Brantley, S. L., et al.: Hillslope hydrology in global change research
and Earth system modeling, Water Resour. Res., 55, 1737–1772, https://doi.org/10.1029/2018WR023903, 2019.

- Fu, B.: On the calculation of the evaporation from land surface, Chin. J. Atmos. Sci., 5, 23–31, 1981.

- Marthews, T. R., Dadson, S. J., Lehner, B., Abele, S., and Gedney, N.: High-resolution global topographic index values for use
in large-scale hydrological modelling, Hydrol. Earth Syst. Sci., 19, 91–104, https://doi.org/10.5194/hess-19-91-2015, 2015.

- Pastorello, G., Trotta, C., Canfora, E., Chu, H., Christianson, D., Cheah, Y.-W., Poindexter, C., Chen, J., Elbashandy, A.,
Humphrey, M., Isaac, P., Polidori, D., Reichstein, M., Ribeca, A., van Ingen, C., Vuichard, N., Zhang, L., Amiro, B., Ammann, C.,
Arain, M. A., Ardö, J., Arkebauer, T., Arndt, S. K., Arriga, N., Aubinet, et al.: The FLUXNET2015 dataset and the ONEFlux
processing pipeline for eddy covariance data, Sci. Data, 7, 225, https://doi.org/10.1038/s41597-020-0534-3, 2020.

- Pelletier, J. D., Broxton, P. D., Hazenberg, P., Zeng, X., Troch, P. A., Niu, G., Williams, Z. C., Brunke, M. A., and Gochis, D.: Global
1-km gridded thickness of soil, regolith, and sedimentary deposit layers, ORNL DAAC, Oak Ridge, Tennessee, USA,
https://doi.org/10.3334/ORNLDAAC/1304, 2016.

- Stocker, B. D., Wang, H., Smith, N. G., Harrison, S. P., Keenan, T. F., Sandoval, D., Davis, T., and Prentice, I. C.: P-model v1.0:
an optimality-based light use efficiency model for simulating ecosystem gross primary production, Geosci. Model Dev., 13,
1545–1581, https://doi.org/10.5194/gmd-13-1545-2020, 2020.

- Ukkola, A.: PLUMBER2: forcing and evaluation datasets for a model intercomparison project for land surface models v1.0,
University of New South Wales, https://dx.doi.org/10.25914/5fdb0902607e1, 2021.

- Zhang, L., et al.: A rational function approach for estimating mean annual evapotranspiration, Water Resour. Res., 40, 1–14,
https://doi.org/10.1029/2003WR002710, 2004.
