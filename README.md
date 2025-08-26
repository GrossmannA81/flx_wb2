# Actual Evapotranspiration in Arid and Semi-arid Regions  

This project uses Eddy Covariance FluxDataKit, Condensation, Soil Thickness Data from Pelletier and 
Compound Topographic Index (CTI) Data to explain deviations of high aridity in 
combination with high actual evapotranspiration from the theoretical Budyko framework.



## Data 

### FluxDataKit
The FLuxDataKit (version 3.4) provides flux tower records from [Zenodo](https://doi.org/10.5281/zenodo.10885933)**  and delivers half hourly and 
daily data from PLUMBER2 (Ukkola, 2021), Ameriflux (Ameriflux, 2023; Pastorello, 2020), ICOS
Drought2018 (ICOS, 2018), ICOS WarmWinter2020 (ICOS, 2020).
The data is available for each site.

path to files:
```bash
- /data_2/FluxDataKit/v3.4/zenodo_upload/fdk_site_info.csv
```
```bash
- /data_2/FluxDataKit/v3.4/zenodo_upload/fdk_site_fullyearsequence.csv
```

### Topography from Marthews (2015)
This NetCDF dataset is at grid layer which was upscaled to a resolution of 15 arcsec. 
Each grid cell has a geographical location holding a specific value for CTI. 


path to file:
```bash
- /data/archive/gti_marthews_2015/data/ga2.nc
```


### Soil Thickness from Pelletier (2016)
This additional variable is an estimated thickness from bedrock to land surface. 
It comes with a spatial resolution of 1km grid, also provided in the NetCDF format.
With exact coordinates, thickness is extracted and assigned to the  sitenames of the FluxDataKit.

path to file:
```bash
- /data/archive/soil_pelletier_2016/data/Global_Soil_Regolith_Sediment_1304/data/average_soil_and_sedimentary-deposit_thickness.tif
```

### Condensation
The P-Model calculates condensation by converting night time total net radiation
into the equivalent mass of water.



## Structure
This project is structured as followed and must be run in sequenced order:

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
   ├─ 02_CTI_Plots.R
   ├─ 03_Site-Characteristics.R
   ├─ 04_Statistics.R
   ├─ 05_RandomForest_Model.R
   ├─ pics/     #folder for outputs
   ├─ tables/   #folder for outputs
```

