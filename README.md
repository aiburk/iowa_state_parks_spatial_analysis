## Iowa State Parks Spatial Analysis
A repository of R Code I created for spatial analysis of Iowa state parks. 

All data used are obtained from public sources.

## Files:
* __Iowa_State_Parks_Scrape_R_Script.R__:
  
  Scrapes address information for state parks from the Iowa DNR website, cleans & writes to the rds file __iowa_state_parks_scrape.rds__.

  
* __Iowa_State_Parks_Spatial_R_Script.R__

  * Pulls state parks geometry data from Iowa DNR's ArcGIS REST directory, as well as IA county and city/place census data.
  * Performs spatial operations to create figures/maps.
  * Runs bivariate & multivariate linear regression models.
  * Cleans pulled state parks data & writes to the rds file __iowa_state_parks_df.rds__.



