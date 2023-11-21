## About
A workflow for generating high resolution plots of sampling locations.  


## Setup
### Folders

`in_sampling_sites/` : Expects a `.csv` file(s) with the columns `lat` and `long` in decimal degrees for positioning data. Multiple files will be displayed distinctly in figures and filenames visable in figure captions

`in_eez/` : Expects a `.shp` shapefile of Exclusive Economic Zones or other boundaries you wish to display without a fill. For example : https://www.marineregions.org/downloads.php 

`in_marine_parks/` : Expects a `.shp` shapefile of Marine Parks or other boundaries you wish to display with a fill (default : darkgreen). For example : https://fed.dcceew.gov.au/datasets/erin::australian-marine-parks/about 

`in_raster/` : Expects a `.tif` rasterfile or other background. For example : https://www.naturalearthdata.com/downloads/10m-raster-data/

`temp/` : No user input required; contains downloaded naturalearth files calculated raster extents created during the workflow to reduce compute load on subsequent runs. 

## Authors and contributors 
Samuel Thompson
