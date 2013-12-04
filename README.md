# gcm - use GRASS colormaps in R


This R package makes it easy to use GRASS's raster colormaps and colormaps build according to GRASS rules (see [r.colors](http://grass.osgeo.org/grass64/manuals/r.colors.html)) when plotting raster maps with the raster package.

[The R Project for Statistical Computing](www.r-project.org/) is a free software environment for statistical computing and graphics.

[raster](cran.r-project.org/web/packages/raster/‎) is a R package for Geographic data analysis and modeling.

[GRASS GIS](http://grass.osgeo.org/) is a free and open source Geographic Information System (GIS) software suite used for geospatial data management and analysis, image processing, graphics and maps production, spatial modeling, and visualization.


----

### Installation
unzip, 
open Terminal/C-shell/Bash, navigate to the folder where you unzipped the Archive and type:

```
R CMD check gcm-master (optional)
R CMD build gcm-master
R CMD INSTALL gcm_1.0.tar.gz
```

Done.


If you don't want to install, just **source** the *.R files in the R-folder
```
source("path/plot.gcm.R")
source("path/plot.gcm.legend.R")
```

### Provides
```
plot.gcm(x,cmap, method=2, inflate=1, min=NA, max=NA, force.breaks=FALSE, legend=FALSE, out=NULL, ... )
```
```
plot.gcm.legend(gcm, min=NULL, max=NULL, ... )
```


### Example
refer to the package documentation
```
library(gcm)
?gcm
```
or check [janhooo.wordpress.com](http://janhooo.wordpress.com/2013/11/20/gcm-use-grass-colormaps-in-r-raster-plots/) for an introduction




### License

This package is licensed to you under the terms of the [GNU General Public License](http://www.gnu.org/licenses/gpl.html) version 3 or later.
