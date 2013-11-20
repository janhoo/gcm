# gcm - use GRASS colormaps in R

[The R Project for Statistical Computing](www.r-project.org/) is a free software environment for statistical computing and graphics.

[raster](cran.r-project.org/web/packages/raster/‎) is R package for Geographic data analysis and modeling.

[GRASS GIS](http://grass.osgeo.org/) is a free and open source Geographic Information System (GIS) software suite used for geospatial data management and analysis, image processing, graphics and maps production, spatial modeling, and visualization.

This R package makes it easy to use GRASS's raster colormaps and colormaps build according to GRASS rules when plotting raster maps with the raster package. [r.colors](http://grass.osgeo.org/grass64/manuals/r.colors.html).

----

provides
plot.gcm(x,cmap, method=2, inflate=1, min=NA, max=NA, force.breaks=FALSE, legend=F, out=NULL, ... )

plot.gcm.legend(gcm,min=NULL,max=NULL, ... )


(todo: list of arguments)

### Example

refer to [janhooo.wordpress.com](http://janhooo.wordpress.com/2013/11/20/gcm-use-grass-colormaps-in-r-raster-plots/) for an introduction



### License

This package is licensed to you under the terms of the [GNU General Public License](http://www.gnu.org/licenses/gpl.html) version 3 or later.

Copyright 2013 jan holstein <holstein@uni-bremen.de>