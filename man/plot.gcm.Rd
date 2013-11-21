\name{plot.gcm}

\docType{methods}

\alias{plot.gcm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot a Raster layer object
}
\description{
plot a Raster layer object with GRASS color map
}
\usage{
plot.gcm(x, cmap, method = 2, inflate = 1, min = NA, max = NA, force.breaks = FALSE, legend = F, out = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Raster layer object
}
  \item{cmap}{
  	either path and filename of a colormap obeying GRASS colormap style or a data.frame of type gcm$gcm (see Example) 
}
  \item{method}{
method to use for colormap and break calculation: 1 or 2. (see Details)
}
  \item{inflate}{
increase (>1) or decrease (<1) the number of breaks (see Details)
}
  \item{min}{
minimum value of raster layer - is calculated if NA
}
  \item{max}{
maximum accordingly
}
  \item{force.breaks}{
if method=1 and breaks are all relative you may use the linear breaks of graphics::plot if force.breaks=FALSE and legend=TRUE
}
  \item{legend}{
plain vanilla legend argument. Per default set to FALSE du to the anticipated high number of breaks
}
  \item{out}{
if out="gcm" there is no plot. Instead a list is output needed as arguments for \link[gcm]{plot.gcm.legend}. It contains the color/breakmap, min and max
}
  \item{\dots}{
Graphical parameters. Any argument that can be passed to \code{\link[fields]{image.plot}} and to \code{\link[graphics]{plot}}, 
        such as axes=FALSE, main='title', ylab='latitude'
}
}
\details{
plot.gcm evaluates the GRASS-style colormap, which is a set of  major breaks and associated 
colors which define the color gradient between two neigboring breaks 
(see \href{http://grass.osgeo.org/grass64/manuals/r.colors.html}{r.colors})
method 2 (default) constructs 20 breaks intervals between each major break. This number can be modified by the factor inflate.
method 1 constucts breaks intervals depending on the magnitude of the values of the Raster, e.g. if the values range between min=10 
and max=100, the N=90 breaks are constructed. 
Use inflate to adapt the break number to you data. 
Not recommended if values span several orders of magnitude.
}
\value{
plot or (if out="gcm") a list}

\author{
Jan M. Holstein <holstein@uni-bremen.de>
}
\note{
gcm uses \code{\link[raster]{plot}} from the raster package. It is designated only to work with Raster layers. Using raster stacks or bricks will lead to 
}



\seealso{
\link[gcm]{plot.gcm.legend}
}
\examples{
library(raster)

# make raster
dat=list()
I<-0.04
C<-0.01
dat$x=seq(-1,by=0.01,len=201)
dat$y=seq(-1,by=0.01,len=201)
m<-matrix(0,nrow=201, ncol=201,byrow=TRUE)
m<-I*exp(-(((row(m)-101)/ 40)^2 + ((col(m)-101)/ 100)^2)) -C
dat$z=matrix(m, 201, 201)
r=raster(dat);plot(r)

# construct a GRASS-style colormap. Notice, that you can mix absolute and relative breaks. Notice, that you can mix varios color definitions
rstcurv<-    '-0.1      purple NA  NA
-0.01        0 0  255
-0.001         indigo  NA NA
-0.00001        cyan NA  NA
0 200:255:200 NA  NA
0.00001      yellow NA  NA
0.001      orange NA  NA
0.01         "#FF0000" NA  NA
100\%   255:0:200 NA  NA'
tf<-tempfile()
writeLines(rstcurv,tf)

# check different methods, play with inflate
plot.gcm(x=r,method=1,inflate=1,cmap=rstcurv,legend=FALSE,axes=FALSE,box=FALSE)
plot.gcm(x=r,method=1,inflate=100,cmap=rstcurv,legend=FALSE,axes=FALSE,box=FALSE)
plot.gcm(x=r,method=1,inflate=1000,cmap=rstcurv,legend=FALSE,axes=FALSE,box=FALSE)
plot.gcm(x=r,method=2,inflate=1,cmap=rstcurv,legend=FALSE,axes=FALSE,box=FALSE)
plot.gcm(x=r,method=2,inflate=0.01,cmap=rstcurv,legend=FALSE,axes=FALSE,box=FALSE)

# take a look at the colormap
gcm<-plot.gcm(x=r,method=1,cmap=tf,out="gcm")
gcm

# see the difference in legend
plot.gcm.legend(gcm=gcm) # legend covers only intervalls where values exist
plot.gcm.legend(gcm=gcm$gcm) # legend covers whole range defined in colormap
}
\keyword{methods}
\keyword{spatial}
