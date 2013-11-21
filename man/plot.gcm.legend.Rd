\name{plot.gcm.legend}

\docType{methods}

\alias{plot.gcm.legend}

\title{
add legend to Raster layer object
}
\description{
add legend to Raster layer object preferably plotted with \code{\link[gcm]{plot.gcm}}
}
\usage{
plot.gcm.legend(gcm, min = NULL, max = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gcm}{
either a list as output by plot.gcm(x, out=TRUE, ...) or the color/breakmap data.frame
}
  \item{min}{
is taken from the list as output by plot.gcm(x, out=TRUE, ...), but can be provided seperatly for flexibility. If NULL the legend will span the range of the whole colormap given. If provided the legend will be cut of according to raster values
}
  \item{max}{
accordingly
}
  \item{axis.args}{
Arguments to be passed to raster::plot
}
  \item{\dots}{
Graphical parameters. Any argument that can be passed to \code{\link[fields]{image.plot}} and to \code{\link[graphics]{plot}}, 
        such as legend.width, legend.args, axis.args
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
object of type plot(x, legend.only=TRUE, ... )
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Jan M. Holstein <holstein@uni-bremen.de>
}

\seealso{
\link[gcm]{plot.gcm}, \link[fields]{image.plot}, \link[raster]{plot}
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
