plot.gcm.legend <-
function(gcm,min=NULL,max=NULL, axis.args=NULL, ... )
{
	make.raster<-function(lc)
	{
		require(raster)
		dat=list()
		dat$x=seq(1,by=1,len=2)
		dat$y=seq(1,by=1,len=lc)
		m<-matrix(0,nrow=2, ncol=lc,byrow=TRUE)
		m<-col(m)
		dat$z=matrix(m,2,lc)
		raster(dat);#plot(r,col=colorRampPalette(df$color)(64))
	}
	if (class(gcm)=="list")
	{
		min<-gcm$min
		max<-gcm$max
		gcm<-gcm$gcm
	}
	
	df <-gcm[!duplicated(gcm$breaks),]
	if(!is.null(min))
	{
		ix<-1;x<-NA
		for(i in 1:(nrow(df)-1)){
			if(min>=df$breaks[i+1]){x[ix]<-i;ix<-ix+1}
		}
		if(any(x,na.rm=T)){df<-df[-x,]}
	}
	if(!is.null(max))
	{
		ix<-1;x<-NA
		for(i in (nrow(df):2)){
			if(max<=df$breaks[i-1]){x[ix]<-i;ix<-ix+1}
		}
		if(any(x,na.rm=T)){df<-df[-x,]}
	}	
	lc<-nrow(df)
	axis.args=append(axis.args,list(at=seq(1, lc),labels=df$breaks))
	r=make.raster(lc)

	plot(r, legend.only=TRUE, col=colorRampPalette(df$color)(64), axis.args=axis.args, ...)
}
