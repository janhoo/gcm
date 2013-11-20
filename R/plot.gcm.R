plot.gcm <-
function(x,cmap, method=2, inflate=1, min=NA, max=NA, force.breaks=FALSE, legend=F, out=NULL, ... )
{
	
	is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}

number.of.character<-function(string,char=":")
{
	pattern<-paste0("[^",char,"]")
	return(nchar(gsub(pattern,"",string)))
}


remove.nodata.rows<-function(df)
{
	nvrow<-grep("nv",df$breaks)
	if(!is.integer0(nvrow)){
		df<-df[-nvrow,]
	}
	return(df)
}

break.type<-function(breaks)
{
	relbreaks<-grep("%$",breaks)
	if(is.integer0(relbreaks)){ 		
		out<-"absolute breaks \n"
		break.type<-"abs"
	} else if(length(relbreaks)==length(breaks)){ 
		out<-"relative breaks \n"
		break.type<-"rel"
	} else {
		break.type<-"mix"
		out<-"mix of relative and absolute breaks \n"
	}	
return(break.type)
} 

make.breaks<-function(breaks,min=NA,max=NA)
{
	relbreaks<-grep("%$",breaks)
	if(is.integer0(relbreaks)){ # only absolute breaks		
		br<-as.numeric(breaks)
	} else {# partly relative breaks
		breaks[relbreaks]<-abs.break(breaks[relbreaks],min,max)
		br<-as.numeric(breaks)
	}	
	return(br)
}

abs.break<-function(r,min,max,relbreaks)
{
	r<-as.numeric(sub('%$','',r))
	x<-min+(max-min)*as.numeric(r)/100
	return(as.character(x))
}

rgbcolors<-function(df)
{
	if(is.na(df[2]) & is.na(df[3])){ 						# x NA NA
		if (number.of.character(as.character(df[1]))==2){		#  r:g:b NA NA
			as.numeric(unlist(strsplit(df[1], "[:]")))
		} else {	
			if(df[1]=="indigo")	{df[1]<-"#6F00FF"}
			if(df[1]=="aqua")	{df[1]<-"#00FFFF"}
			as.vector(col2rgb(df[1]))
		}		
	} else {
		as.numeric(df)											# r g b 
	}
}

color.map1<-function(gcm,inflate=inflate)
{ # number of colors in interval is proportional to interval-size: good if intervals have approx same size. inflate~=1/magnitude
	if(class(gcm[,2])=="factor"){
		gcm[,2]<-as.character(gcm[,2])
	}
	# edit to integrate more flexibility
	gcm.break<-gcm[,1]
	gcm.color<-gcm[,2]
	gcm.length<-nrow(gcm)
	#
	
	col<-NA
	break.start<-gcm.break[1]; break.end<-gcm.break[gcm.length]
	for(i in 1:(gcm.length-1)){		
		break.from<-gcm.break[i]; break.to<-gcm.break[i+1]
		color.from<-gcm.color[i]; color.to<-gcm.color[i+1]
		pal<-colorRampPalette(c(color.from, color.to))
		col<-append(col,pal(abs(break.from-break.to)*inflate))
	}
	colormap<-col[-1]
	breakmap<-seq(break.start,break.end,length.out=length(colormap)+1)
	li<-list(colormap=colormap, breakmap=breakmap)
	return(li)
}

color.map2<-function(gcm,inflate=inflate)
{  # same number of colors between interpolation points (better if you cover several magnitudes)
	if(class(gcm[,2])=="factor"){
		gcm[,2]<-as.character(gcm[,2])
	}
	inflate<-inflate*20
	# edit to integrate more flexibility
	gcm.break<-gcm[,1]
	gcm.color<-gcm[,2]
	gcm.length<-nrow(gcm)
	#
	
	col<-NA;breaks<-NA
	fin<-gcm.length-1
	for(i in 1:fin){	
		break.from<-gcm.break[i]; break.to<-gcm.break[i+1]
		if(break.from-break.to==0){next}
		color.from<-gcm.color[i]; color.to<-gcm.color[i+1]
		pal<-colorRampPalette(c(color.from, color.to))
		b<-seq(break.from,break.to,length.out=inflate+1)
		breaks<-append(breaks,b,after=length(breaks)-1)
		breaks<-breaks[-length(breaks)]
		col<-append(col,pal(inflate),after=length(col)-1)		
	}
	colormap<-col[-length(col)]
	breakmap<-breaks
	li<-list(colormap=colormap, breakmap=breakmap)
	return(li)
	
	break.labels<-gcm[!duplicated(gcm[1]),1]
	li$colormap[match(break.labels,li$breakmap)]
}

	c<-read.table(file=cmap, stringsAsFactors=F, col.names=c("breaks","r","g","b"), fill=T,sep="")
	c<-remove.nodata.rows(c)
	if(class(x)=="RasterLayer")
	{
		if(is.na(min)){min<-minValue(x)}
		if(is.na(max)){max<-maxValue(x)}
	}
	breaks<-make.breaks(c$breaks,min=min,max=max)
	colors<-rgb(t(apply(c[,2:4],1, rgbcolors)/255))
	gcm<-data.frame(breaks=breaks,colors=colors,stringsAsFactors=FALSE)
	cm<-switch(method, color.map1(gcm,inflate), color.map2(gcm,inflate))
	brks<-cm$breakmap
	if(break.type(c$breaks)=="rel" && method==1 && force.breaks==FALSE ){brks<-NULL}
	if(is.null(out)){
		plot(x, col=cm$colormap, breaks=brks, legend=legend, ... )	
	} else if (out=="gcm"){
		list(gcm=gcm,min=min,max=max)
	}
}
