## Changelog:
# MH 0.0.1 2021-10-25: initial programming

## Documentation
#' @title Plot density
#' @description Function that generates density plots
#' @param object The object returned from calling function \code{intervention_effect}.
#' @param plot logical, if TRUE plots are displayed
#' @return \code{plot_density} returns ggplot2 code for density plots
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}

## Function definition
plot_density <- function( object, plot=TRUE ){

	# function name
	fun.name <- "plot_density"

	# function version
	fun.version <- "0.0.1 2021-10-25"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# get verbose argument
	verbose <- object$control$verbose

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", Sys.time(), "\n" ) )
	
	# packages
	require( ggplot2 )
	
	# theme
	theme <- theme_bw() +
         theme( axis.title=element_text(size=14, face="bold"),
                axis.text=element_text(size=14, colour="black"),
                panel.grid=element_blank(),
                panel.grid.major=element_line(colour="lightgrey", size=0.25),
                panel.grid.minor=element_blank(),
                legend.text=element_text(size=12),
                legend.title=element_text(size=14, face = "bold"),
                legend.justification=c(1,1),
                legend.position=c(0.98,0.98),
                legend.key=element_rect(linetype=0),
                legend.key.height=unit(15, "points"),
                legend.key.width=unit(40, "points"),
                legend.spacing.x=unit(0, "points"),
                strip.text.x=element_text(size=12, face="bold"),
                strip.text.y=element_text(size=12, face="bold"),
                strip.background=element_rect(colour="black", fill="white"),
                plot.background=element_rect(fill="white"),
                plot.title=element_text(face="bold", size=16)
               )

	# variable names
	xnames <- names(object$interventional_distribution$density$pdf)
	
	# omit interventional variables for plotting
	xnames <- xnames[ !xnames %in% object$info_interventions$intervention_name ]
	
	if( length( xnames ) > 0 ){

		# count the number of integer digits
		# https://stackoverflow.com/questions/47190693/count-the-number-of-integer-digits
		n_int_digits = function(x) {
		  result = floor(log10(abs(x)))
		  result[!is.finite(result)] = 0
		  result
		}


		### x breaks
		# get all x values
		allx <- unname( do.call( "c", sapply( xnames, function( xname ) object$interventional_distribution$density$pdf[[xname]][,"x"], simplify=FALSE ) ) )
		
		# extremest absolute value
		extremex <- max( abs( min( allx ) ), abs( max( allx ) ) )
		extrx.ndig <- n_int_digits( extremex )		
		extrx <- round( max( abs( min( allx ) ), abs( max( allx ) ) ), -extrx.ndig )
		
		# lowest break
		if( min( allx ) < 0 ){
			xlow <- -extrx
		} else {
			xlow <- min( allx )
		}

		# highest break
		if( max( allx ) < 0 ){
			xhigh <- max( allx )
		} else {
			xhigh <- extrx
		}

		# x breaks/labs
		x.breaks <- c( seq( xlow, xhigh, length.out=11 ) )
		x.labs <- formatC( x.breaks, format="f", digits=ifelse( extrx.ndig > 0, 0, -extrx.ndig ) )


		### y breaks
		# get all y values
		ally <- unname( do.call( "c", sapply( xnames, function( xname ) object$interventional_distribution$density$pdf[[xname]][,"pdf.values"], simplify=FALSE ) ) )

		# extremest absolute value
		extremey <- max( abs( min( ally ) ), abs( max( ally ) ) )
		extry.ndig <- n_int_digits( extremey )		
		extry <- round( max( abs( min( ally ) ), abs( max( ally ) ) ), -extry.ndig )

		# lowest break
		if( min( ally ) < 0 ){
			ylow <- -extry
		} else {
			ylow <- min( ally )
		}

		# highest break
		if( max( ally ) < 0 ){
			yhigh <- max( ally )
		} else {
			yhigh <- extry
		}
		
		# y breaks/labs
		y.breaks <- c( seq( ylow, yhigh, length.out=11 ) )
		y.labs <- formatC( y.breaks, format="f", digits=ifelse( extry.ndig > 0, 0, -extry.ndig+1 ) )


		# function to generate density plot
		gen.density.plot <- function( xname ){
		
			# data
			d <- as.data.frame( object$interventional_distribution$density$pdf[[xname]] )
			
			# generate plot
			p <- ggplot( data=d, aes( y=pdf.values, x=x ) )
			p <- p + geom_line()
			p <- p + scale_x_continuous( limits=c(x.breaks[1], x.breaks[length(x.breaks)]), breaks=x.breaks, labels=x.labs, expand = expansion(mult = c(0, 0)) )
			p <- p + scale_y_continuous( limits=c(y.breaks[1], y.breaks[length(y.breaks)]), breaks=y.breaks, labels=y.labs, expand = expansion(mult = c(0, 0)) )
			p <- p + xlab( paste0( xname, "\n" ) )
			p <- p + ylab( "interventional pdf\n" )
			p <- p + theme
			# https://stackoverflow.com/questions/18252827/increasing-area-around-plot-area-in-ggplot2
			# top right bottom left
			p <- p + theme( plot.margin = unit(c(5.5, 15.5, 5.5, 5.5), "points") )
			
			# return plot
			return( p )
		}

		# generate density plots
		p.l <- sapply( xnames, gen.density.plot, simplify=FALSE )

		# plot the plots
		if( plot ) {
			for( p in p.l ){
				dev.new()
				plot( p )
			}
		}
	
	} else {
		# console output
		if( verbose >= 2 ) cat( paste0( "nothing to plot, names(object$interventional_distribution$density$pdf) does not include any non-interventional variables", "\n" ) )
		
		# return object NULL
		p.l <- NULL
	}
	
	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ", Sys.time(), "\n" ) )

	# return list of plots
	return( p.l )
}

### development
# Rfiles <- list.files( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R", pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% "plot_density.R" ]
# for( Rfile in Rfiles ){
	# source( Rfile )
# }

## test object 00_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/00_lavaan_test_object.Rdata" ) )
# object00 <- intervention_effect( model=o00_lavaan_test_object,intervention="x2",intervention_level=2)
# p.l <- plot_density( object00 )


## test object 01_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/01_lavaan_test_object.Rdata" ) )
# object01 <- intervention_effect( model=o01_lavaan_test_object,intervention="x2",intervention_level=2)
# p.l <- plot_density( object01 )


## test object 02_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/02_lavaan_test_object.Rdata" ) )
# object02 <- intervention_effect( model=o02_lavaan_test_object,intervention="x2",intervention_level=2)
# p.l <- plot_density( object02 )


## test object 03_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/03_lavaan_test_object.Rdata" ) )
# object03 <- intervention_effect( model=o03_lavaan_test_object,intervention="x2",intervention_level=2)
# p.l <- plot_density( object03, FALSE )


### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
