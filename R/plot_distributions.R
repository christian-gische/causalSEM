## Changelog:
# MH 0.0.3 2022-02-11:
#    -- implemented conditional multivariate distribution
#    -- added argument "scales.free" 
#    -- colored means
# CG 0.0.2 2022-02-07: replace sigma by stats::sigma
# MH 0.0.1 2022-01-29: initial programming
## Documentation
#' @title Plot distributions
#' @description Function that generates distribution plots
#' @param object The object returned from calling
#'        function \code{intervention_effect}.
#' @param plot, logical, if TRUE plots are displayed
#' @param plot, character, directory to save pdf plot files
#' @param scales.free, logical, FALSE (default): all plots have the same
#'        x-axis and y-axis ticks, TRUE: free x-axis and y-axis ticks
#' @return \code{plot_distributions} returns ggplot2 code
#'         for density plots
#' @references Gische, C., Voelkle, M.C. (2021) Beyond the mean: a
#'             flexible framework for studying causal effects using linear
#'             models. Psychometrika (advanced online publication).
#'             https://doi.org/10.1007/s11336-021-09811-z

## Function definition
plot_distributions <- function( object, plot=TRUE, plot.dir=NULL,
														   scales.free=FALSE ){

	# function name
	fun.name <- "plot_distributions"

	# function version
	fun.version <- "0.0.3 2022-02-11"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# get verbose argument
	verbose <- object$control$verbose

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version,
							" ", Sys.time(), "\n" ) )

	# packages
	require( reshape2 )
	require( ggplot2 )
	require( gridExtra )

	
	### declarations

	# names of outcome variables
	outvars <- object$info_interventions$outcome_names
	
	# names of intervention variables
	intvars <- object$info_interventions$intervention_names
	
	# intervention levels
	intlevels <- object$info_interventions$intervention_levels
	# column vector
	intlevels.cv <- matrix( intlevels, nrow=1 )
	
	# get data (lavaan only!!)
	d <- as.data.frame( lavInspect( object$fitted_object, "data" ) )
	
	# MH 0.0.3 2022-02-11
	# get model implied covariance matrix (lavaan only!!)
	fitted <- fitted( object$fitted_object )
	Sigma.fitted <- fitted$cov
	Sigma <- matrix( Sigma.fitted, nrow=dim(Sigma.fitted)[1],
													ncol=dim(Sigma.fitted)[2] )
	rownames( Sigma ) <- rownames( Sigma.fitted )
	colnames( Sigma ) <- colnames( Sigma.fitted )
	
	# get model implied means (lavaan only!!)
	if( any( names( fitted ) %in% "mean" ) ){
			mu.fitted <- fitted$mean
			mu <- as.vector( mu.fitted )
			names( mu ) <- names( mu.fitted )
	} else {
			mu <- rep( 0, dim( Sigma )[1] )
			names( mu ) <- colnames( Sigma )
	}
	
	
	### sampling statistics
	smplstat <- data.frame( "outvar"=outvars, 
							"mean"=sapply( d[,outvars], mean ),
							"sd"=sapply( d[,outvars], sd ),
							"stat"="smpl" )
	
	
	### conditional statistics
	
	# MH 0.0.3 2022-02-11: commented out
	
	# lm model strings
	# lm.strs <- paste0( "lm( ", outvars, " ~ ", 
						# paste( intvars, collapse=" + " ), ", data=d )" )
	
	# estimate lm models
	# fitted.l <- sapply( lm.strs, function( str ) eval( parse( text=str ) ),
						# simplify=FALSE )

	# make data frame for intervention levels (for lm prediction)
	# int.long <- data.frame( "intvar"=intvars, "intlevel"=intlevels )
	# int.wide <- dcast( int.long, . ~ intvar, value.var="intlevel" )[,-1,
															# drop=FALSE]

	# mean predictions
	# cond.mean <- unname( sapply( fitted.l, function( fitted ) 
							# stats::predict.lm( object=fitted,
							#								 newdata=int.wide),
							# simplify=TRUE ) )
	
	# sd
	# CG 0.0.2 2022-02-07: replace sigma by stats::sigma
	# cond.sd <- unname( sapply( fitted.l, stats::sigma, simplify=TRUE ) )

	# conditional statistics data frame
	# condstat <- data.frame( "outvar"=outvars, 
							# "mean"=cond.mean,
							# "sd"=cond.sd,
							# "stat"="cond" )
	
	# MH 0.0.3 2022-02-11: conditional multivariate normal
	# https://en.wikipedia.org/wiki/Multivariate_normal_distribution#
	#                                                 Conditional_distributions
	# mu1: outvars
	mu1 <- matrix( mu[outvars], ncol=1 )
	# mu2: intvars
	mu2 <- matrix( mu[intvars], ncol=1 )
	# Sigma11: outvars
	Sigma11 <- Sigma[ outvars, outvars, drop=FALSE ]
	# Sigma22: intvars
	Sigma22 <- Sigma[ intvars, intvars, drop=FALSE ]	
	# Sigma21: intvars x outvars
	Sigma21 <- Sigma[ intvars, outvars, drop=FALSE ]
	# Sigma12: outvars x intvars
	Sigma12 <- Sigma[ outvars, intvars, drop=FALSE ]	

	# conditional mean
	mu.cond <- (mu1 + Sigma12 %*% solve(Sigma22) %*% (intlevels.cv - mu2))[,1]
	
	# conditional covariance
	Sigma.cond <- Sigma11 - Sigma12 %*% solve( Sigma22 ) %*% Sigma21
	
	# conditional statistics data frame
	condstat <- data.frame( "outvar"=outvars, 
							"mean"=mu.cond,
							"sd"=sqrt( diag( Sigma.cond ) ),
							"stat"="cond" )	
	rownames( condstat ) <- seq( along=rownames( condstat ) )


	### causal statistics
	
	# mean
	causal.mean <- object$tables$interventional_means$Est.
	# for safeness: sort again supposedly already sorted vector
	names( causal.mean ) <- object$tables$interventional_means$Variable
	causal.mean <- causal.mean[ outvars ]

	# sd
	causal.sd <- sqrt( object$tables$interventional_variances$Est. )
	# for safeness: sort again supposedly already sorted vector
	names( causal.sd ) <- object$tables$interventional_variances$Variable
	causal.sd <- causal.sd[ outvars ]
	
	# causal statistics data frame
	causalstat <- data.frame( "outvar"=outvars, 
							  "mean"=causal.mean,
							  "sd"=causal.sd,
							  "stat"="causal" )
	
	
	### data frame with all stats
	stat <- do.call( "rbind", list( smplstat, condstat, causalstat ) )
	
	# factors
	stat$outvar <- factor( stat$outvar, levels=outvars )
	stat$stat   <- factor( stat$stat, levels=c("smpl","cond","causal") )
	
	# sort
	stat <- stat[,c("outvar","stat","mean","sd")]
	stat <- stat[ order( stat$outvar, stat$stat ), ]
	rownames( stat ) <- seq( along=rownames( stat ) )

	
	### data frame with x values ("grid") and y values (pdf values)
	
	# generate x values and calculate pdfs for each variable, return list
	l <- mapply( function( outvar, stat, mean, sd ){

							# generate x-axis values
							x <- seq( -3*sd, 3*sd, length.out=200 ) + mean

							# get pdf values
							pdf.values <- stats::dnorm( x, mean=mean, sd=sd )

							# data frame for distribution line
							d <- data.frame( "outvar"=outvar, "stat"=stat,
							            "x"=x, "pdf.values"=pdf.values )
										
							# data frame for shaded 95% area
							s <- d[ d$x > ( stats::qnorm(0.025)*sd + mean ) &
							        d$x < ( -stats::qnorm(0.025)*sd + mean ), ]

							# data frame for mean, q025, q975
							x2 <- c( mean,
							         stats::qnorm(0.025)*sd + mean,
									 -stats::qnorm(0.025)*sd + mean )
							m <- data.frame( "outvar"=outvar, "stat"=stat,
											 "q"=c("mean","q025","q975"),
											 "x"=x2,
											 "pdf.values"=
													 sapply( x2, function( x )
											 stats::dnorm(x,mean=mean,sd=sd) )
											)
							# return
							return( list( d, s, m ) )

					}, stat$outvar, stat$stat, stat$mean, stat$sd,
					                                           SIMPLIFY=FALSE )
	# complete data frame for distribution lines
	pdf <- do.call( "rbind", sapply( l, "[", 1 ) )
	# complete data frame for shaded 95% areas
	s95 <- do.call( "rbind", sapply( l, "[", 2 ) )
	# complete data frame for mean, q025, q975
	m <- do.call( "rbind", sapply( l, "[", 3 ) )
	
	# factors
	pdf$outvar <- factor( pdf$outvar, levels=levels( stat$outvar ) )
	s95$outvar <- factor( s95$outvar, levels=levels( stat$outvar ) )
	pdf$stat   <- factor( pdf$stat,   levels=levels( stat$stat ) )
	s95$stat   <- factor( s95$stat,   levels=levels( stat$stat ) )
	
	# count the number of integer digits
	# https://stackoverflow.com/questions/47190693/
	#                                        count-the-number-of-integer-digits
	n_int_digits = function(x) {
	  result = floor(log10(abs(x)))
	  result[!is.finite(result)] = 0
	  result
	}

	### x breaks
	# get all x values
	# allx <- unname( do.call( "c", sapply( xnames, function( xname )
	#                                  pdf[[xname]][,"x"], simplify=FALSE ) ) )
	allx <- pdf$x
	
	# extremest absolute value
	extremex <- max( abs( min( allx ) ), abs( max( allx ) ) )
	extrx.ndig <- n_int_digits( extremex )		
	extrx <- round( max( abs( min( allx ) ), abs( max( allx ) ) ), -extrx.ndig)
	
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
	x.labs <- formatC( x.breaks, format="f", digits=ifelse( extrx.ndig > 0, 0,
																-extrx.ndig ) )

	### y breaks
	# ally <- unname( do.call( "c", sapply( xnames, function( xname ) c(	
												# pdf[[xname]][,"pdf.values"],
												# pdf[[xname]][,"UL95"],
												# pdf[[xname]][,"LL95"] ),
												# simplify=FALSE ) ) )
	ally <- pdf$pdf.values

	# lowest break
	ylow <- min( ally )

	# highest break
	yhigh <- max( ally )

	# extremest absolute value
	extremey <- max( abs( min( ally ) ), abs( max( ally ) ) )
	extry.ndig <- n_int_digits( extremey )		
	
	## y breaks/labs
	seq.start <- ifelse( ylow<0, ylow, 0 )
	y.breaks <- c( seq( seq.start, yhigh+diff(range(ally))*0.01, length.out=5))
	y.labs <- formatC( y.breaks, format="f", digits=ifelse( extry.ndig > 0, 0,
	                                                          -extry.ndig+1 ) )
	# lowest y limit is the UL95, no label
	y.labs[1] <- ""

	# theme
	theme <- theme_bw() +
         theme( axis.title=element_text(size=14, face="bold"),
                axis.text=element_text(size=14, colour="black"),
                panel.grid=element_blank(),
                # panel.grid.major=element_line(colour="lightgrey", size=0.25),
                panel.grid.major=element_blank(),
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
                plot.title=element_text(face="bold", size=16),
				strip.text.y.left=element_text(angle = 0)
               )

	### function to generate distribution plot
	# inspired by https://sebastiansauer.github.io/shade_Normal_curve/
	#             https://sebastiansauer.github.io/normal_curve_ggplot2/
	gen.distribution.plot <- function( outvar, stat ){

		# data
		pdf.  <- pdf[ pdf$outvar %in% outvar, ]
		s95.  <- s95[ s95$outvar %in% outvar, ]
		stat. <- stat[ stat$outvar %in% outvar, ]
		m. <- m[ m$outvar %in% outvar, ]
		m.$x2 <- 0
		
		# limits 95%
		l. <- m.[m.$q %in% c("q025","q975"),]
		
		# strip labels
		stat.labs <- c( paste0( "P(", outvar, ")" ),
						paste0( "P(", outvar, "|", paste( 
											paste0( intvars, "=", intlevels ) ,
											collapse="," ) , ")" ),
						paste0( "P(", outvar, "|do(", paste(
											paste0( intvars, "=", intlevels ) ,
											collapse="," ) , "))" )
                        )
		names( stat.labs ) <- levels( pdf.$stat )

		### MH 0.0.3 2022-02-11
		### colored means 

		# rotate x/y over stat, twice
		m.1 <- m.[m.$q %in% "mean",]
		m.2 <- m.1
		m.2$x <- c(m.2$x[2:3],m.2$x[1])
		m.3 <- m.2
		m.3$x <- c(m.3$x[2:3],m.3$x[1])

		# total data set for means
		m.. <- do.call( "rbind", list( m.1, m.2, m.3 ) )

		# set pdf values for dashed lines
		m..$pdf.values[4] <- stats::dnorm( x=m..$x[4],
			mean=stat$mean[stat$stat %in% "smpl" & stat$outvar %in% outvar],
			sd=stat$sd[stat$stat %in% "smpl" & stat$outvar %in% outvar] )
		m..$pdf.values[7] <- stats::dnorm( x=m..$x[7],
			mean=stat$mean[stat$stat %in% "smpl" & stat$outvar %in% outvar],
			sd=stat$sd[stat$stat %in% "smpl" & stat$outvar %in% outvar] )
		m..$pdf.values[5] <- stats::dnorm( x=m..$x[5],
			mean=stat$mean[stat$stat %in% "cond" & stat$outvar %in% outvar],
			sd=stat$sd[stat$stat %in% "cond" & stat$outvar %in% outvar] )
		m..$pdf.values[8] <- stats::dnorm( x=m..$x[8],
			mean=stat$mean[stat$stat %in% "cond" & stat$outvar %in% outvar],
			sd=stat$sd[stat$stat %in% "cond" & stat$outvar %in% outvar] )
		m..$pdf.values[6] <- stats::dnorm( x=m..$x[6],
			mean=stat$mean[stat$stat %in% "causal" & stat$outvar %in% outvar],
			sd=stat$sd[stat$stat %in% "causal" & stat$outvar %in% outvar] )
		m..$pdf.values[9] <- stats::dnorm( x=m..$x[9],
			mean=stat$mean[stat$stat %in% "causal" & stat$outvar %in% outvar],
			sd=stat$sd[stat$stat %in% "causal" & stat$outvar %in% outvar] )
		
		# line colors for means
		line.colors.1 <- c( "#ed553b", "#3caea3", "#0568bf" )
		line.colors.2 <- c( line.colors.1[2:3], line.colors.1[1] )
		line.colors.3 <- c( line.colors.2[2:3], line.colors.2[1] )
		line.colors <- c( line.colors.1, line.colors.2, line.colors.3 )
		
		# line types for means
		line.types <- c( rep("dashed",3), rep("solid",6) )


		### generate plot
		p <- ggplot( data=pdf., aes( x=x, y=pdf.values ) )
		# limits 95%
		p <- p + geom_segment(data=l., aes(x=x, y=0, xend=x, yend=pdf.values),
															 show.legend=FALSE)
		# means
		p <- p + geom_segment(data=m.., aes(x=x, y=0, xend=x, yend=pdf.values,
					color=line.colors, linetype=line.types), show.legend=FALSE)
		p <- p + geom_line()
		p <- p + geom_area( data=s95., fill = "#000000",
													 color = NA, alpha = 0.20 )
		p <- p + facet_wrap( ~ stat,
							   ncol=1,
							   nrow=3,
							   strip.position="top",
							   labeller = labeller( stat = stat.labs ) )
		# MH 0.0.3 2022-02-11
		if( scales.free ){
			p <- p + scale_x_continuous( expand = expansion( mult = c(0, 0),
															 add=c(0, 0) ) )
		} else {
			p <- p + scale_x_continuous( limits=c(x.breaks[1],
										 x.breaks[length(x.breaks)]),
										 breaks=x.breaks, labels=x.labs,
										 expand = expansion( mult = c(0, 0),
															 add=c(0, 0) ) )
		}
		# MH 0.0.3 2022-02-11
		if( scales.free ){
			p <- p + scale_y_continuous( expand = expansion( mult = c(0, 0),
															 add=c(0, 0) ) )
		} else {
			p <- p + scale_y_continuous( limits=c(y.breaks[1],
										 y.breaks[length(y.breaks)]),
										 breaks=y.breaks,
										 labels=y.labs,
										 expand = expansion( mult = c(0, 0),
															 add=c(0, 0) ) )
		}
		p <- p + xlab( paste0( "\n", outvar ) )
		p <- p + ylab( "density function\n" )
		p <- p + theme
		# https://stackoverflow.com/questions/18252827/
		#                           increasing-area-around-plot-area-in-ggplot2
		# top right bottom left
		p <- p + theme( plot.margin = unit(c(15.5, 25.5, 5.5, 5.5), "points") )
		
		# return plot
		return( p )
	}

	# generate plots
	p.l <- sapply( outvars, gen.distribution.plot, stat, simplify=FALSE )

	# plot the plots
	if( plot ) {
		for( p in p.l ){
			grDevices::dev.new()
			plot( p )
		}
	}
	
	# output pdf plots
	if( !is.null( plot.dir ) && is.character( plot.dir ) && 
								  dir.exists( plot.dir ) &&
								  ( file.access( plot.dir, mode=2 ) %in% 0 ) ){
	
		# single plots (one for each non-interventional variable)
		for( i in seq( along = p.l ) ){
			p <- p.l[[i]]
			plot.path <- file.path( plot.dir, paste0( "distribution_plot_",
												names( p.l )[i], ".pdf" ) )
			ggsave( plot.path, p, width=297/1.5, height=297, units="mm" )
		}

		# all single plots in one plot 
		plots <- arrangeGrob( grobs=p.l,ncol=1 )
		if( !plot ) grDevices::dev.off()
		plot.path2 <- file.path( plot.dir, paste0( "distribution_plot_",
		                        paste( names( p.l ), collapse="_" ), ".pdf" ) )
		ggsave( plot.path2, plots, width=297/1.5, height=0+length(p.l)*250,
		                                                           units="mm" )

	}
	
	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version,
	                                                  " ", Sys.time(), "\n" ) )

	# return list of plots
	return( p.l )
}

### development
# user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
# Rfiles.folder <- file.path( user.profile,
                            # "Dropbox/68_causalSEM/04_martinhecht/R" )
# Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% "plot_distributions.R" ]
# for( Rfile in Rfiles ){
	# source( file.path( Rfiles.folder, Rfile ) )
# }


## test object 00_lavaan_test_object
# load( file.path( user.profile, 
      # "Dropbox/causalSEM_R_Package/test_object/00_lavaan_test_object.Rdata"))
# object00 <- intervention_effect(model=o00_lavaan_test_object,
									 # intervention="x2", intervention_level=2)
# p.l <- plot_distributions( object00,plot.dir="c:/users/martin/Desktop/plots")


## test object 01_lavaan_test_object
# load( file.path( user.profile, 
      # "Dropbox/causalSEM_R_Package/test_object/01_lavaan_test_object.Rdata"))
# object01 <- intervention_effect( model=o01_lavaan_test_object,
#									   intervention="x2", intervention_level=2)
# p.l <- plot_distributions( object01,plot.dir="c:/users/martin/Desktop/plots")


## test object 02_lavaan_test_object
# load( file.path( user.profile, 
      # "Dropbox/causalSEM_R_Package/test_object/02_lavaan_test_object.Rdata"))
# object02 <- intervention_effect( model=o02_lavaan_test_object,
#									   intervention="x2", intervention_level=2)
# p.l <- plot_distributions(object02, plot.dir="c:/users/martin/Desktop/plots")


## test object 03_lavaan_test_object
# load( file.path( user.profile, 
      # "Dropbox/causalSEM_R_Package/test_object/03_lavaan_test_object.Rdata"))
# object03 <- intervention_effect( model=o03_lavaan_test_object,
#									   intervention="x2", intervention_level=2)
# p.l <- plot_distributions( object03,plot.dir="c:/users/martin/Desktop/plots")


### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
