digitizer <- function(n = NA, files = tk_choose.files(), mode = c('contour', 'perpendiculars'), title = T, randomOrder = T, closeDev = T, col = "red", lines = T, rmBroken = T, savePlot = F)
# Digitizes landmarks on graphic file(s).
# n		Maximum required number of landmarks or number of
#		perpendiculars to be drawn. If not specified, press the right
#		mouse button to stop.
# mode		'contour': nothing is drawn, you're asked to put landmarks by
#		locator; 'perpendiculars': you're asked to draw a line to which
#		a specified number (n) of perpendiculars will be drawn. You may
#		then put landmarks on the intersections of perpendiculars with
#		some countour.
# title		Whether or not to title each picture.
# randomOrder	Should pictures be given in random order to avoid the
#		influence of learning?
# closeDev	Close or not each picture after digitizing.
# col, lines	Are transferred to 'locator'
# rmBroken	If TRUE, elements that contain one point only would be removed
#		from the resulting list.
#		This way inconvenient pictures may be treated.
# savePlot	Save a copy of each picture with landmarks drawn or not?
{
	sapply(c('readbitmap', 'grDevices', 'tcltk'), require, character = T)
	if (randomOrder) files <- files[sample(m <- length(files), m)]
	P <- vector(mode = "list", length = length(files))
	for (i in 1:length(files)) {
	   im <- read.bitmap(files[i], native = T)
	   # Scaling images to 7 inches height to fit the screen and 
	   # to sample equally precise both small and large ones:
	   sc <- dim(im)[1]/700
	   dims <- c(dim(im)[1]/sc/100, dim(im)[2]/sc/100)
	   x11(width = dims[2], height = dims[1], xpos = 0, ypos = 0)
	   par(mai=c(0, 0, 0, 0))
	   plot.new()
	   plot.window(xlim = c(0, dims[2]), ylim = c(0, dims[1]))
	   rasterImage(im, 0, 0, dims[2], dims[1])
	   if (title) {
	      text(dims[2]/2, dims[1]-0.2,
	         paste(basename(files[i]), '\n', i, '/', length(files)),
	         col = 'white')
	   }
		   mode <- match.arg(mode)
           if (mode == 'perpendiculars') {
              perps <- function() {
				 n <- ifelse(is.numeric(n), n, 10)
             	 pts <- locator(2, type = 'o', pch = 20, col = 'blue')
              	 a <- -1/((pts$y[2]-pts$y[1])/(pts$x[2]-pts$x[1]))
              	 if (is.infinite(a)) a <- 1e16; if (a == 0) a <- 1e-16
              	 b1 <- pts$y[1] - a*pts$x[1]; b2 <- pts$y[2] - a*pts$x[2]
              	 for (i in seq(b1, b2, length = n)) abline(i, a, col = 2) 
              }
              perps()
	   }
	   if (is.na(n)) n <- 1000
	   p <- list(0)
	   for (ii in 1:n) {
		   p[[ii]] <- if (!is.null(tmp <- locator(1, type = ifelse(lines, "o", "p"), pch = 16, col = col))) tmp else break
		   text(p[[ii]], lab = ii, pos = 4, cex = 0.7, col = 'blue')
	  }
	   p <- matrix(unlist(p), ncol = 2, byrow = T, dimnames = list(NULL, c('x', 'y')))
	   # Scaling back:
	   P[[i]] <- p*sc
	   names(P)[i] <- basename(files[i])
	   if (savePlot) savePlot(file = paste0('land_', basename(files[i])))
	   if (closeDev) dev.off()
	}
	if (rmBroken) P[which(sapply(P, length) == 2)] <- NULL
	return(P)
}
