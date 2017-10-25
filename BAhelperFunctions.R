w.min<-0
w.max<-3
g.min<-0
g.max<-0.3
# tweak to raise or lower tolerance of guessing 

#assumes ther is a column called rt 
filterByJoulicouer <- function(data) { 
	if(length(data$RT) == 0) { 
        print("JOLICOUER FUNCTION NEEDS COLUMN CALLED RT")
        return(NA)
	}
    
	#non-recursive jolicouer outlier method
	#max number of trials must be >= 100
	#must have at least 4 trials
	max.num.trials <- 1000
	jol.cutoff <- rep(NA,200)
	jol.cutoff[4] <- 1.458
	jol.cutoff[5] <- 1.68
	jol.cutoff[6] <- 1.841
	jol.cutoff[7] <- 1.961
	jol.cutoff[8] <- 2.050
	jol.cutoff[9] <- 2.12
	jol.cutoff[10] <- 2.173
	jol.cutoff[11] <- 2.22
	jol.cutoff[12] <- 2.246
	jol.cutoff[13] <- 2.274
	jol.cutoff[14] <- 2.31
	jol.cutoff[15] <- 2.326
	for(i in seq(15,20))
		jol.cutoff[i] <- ( (2.391 - 2.326) * ((i-15)/5)) + 2.326
	for(i in seq(20,25))
		jol.cutoff[i] <- ( (2.410 - 2.391) * ((i-20)/5)) + 2.391
	for(i in seq(25,30))
		jol.cutoff[i] <- ( (2.4305 - 2.410) * ((i-25)/5)) + 2.410
	for(i in seq(30,35))
		jol.cutoff[i] <- ( (2.45 - 2.4305)  * ((i-30)/5)) + 2.4305
	for(i in seq(35,50))
		jol.cutoff[i] <- ((2.48-2.45)*((i-35)/15))+2.45
	for(i in seq(50,100))
		jol.cutoff[i] <- ((2.50-2.48)*((i-50)/50))+2.48
	for(i in seq(100,max.num.trials))
		jol.cutoff[i] <- 2.50
	
	N <- length(data$RT)
	mean <- mean(data$RT)
	sd <- sd(data$RT)
    N.before = nrow(data)
	data <- data[abs(data$RT-mean)/sd < jol.cutoff[N],]
    N.after = nrow(data)
	return(list("Nremoved"=(N.before-N.after),"data"=data))
}


# ryan's version of image.plot() -- differences are that it draws a scatter plot, 
# you can pass in minz and maxz, and a fix for adding plots to other image plots
library(fields) 
image.plot.colors<-function (..., minz=NULL, maxz=NULL, add = FALSE, nlevel = 64, 
    horizontal = FALSE, 
    legend.shrink = 0.9, legend.width = 1.2, legend.mar = ifelse(horizontal, 
        3.1, 5.1), legend.lab = NULL, graphics.reset = FALSE, 
    bigplot = NULL, smallplot = NULL, legend.only = FALSE, col = tim.colors(nlevel), 
    lab.breaks = NULL, axis.args = NULL, legend.args = NULL, 
    midpoint = FALSE) 
{
    old.par <- par(no.readonly = TRUE)
    info <- image.plot.info(...)
    if (add) {
        bigplot <- old.par$plt
    }
    if (is.null(legend.mar)) {
        legend.mar <- ifelse(horizontal, 3.1, 5.1)
    }
    temp <- image.plot.plt(add = add, legend.shrink = legend.shrink, 
        legend.width = legend.width, legend.mar = legend.mar, 
        horizontal = horizontal, bigplot = bigplot, smallplot = smallplot)
    smallplot <- temp$smallplot
    bigplot <- temp$bigplot
    if(is.null(minz)) minz <- info$zlim[1]
    if(is.null(maxz)) maxz <- info$zlim[2]
    if (!add) {
        par(plt = bigplot)
    }
    if (!info$poly.grid) {
        #image(..., add = add, col = col)
    N<-length(list(...)$z)
	  col2<-rep("",N)
	  for (i in seq(1,N))
	  {
		col2[i]<-col[(list(...)$z[i]-minz)/((maxz-minz)/(length(col)-1))+1]
	  }
	  # suppress warning that z is unused
	  if (!add)
		suppressWarnings(plot(...,col=col2))
	  else
		suppressWarnings(points(...,col=col2))
    }
    big.par <- par(no.readonly = TRUE)
    if ((smallplot[2] < smallplot[1]) | (smallplot[4] < smallplot[3])) {
        par(old.par)
        stop("plot region too small to add legend\n")
    }
    ix <- 1
    binwidth <- (maxz - minz)/nlevel
    midpoints <- seq(minz + binwidth/2, maxz - binwidth/2, by = binwidth)
    iy <- midpoints
    iz <- matrix(iy, nrow = 1, ncol = length(iy))
    breaks <- list(...)$breaks
    par(new = TRUE, pty = "m", plt = smallplot, err = -1)
        axis.args <- c(list(side = ifelse(horizontal, 1, 4), 
            mgp = c(3, 1, 0), las = ifelse(horizontal, 0, 2)), 
            axis.args)
    if (!horizontal) {
        if (is.null(breaks)) {
            image(ix, iy, iz, xaxt = "n", yaxt = "n", xlab = "", 
                ylab = "", col = col)
        }
        else {
            image(ix, iy, iz, xaxt = "n", yaxt = "n", xlab = "", 
                ylab = "", col = col, breaks = breaks)
        }
    }
    do.call("axis", axis.args)
    box()
    if (!is.null(legend.lab)) {
        legend.args <- list(text = legend.lab, side = ifelse(horizontal, 
            1, 4), line = legend.mar - 2)
    }
    if (!is.null(legend.args)) {
        do.call(mtext, legend.args)
    }
    mfg.save <- par()$mfg
    #if (graphics.reset | add) {
    if (graphics.reset) {
        par(old.par)
        par(mfg = mfg.save, new = FALSE)
        invisible()
    }
    else {
        par(big.par)
        par(plt = big.par$plt, xpd = FALSE)
        par(mfg = mfg.save, new = FALSE)
        invisible()
    }
}
    


# Justin's weber model
weber.model<-function(w,r) {
        return(pnorm((r-1)/(w*sqrt(1+r^2))))
}

# Justin's weber model with the guess parameter
weber.model.guess<-function(params,r) {
        w<-params[1]; g<-params[2]
        if (g<g.min || g>g.max || w<w.min || w>w.max)
                return(NA)
        return((1-g)*weber.model(w,r)+g/2)
} 

# Justin's weber model with d, the shifty parameter 
# takes in ratio that's bigger/smaller, s.t. the axis is different on either side of 0 
weber.model.shifty<-function(params,r,quadrent=1) {
    w<-params[1]; d<-params[2] 
    #return(ifelse(quadrent == 1, pnorm((r-d-1)/(w*sqrt(1+(r-d)^2))), 1-(pnorm(((2-r)-d-1)/(w*sqrt(1+((2-r)-d)^2)))))) #Don't need this eq since we've already transformed the data s.t. 1:1 works both when left is right and right is right
    #return(ifelse(r >= 1+d, pnorm((r-d-1)/(w*sqrt(1+(r-d)^2))), 1-(pnorm(((r-d-1)/(w*sqrt(1+(r-d)^2))))))) #tried deciding quadrent by what subj picks >50% of the time -- no luck 
    #return(ifelse(quadrent == 1, pnorm((r-d-1)/(w*sqrt(1+(r-d)^2))), 1-(pnorm(((r-d-1)/(w*sqrt(1+(r-d)^2))))))) #this seems to work when d=0 and no other time... 
    #return(ifelse(quadrent == 1, pnorm((r-d-1)/(w*sqrt(1+(r-d)^2))), pnorm((((2-r)-d-1)/(w*sqrt(1+((2-r)-d)^2)))))) #this makes it taper off too quickly for q3
    #return(ifelse(quadrent == 1, pnorm((r-d-1)/(w*sqrt(1+(r-d)^2))), (pnorm(((r-d-1)/(w*sqrt(1+(r-d)^2))),lower.tail = TRUE)))) #this seems to work when d=0 and no other time... 
    #return(pnorm(q=r, mean=d+1, sd=w)) #normal built-in CDF 
    return(pnorm(q=r, mean=d, sd=w*sqrt(1+(r-d)^2))) #normal built-in CDF NB: d is centered
} 

# weber shifty with guess (w, d, g) 
weber.model.shifty.guess<-function(params,r,quadrent=1) {
  w<-params[1]; d<-params[2]; g<-params[3]
  forShifty <- c(w,d) 
  #((1-g)*weber.model(w,r)+g/2)
  return((1-g)*(weber.model.shifty(forShifty,r,quadrent))+(g/2)) 
} 

# Log likelihood function for the weber model
loglikelihood.weber<-function(w,r,y) {
        psi<-weber.model(w,r)
        return(sum(y*log(psi)+(1-y)*log(1-psi)))
}

# Log likelihood function for the weber model with g
loglikelihood.weber.guess<-function(params,r,y) {
        psi<-weber.model.guess(params,r)
        if (is.na(sum(psi)))
                return(-1e9)
        return(sum(y*log(psi)+(1-y)*log(1-psi)))
}

# Log likelihood function for the weber model with d (shifty) 
loglikelihood.weber.shifty<-function(params,r,y,quadrent=1) {
        psi<-weber.model.shifty(params,r,quadrent)
        if (is.na(sum(psi)))
                return(-1e9)
        return(-sum(y*log(psi)+(1-y)*log(1-psi))) #see if this logic makes sense 
        #return(-sum(log(psi)))
}

# Log likelihood function for the weber model with d (shifty) and g
loglikelihood.weber.shifty.guess<-function(params,r,y,quadrent=1) {
  psi<-weber.model.shifty.guess(params,r,quadrent)
  if (is.na(sum(psi)))
    return(-1e9)
  return(-sum(y*log(psi)+(1-y)*log(1-psi))) #see if this logic makes sense 
  #return(-sum(log(psi)))
}

# Find w by minimizing least-squares error
ls.weber<-function(r,y) {
        w.nls.start<-1.0                                                # set nls' starting w value
        w.nls.decrement<-0.3                                    # for backup w values
        w.nls<-w.nls.start
        d.temp<-data.frame(x=r,y=y)
        colnames(d.temp)<-c("r","y")
        H<-NA                                                           # the model information
        while (is.na(H)[1] && w.nls>w.min) {
                xx<-try(H<-nls(y~weber.model(w,r),data=d.temp,start=list(w=w.nls)),silent=T) # <---
                if (inherits(xx,"try-error")) {
                        #cat("-- failed value: w =",w.nls,"\n")
                        H<-NA
                        w.nls<-w.nls-w.nls.decrement            # decrement starting w if failed
                }
        }
        if (is.na(H)[1]) {
                #cat("-- could not solve the nonlinear model (least squares)\n")
        }
        else {
          if(coef(H)[[1]] > w.max) {
            return(NA)
          }
        }
        return(H)       # coef returns a list, [[1]] to get 1st el
}



# Find w by maximum likelihood estimation using optimize()
# optimize() searches the interval [...] for a maximum of the function f 
# with respect to its first argument.
# We cannot use optim() because optim() says "one-dimensional optimization by 
# Nelder-Mead is unreliable: use optimize"
mle.weber<-function(r,y) {
        optim.val<-optimize(loglikelihood.weber,
                                interval=c(w.min,w.max),
                                r=r,y=y,maximum=TRUE)
        return(optim.val)
}

# MLE weber with d -- use optim instead of optimize() 
mle.weber.shifty<-function(r,y,quadrent=1) {
        w.optim.start<-1.0                                      # set optim's starting w value
        w.optim.decrement<-0.3                                  # for backup w values
        w.optim<-w.optim.start
        d.optim<-0.01                                           # set optim's starting d value
        optim.val<-NA
        while (is.na(optim.val)[1] && w.optim>w.min) {
                xx<-try(optim.val<-optim(c(w.optim,d.optim),
                        loglikelihood.weber.shifty,
                        r=r,y=y,quadrent=quadrent, 
                        control=list(fnscale=-1)))
                if (inherits(xx,"try-error")) {
                        optim.val<-NA
                        w.init<-w.optim-w.optim.decrement       # decrement starting w if failed
                }
        }
        if (is.na(optim.val)[1]) {
                #cat("-- could not solve the nonlinear model (maximum likelihood w/ guess)\n")
        } 
        return(optim.val)
}

# Find w and g by maximum likelihood estimation using optim()
# optim() is general-purpose optimization based on the Nelder�Mead algorithm.
# We cannot use optimize() because that optimizes based on a single paramter
mle.weber.guess<-function(r,y) {
        w.optim.start<-1.0                                      # set optim's starting w value
        w.optim.decrement<-0.3                                  # for backup w values
        w.optim<-w.optim.start
        g.optim<-0.01                                           # set optim's starting g value
        optim.val<-NA
        while (is.na(optim.val)[1] && w.optim>w.min) {
                xx<-try(optim.val<-optim(c(w.optim,g.optim),
                        loglikelihood.weber.guess,
                        r=r,y=y,
                        control=list(fnscale=-1)))
                if (inherits(xx,"try-error")) {
                        optim.val<-NA
                        w.init<-w.optim-w.optim.decrement       # decrement starting w if failed
                }
        }
        if (is.na(optim.val)[1]) {
                #cat("-- could not solve the nonlinear model (maximum likelihood w/ guess)\n")
        }
        return(optim.val)
}

# Find w by minimizing least-squares error
ls.weber.guess<-function(r,y) {
        w.nls.start<-1.0                                                # set nls' starting w value
        w.nls.decrement<-0.3                                    # for backup w values
        w.nls<-w.nls.start
        d.temp<-data.frame(x=r,y=y)
        colnames(d.temp)<-c("r","y")
        H<-NA                                                           # the model information
        while (is.na(H)[1] && w.nls>w.min) {
                xx<-try(H<-nls(y~weber.model.guess(c(w,g),r),
                               algorithm="port",
                               data=d.temp,
                               lower=list(w=w.min,g=g.min),
                               upper=list(w=w.max,g=g.max),
                               start=list(w=w.nls,g=0)),
                        silent=T)
                if (inherits(xx,"try-error")) {
                        H<-NA
                        w.nls<-w.nls-w.nls.decrement            # decrement starting w if failed
                }
        }
        if (is.na(H)[1]) {
                #cat("-- could not solve the nonlinear model (least squares)\n")
        }
        return(H)       # coef returns a list, [[1]] to get 1st el
}

find.combined.w <- function(cv1, cv2) {
    ratio = 1
    ratios <- NA
    values <- NA
    for(i in seq(1,150,size=1000)) {
        m1 = 20
        m2 = m1*(1/ratio)
        
        m3 = m1 - m2
        sd1 = (m1*cv1)^2
        sd2 = (m2*cv2)^2
        sd3 = sqrt(sd1+sd2)       
        
        v = 1-(pnorm(0,mean=m3,sd=sd3))  
        values[i] <- v
        ratios[i] <- ratio
        ratio = ratio + 0.01;
    }
    
    plot(ratios,values,type="l")
    
    fit <- mle.weber(ratios,values)
    w = NA
    if(!is.na(fit)[1]) {
        w = fit$maximum
    }
    
    return(ratios=ratios,values=values,w)
}
