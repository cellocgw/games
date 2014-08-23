# because why not
#	Arguments: (for now, require base and roll to be integers)
#	base: static circle diameter
#	roll: moving circle diameter
#	draw: radius of roll circle where pen is
#	pencol: vector of color designators
#	inout: "in" or "out" for placement of draw circle on roll circle
#	length: how many times around base circle to draw. If I did it right, for 
#		comensurable circles this will close the figure.
#	maxlen: cut off the pattern when wiseasses load radii 37, 3457.
#
#  TODO

#	--BUG fix frame size -- for an outside case with pen > roll, current algorithm
#		fails. 
#	-- Add option for an angle offset of starting position. This allows overlay of a
#		pattern, or alignment of inside vs outside (since pen starts at non-mirrored
#		 position wrt direction of rotation).
#	-- Animate: redraw with a moving roll circle, a red dot at the pen point, etc.
#
# if e.g. 
# roll == base/2.5, then it does 2.5 rotations per cycle, implying 2 cycles required.
# but '2.5' means 5 rotations = 2 base circumfs, or 5:2 radii. could be (2,5) or
# say (6,15) , and in either case roll/gcd(b,r) is roll/gcd .
spiro <- function(base=6, roll=5, draw=1, inside=TRUE,  maxlen=100, plotit=TRUE, pencol='red', offang = 0, ... ){
# and the rest is just sines and cosines :-)
if (length(base) > 1 ) {
	warning("length(base) > 1; only first value used")
	base <- base[1]
	}
# locate pen start at 0,0 radians (base,draw) and initial radius as follows from inputs.
incr <- pi/250 #may change this stepsize
for (j in 1: length(roll) ) runlen <- roll/gcd(base,roll[j])
#change to pmin to get vector of reps
reps <- 2*pi/incr * pmin(runlen,maxlen)
# This version of graph limits appears compatible with all draw vs. roll sizes.
lims <- max(base, base+draw +  roll*(-1)^inside)
# base circle 
par(mar=c(.1,.1,.1,.1))
plot(base*sin(seq(0,2*pi,by=.01)),base*cos(seq(0,2*pi,by=.01)),t='l',xlab='',ylab='',axes=FALSE,xlim=c(-lims,lims),ylim=c(-lims,lims),asp=1 )
# Now, here, I think I'll calculate angb and angr inside the per-pattern loop.
# Must be a cleaner way to set up all these lengths!
rp <- max(length(draw), length(roll), length(inside), length(offang) )
draw <- rep(draw, length=rp)
roll <- rep(roll, length=rp) 
pencol <- rep(pencol,length=rp)
reps <- rep(reps, length=rp)
inside <- rep(inside, length=rp)
offang <- rep(offang, length=rp)
# now just stick 'jk' index all over the place in the loop.
for (jk in 1:length(roll) ){
angb <- seq(0,by=incr,length=reps[jk])  #may need to lengthen by one element
# when outside, flip sign; I think this is where offang comes into play
angr <- angb * (1 +(-1)^inside[jk] * base/roll[jk]) +offang[jk]
#angr <- angb * (1 - base/roll[jk]) 

# need a switch or something to deal with inside==FALSE . Do later.
	# get location of rollcircle in cartesian 
	# here, do want to subtract since rollcenter is 'roll' inwards from base circle
	# (at least so long as inout=='in'
	# same change in sign for outside
	rollx <- (base + (-1)^inside[jk] * roll[jk])*cos(angb)
	rolly <- (base + (-1)^inside[jk] * roll[jk])*sin(angb)
	# find pen location relative to center of rollcircle, in "local" cartesian
	dx <- (draw[jk])*cos(angr)
	dy <- (draw[jk])*sin(angr)
	# add cartesian vectors and attach to array of coords
	newx <- dx + rollx 
	newy <- dy + rolly 
	pen <- cbind(newx,newy)
	#points(newx,newy,pch='.') # or lines below
for (j in 2:reps[jk]) {
		lines(pen[(j-1):j,1], pen[(j-1):j,2],col=pencol[jk], ...)
		Sys.sleep(.005) #to watch the pattern grow
	# if want to plot roll as well, then need to redraw the entire plot:
	#	if(rollit) {
	#		#same initialization
	#		#draw roll centered at angb
	#		# draw all of pen
	#	}
	}
}
return(invisible(pen))
# restore graphics par
par( mar=c(5,4,4,2)+0.1 ) #default settings
}

