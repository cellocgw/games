# Randomized Prim's Algorithm
# Choose a random cell within the maze grid (given by its width and height) and design it as a start cell.
# Add the start cell to (by now empty) inCells set.
# Mark cells around the start cell as frontier, i.e. add them to frontierCells set.
# While frontierCells set is not empty:
# Choose a random frontier cell cF from frontierCells.
# Choose a random in-cell cI adjacent to cF.
# Add cF to inCells.
# Mark all out-cells around cF as frontier.
# Add a path between cI and cF to the maze paths set.
# Remove cF from frontierCells set
#################
# Set slowly to any value to sleep that much per drawing cycle
pmaze <- function(dimx, dimy, slowly=FALSE, tcols,  ...) {
	if(missing(tcols) ) tcols <- rainbow((dimx+dimy)/10) # a wag at count
#plotting the full grid:
mardef<-par()$mar
par(mar=c(.1,.1,.1,.1))
#build init matrices; remember cells are ID'd by "top left" coord
icell<-matrix(0,nrow=(dimy),ncol=(dimx) )
fcell<-matrix(0,nrow=(dimy),ncol=(dimx) )
walls <- NULL #store indices of removed walls
dewall<-vector(length=4)
# select starting cell
initx<-sample(1:dimx,1) 
inity<-sample(1:dimy,1)
icell[inity,initx] <- 1
# calculate frontier cells from that cell.  Watch for borders!
# zero values are ignored, hence the mod func
fcell[inity, c(initx-1,(initx+1)%%(dimx+1) )]<-1
fcell[c(inity-1,(inity+1)%%(dimy+1) ), initx]<-1
#OK, so now fcell is nonempty, and can start the while loop
while (sum(fcell) > 0 ) {
	# pick a random frontier cell.  
	fronts <- which (fcell>0, arr.ind=TRUE)
	fgrab <- fronts[sample(1:nrow(fronts),1),] 
	doorx <- fgrab[2] + c(1,-1)
	doorx <- doorx[doorx %in% 1:dimx]
	doory <- fgrab[1] + c(1,-1)
	doory <- doory[doory %in% 1:dimy]
	# want only the "cross" 4 cells, and only the ones which are in Icell
	adjac<-NULL
	for (j in 1:length(doorx) ) if( icell[fgrab[1],doorx[j]]>0 ) adjac<-rbind(adjac,c(fgrab[1],doorx[j]) ) 
	for (j in 1:length(doory) ) if( icell[doory[j],fgrab[2]]>0 ) adjac<-rbind(adjac,c(doory[j],fgrab[2]) )
	# pick one at random
	igrab <- adjac[sample(1:nrow(adjac),1),] 
	# Now figure out the coordinates of the endpoints of this segment.
	#go from (max_row_ind,max_col_ind) and increment whichever index
	# was the same for both.
	dewall[2] <- max(igrab[1],fgrab[1])
	dewall[1] <- max(igrab[2],fgrab[2])
	dewall[4] <- dewall[2] + 1*(igrab[1]==fgrab[1])
	dewall[3] <- dewall[1] + 1*(igrab[2]==fgrab[2])
	# add it to a matrix.
	 walls <- rbind(walls,dewall)
#clean up
rownames(walls) <- NULL
colnames(walls) <- c('x1', 'y1','x2','y2')
# need to zero the frontier cell and unzero the matching icell
	fcell[fgrab[1],fgrab[2]] <-0
	icell[fgrab[1],fgrab[2]] <-1
# and set neighbors of fgrab to 1 in fcell.
# But there's more: do NOT assign a "former" frontier cell even if it's a 
# neighbor of the current fgrab.  This can be done by skipping neighbors
# whose icell value is 1.
	for (j in 1:length(doorx) ) 
		if( icell[fgrab[1],doorx[j]] == 0 ) fcell[fgrab[1], doorx[j]] <- 1
	for (j in 1:length(doory) ) 
		if( icell[doory[j],fgrab[2]] == 0 ) fcell[doory[j], fgrab[2]] <- 1 
# now rinse lather repeat. 
	} # end of while
# modify limits to make it easier to plot in/out labels
plot( c(1,(dimx+1),(dimx+1),1,1), c(1,1,(dimy+1),dimy+1,1),t='l',axes=F,xlab='',ylab='',xlim=c(0,dimx+2),ylim=c(0,dimy+2) )
 df = as.matrix(expand.grid( xvert= seq_len(dimx),yvert= seq_len(dimy)))
 dfv <- cbind(df,df[,1],df[,2]+1)
 df2 <- as.matrix(expand.grid(yvert= seq_len(dimy), xvert= seq_len(dimx)))
 dfh <- cbind(df2[,2],df2[,1],df2[,2]+1,df2[,1])
# concatenate each row w/ delimiter (so that 2_13 is not same as 21_3 )
#nb alternative methods as found on SO turn out to be much slower
 allwalls<-rbind(dfv,dfh)
 allrows<-unlist(sapply(1:nrow(allwalls),function(j) paste(allwalls[j,],collapse='_')) )
# the maze walls:
 allfoo <- unlist(sapply(1:nrow(walls),function(j) paste(walls[j,],collapse='_')))
 thewalls<-setdiff(allrows,allfoo)
 dowalls<-allwalls[allrows%in%thewalls,]
 ###################################################################
 #  New project: start at any wall touching border, and plot all segments in
 #	that 'tree,' defineds as segments w/ common endpoints. Then pick a remaining
 #	segment touching border, etc.  
 # Note: fortunately, which(dist(rbind(foo,empty_matrix))) returns nothing rather
 # than any crash.
 # pick any wall to start, remove it from starting set
 treecnt<-1
 #kill edge walls -oops , not equality but equal to 1 or nrow/ncol
 vedges <- which( (dowalls[,1]==dowalls[,3]) & (dowalls[,1]==1 | dowalls[,1]==dimx+1) )
 hedges <- which( (dowalls[,2]==dowalls[,4]) & (dowalls[,2]==1 | dowalls[,2]==dimy+1) )
 dowalls<-dowalls[-c(vedges,hedges),,drop=FALSE]
 # now sort into trees 
 # this appears to take rather a while.  What can I vectorize?
 while(nrow(dowalls)>0 ) {
	 tree <- matrix(dowalls[1,],nr=1) #force dimensions 
	 dowalls<-dowalls[-1,,drop=FALSE]
	 treerow <- 1 #current row of tree we're looking at
	 while ( treerow <= nrow(tree) ) {
 #only examine the first 'column' of the dist() matrix 'cause those are the
 # distances from the tree[] endpoints
# Still seems to me that one bigass dist(dowalls) matrix should be 
# sortable by identified row/column per tree[] element, thus saving
# an absurd amount of processing.  Let's get on that
		 touch <- c( which(dist(rbind(tree[treerow,1:2],dowalls[,1:2]) )[1:nrow(dowalls)]==0),  which(dist(rbind(tree[treerow,1:2],dowalls[,3:4]) )[1:nrow(dowalls)]==0), which(dist(rbind(tree[treerow,3:4],dowalls[,1:2]) )[1:nrow(dowalls)]==0), which(dist(rbind(tree[treerow,3:4],dowalls[,3:4]) )[1:nrow(dowalls)]==0) )
		 if(length(touch) ) {
			tree <- rbind(tree,dowalls[c(touch),])
			dowalls <- dowalls[-c(touch),,drop=FALSE] 
			}
# track the row of tree[] we're working with AND track how many rows there currently are in tree[]
		treerow <- treerow + 1 
	} #end of while treerow <= nrow 
	for (k in 1:nrow(tree) ) {
		lines(tree[k,c(1,3)],tree[k,c(2,4)] , col=tcols[treecnt%%(length(tcols)-1) +1 ] )
		if(slowly) Sys.sleep(slowly)
	 }
	treecnt <- treecnt + 1 
} #end of while dwalls exists
 # end of new project 
 # original method:
 # dowalls <- dowalls[order(rowSums(dowalls[,1:2])),]
 # timng is kinda cruddy here.  Experiment with (if(slowly & !j%%10)) sort of thing
 # for(j in 1:nrow(dowalls) ) {
	# lines(dowalls[j,c(1,3)],dowalls[j,c(2,4)],...)
	# if(slowly) Sys.sleep(slowly)
	# }
##
par(mar=c(mardef))
return(invisible(walls))
}
#
#################################
#
#  This will "kick off" my "fill dead end" function,
fillij <- function(wallmat, into=c(1,1), outof=c(apply(wallmat[,1:2],2, max)),...) {
dimx<-max(wallmat[,1])
dimy<-max(wallmat[,2])
ijfoo<-cbind(expand.grid(x=1:dimx,y=1:dimy), matrix(0, nc=4))
for(j in 1:(nrow(wallmat)) ) {
	xind <- wallmat[j,1]
	yind <- wallmat[j,2]
# Another win for DWIM!
	ijrow <- which(ijfoo[,1]==xind & ijfoo[,2]==yind)
	if( xind == wallmat[j,3])  {
	# OK, verified that initial ijfoo is dead on (at least for test case)
		ijfoo[ijrow,6]<- 1
# we never remove a vertical wall where x==1, so ijrow-1 is OK
		ijfoo[(ijrow-1),4] <- 1
	} else {
		#only other possibility is horiz wall
		ijfoo[ijrow,3] <- 1
# want the row with same x values
# and y values one less: ijrow- dimx , (dimx is the fast-changing index)
		ijfoo[ijrow-dimx,5] <- 1
	}
	} #end for (j) loop
	# now turn any deadend into an x,y,0,0,0,0 row and assign a zero to
	# the matching wall of its neighbor. But first...  
	# lockdown 'into' and 'outof' rows by setting to 5 and 6 
	oldijfoo<-ijfoo
	inrow <- into[1]+dimx*(into[2]-1)
	outrow <- outof[1] + dimx*(outof[2]-1)
	ijfoo[inrow,3:6]<-5
	ijfoo[outrow,3:6]<-6
	startijfoo<-ijfoo
	changed <- TRUE
	while (changed) {
		deads <- which(rowSums(ijfoo[,3:6]) == 1 )
# aha: if have two deads, and the first one cleans out the second one,
# that's where I get the zero-length fillwall!
		for (k in deads) {
			#find the live wall and so on. k= 1,2,3,4 --> N,E,S,W
			fillwall<-which(ijfoo[k,3:6] ==1 )
			if( length(fillwall) > 0) {
				ijfoo[k,(fillwall+2)] <- 0
				# and its neighbor...
				switch(fillwall,
					{ijfoo[(k - dimx),5] <- 0 },
					{ijfoo[(k + 1),6] <- 0 },
					{ijfoo[(k + dimx),3] <- 0 },
					{ijfoo[ (k - 1),4] <- 0} )
				}
			} #end for k in deads loop
	# reset start and end cells to 'lockout' value, just in case
		ijfoo[inrow,3:6]<-5
		ijfoo[outrow,3:6]<-6
		changed <- !identical(oldijfoo,ijfoo)
		oldijfoo<-ijfoo
		}
# so now ijfoo has been reduced to valid paths; all other rows sum to zero
# label start and stop 
	incell<-which(ijfoo[,3] == 5)
	outcell<-which(ijfoo[,3] ==6)
xfoo<-ijfoo[incell,1]
yfoo<-ijfoo[incell,2]
	text(xfoo-1.5*(xfoo==1)+ .5+ 1.5*(xfoo>=dimx), yfoo-(yfoo==1)+(yfoo>=dimy) + .5,'IN',font=2,cex=.6)
xfoo<-ijfoo[outcell,1]
yfoo<-ijfoo[outcell,2]
	text(xfoo-1.5*(xfoo==1)+ .5+ 1.5*(xfoo>=dimx), yfoo-(yfoo==1)+(yfoo>=dimy) + .5, 'OUT',font=2,cex=.6)
	return(invisible(ijfoo))
}

############################  draw the path
#
# wallmat: matrix generated with pmaze() containing "door" vertex coordinates
# into: coordinates of cell for start/entering the maze
# outof: coordinates of cell at finish of maze
# color: color of path drawn
# sleeptime: delay between increments of path drawing
# ... :  optional arguments compatible with lines() 
dopath<-function(wallmat, into=c(1,1), outof=c(apply(wallmat[,1:2],2, max)), color='red', sleeptime=0, ...) {
dimx<-max(wallmat[,1])
dimy<-max(wallmat[,2])
#  do input validation. But if someone sends a negative dim,poop on him
into <- c(min(into[1],dimx), min(into[2],dimy) )
outof <- c(min(outof[1],dimx), min(outof[2],dimy) )
# call fillij() here
 pathmat<-fillij(wallmat, into,outof )
# remove deadends - I think this is redundant, but leave in for the moment
pathmat <- pathmat[rowSums(pathmat[,3:6])>0,]
# Start at IN, find the location of distance==1, that may be in 
# either the IN row or IN column of distmat.  Draw the path, delete/change
# that "1" value and find the distance==1 for the adjacent cell we just found,
# and so on until  we find the "END" cell.
startcell<- which(pathmat[,3]==5)  
distmat<- dist(pathmat[,1:2]) # object vector class 'dist'
matdist<-as.matrix(distmat)
mbar<-lower.tri(matdist)*matdist #kill upper triangle
 # run until startcell has "6" in it
 while (pathmat[startcell,3] != 6 ) {
nextcell<- c(which(mbar[,startcell]==1),which(mbar[startcell,]==1))
# fix to "bad" distance==1 locations (with overkill):
 mbar[nextcell,startcell] <- 0
 mbar[startcell,nextcell] <- 0
## that works because I will never want to "find" startcell as a
## "nextcell" to some other cell. 
	if(length(nextcell)>1)  {
		mated<-vector(length=length(nextcell))
		startloc<-pathmat[startcell,1:2]  #move outside jj loop duh
		for( jj in 1: length(nextcell)) {
## compare Nvs S and E vs W . Want a pair to be "1" --necessary but not sufficient
		delcell <- ( pathmat[startcell,3:6] * pathmat[nextcell[jj],c(5,6,3,4)] )
## now reject NS or EW if it's in wrong direction, i.e. calculate which wall/door
## the two have in common.  if startcell is N of nextcell, y1<y2 . and so on.
		nextloc <- pathmat[nextcell[jj],1:2]
		matchwall<-c(startloc[2]>nextloc[2], startloc[1]<nextloc[1], startloc[2]<nextloc[2], startloc[1]>nextloc[1])
# mated is TRUE only when they share a door
		mated[jj]<- sum(delcell * matchwall )
		} 
## so now set actual nextcell to the mated==TRUE element
	nextcell<-nextcell[as.logical(mated)]
	} #end of if(length) block
# loop over NESW
	for (j in 3:6) {
	#  create the offsets for the given column
		offs <- c( 0.5*(-1)^(j==6), 0.5 + (j==4), 0.5*(-1)^(j==3), 0.5 +(j==5) )
		if ( pathmat[nextcell,j] == 1 ) {
			lines(pathmat[nextcell,1]+offs[1:2], pathmat[nextcell,2]+offs[3:4], col=color, ...)
		}
	}
	Sys.sleep(sleeptime)
	# update starting location
	startcell<-nextcell
} # end of while(not in END cell)
# get the  path 
return(invisible(pathmat)) 
}
#
############################   loop it
## edited to handle any maze w/ compatible walls output
# old "dopmaze" doesn't have a 'mazename' arg
#dopmaze <- function(reps,slowly=.01, sleep=.2) {
domaze <- function(reps,mazetype=c('rbmaze','pmaze'),slowly=.01, sleep=.2) {
cols <- c('red','blue','green','purple','pink')
dimrange <- 15:50
	for (j in 1: reps) {
# change to a do.call
#	foo <- pmaze(sample(dimrange,1),sample(dimrange,1),slowly=slowly )
	foo<-do.call(mazetype, args=list(dimx=sample(dimrange,1),dimy=sample(dimrange,1),slowly=slowly) )
# into and outof are forced to be on
# the perimeter. 
	dimx <- max(foo[,1])
	dimy <- max(foo[,2])
	insamp <- rbind(c(1,dimx,sample(2:(dimx-1),2)),c(sample(2:(dimy-1),2),1,dimy) )
	outsamp <- rbind(c(1,dimx,sample(2:(dimx-1),2)),c(sample(2:(dimy-1),2),1,dimy) )
	into <- c(insamp[,sample(1:4,1)] )
	outof <- c(outsamp[,sample(1:4,1)] )
# semiBUG: don't allow IN and OUT to be too close to each other, or
# the fact that dopath never zeroes out those rows will lead to a crash
# force separation of IN and OUT
	while (dist(rbind(into,outof))< 4 ) {
		outsamp <- rbind(c(1,dimx,sample(2:(dimx-1),2)),c(sample(2:(dimy-1),2),1,dimy) )
		outof <- c(outsamp[,sample(1:4,1)] )
	}
	dopath(foo, into=into, outof=outof, col=sample(cols,1), sleep = sleep)
	Sys.sleep(5)
	}
# just in case I'm interested.
outs <- list(fmaze=foo, into=into, outof=outof)
return(invisible(outs)) 
}

######################### a simple "replot" function to take the 
#   output of pmaze and redraw the maze
repmaze<-function(walls) {
	mardef<-par()$mar
	dimx<-max(walls[,1])
	dimy<-max(walls[,2])
	par(mar=c(.1,.1,.1,.1))
plot( c(1,(dimx+1)), c(1,(dimy+1)),t='n',axes=F,xlab='',ylab='')
# kill the sapplys if go to post-wall cell plotting 
sapply(1:(dimy+1), function(j) lines(c(1,dimx+1),c(j,j)) )->ssfoo
sapply(1:(dimx+1), function(j) lines(c(j,j),c(1,dimy+1)) )->ssfoo
for( j  in 1:nrow(walls) ) lines(walls[j,c(1,3)],walls[j,c(2,4)],col='white')
par(mar=c(mardef))
}