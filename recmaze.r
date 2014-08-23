### recursive division maze

# Begin with an empty field.
# Bisect the field with a wall, either horizontally or vertically. Add a single passage through the wall.
# Repeat step #2 with the areas on either side of the wall.
# Continue, recursively, until the maze reaches the desired resolution.
# Recommended: look at aspect ratio of each new "field" and choose vert or horiz
# wall to avoid long skinny passages. 
# http://weblog.jamisbuck.org/2011/1/12/maze-generation-recursive-division-algorithm  
# likes following one 
# 'sequence' of subdivisions until a field is at the cell level, then backing to 
# some other field.



## recursive backtracking maze

# Make the initial cell the current cell and mark it as visited
# While there are unvisited cells
	# If the current cell has any neighbours which have not been visited
		# Choose randomly one of the unvisited neighbours
		# Push the current cell to the stack
		# Remove the wall between the current cell and the chosen cell
		# Make the chosen cell the current cell and mark it as visited
	# Else if stack is not empty
			# Pop a cell from the stack
			# Make it the current cell
		# Else
			# Pick a random unvisited cell, make it the current cell and mark it as visited

rbmaze<-function( dimx, dimy, slowly=FALSE, tcols, ...) {
require(reshape2)
mardef<-par()$mar
par(mar=c(.1,.1,.1,.1))
#build init matrices; remember cells are ID'd by "top left" coord
scell<-matrix(0,nrow=(dimy),ncol=(dimx) )
icell<-melt(scell)
walls <- NULL #store indices of removed walls
dewall<-vector(length=4)
# select starting cell -- rumor has it that the starting cell
# should be at 0,0; dunno why, tho.
initx<-sample(1:dimx,1) 
inity<-sample(1:dimy,1)
icell[icell[,1]==inity & icell[,2]==initx,3] <- 1
# let's build my stack.  set scell[i,j] to -1 when all neighbors are visited
stackcount<-1
scell[inity,initx]<-stackcount
stackcount<-stackcount + 1  
# now loop until all cells have been visited... I think
while( any(0%in% icell[,3] )) {
# pick a wall to remove; for initial cell, don't need to check whether
# neighbor has been visited.  BUT need to check for boundary
# 1234= NESW
##watch for index crash at edges -- gotta be a smoother way
	neighborx <- initx+c(-1,1)
	neighborx <- neighborx[neighborx>0 & neighborx <=dimx]
	neighbory <- inity+c(-1,1)
	neighbory <- neighbory[neighbory>0 & neighbory <=dimy]
# find out how many neighborxy
# values showed up and sample amongst them. 
	visith <- icell[(icell[,1]==inity & abs(icell[,2]-initx)==1 &icell[,3]==0),]
	visitv <- icell[(abs(icell[,1]-inity)==1  & icell[,2]==initx &icell[,3]==0),]
	visits<-rbind(visith,visitv)
# in case all neighbors are already visited, skip the "build"
	if( nrow(visits) ) {
		pickit <- sample(1:nrow(visits),1)
		newy <- visits[pickit,1]
		newx <- visits[pickit,2]
		scell[newy,newx]<-stackcount
##see same-ish algorithm for "dewall" in pmaze
		kwall<- c(max(initx,newx), max(newy,inity), max(newx,initx)+1*(initx==newx), max(newy,inity) + 1*(inity==newy) )
		walls<-rbind(walls, kwall)
	#Finished off the work when visits[] had rows 
	} else {
# just pick a new place to start; new approach is to back up the stackcount
		scell[inity,initx]<- -1  #no nonvisited neighbors; removes current
#			"stack count" value from array
		newcoord<-which(scell==max(scell),arr.ind=TRUE)
		newx<-newcoord[2]
		newy<-newcoord[1]
	} # end of if else
	stackcount<-stackcount+1
	initx <- newx
	inity <- newy
	icell[icell[,1]==inity & icell[,2]==initx,3] <- 1 
	scell[inity,initx]<-stackcount
} #end of while any 0
#
## now plot the maze, using same techniques as in pmaze.  
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
# original method, since I'm not sure "trees" exist in this maze yet.
dowalls <- dowalls[order(rowSums(dowalls[,1:2])),]
 # timng is kinda cruddy here.  Experiment with (if(slowly & !j%%10)) sort of thing
for(j in 1:nrow(dowalls) ) {
	 lines(dowalls[j,c(1,3)],dowalls[j,c(2,4)],...)
	 if(slowly) Sys.sleep(slowly)
	 }
#cleanup
par(mar=c(mardef))
return(invisible(walls))
}
