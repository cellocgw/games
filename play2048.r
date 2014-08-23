# for truly absurd fun, write 2048 for R
# This will be a cheap, sloppy version at best.  Use readline to get the next
# direction of motion and re-draw the grid with the new numbers. Track empty cells 
# and fill one randomly on each cycle.  Establish a "top-to-bottom" sort of algorithm
# to determine which pair of 3-in-a-row get merged.  And so on
# figure out options later, if any
# and how are points awarded? Wiki says value of new tile(s); also says
# new tile might be a 4 as well as a 2. hmmm... that's an input option to do
#Since it's 4x4, keep things simple by tracking cells in a 16-elt vector.
## for first round, just do ascii
#
# TODO
#
#
play2048<-function(brd=structure(list(score=0,brd=matrix(0,4,4)),class='brd2048') ) {

# init
#brd<-matrix(0,4,4)
if (! 'brd2048'%in%class(brd) ) stop('Input must be of class "brd2048".') 
	score <- brd$score
	brd <- brd$brd
# now check for newness
if ( !sum(brd) ) {
	newspot <- sample(1:16,2)
	brd[newspot] <- sample(c(2,4),2,rep=TRUE,prob=c(.9,.1))
	score <- 0
	}
print(brd)
#it's OK for board to be full, just not full AND no legal move!
# so add a comparison with "oldbrd" .  In fact, a good "tag" can replace the
## identical() test, I think. 
# probably need an initial tag value here, as
# tag<-c(FALSE,FALSE,FALSE)    
# then the conditional could be !(sum(tag)) 
youlose<-FALSE
while (length(which(brd==0)) || !youlose ) {
	#bustamove
	oldbrd <- brd # for later comparison
	move<-readline('u,d,l,r,s(ave)? ')
	# first flipflop matrix, do move, flopflip back

	switch(move,
		'u'=brd <- brd[4:1,] , # flip(brd,1),
		'l' = brd <- t(brd)[4:1,], #flip(t(brd),1),
		'r' = brd <- t(brd)[,4:1] , #flip(t(brd),2),
		'd' = "", #do nothing
		's' = {savebrd<-structure(list(score=score,brd=brd),class='brd2048'); assign('savebrd',savebrd,env=.GlobalEnv);print("Game saved to 'savebrd'");return(invisible() ) }   #just to bail out
		) 

getsq <- squoosh(brd)
brd<-getsq$brd
score <- score + getsq$score

	# re-orient board
	switch(move,
		'u'=brd<- brd[4:1,] , #flip(brd,1),
		'l' = brd <- t(brd)[,4:1], #flip(t(brd),2),
		'r' = brd <- t(brd)[4:1,],
		'd' = '' ) # flip(t(brd),1) )
## check that it was a legal, i.e. productive, move
## or the board is full and jammed.  So when the board is full, 
## AND the selected move caused no change, need to "call" all 3 other moves
## to see if any produce a change, and if so, skip to "illegal move try again"
## option.  
	if ( identical(brd, oldbrd) ) {
		if(!length(which(brd==0))) {
	# here is where I'll need to cycle thru (u,d,l,r)!%in% move , so to speak
	# and if any of them get !identical, then want 'try again'
		trymov<- c('u','d','l','r') 
		trymov<-trymov[! trymov%in% move]
		idtag<-vector(length=3)
		tmpbrd<-brd
		for( jj in 1:3) {
			switch(trymov[jj],
			'u'=tmpbrd<- tmpbrd[4:1,] , #flip(tmpbrd,1),
			'l' = tmpbrd <- t(tmpbrd)[,4:1], #flip(t(tmpbrd),2),
			'r' = tmpbrd <- t(tmpbrd)[4:1,],
			'd' = '' )
	# notice that for comparison purpboses, I don't need to reorient the temp
			idtag[jj]<- identical(tmpbrd,squoosh(tmpbrd)$brd)
		}
	#OK, now if idtag has any FALSEs, the game ain't over.
		youlose <- !(FALSE%in%idtag)
		if(youlose) {
			print("Game Over")
			break # get out of while loop
			} else print("Illegal move: try again.") 
		} else print("Illegal move: try again.") 
# that finished if (!length(which(brd==0
# the following "else" refers back to if (identical  
		} else { 
	print(brd)
	# add new tile. AGGGGGGGHHHHH remember what happens if the first argument
	# is a single integer? you get the whole damn string! use my 'cheapfix' 
	newspot <- sample(rep(which(brd==0),2),1)
	brd[newspot] <- sample(c(2,4),1,prob=c(.9,.1))
	print("updating...")
	print(brd)
	} #end of ifelse 
} #end while
# might be nice to return max tile as well. Notice I don't return a brd2048-class
# element 'cause we only get here when the board is full and busted.
return(invisible( list(score=score, maxtile=max(brd), brd=brd) ) )
}

########### crunching engine
squoosh <- function(brd) {
	#internal score only
	score<-0
	for (j in 1:4) {
		#drop column as far as possible 
		brd[,j]<-brd[order(as.logical(brd[,j])),j]
		for(k in 4:2) {
			if(brd[k,j]==brd[(k-1),j] & brd[k,j]>0 ) {
				brd[(k-1):k,j]<- c(0,2*brd[k,j])
		# don't drop or re-squoosh here because that could add an existing '4' to
		# a newly minted '2+2-->4' and that's not how it works.
		# every collapse adds new tile val to score
				score <- score + brd[k,j]
			}
		}
	# repeat drop (but do NOT repeat sqooshing)
		brd[,j]<-brd[order(as.logical(brd[,j])),j]
	} #end all board moves

return(invisible(list(brd=brd,score=score) ) )
}
