// Part 2 about finding a single tour for a board
//================================================

// copy any function from file knight1.scala

object CW7b {

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(1a) function that tests whether the position 
//     is inside the board and not yet element in the path.

def is_legal(dim: Int, path: Path)(x: Pos) : Boolean = {
	if (x._1 < dim && x._1 >= 0 && x._2 < dim && x._2 >= 0) {
		if (!(path.contains(x))) true 
		else false
	}
	else false 
}
		
//(1b) function that calculates for a position 
//     all legal onward moves that are not already in the path. 
//     The moves should be ordered in a "clockwise" manner.
 
def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
	val x1 = (x._1 +1, x._2 +2)
	val x2 = (x._1 +2, x._2 +1)
	val x3 = (x._1 +2, x._2 -1)
	val x4 = (x._1 +1, x._2 -2)
	val x5 = (x._1 -1, x._2 -2)
	val x6 = (x._1 -2, x._2 -1)
	val x7 = (x._1 -2, x._2 +1)
	val x8 = (x._1 -1, x._2 +2)
	
	val tempList = List(x1,x2,x3,x4,x5,x6,x7,x8)
	
	val legalList = for (pos <- tempList;
	if(is_legal(dim, path)(pos) == true))
	yield (pos) 
		
	legalList
}

//(2a) a first-function that finds the first 
//     element, say x, in the list xs where f is not None. 
//     In that case Return f(x), otherwise None. If possible,
//     calculate f(x) only once.

def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = {

	if (xs == Nil) {
		None
	}
	
	else {
	
		val fxsHead = f(xs.head)
	
		if ( fxsHead != None) fxsHead
		
		else {
			val anotherList = xs.drop(1)
			first(anotherList, f)
		}
		
	}
}

//(2b) function that uses the first-function for
//     trying out onward moves, and searches recursively for a
//     knight tour on a dim * dim-board.

def first_tour(dim: Int, path: Path) : Option[Path] = {

	val legalMoves = legal_moves(dim, path, path.head)
	
	if (path.length == dim*dim) {
		Some(path)
	}
	else {
		first(legalMoves, (x:(Pos)) => first_tour(dim, x::path))		
	}
}
	
}