// Part 1 about finding and counting Knight's tours
//==================================================

object CW7a {

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(1a) function that tests whether the position 
//     is inside the board and not yet element in the path.

def is_legal(dim: Int, path: Path)(x: Pos) : Boolean = {
	if (x._1 < dim && x._1 >= 0 && x._2 < dim && x._2 >= 0) {

		if (!(path.contains(x))) {//true
		true }
		else { 
		false}
	}
	else { 
	false 
	}
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

//some test cases
//
//assert(legal_moves(8, Nil, (2,2)) == 
//  List((3,4), (4,3), (4,1), (3,0), (1,0), (0,1), (0,3), (1,4)))
//assert(legal_moves(8, Nil, (7,7)) == List((6,5), (5,6)))
//assert(legal_moves(8, List((4,1), (1,0)), (2,2)) == 
//  List((3,4), (4,3), (3,0), (0,1), (0,3), (1,4)))
//assert(legal_moves(8, List((6,6)), (7,7)) == List((6,5), (5,6)))

//(1c) two recursive functions below. 
//     They exhaustively search for knight's tours starting from the 
//     given path. The first function counts all possible tours, 
//     and the second collects all tours in a list of paths.

def count_tours(dim: Int, path: Path) : Int = {

	val count = 0

	if (path != Nil || dim < 5) {
	
		val legalMoves = legal_moves(dim, path, path.head)

			if (!legalMoves.isEmpty) {

				val listOfCounts = for (pos <- legalMoves) 
					yield count_tours(dim, pos::path)	
				
				val amountOnes = listOfCounts.sum
				
				val countUp : Int = count + amountOnes			
				
				countUp
						
			}
			
			else {

				if (path.length == dim * dim) {

					val countUp : Int = count + 1
					countUp
				}
				
				else {
					count
					
				}
			}
	}
	
	else {
	count
	}
	
}

def enum_tours(dim: Int, path: Path) : List[Path] = {
	
	if (path != Nil || dim < 5) {
	
		val legalMoves = legal_moves(dim, path, path.head)
				
		if (!legalMoves.isEmpty) {

			val listOfPaths = for (pos <- legalMoves)
			yield enum_tours(dim, pos::path).flatten
			
			val flatListOfTours = listOfPaths.flatten
				
			flatListOfTours.grouped(dim * dim).toList
				
		}
			
		else {	
		
			if (path.length == dim * dim) {

				List(path)
			}
				
			else {

				Nil
				
			}
		}
	}

	else {
	Nil
	}
}

}