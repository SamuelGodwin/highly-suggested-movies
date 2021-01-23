// Part 3 about finding a single tour using the Warnsdorf Rule
//=============================================================

// copy any function from files knight1.scala and
// knight2.scala

object CW7c {

import scala.annotation.tailrec

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

@tailrec
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

//(3a) function that calculates a list of onward
//     moves like in (1b) but orders them according to Warnsdorf’s 
//     rule. That means moves with the fewest legal onward moves 
//     should come first.

def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
	
	val newPath = legal_moves(dim, path, x) // legal list for which to see how many further options 
	
	val lengthList = for (option <- newPath) yield {
		legal_moves(dim, path, option).length
	}
	
	val pairKeys = (0 to newPath.length-1).toList
	val lenKeys = (0 to lengthList.length-1).toList

	val pairMap = (pairKeys zip newPath) toMap
	val lenMap = (lenKeys zip lengthList) toMap
	
	val olGrowingList = List()
	
	val warnsPath = rec_list(pairMap, lenMap, olGrowingList).reverse

	warnsPath
	
}


def rec_list(pairMap : Map[Int, Pos], lenMap : Map[Int, Int], olGrowingList : Path) : Path = {

	if (!(lenMap.isEmpty)) {
	// new list append previous growing list with
	
		val keyToRemove = lenMap.minBy(_._2)._1
		
		val growingList : Path = pairMap.get(keyToRemove).get :: olGrowingList
		
		val newLenMap = lenMap - keyToRemove
		
		val listToRet = rec_list(pairMap, newLenMap, growingList)
		
		listToRet
		
	}
	else olGrowingList
}


//(3b) function that searches for a single *closed* 
//     tour using the ordered moves function.

def first_closed_tour_heuristic(dim: Int, path: Path) : Option[Path] = {
	
	val orderedMoves = ordered_moves(dim, path, path.head)
	
	val startPos : Pos = (dim/2, dim/2)
	
	if (path.length == dim*dim) {
		
		// if path head is within 1 knight's move of the beginning ...
		
		val x1 = (path.head._1 +1, path.head._2 +2)
		val x2 = (path.head._1 +2, path.head._2 +1)
		val x3 = (path.head._1 +2, path.head._2 -1)
		val x4 = (path.head._1 +1, path.head._2 -2)
		val x5 = (path.head._1 -1, path.head._2 -2)
		val x6 = (path.head._1 -2, path.head._2 -1)
		val x7 = (path.head._1 -2, path.head._2 +1)
		val x8 = (path.head._1 -1, path.head._2 +2)
		
		if ((x1 == startPos) | (x2 == startPos) | (x3 == startPos) | (x4 == startPos) | (x5 == startPos) | (x6 == startPos) | (x7 == startPos) | (x8 == startPos)) {
	
			Some(path)
		
		}	
		else {
			
			first(orderedMoves, (x:(Pos)) => first_closed_tour_heuristic(dim, x::path))		

		}
	}
	else {
		first(orderedMoves, (x:(Pos)) => first_closed_tour_heuristic(dim, x::path))		
	}
}


//(3c) Same as (3b) but searches for *non-closed* tours. However, 
//     have to be careful to write a tail-recursive version as this 
//     function will be called with dimensions of up to 40 * 40.

def first_tour_heuristic(dim: Int, path: Path) : Option[Path] = {
	
	val orderedMoves = ordered_moves(dim, path, path.head)
	
	val startPos : Pos = (dim/2, dim/2)
	
	if (path.length == dim*dim) {
	
		Some(path)
		
	}
	else {
		first(orderedMoves, (x:(Pos)) => first_tour_heuristic(dim, x::path))		
	}
	
}

}