package ai4p.reasoning.queens

// The colours we can play with
enum Colour(val letter:Char):
    case Red extends Colour('r')
    case Orange extends Colour('o')
    case Yellow extends Colour('y')
    case Green extends Colour('g')
    case Blue extends Colour('b')
    case Pink extends Colour('p')
    case White extends Colour('w')
    case Dark extends Colour('d')
    case Cyan extends Colour('c')
    case Purple extends Colour('u')
    
/** all the different directions a straight line can go in*/
enum Direction(val dx:Int, val dy:Int):
    case North extends Direction(0, -1)
    case NorthEast extends Direction(1, -1)
    case East extends Direction(1, 0)
    case SouthEast extends Direction(1, 1)
    case South extends Direction(0, 1)
    case SouthWest extends Direction(-1, 1)
    case West extends Direction(-1, 0)
    case NorthWest extends Direction(-1, -1)

// A list of all the directions, North, NorthEast, etc.
def allDirections:Seq[Direction] = Direction.values.toSeq    

/** A location on the board. Zero-indexed. Location is a type alias for (Int, Int). e.g. (0, 0) is the top-left */
type Location = (Int, Int)

/** 
 * Defines some methods we can call on Locations. Note, not quite as many as we put into the Reversi game.
 */
extension (l:Location) {
    // Lets us add a direction to a location to step in that direction
    // e.g. (1, 1) + Direction.North == (1, 0)
    def +(dir:Direction):Location = 
        val (x, y) = l
        (x + dir.dx, y + dir.dy)

}

// Squares can contain a queen or a blank
enum Contents:
    case Queen
    case Blank

// A row of coloured squares
type Row = Seq[Colour]

// A sequence of rows makes up our coloured grid
type Grid = Seq[Row]

// To solve the grid, we keep track of what's possible
// i.e. for every location, we say "this could contain a Queen or a Blank"
// and for some squares, we'll get to eliminate "Queen" and for other squares we'll get to eliminate "Blank"
type PossibilityMap = Map[Location, Set[Contents]]

/** Methods we can call on Grids */
extension (grid:Grid) {

    /** implemented for you, the Set of colours present in the grid */
    def colours:Set[Colour] = 
        (for 
            row <- grid
            col <- row
        yield col).toSet

    /** Implemented for you, looks up what colour a square in the grid is, if it's on the board */
    def apply(location:Location):Option[Colour] = 
        val (x, y) = location
        if grid.allSquares.contains(location) then Some(grid(y)(x)) else None

    /** implemented for you, all the squares of a particular column */
    def column(x:Int):Seq[Location] = for y <- grid.indices yield (x, y)

    /** implemented for you, all the squares of a particular row */
    def row(y:Int):Seq[Location] = for x <- grid(y).indices yield (x, y)

    /** implemented for you, all the locations on the board */
    def allSquares:Seq[Location] = 
        for 
            y <- grid.indices
            x <- grid(y).indices 
        yield (x, y)

    /** implemented for you, all the squares of a particular colour */
    def squares(colour:Colour):Seq[Location] =
        for sq <- allSquares if grid(sq).contains(colour) yield sq    

    // The neighbouring squares that are in the grid
    def neighbours(loc:Location):Seq[Location] =
        for 
            d <- Direction.values.toSeq
            p = loc + d if grid.allSquares.contains(loc)
        yield p

    // Generates a possibility map that thinks any square could be a Queen or a Blank
    def allPossibilities:PossibilityMap = 
        (for 
            y <- grid.indices
            x <- grid(y).indices
        yield (x, y) -> Contents.values.toSet).toMap
        

}

object Grid {
    // Reads in a grid of colours from a string like
    //
    // wwrb
    // wrrb
    // rrrb
    // bbbb
    def fromPrettyString(s:String):Grid = 
        for line <- s.linesIterator.toSeq yield {
            // The .get here is slightly poor practice ("what if it didn't match?")
            // but as we're creating the pretty strings ourselves, we want it to fail if there's an unrecognised character, which it will
            line.map { (c) => 
                Colour.values.find(_.letter == c) match {
                    case Some(col) => col
                    case _ => throw RuntimeException(s"Grid contained $c which I don't have a colour for")
                }
            }
        } 
}


/** Some methods on possibility maps */
extension (map:PossibilityMap) {

    // A map is solved if we know whether every square is a Queen or a Blank
    def solved:Boolean = 
        map.forall((loc, contents) => contents.size == 1)

    // All locations that could contain a queen, from what we know so far
    def queenLocations:Seq[Location] = 
        for (loc, contents) <- map.toSeq if contents.contains(Contents.Queen) yield loc


    /** Sets a square to be a Queen, also marking its neighbours and other squares in the colour, row, and colum to be Blank */
    def setQueen(grid:Grid, loc:Location):PossibilityMap = 
        // If this square is a Queen, these can't be
        val thereforBlank:Seq[Location] =
            grid.column(loc._1).filter(_ != loc) ++  // The other squares in the column            
            grid.row(loc._2).filter(_ != loc) ++ // The other squares in the row
            {
                grid(loc) match {
                    case Some(c) => grid.squares(c) // The other squares of the same colour
                    case _ => throw RuntimeException(s"Looked up a square that had no colour")
                }   
            } ++ 
            grid.neighbours(loc) // The neighbouring squares

        map ++ thereforBlank.map(_ -> Set(Contents.Blank)) + (loc -> Set(Contents.Queen))

    /** Sets a square to be a Blank. Takes the grid as input for consistency of API with setQueen */
    def setBlank(grid:Grid, loc:Location):PossibilityMap = 
        map + (loc -> Set(Contents.Blank))

    /** Returns true if there are two definite Queens in this map that are touching */
    def hasTwoQueensTouching(grid:Grid):Boolean = 
        grid.allSquares.exists((l) => map(l) == Set(Contents.Queen) && grid.neighbours(l).exists((ll) => map(ll) == Set(Contents.Queen)))

    /** Implement this. Returns true if a row in this map definitely breaks a rule. i.e. it has 2 definite queens or is all definite blanks */
    def rowFails(grid:Grid):Boolean = 
        grid.indices.exists { (y) => 
            val row = grid.row(y)
            row.count(map(_) == Set(Contents.Queen)) > 1 || row.count(map(_).contains(Contents.Queen)) < 1 
        }

    /** Implement this. Returns true if a colum in this map definitely breaks a rule. i.e. it has 2 definite queens or is all definite blanks */
    def columnFails(grid:Grid):Boolean = 
        grid(0).indices.exists { (x) => 
            val column = grid.column(x)
            column.count(map(_) == Set(Contents.Queen)) > 1 || column.count(map(_).contains(Contents.Queen)) < 1 
        }

    /** Implement this. Returns true if a colour in this map definitely breaks a rule. i.e. it has 2 definite queens or is all definite blanks */        
    def colourFails(grid:Grid):Boolean = 
        grid.colours.exists { (c) => 
            val sqs = grid.squares(c)
            sqs.count(map(_) == Set(Contents.Queen)) > 1 || sqs.count(map(_).contains(Contents.Queen)) < 1 
        }

    /** For the last task in the assignment. You can add another logical rule that can invalidate a grid if it's impossible - 
     *  e.g. that if 2 colours only have the same 1 column free, it's invalid */
    def extraRuleFails(grid:Grid):Boolean = 
        false

    // Returns true if this set of possibilities breaks a rule -- e.g. doesn't have queens in a row, column, or colour, has two queens touching,
    // or definitely has two queens in a row, column, or colour    
    def invalidFor(grid:Grid):Boolean = 
        hasTwoQueensTouching(grid) || rowFails(grid) || columnFails(grid) || colourFails(grid) || extraRuleFails(grid)
     

    // You need to implement this. Make some progress in solving the grid
    // You should look for:
    // 
    // * a square where setting it to a blank would make the new board invalid (in which case it must be a queen)
    // * a square where setting it to a queen would make the new board invalid (in which case it must be a blank), or
    //
    // There is one grid that this technique won't solve. You'll need to add another logical rule for if the first two didn't find anything
    // e.g. if there are now two colours that can only have queens in the same two columns, no other colour could have a queen in those columns
    // I created a function "extraRuleFails" where you could implement your rule; or you could do it directly here.
    def makeAstep(grid:Grid):PossibilityMap =
        //???

        grid.allSquares.find((sq) => map(sq).size > 1 && map.setBlank(grid, sq).invalidFor(grid)) match {
            case Some(p) => 
                map.setQueen(grid, p)
            case None => 
                grid.allSquares.find((sq) => map(sq).size > 1 && map.setQueen(grid, sq).invalidFor(grid)) match {
                    case Some(p) => 
                        map.setBlank(grid, p)
                    case None =>                         
                        println("none")
                        map
                }
        }




}

val puzzles = Map(

    "#77" -> Grid.fromPrettyString("""|ppoopbbb
                                      |pgoopbbb
                                      |pwwppbbb
                                      |pwwppppp
                                      |pppuuupp
                                      |pppuuupp
                                      |ccpuuurp
                                      |ccpppppp""".stripMargin),

    "#170" -> Grid.fromPrettyString("""|byyydwww
                                       |bbbydddw
                                       |byyydwww
                                       |yyuyywrr
                                       |gguyowwr
                                       |guuuooor
                                       |grrrorrr
                                       |rrrrrrrr""".stripMargin),                                      

    "#332" -> Grid.fromPrettyString("""|pppppppp
                                       |poppppop
                                       |pobbggow
                                       |poobgoow
                                       |poooooow
                                       |prrrrrrw
                                       |ppyrrdww
                                       |ppyyddww""".stripMargin),

    "#151 (harder)" -> Grid.fromPrettyString("""|uuuuoobb
                                                |ugguowwb
                                                |ggggowwb
                                                |ggggggbb
                                                |rrgggggg
                                                |rygggggg
                                                |ryggdggd
                                                |rrggdddd""".stripMargin),

)




