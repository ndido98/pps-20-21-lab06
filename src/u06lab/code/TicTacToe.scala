package u06lab.code

object TicTacToe {

    sealed trait Player {
        def other: Player = this match {
            case X => O;
            case _ => X
        }

        override def toString: String = this match {
            case X => "X";
            case _ => "O"
        }
    }

    case object X extends Player

    case object O extends Player

    case class Mark(x: Int, y: Int, player: Player)

    type Board = List[Mark]
    type Game = List[Board]
    val boardSize = 3
    val emptyBoard: Board = List.empty
    val emptyGame: Game = List(emptyBoard)

    def find(board: Board, x: Int, y: Int): Option[Player] =
        board collectFirst {
            case Mark(mx, my, p) if x == mx && y == my => p
        }

    def placeAnyMark(board: Board, player: Player): Seq[Board] =
        for {
            x <- 0 until boardSize
            y <- 0 until boardSize
            if !board.exists(mark => mark.x == x && mark.y == y)
        } yield board :+ Mark(x, y, player)


    def computeAnyGame(player: Player, moves: Int): LazyList[Game] = moves match {
        case 0 => LazyList(emptyGame) // empty board
        case n => for {
            game <- computeAnyGame(player.other, n - 1) // take all the games generated from the previous move
            nextMark <- placeAnyMark(game.last, player) // for each game append a new mark
        } yield if (getWinner(game.last).isEmpty) game :+ nextMark else game
    }

    def getWinner(board: Board): Option[Player] = Seq(X, O).find(isPlayerWinner(board, _))

    def isPlayerWinner(board: Board, player: Player): Boolean = {
        def _horizontal(y: Int): Set[Mark] = (0 until boardSize).map(x => Mark(x, y, player)).toSet
        def _vertical(x: Int): Set[Mark] = (0 until boardSize).map(y => Mark(x, y, player)).toSet
        val _diagonal = (0 until boardSize).map(i => Mark(i, i, player)).toSet
        val _antiDiagonal = (0 until boardSize).map(i => Mark(boardSize - 1 - i, i, player)).toSet

        val horizontals = (0 until boardSize).map(y => _horizontal(y)).toSet
        val verticals = (0 until boardSize).map(x => _vertical(x)).toSet
        val allWinningStates = horizontals ++ verticals + _diagonal + _antiDiagonal

        allWinningStates.exists(state => state.subsetOf(board.toSet))
    }

    def printBoards(game: Seq[Board]): Unit =
        for (y <- 0 to 2; board <- game.reverse; x <- 0 to 2) {
            print(find(board, x, y) map (_.toString) getOrElse ("."))
            if (x == 2) {
                print(" "); if (board == game.head) println()
            }
        }

//    // Exercise 1: implement find such that..
//    println(find(List(Mark(0, 0, X)), 0, 0)) // Some(X)
//    println(find(List(Mark(0, 0, X), Mark(0, 1, O), Mark(0, 2, X)), 0, 1)) // Some(O)
//    println(find(List(Mark(0, 0, X), Mark(0, 1, O), Mark(0, 2, X)), 1, 1)) // None
//
//    // Exercise 2: implement placeAnyMark such that..
//    printBoards(placeAnyMark(List(), X))
//    //... ... ..X ... ... .X. ... ... X..
//    //... ..X ... ... .X. ... ... X.. ...
//    //..X ... ... .X. ... ... X.. ... ...
//    printBoards(placeAnyMark(List(Mark(0, 0, O)), X))
//    //O.. O.. O.X O.. O.. OX. O.. O..
//    //... ..X ... ... .X. ... ... X..
//    //..X ... ... .X. ... ... X.. ...
//
//    // Exercise 3 (ADVANCED!): implement computeAnyGame such that..
//    computeAnyGame(O, 4) foreach { g => printBoards(g); println() }
//    //... X.. X.. X.. XO.
//    //... ... O.. O.. O..
//    //... ... ... X.. X..
//    //              ... computes many such games (they should be 9*8*7*6 ~ 3000).. also, e.g.:
//    //
//    //... ... .O. XO. XOO
//    //... ... ... ... ...
//    //... .X. .X. .X. .X.
//
//    // Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
}
