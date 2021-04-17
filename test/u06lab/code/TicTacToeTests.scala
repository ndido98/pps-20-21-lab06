package u06lab.code

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test
import u06lab.code.TicTacToe._

class TicTacToeTests {
    val winningHorizontal = List(Mark(0, 0, X), Mark(1, 0, X), Mark(2, 0, X))
    val winningVertical = List(Mark(0, 0, O), Mark(0, 1, O), Mark(0, 2, O))
    val winningDiagonal = List(Mark(0, 0, X), Mark(1, 1, X), Mark(2, 2, X))
    val winningAntiDiagonal = List(Mark(2, 0, O), Mark(1, 1, O), Mark(0, 2, O))
    val notWinning = List(Mark(0, 0, X), Mark(0, 1, X), Mark(0, 2, O))

    @Test
    def testFind(): Unit = {
        assertEquals(Some(X), find(List(Mark(0, 0, X)), 0, 0))
        assertEquals(Some(O), find(List(Mark(0, 0, X), Mark(0, 1, O), Mark(0, 2, X)), 0, 1))
        assertEquals(None, find(List(Mark(0, 0, X), Mark(0, 1, O), Mark(0, 2, X)), 1, 1))
    }

    @Test
    def testPlaceAnyMark(): Unit = {
        val xEmpty = placeAnyMark(List(), X)
        assertEquals(9, xEmpty.size)
        for {
            x <- 0 until boardSize
            y <- 0 until boardSize
        } assertTrue(xEmpty contains List(Mark(x, y, X)))
        val yUpperLeft = placeAnyMark(List(Mark(0, 0, O)), X)
        assertEquals(8, yUpperLeft.size)
        for {
            x <- 0 until boardSize
            y <- 0 until boardSize
            if x != 0 || y != 0
        } assertTrue(yUpperLeft contains List(Mark(0, 0, O), Mark(x, y, X)))
    }

    @Test
    def testComputeAnyGame(): Unit = {
        val gameWithZeroMoves = computeAnyGame(O, 0)
        assertEquals(1, gameWithZeroMoves.size)
        assertEquals(emptyGame, gameWithZeroMoves.head)
        val gameWithOneMove = computeAnyGame(O, 1).toList
        assertEquals(9, gameWithOneMove.size)
        for {
            x <- 0 until boardSize
            y <- 0 until boardSize
        } assertTrue(gameWithOneMove contains List(emptyBoard, List(Mark(x, y, O))))
        val gameWithFourMoves = computeAnyGame(O, 4).toList
        val validGame = List(
            emptyBoard,
            List(Mark(0, 0, X)),
            List(Mark(0, 0, X), Mark(0, 1, O)),
            List(Mark(0, 0, X), Mark(0, 1, O), Mark(0, 2, X)),
            List(Mark(0, 0, X), Mark(0, 1, O), Mark(0, 2, X), Mark(1, 0, O))
        )
        assertTrue(gameWithFourMoves contains validGame)
    }

    @Test
    def testComputeAnyGameStopsWhenWin(): Unit = {
        val fiveMovesGames = computeAnyGame(X, 7).toList
        val winningGame = List(
            emptyBoard,
            List(Mark(0, 0, X)),
            List(Mark(0, 0, X), Mark(0, 1, O)),
            List(Mark(0, 0, X), Mark(0, 1, O), Mark(1, 0, X)),
            List(Mark(0, 0, X), Mark(0, 1, O), Mark(1, 0, X), Mark(1, 1, O)),
            List(Mark(0, 0, X), Mark(0, 1, O), Mark(1, 0, X), Mark(1, 1, O), Mark(2, 0, X))
        )
        assertEquals(Some(X), getWinner(winningGame.last))
        assertTrue(fiveMovesGames contains winningGame)
    }

    @Test
    def testIsPlayerWinner(): Unit = {
        assertTrue(isPlayerWinner(winningHorizontal, X))
        assertTrue(isPlayerWinner(winningVertical, O))
        assertTrue(isPlayerWinner(winningDiagonal, X))
        assertTrue(isPlayerWinner(winningAntiDiagonal, O))
        assertFalse(isPlayerWinner(notWinning, X))
        assertFalse(isPlayerWinner(notWinning, O))
    }

    @Test
    def testGetWinner(): Unit = {
        assertEquals(Some(X), getWinner(winningHorizontal))
        assertEquals(Some(O), getWinner(winningVertical))
        assertEquals(Some(X), getWinner(winningHorizontal))
        assertEquals(Some(O), getWinner(winningAntiDiagonal))
        assertEquals(None, getWinner(notWinning))
    }
}
