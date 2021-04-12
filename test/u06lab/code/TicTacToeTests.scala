package u06lab.code

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u06lab.code.TicTacToe._

class TicTacToeTests {
    @Test
    def testFind(): Unit = {
        assertEquals(Some(X), find(List(Mark(0, 0, X)), 0, 0))
        assertEquals(Some(O), find(List(Mark(0, 0, X), Mark(0, 1, O), Mark(0, 2, X)), 0, 1))
        assertEquals(None, find(List(Mark(0, 0, X), Mark(0, 1, O), Mark(0, 2, X)), 1, 1))
    }
}
