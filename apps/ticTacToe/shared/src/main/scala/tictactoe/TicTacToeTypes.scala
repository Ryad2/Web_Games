
package tictactoe

import cs214.webapp.UserId

import scala.util.{Failure, Success, Try}

/** Stores all information about the current game. */
// Change this type to hold actual information (use an enum, class, …)

enum TicTacToeState:
  /** Game is in progress. */
  case Playing(board: Board, turn: UserId, other: UserId)

  /** Game is over. */
  case Finished(winner: Option[UserId])



/** There is only one event in tic-tac-toe: clicking a cell. */
enum TicTacToeEvent:
  /** User clicked cell (x, y) */
  case Move(x: Int, y: Int)

/** Client views reflect the state of the game: playing or finished. */
enum TicTacToeView:
  /** Game in progress. */
  case Playing(board: Board, yourTurn: Boolean)

  /** Game over. [[winner]] is [[None]] if the game ended in a tie. */
  case Finished(winner: Option[UserId])


// Change this class definition to store board states.
case class Board(cells : Vector[Option[UserId]] ) :
  /** Get the value in the cell at (r, c). */
  def apply(r: Int, c: Int): Option[UserId] =
    require(r < 3 && r >= 0 && c < 3 && c >= 0) // Add an appropriate precondition
    cells(r + c*3 )

