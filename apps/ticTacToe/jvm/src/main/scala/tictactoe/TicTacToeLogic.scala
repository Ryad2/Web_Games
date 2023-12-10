package tictactoe
import scala.collection.immutable.Queue
import scala.util.{Failure, Success, Try}
import cs214.webapp.*
import cs214.webapp.exceptions.*
import cs214.webapp.server.WebServer
import cs214.webapp.messages.Action
import jdk.internal.foreign.abi.Binding.Move


object TicTacToeStateMachine extends cs214.webapp.StateMachine[TicTacToeEvent, TicTacToeState, TicTacToeView]:

  val name: String = "tictactoe"
  val wire = TicTacToeWire

  override def init(clients: Seq[UserId]): TicTacToeState =
    val emptyBoard = Board(Vector.fill(9)(None))
    TicTacToeState.Playing(emptyBoard, clients.head, clients.last)

  
  // Failures in the Try must hold instances of AppException
  // (from Exceptions.scala under lib/shared/)
  def winning (board: Board, player : UserId) : Boolean =
    (for j <- 0 to 2
      yield
        (for i <- 0 to 2
          yield board.apply(i, j)).forall(_.contains(player))).exists(identity)
    ||
    (for j <- 0 to 2
      yield
        (for i <- 0 to 2
          yield board.apply(j, i)).forall(_.contains(player))).exists(identity)
    ||
    (for i <- 0 to 2
      yield board.apply(i, i)).forall(_.contains(player))
    ||
    (for i <- 0 to 2
      yield board.apply(i, 2 - i)).forall(_.contains(player))


  def isOver(board: Board): Boolean =
    !(for i <- 0 to 2
         j <- 0 to 2
      yield board.apply(i, j)
      ).contains(None)

  override def transition(state: TicTacToeState)(uid: UserId, event: TicTacToeEvent): Try[Seq[Action[TicTacToeState]]] =

    (state, event) match
      case (TicTacToeState.Playing(board, player1, player2), TicTacToeEvent.Move(x, y)) =>
        if uid != player1 then Failure( NotYourTurnException() )
        else if x < 0 || x > 2 || y < 0 || y > 2 then Failure( IllegalMoveException("this cell does not exist !!!") )
        else if board.apply(x, y).isEmpty then
            val newBoard = Board(board.cells.updated(x + 3*y, Some(uid)))
            if winning(newBoard, uid) then Success(Seq(Action.Render(TicTacToeState.Finished(Some(uid)))))
            else if isOver(newBoard) then Success(Seq(Action.Render(  TicTacToeState.Finished(None) )))
            else Success(Seq(Action.Render(  TicTacToeState.Playing(newBoard, player2, uid)  )))
        else Failure( IllegalMoveException("this cell is already occupied !!!") )
      case (TicTacToeState.Finished(_), _) =>
          Failure( IllegalMoveException("game is finished !!!") )


            
            
            
  override def project(state: TicTacToeState)(uid: UserId): TicTacToeView =
    state match
      case TicTacToeState.Playing(board, player1, player2) =>
        TicTacToeView.Playing(board, uid == player1)
      case TicTacToeState.Finished(winner) =>
        TicTacToeView.Finished(winner)

// Server registration magic
class register:
  WebServer.register(TicTacToeStateMachine)
