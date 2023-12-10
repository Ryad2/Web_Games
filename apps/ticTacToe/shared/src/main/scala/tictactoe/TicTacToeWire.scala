package tictactoe

import ujson.*
import scala.util.{Failure, Success, Try}

import cs214.webapp.wires.*
import cs214.webapp.exceptions.DecodingException
import cs214.webapp.{AppWire, WireFormat, UserId}
import geny.Generator.from

object TicTacToeWire extends AppWire[TicTacToeEvent, TicTacToeView]:
  import TicTacToeEvent.*
  import TicTacToeView.*

  override object eventFormat extends WireFormat[TicTacToeEvent]:
    override def encode(t: TicTacToeEvent): Value =
      t match
        case Move(x, y) =>
          ujson.Obj(
          "x" -> IntWire.encode(x),
          "y" -> IntWire.encode(y)
        )

    override def decode(json: Value): Try[TicTacToeEvent] = Try {
      val x = IntWire.decode(json.obj("x")).get
      val y = IntWire.decode(json.obj("y")).get
      Move(x, y)
    }


  override object viewFormat extends WireFormat[TicTacToeView]:

    def encode(t: TicTacToeView): Value =
      t match
        case Playing(board, yourTurn) => ujson.Obj(
          "type" -> ujson.Bool(true),
          "board" -> SeqWire(OptionWire(StringWire)).encode(board.cells),
          "yourTurn" -> BooleanWire.encode(yourTurn)
        )
        case Finished(winner) => Obj(
          "type" -> BooleanWire.encode(false),
          "winner" -> OptionWire(StringWire).encode( winner )
        )


    def decode(json: Value): Try[TicTacToeView] =
      Try {
        if BooleanWire.decode(json.obj("type")).get then
          val board = SeqWire(OptionWire(StringWire)).decode(json.obj("board")).get
          val yourTurn = BooleanWire.decode(json.obj("yourTurn")).get

          TicTacToeView.Playing(Board(board.toVector), yourTurn)
        else
          val winner = OptionWire(StringWire).decode(json.obj("winner")).get
          Finished(winner)
      }.recoverWith {
        case _: Exception => Failure(DecodingException("Invalid JSON"))
      }