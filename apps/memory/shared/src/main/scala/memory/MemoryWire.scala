package memory
import scala.util.{Failure, Success, Try}
import cs214.webapp.*
import cs214.webapp.wires.{BooleanWire, *}
import cs214.webapp.exceptions.DecodingException
import cs214.webapp.wires.BooleanWire.*
import memory.CardView.FaceDown
import memory.StateView.Playing

object MemoryWire extends AppWire[MemoryEvent, MemoryView]:
  import MemoryEvent.*
  import MemoryView.*
  import ujson.*

  override object eventFormat extends WireFormat[MemoryEvent]:
    override def encode(event: MemoryEvent): Value =
      event match
        case Toggle(n) => ujson.Obj(
          "type" -> BooleanWire.encode(true),
          "cards" -> IntWire.encode(n)
        )
        case FlipSelected => ujson.Obj(
          "type" -> BooleanWire.encode(false)
        )


    override def decode(js: Value): Try[MemoryEvent] =
      try {
      if BooleanWire.decode(js("type")).get then
        Success(Toggle(IntWire.decode(js("cards")).get))
      else Success(FlipSelected)
      }
      catch {
        case _: Exception => Failure(DecodingException("Invalid JSON"))
      }









  override object viewFormat extends WireFormat[MemoryView]:

    override def encode(v: MemoryView): Value =
      v.stateView match
        case Playing(phase, currentPlayer, board ) =>
          val table = board.map {
            case CardView.FaceDown => ("FaceDown", "")
            case CardView.Selected => ("Selected", "")
            case CardView.FaceUp(card) => ("FaceUp", card)
            case CardView.AlreadyMatched(card) => ("AlreadyMatched", card)
          }
          val phaseString = phase match
            case PhaseView.SelectingCards => "SelectingCards"
            case PhaseView.CardsSelected => "CardsSelected"
            case PhaseView.Waiting => "Waiting"
            case PhaseView.GoodMatch => "GoodMatch"
            case PhaseView.BadMatch => "BadMatch"

          ujson.Obj(
          "matchedCards" -> MapWire(StringWire, SeqWire(StringWire)).encode(v.alreadyMatched),
          "type" -> BooleanWire.encode(true),
          "player" -> StringWire.encode(currentPlayer),
          "board"   -> SeqWire(PairWire(StringWire, StringWire)).encode(table),
          "phase" -> StringWire.encode(phaseString)
          )

        case StateView.Finished(winnerIds) =>
          ujson.Obj(
            "type" -> BooleanWire.encode(false),
            "winnerIds" -> SetWire(StringWire).encode(winnerIds),
            "matchedCards" -> MapWire(StringWire, SeqWire(StringWire)).encode(v.alreadyMatched)
          )



    override def decode(js: Value): Try[MemoryView] =

      if BooleanWire.decode(js("type")).get then
        val currentPlayer = StringWire.decode(js("player")).get
        val board = SeqWire(PairWire(StringWire, StringWire)).decode(js("board")).get
        val boardView = board.map {
          case ("FaceDown", _) => CardView.FaceDown
          case ("Selected", _) => CardView.Selected
          case ("FaceUp", card) => CardView.FaceUp(card)
          case ("AlreadyMatched", card) => CardView.AlreadyMatched(card)
        }
        val phase = StringWire.decode(js("phase")).get match
          case "SelectingCards" => PhaseView.SelectingCards
          case "CardsSelected" => PhaseView.CardsSelected
          case "Waiting" => PhaseView.Waiting
          case "GoodMatch" => PhaseView.GoodMatch
          case "BadMatch" => PhaseView.BadMatch

        Success(MemoryView(StateView.Playing(phase, currentPlayer, boardView), MapWire(StringWire, SeqWire(StringWire)).decode(js("matchedCards")).get))

      else
        val winnerIds = SetWire(StringWire).decode(js("winnerIds")).get
        Success(MemoryView(StateView.Finished(winnerIds), MapWire(StringWire, SeqWire(StringWire)).decode(js("matchedCards")).get))
