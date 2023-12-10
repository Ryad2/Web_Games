package memory

import scala.util.{Failure, Random, Success, Try}
import ujson.Value
import cs214.webapp.*
import cs214.webapp.messages.*
import cs214.webapp.exceptions.*
import cs214.webapp.server.WebServer
import memory.*

// Feel free to tweak this value!
private val SHOW_CARDS_PAUSE_MS = 2500

object MemoryStateMachine extends cs214.webapp.StateMachine[MemoryEvent, MemoryState, MemoryView]:

  val name: String = "memory"
  val wire = MemoryWire

  def Deck(cards: String): Vector[String] =
    cards.trim.split(" +").to(Vector)

  val DECKS: Map[String, Vector[String]] = Map(
    "Simple" -> Deck("""
      💫 ⭐️
    """),
    "Stars" -> Deck("""
      💫 ⭐️ 🌟 ✨ ☀️
    """),
    "Animals" -> Deck("""
      🐵 🐒 🦍 🦧 🐶 🐕 🦮 🐕‍🦺
      🐩 🐺 🦊 🦝 🐱 🐈 🐈‍⬛ 🦁
      🐯 🐅 🐆 🐴 🫎 🫏 🐎 🦄
      🦓 🦌 🦬 🐮 🐂 🐃 🐄 🐷
      🐖 🐗 🐽 🐏 🐑 🐐 🐪 🐫
      🦙 🦒 🐘 🦣 🦏 🦛 🐭 🐁
      🐀 🐹 🐰 🐇 🐿️ 🦫 🦔 🦇
      🐻 🐻‍❄️ 🐨 🐼 🦥 🦦 🦨 🦘
      🦡
    """),
    "Birds" -> Deck("""
      🦃 🐔 🐓 🐣 🐤 🐥 🐦 🐧
      🕊️ 🦅 🦆 🦢 🦉 🦤 🪶 🦩
      🦚 🦜 🪽 🐦‍⬛ 🪿
    """),
    "Marine & Reptiles" -> Deck("""
      🐸 🐊 🐢 🦎 🐍 🐲 🐉 🦕
      🦖 🐳 🐋 🐬 🦭 🐟 🐠 🐡
      🦈 🐙 🐚 🪸 🪼 🦀 🦞 🦐
      🦑 🦪
    """),
    "Bugs" -> Deck("""
      🐌 🦋 🐛 🐜 🐝 🪲 🐞 🦗
      🪳 🕷️ 🕸️ 🦂 🦟 🪰 🪱 🦠
    """),
    "Plants" -> Deck("""
      💐 🌸 💮 🪷 🏵️ 🌹 🥀 🌺
      🌻 🌼 🌷 🪻 🌱 🪴 🌲 🌳
      🌴 🌵 🌾 🌿 ☘️ 🍀 🍁 🍂
      🍃 🍄 🪨 🪵
    """)
  )

  // Use any strings you want here — the tests don't check for these specific emoji
  val CARDS: Vector[String] = DECKS("Birds")

  /** Creates a new application state. */
  override def init(clients: Seq[UserId]): MemoryState =
    val board = Random.shuffle(CARDS ++ CARDS).zip(Seq.fill(CARDS.size * 2)(CardView.FaceDown))
    MemoryState.Playing(PhaseView.SelectingCards, 0, clients.toVector, board, clients.map(client => client -> Seq.empty).toMap)




  override def transition(state: MemoryState)(userId: UserId, event: MemoryEvent): Try[Seq[Action[MemoryState]]] =
    state match
      case MemoryState.Playing(phase, currentPlayer, allPlayers, board, mapScore) =>

        val nextPlayer = (currentPlayer + 1) % allPlayers.size
        if userId != allPlayers(currentPlayer) || phase == PhaseView.Waiting
          then Failure( NotYourTurnException() )
        else if !board.map(_._2).contains(CardView.FaceDown)
          then Success( Seq( Action.Render(MemoryState.Finished(board, mapScore))) )
        else if phase == PhaseView.GoodMatch
          then Success( Seq( Action.Render(MemoryState.Playing(PhaseView.SelectingCards, currentPlayer, allPlayers, board, mapScore))) )
        else if phase == PhaseView.BadMatch
          then Success( Seq( Action.Render(MemoryState.Playing(PhaseView.Waiting, nextPlayer, allPlayers, board, mapScore))) )

        else
          event match
            case MemoryEvent.Toggle(cardId) =>
              if //board(cardId)._2 == CardView.FaceUp(board(cardId)._1) ||
                 board(cardId)._2 == CardView.AlreadyMatched(board(cardId)._1)
              then
                Failure( IllegalMoveException("Already selected 05 !!!") ) // todo enter here

              else if board(cardId)._2 == CardView.Selected then
                val newBoard = board.updated(cardId, (board(cardId)._1, CardView.FaceDown))
                Success( Seq( Action.Render(MemoryState.Playing(PhaseView.SelectingCards, currentPlayer, allPlayers, newBoard, mapScore)) ) )

              else if phase == PhaseView.SelectingCards then
                val newBoard = board.updated(cardId, (board(cardId)._1, CardView.Selected))
                val newPhase =
                  if board.map(_._2).contains(CardView.Selected) && board(cardId)._2 != CardView.Selected then  PhaseView.CardsSelected
                  else  PhaseView.SelectingCards

                Success( Seq( Action.Render(MemoryState.Playing(newPhase, currentPlayer, allPlayers, newBoard, mapScore))) )

              else Failure( IllegalMoveException("Select only two cards 01 !!!") ) // todo enter here



            case MemoryEvent.FlipSelected =>
              phase match
                case PhaseView.SelectingCards =>
                  Failure( IllegalMoveException("Select two cards 02 !!!") )

                case PhaseView.Waiting =>
                  Failure( NotYourTurnException() )

                case PhaseView.CardsSelected =>

                  val selectedCards = board.zipWithIndex.filter(_._1._2 == CardView.Selected)
                  val newBoard = board
                    .updated(selectedCards.head._2, (selectedCards.head._1._1, CardView.FaceUp(selectedCards.head._1._1)))
                    .updated(selectedCards.last._2, (selectedCards.last._1._1, CardView.FaceUp(selectedCards.last._1._1)))

                  if selectedCards.head._1 == selectedCards.last._1 then


                    val newScore = mapScore.updated(
                      allPlayers(currentPlayer),
                      mapScore(allPlayers(currentPlayer)) ++ selectedCards.map(_._1._1) )

                    Success(Seq(Action.Render(MemoryState.Playing(PhaseView.GoodMatch, currentPlayer, allPlayers, newBoard, newScore))))

                  else

                    Success(Seq(Action.Render(MemoryState.Playing(PhaseView.BadMatch, nextPlayer,  allPlayers, newBoard, mapScore))))

      case MemoryState.Finished(board, alreadyMatchedScore) =>
        Success(Seq(Action.Alert("the game is over 03")))





  override def project(state: MemoryState)(userId: UserId): MemoryView =
    state match
      case MemoryState.Playing(phase, currentPlayer, allPlayers, board, alreadyMatched) =>
        MemoryView(StateView.Playing(phase, allPlayers(currentPlayer), board.map(_._2)), alreadyMatched)
        
      case MemoryState.Finished(board, alreadyMatched) =>
        val maxScore = alreadyMatched.values.map(_.size).max
        val winners = alreadyMatched.filter ( _._2.size == maxScore ).keys.toSet
        MemoryView(StateView.Finished(winners), alreadyMatched)

// Server registration magic
class register:
  WebServer.register(MemoryStateMachine)
