package memory

import scala.util.{Failure, Random, Success, Try}
import ujson.Value
import cs214.webapp.*
import cs214.webapp.messages.*
import cs214.webapp.exceptions.*
import cs214.webapp.server.WebServer
import memory.*
import memory.PhaseView.SelectingCards

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
        def updateBoard(cardId: Int, cardView: CardView): Vector[(String, CardView)] = board.updated(cardId, (board(cardId)._1, cardView))
        if userId != allPlayers(currentPlayer) then Success( Seq( Action.Render(MemoryState.Playing(PhaseView.Waiting, currentPlayer, allPlayers, board, mapScore))))
        else if board.forall(c => c._2 == CardView.AlreadyMatched(c._1)) then Success(Seq(Action.Render(MemoryState.Finished(board, mapScore))))
        else
        phase match
          case SelectingCards =>
            event match
              case MemoryEvent.Toggle(cardId) =>
                if board(cardId)._2 == CardView.Selected then
                  val newBoard = updateBoard(cardId, CardView.FaceDown) //todo down
                  Success( Seq( Action.Render(MemoryState.Playing(PhaseView.SelectingCards, currentPlayer, allPlayers, newBoard, mapScore)) ) )
                else
                  val newBoard = board.updated(cardId, (board(cardId)._1, CardView.Selected))
                  val newPhase =
                    if board.map(_._2).contains(CardView.Selected) then PhaseView.CardsSelected
                    else  PhaseView.SelectingCards
                  Success( Seq( Action.Render(MemoryState.Playing(newPhase, currentPlayer, allPlayers, newBoard, mapScore))))
              case MemoryEvent.FlipSelected =>
                // todo Failure( IllegalMoveException("Select two cards 01 !!!"))
                Success(Seq(Action.Render(MemoryState.Playing(PhaseView.SelectingCards, currentPlayer, allPlayers, board, mapScore))))

          case PhaseView.CardsSelected =>
            event match
              case MemoryEvent.Toggle(cardId) =>
                if board(cardId)._2 == CardView.Selected then
                  val newBoard = updateBoard(cardId, CardView.FaceDown)//todo down
                  Success(Seq(Action.Render(MemoryState.Playing(PhaseView.SelectingCards, currentPlayer, allPlayers, newBoard, mapScore))))
                else
                  Failure( IllegalMoveException("Select only two cards 02 !!!"))


              case MemoryEvent.FlipSelected =>

                val pair = board.zipWithIndex.filter(_._1._2 == CardView.Selected)
                val newBoard = updateBoard(pair.head._2, CardView.FaceUp(pair.head._1._1)).updated(pair.last._2, (pair.last._1._1, CardView.FaceUp(pair.last._1._1)))
                val newPhase =
                  if pair.head._1._1 == pair.last._1._1 then PhaseView.GoodMatch
                  else PhaseView.BadMatch

                val newScore = if newPhase == PhaseView.GoodMatch then  mapScore.updated(allPlayers(currentPlayer), mapScore(allPlayers(currentPlayer)) ++ pair.map(_._1._1))
                                else mapScore

                val newPlayer = if newPhase == PhaseView.GoodMatch then currentPlayer else (currentPlayer + 1) % allPlayers.size
                val endBord =
                  if newPhase == PhaseView.GoodMatch
                    then
                      val pair = newBoard.zipWithIndex.filter(card => card._1._2 == CardView.FaceUp(card._1._1))
                      val WinCard = pair.head._1._1
                      newBoard.map(card => if card._2 == CardView.FaceUp(WinCard) then (card._1, CardView.AlreadyMatched(WinCard)) else card)

                  else
                    newBoard.map(card => if card._2 == CardView.FaceUp(card._1) then (card._1, CardView.FaceDown) else card)


                if !endBord.map(_._2).contains(CardView.FaceDown) then
                  Success(Seq(
                    Action.Render(MemoryState.Playing(newPhase, currentPlayer, allPlayers, newBoard, mapScore)),
                    Action.Pause(SHOW_CARDS_PAUSE_MS),
                    Action.Render(MemoryState.Finished(endBord, newScore)
                  )))

                else

                Success(Seq(
                  Action.Render(MemoryState.Playing(newPhase, currentPlayer, allPlayers, newBoard, mapScore)),
                  Action.Pause(SHOW_CARDS_PAUSE_MS),
                  Action.Render(MemoryState.Playing(SelectingCards, newPlayer, allPlayers, endBord, newScore))
                ))


      case MemoryState.Finished(board, alreadyMatchedScore) =>
          Failure( IllegalMoveException("Game is over !!!"))







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
