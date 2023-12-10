import memory.MemoryStateMachine.CARDS
import memory.PhaseView.SelectingCards
import memory.{CardView, MemoryEvent, MemoryState, MemoryStateMachine}

import scala.util.Random
val clients = Seq("A", "B", "C")
val score = clients.map(client => client -> Seq.empty).toMap
val board = Random.shuffle(CARDS ++ CARDS).zip(Seq.fill(CARDS.size * 2)(CardView.FaceDown))
val state = MemoryState.Playing(SelectingCards, 0, clients.toVector, board, score)

val test1flipping = MemoryStateMachine.transition(state)("A", MemoryEvent.Toggle(0))




val test2waitingState = MemoryStateMachine.transition(state)("B", MemoryEvent.Toggle(15))
val board2 = Random.shuffle(CARDS ++ CARDS).zip(Seq.fill(CARDS.size * 2)(CardView.Selected))
val state2 = MemoryState.Playing(SelectingCards, 2, clients.toVector, board2, score)
MemoryStateMachine.transition(state2)("C", MemoryEvent.Toggle(0))
