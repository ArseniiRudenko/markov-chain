package work.arudenko.markov

trait MarkovChainTeachable[T] {
  def addEvent(from: T, to: T): MarkovChainTeachable[T]
  def addEnd(from: T): MarkovChainTeachable[T]
  def addStart(to: T): MarkovChainTeachable[T]
  def materializeModel: MarkovChain[T]
}