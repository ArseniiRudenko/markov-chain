package work.arudenko.markov.discrete

import breeze.stats.distributions.{Rand, Uniform}
import work.arudenko.linalg.SparseMatrix
import work.arudenko.markov.{MarkovChain, StartFrom}
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

/**
 *
 *
 * @param values  array of possible values
 * @param weights 2D array of  with dimensions of values.length+1
 *                where each row sum is 1
 *                last row represents start node which can go to nodes but does not have nodes that go to it
 *                and last column represents termination node
 * @tparam T value type
 */
case class DiscreteMarkovChain[T](values: ArraySeq[T], weights: SparseMatrix[Double]) extends MarkovChain[T] {

  val defaultMaxLength: Int = 1000

  override def walk(targetLength: Option[Int] = None, startNode: Option[StartFrom[T]] = None): Seq[T] = {
    startNode match {
      case Some(value) =>
        val index = values.indexOf(value.value)
        if (index == -1) {
          if (value.force)
            throw new NoSuchElementException("Required node value does not exist in the chain")
          else
            walkRecursive(targetLength).map(i => values(i))
        }
        if (value.anyNode) {
          walkRecursive(targetLength, index).map(i => values(i))
        } else {
          if (weights.getRow(weights.rows - 1).get(index) > 0)
            walkRecursive(targetLength, index).map(i => values(i))
          else {
            if (value.force)
              throw new NoSuchElementException("Required node value is not accessible from start node")
            else
              walkRecursive(targetLength).map(i => values(i))
          }
        }
      case None => walkRecursive(targetLength).map(i => values(i))
    }
  }


  protected lazy val distribution: Rand[Double] = new Uniform(0, 1)


  @tailrec
  private def walkRecursive(
                             targetLength: Option[Int],
                             currentNode: Int = weights.rows - 1,
                             currentSeq: Seq[Int] = Seq.empty,
                             curLen: Int = 0
                           ): Seq[Int] = {
    targetLength match {
      case Some(value) =>
        val terminationNodeWeight = weights(currentNode, weights.columns - 1)
        //check if chain is long enough
        if (curLen >= value || terminationNodeWeight == 1.0) {
          if (terminationNodeWeight > 0)
            currentSeq
          else {
            val nextNode = getNext(weights.getRow(currentNode).get, distribution.sample())
            walkRecursive(targetLength, nextNode, currentSeq :+ nextNode, curLen + 1)
          }
        } else {
          //exclude termination node
          val allExceptTheTerminationNode = weights.getRow(currentNode).get.dropRight(1)
          val nonZero: Double = allExceptTheTerminationNode.count(_ > 0)
          val adjustedProbabilities =
            allExceptTheTerminationNode.map(prob => if (prob > 0) prob + (terminationNodeWeight / nonZero) else prob)
          val nextNode = getNext(adjustedProbabilities, distribution.sample())
          walkRecursive(targetLength, nextNode, currentSeq :+ nextNode, curLen + 1)
        }
      //no target length is specified so we can just go as usual
      case None =>
        val nextNode = getNext(weights.getRow(currentNode).get, distribution.sample())
        //check if we hit the termination node
        if (nextNode == weights.columns - 1)
          currentSeq
        else
          walkRecursive(targetLength, nextNode, currentSeq :+ nextNode, curLen + 1)
    }
  }

  private def getNext(probabilityVector: Seq[Double], randomValue: Double): Int = {
    var sum: Double = 0
    val result = probabilityVector.iterator.takeWhile(value => {
      sum += value
      sum < randomValue
    }).length
    result
  }

}
