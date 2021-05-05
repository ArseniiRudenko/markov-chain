package work.arudenko.markov

case class StartFrom[T](value:T,anyNode:Boolean =false,force:Boolean=false)


trait MarkovChain[T] {

  /**
   *
   * @param targetLength will try to return close but not less than length values
   *                     by ignoring termination nodes if chain is shorter than length,
   *                     and selecting first termination node after length is reached.
   *                     Is a suggestion, length is not guaranteed.
   * @param startNode    Take value as a first in the chain.
   *                     If "anyNode" flag is set,
   *                     chain will start from specified value even if it is not valid start point.
   *                     If "force" flag is set, walk will throw exception if no such value exists,
   *                     otherwise, if specified value is not available, it will just go with random start point instead.
   */
  def walk(targetLength: Option[Int] = None, startNode: Option[StartFrom[T]] = None): Seq[T]
}


