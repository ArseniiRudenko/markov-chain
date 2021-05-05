package work.arudenko.markov.discrete

import work.arudenko.linalg.{SparseMatrix, SparseVector}
import work.arudenko.markov.{MarkovChain, MarkovChainTeachable}
import work.arudenko.markov.discrete.DiscreteMarkovChainTeachable.getValueForElement

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

case class DiscreteMarkovChainTeachable[T:ClassTag](
  elements:Map[T,Int] = Map.empty[T,Int],
  events:SparseMatrix[Int] = SparseMatrix(0,0),
  start:SparseVector[Int] = SparseVector(0),
  end:SparseVector[Int] = SparseVector(0)
) extends MarkovChainTeachable[T] {

  def getStart(el:T): Int = getValueForElement(el,elements,start)
  def getEnd(el:T): Int = getValueForElement(el,elements,end)


  def +(other: DiscreteMarkovChainTeachable[T]): DiscreteMarkovChainTeachable[T] =
  {
    val newElements = (elements.keySet ++ other.elements.keySet).zipWithIndex.toMap
    var start = SparseVector[Int](newElements.size)
    var end = SparseVector[Int](newElements.size)
    var events = SparseMatrix[Int](newElements.size,newElements.size)
    newElements.foreachEntry((e,i)=>{
      start=start.update(i,getStart(e)+other.getStart(e))
      end=end.update(i,getEnd(e)+other.getEnd(e))
      val rowThis = elements.get(e).flatMap(this.events.getRow).getOrElse(SparseVector[Int](elements.size))
      val rowThat = elements.get(e).flatMap(this.events.getRow).getOrElse(SparseVector[Int](other.elements.size))
      var newRow = SparseVector[Int](newElements.size)
      newElements.foreachEntry((e,i)=>{
        val eventsThis = elements.get(e).flatMap(rowThis.get).getOrElse(0)
        val eventsThat = other.elements.get(e).flatMap(rowThat.get).getOrElse(0)
        newRow=newRow.update(i,eventsThis+eventsThat)
      })
      events=events.replaceRow(i,newRow)
    })

    new DiscreteMarkovChainTeachable[T](
      newElements,
      events,
      start,
      end
    )
  }


  @tailrec
  final override def addEvent(from:T, to:T):DiscreteMarkovChainTeachable[T] = {
    if(!elements.contains(from))
      addElement(from).addEvent(from, to)
    else if(!elements.contains(to))
      addElement(to).addEvent(from, to)
    else{
      val row = elements(from)
      val column = elements(to)
      this.copy(events=events.updated(row,column,events(row,column)+1))
    }
  }

  private def addElement(element:T):DiscreteMarkovChainTeachable[T] =  this.copy(
    elements.updated(element,start.size),
    events.resize(events.rows+1,events.columns+1),
    start.resize(),
    end.resize()
  )

  @tailrec
  final override def addEnd(from:T):DiscreteMarkovChainTeachable[T] = {
    if(!elements.contains(from))
      addElement(from).addEnd(from)
    else {
      val index = elements(from)
      this.copy(end=end.update(index,end(index)+1))
    }
  }

  @tailrec
  final override def addStart(to:T):DiscreteMarkovChainTeachable[T] = {
    if(!elements.contains(to))
      addElement(to).addStart(to)
    else {
      val index = elements(to)
      this.copy(start=start.update(index,end(index)+1))
    }
  }

  override def materializeModel:MarkovChain[T] ={
    val keySet = elements.iterator.toArray.sortBy(_._2).map(_._1)
    val matrixOfEvents = events.appendRow(start).appendColumn(end)
    val matrixOfProbabilities = Range(0,matrixOfEvents.rows).foldLeft(new SparseMatrix[Double](matrixOfEvents.rows,matrixOfEvents.columns))(
      (matrix,row)=>{
        val sum:Double = matrixOfEvents.getRow(row).get.sum
        matrix.replaceRow(row,matrixOfEvents.getRow(row).get.mapNonEmpty(v=>v/sum))
      }
    )
    DiscreteMarkovChain(
      ArraySeq.from(keySet),
      matrixOfProbabilities
    )

  }
}

object DiscreteMarkovChainTeachable {

  private def getValueForElement[E:ClassTag,R](e:E,elements:Map[E,Int],from:SparseVector[R])(implicit numeric: Numeric[R]):R=
    elements.get(e).map(from(_)).getOrElse(numeric.zero)
}


