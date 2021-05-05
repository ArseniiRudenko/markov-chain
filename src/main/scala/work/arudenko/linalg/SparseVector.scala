package work.arudenko.linalg

import scala.collection.immutable.AbstractSeq

case class SparseVector[T](len:Int, values:Map[Int,T] = Map.empty[Int,T])(implicit n: Numeric[T])
  extends AbstractSeq[T]
  with IndexedSeq[T]
    with Seq[T] {

  override val knownSize: Int = len
  override def length: Int = len

  override def sum[B >: T](implicit num: Numeric[B]): B = values.values.sum

  def + (other:SparseVector[T]):SparseVector[T] =
    SparseVector(
      math.max(len,other.len),
      values ++ other.values.map{
      case (k,v) => (k, values.get(k) match {
        case Some(value) => n.plus(value,v)
        case None => v
      })
    })

  override def apply(index: Int): T = {
    get(index) match {
      case Some(value) => value
      case None =>throw new IndexOutOfBoundsException(s"index $index is out of the bounds of vector, vector length is $len")
    }
  }

  def get(index:Int):Option[T] =
    {
      if(index>=len || index<0)
        None
      else
        Some(values.getOrElse(index,n.zero))
    }


  def update(index:Int,value:T):SparseVector[T] = {
    if(index>=len || index<0)
      throw new IndexOutOfBoundsException(s"index $index is out of the bounds of vector, vector length is $len")
    if(value == n.zero)
      this
    else
      new SparseVector[T](len, values.updated(index, value))
  }

  def mapNonEmpty[R](f:T=>R)(implicit n:Numeric[R]):SparseVector[R] =new SparseVector(len,values.map(v=>(v._1,f(v._2))))

  /**
   * resize vector to new size.
   * If new size is smaller all values outside of new vector will be lost
   * @param newSize - size of output vector
   * @return
   */
  def resize(newSize:Int=len+1):SparseVector[T] = {
    if(newSize>size)
      new SparseVector(newSize,values)
    else
      new SparseVector(newSize,values.filter(v=>v._1<newSize))
  }
}

object SparseVector{

  implicit class MapSparseVectorConverter[T:Numeric](v:Map[Int,T]){
    def toSparseVector(size:Int):SparseVector[T] = new SparseVector[T](size,v)
  }

  implicit class IterableSparseVectorConverter[T:Numeric](v:IterableOnce[(Int,T)]){
    def toSparseVector(size:Int):SparseVector[T] = new SparseVector[T](size,v.iterator.toMap)
  }

  def apply[T](length:Int)(implicit n: Numeric[T]): SparseVector[T] = new SparseVector(length,Map.empty[Int,T])

}
