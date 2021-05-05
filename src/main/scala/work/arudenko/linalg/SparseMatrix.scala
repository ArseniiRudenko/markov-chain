package work.arudenko.linalg
import work.arudenko.linalg.SparseVector._
import scala.collection.immutable.AbstractSeq

case class SparseMatrix[T](rows:Int,columns:Int, values:Map[Int,Map[Int,T]] = Map.empty[Int,Map[Int,T]])(implicit n:Numeric[T]) {

  def apply(row:Int,column:Int):T=values.get(row).flatMap(_.get(column)).getOrElse(n.zero)

  private val enclosing = this

  def getRow(index:Int):Option[SparseVector[T]] = {
    if(index<0 || index>=rows)
      None
    else
      Some(new SparseVector[T](columns,values.getOrElse(index, Map.empty[Int,T])))
  }


  def getColumn(index:Int):Seq[T] = new AbstractSeq[T] with IndexedSeq[T]{
    override val knownSize: Int = columns
    override def apply(i: Int): T = enclosing.apply(i,index)
    override def length: Int = columns
  }

  def replaceRow(index:Int,values:SparseVector[T]):SparseMatrix[T] =
    new SparseMatrix(rows,columns,this.values.updated(index,values.values))


  def mapRows(f:SparseVector[T]=>SparseVector[T]):SparseMatrix[T] =
    SparseMatrix(rows,columns, values.map{case (i,r)=>(i,f(r.toSparseVector(columns)).values)})


  def updated(row:Int, column:Int, value:T):SparseMatrix[T] =
    if(value == n.zero)
      this
    else
      new SparseMatrix[T](rows,columns,values.updated(row,values.getOrElse(row,Map.empty[Int,T]).updated(column,value)))

  def + (other:SparseMatrix[T]):SparseMatrix[T] = {
    SparseMatrix(
      math.max(rows, other.rows), math.max(columns, other.columns),
      values ++ other.values.map {
        case (k, v) => (k, values.get(k) match {
          case Some(value) => v ++ value.map {
            case (i, number) => (i, v.get(i) match {
              case Some(value) => n.plus(number, value)
              case None => number
            }
            )
          }
          case None => v
        })
      })
  }


  def resize(rows:Int,columns:Int):SparseMatrix[T] = {
    if(rows>=this.rows && columns>=this.columns){
      new SparseMatrix(rows,columns,values)
    }else
      ???
  }

  def appendRow(vector: SparseVector[T]):SparseMatrix[T] =
    vector.values.foldLeft(resize(rows+1,columns)){case(matrix,(index,value))=>matrix.updated(rows,index,value)}


  def appendColumn(vector: SparseVector[T]):SparseMatrix[T] =
    vector.values.foldLeft(resize(rows,columns+1)){case(matrix,(index,value))=>matrix.updated(index,columns,value)}

}

object SparseMatrix{
  def apply[T:Numeric](rows:Int,columns:Int):SparseMatrix[T] = new SparseMatrix[T](rows,columns)

  def apply[T](rows: Int, columns: Int, values: Seq[(Int,Seq[(Int,T)])])(implicit n: Numeric[T]): SparseMatrix[T] =
    new SparseMatrix(rows, columns, values.map(f=>(f._1,f._2.toMap)).toMap)(n)


  implicit class MatrixConverter[T:Numeric](values:Seq[SparseVector[T]]){
    def toMatrix: SparseMatrix[T] = apply(values)
  }


  def apply[T:Numeric](values:Seq[SparseVector[T]]):SparseMatrix[T] ={
    val rows = values.length
    val columns =  values.head.len
    values
      .zipWithIndex
      .foldLeft(new SparseMatrix[T](rows,columns))
      {case(matrix,(vector,row)) => vector.values.foldLeft(matrix){case(matrix,(index,value))=>matrix.updated(row,index,value)}}
  }




}
