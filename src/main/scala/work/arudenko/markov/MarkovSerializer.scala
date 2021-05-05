package work.arudenko.markov

import work.arudenko.markov.discrete.DiscreteMarkovChain

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag
import cats.syntax.functor._
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import cats.kernel.Semigroup
import cats.syntax.functor._
import work.arudenko.linalg.SparseMatrix
import scala.util.matching.Regex

object MarkovSerializer {
  implicit val matrixDecoder: Decoder[SparseMatrix[Double]] =
    Decoder.forProduct3[SparseMatrix[Double],Int,Int,Seq[(Int,Seq[(Int,Double)])]]("rows","columns","values")((f,s,t)=>SparseMatrix.apply(f,s,t))

  implicit val matrixEncoder: Encoder.AsObject[SparseMatrix[Double]] =
    Encoder.forProduct3[SparseMatrix[Double],Int,Int,Seq[(Int,Seq[(Int,Double)])]]("rows","columns","values")(m=>(m.rows,m.columns,m.values.toSeq.map(v=>(v._1,v._2.toSeq))))


  implicit def discreteMarkovChainDecoder[T:ClassTag](implicit valDecoder:Decoder[T]):Decoder[DiscreteMarkovChain[T]] =
    (c: HCursor) => {
      for {
        values <- c.downField("values").as[ArraySeq[T]]
        weights <- c.downField("weights").as[SparseMatrix[Double]]
      } yield DiscreteMarkovChain(values,weights)
    }

  implicit def discreteMarkovChainEncoder[T:ClassTag](implicit valEncoder:Encoder[T]):Encoder[DiscreteMarkovChain[T]] =
    (a: DiscreteMarkovChain[T]) => Json.obj(
      ("values", Json.arr(a.values.map(valEncoder.apply): _*)),
      ("weights", a.weights.asJson)
    )

  implicit def encodeAnswer[V:ClassTag](implicit valEncoder:Encoder[V]): Encoder[MarkovChain[V]] = Encoder.instance {
    case a: DiscreteMarkovChain[V] =>a.asJson(discreteMarkovChainEncoder)

  }
}
