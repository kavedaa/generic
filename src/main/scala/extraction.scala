package extraction

import scala.compiletime.*

trait Extractor[A, F[_]]:
  type B
  def extract(a: A): B

type DeepInverseMap[Tup <: Tuple, F[_]] <: Tuple = Tup match
  case F[h] *: t => h *: DeepInverseMap[t, F]
  case DeepInverseMap[h, F] *: t => h *: DeepInverseMap[t, F]
  case EmptyTuple => EmptyTuple

object Extractor:
  transparent inline given [Tup <: Tuple, F[_]]: Extractor[Tup, F] = new Extractor[Tup, F]:
    type B = DeepInverseMap[Tup, F]
    type X[A] = Extractor[A, F]
    val extractors = summonAll[Tuple.Map[Tup, X]].productIterator.toList.asInstanceOf[List[Extractor[Any, F]]]
    def extract(a: Tup): B =
      val elems = a.productIterator.toList
      val extracted = elems.zip(extractors) map { (elem, extractor) => extractor.extract(elem) }
      extracted.foldRight[Tuple](EmptyTuple)(_ *: _).asInstanceOf[B]

extension [Tup <: Tuple](x: Tup)
  transparent inline def extract[F[_]](using extractor: Extractor[Tup, F]) = extractor.extract(x)

case class Foo[T](x: T)

object Foo:
  given [T]: Extractor[Foo[T], Foo] with
    type B = T
    def extract(a: Foo[T]) = a.x

case class Bar[T](x: T)

object Bar:
  given [T]: Extractor[Bar[T], Bar] with
    type B = T
    def extract(a: Bar[T]) = a.x
