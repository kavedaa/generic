package conversion

import scala.deriving.*
import scala.compiletime.*


object First:

  case class Dog(name: String, age: Int)
  case class Cat(name: String, age: Int)

  case class Person(firstName: String, lastName: String)

  def convert[A <: Product, B <: Product](a: A)(using mb: Mirror.ProductOf[B]): B =
    mb.fromProduct(a)


object Second:

  case class Dog(name: String, age: Int)
  case class Cat(name: String, age: Int)

  case class Person(firstName: String, lastName: String)

  extension [A <: Product] (a: A) 
    def convert[B <: Product](using mb: Mirror.ProductOf[B]): B = 
      mb.fromProduct(a)

  extension [A <: Product] (a: A) 
    def convertTyped[B <: Product](using ma: Mirror.ProductOf[A], mb: Mirror.ProductOf[B], ev: ma.MirroredElemTypes =:= mb.MirroredElemTypes): B = 
      mb.fromProduct(a)

object Third:

  case class Dog(name: String, age: Dog.Age)

  object Dog:
    opaque type Age = Int
    def Age(x: Int): Age = x
    given Converter[Age, Int] = identity

  case class Cat(name: String, age: Cat.Age)

  object Cat:
    opaque type Age = Int
    def Age(x: Int): Age = x
    given Converter[Int, Age] = identity

  case class Person(name: String, pet: Dog)
  case class Robot(name: String, pet: Cat)
  case class Thing(name: String, age: Int, dog: Dog)

  trait Converter[A, B]:
    def convert(x: A): B

  object Converter:

    given [A]: Converter[A, A] with
      def convert(x: A) = x

    type Map2[Tup1 <: Tuple, Tup2 <: Tuple, F[_, _]] <: Tuple = (Tup1, Tup2) match
      case (h1 *: t1, h2 *: t2) => F[h1, h2] *: Map2[t1, t2, F]
      case _ => EmptyTuple

    inline given [A <: Product, B <: Product](using ma: Mirror.ProductOf[A], mb: Mirror.ProductOf[B]): Converter[A, B] = 
      new Converter[A, B]:
        type ElemConverters = Map2[ma.MirroredElemTypes, mb.MirroredElemTypes, Converter]
        val elemConverters = summonAll[ElemConverters].toList.asInstanceOf[List[Converter[Any, Any]]]
        def convert(a: A) =
          val elems = a.productIterator.toList
          val converted = elems.zip(elemConverters) map { (elem, converter) => converter.convert(elem) }
          val tuple = converted.foldRight[Tuple](EmptyTuple)(_ *: _)
          mb.fromProduct(tuple)      

  extension [A <: Product] (a: A) 
    def convert[B](using converter: Converter[A, B]) = converter.convert(a)


  
  // val a = Dog("Fido", Dog.Age(3)).convert[Cat]