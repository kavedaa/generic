package matchtypes

import tuples.A

type Inv[Y, F[_]] = Y match
  case F[x] => x

trait Foo[T]

class FooInt extends Foo[Int]

val ev1 = summon[Inv[FooInt, Foo] =:= Int]

trait Bar:
  type T

class BarInt extends Bar:
  type T = Int

type B[A] = Bar { type T = A }

val ev2 = summon[Inv[BarInt, B] =:= Int]

trait Zoo:
  type T
  type F[X]
  type Q = F[T]

trait ZooOption:
  type F[X] = Option[X]

trait Oz extends Zoo

class ZooIntOption extends Oz with ZooOption:
  type T = Int

type Z[A] = Zoo { type Q = A }

val ev3 = summon[Inv[ZooIntOption, Z] =:= Option[Int]]

type InvZ = Tuple.InverseMap[(ZooIntOption, ZooIntOption), Z]

val ev4 = summon[InvZ =:= (Option[Int], Option[Int])]

val a = (new ZooIntOption, new ZooIntOption)

val ev5 = summon[Tuple.InverseMap[a.type, Z] =:= (Option[Int], Option[Int])]
