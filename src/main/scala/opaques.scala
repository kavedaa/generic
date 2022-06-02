package opaques

trait Convert[A, B]:
  def to(x: A): B
  def from(x: B): A

object Convert:
  def identity[A] = new Convert[A, A]:
    def to(x: A) = x
    def from(x: A) = x

object Person:

  opaque type Name = String
  given Convert[Name, String] = Convert.identity

val a = summon[Convert[Person.Name, String]]
