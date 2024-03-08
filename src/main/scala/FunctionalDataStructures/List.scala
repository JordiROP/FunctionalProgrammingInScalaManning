package FunctionalDataStructures

import scala.annotation.tailrec


enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:
  def sum(ints: List[Int]): Int =
    ints match
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)

  def product(doubles: List[Double]): Double =
    doubles match
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  // Exercise 3.2
  def tail[A](as: List[A]): List[A] =
    as match
      case Nil => sys.error("Empty")
      case Cons(_, t) => t

  // Exercise 3.3
  def setHead[A](nh: A, as: List[A]): List[A] =
    as match
      case Nil => sys.error("Empty")
      case Cons(_, t) => Cons(nh, as)

  // Exercise 3.4
  @tailrec
  def drop[A](as: List[A], n: Int): List[A] =
      if n == 0 then as
      else as match
        case Nil => Nil
        case Cons(_, t) => drop(t, n-1)

  // Exercise 3.5
  @tailrec
  def dropWhile[A](as: List[A], f:A => Boolean): List[A] = as match
      case Nil => Nil
      case Cons(h, t) =>
        if f(h) then dropWhile(t, f)
        else as

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))

  // Exercise 3.6
  def init[A](as: List[A]): List[A] =
    as match
      case Nil => sys.error("Empty")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))

  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, f(x, foldRight(xs, acc, f)))

  def sumViaFoldRight(ns: List[Int]) =
    foldRight(ns, 0, (x, y) => x + y)

  def productViaFoldRight(ns: List[Double]) =
    foldRight(ns, 1.0, _ * _)

  // Exercise 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0, (_ , y) => 1 + y)

  // Exercise 3.10
  @tailrec
  def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B =
    as match
      case Nil => acc
      case Cons(x, xs) => foldLeft(xs, f(acc, x), f)

  def sumViaFoldLeft(ns: List[Int]) =
    foldRight(ns, 0, _ +  _)

  def productViaFoldLeft(ns: List[Double]) =
    foldRight(ns, 1.0, _ * _)

  def lengthViaFoldLeft[A](as: List[A]): Int =
    foldLeft(as, 0, (y, _) => y + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A], (h, tl) => Cons(tl, h))