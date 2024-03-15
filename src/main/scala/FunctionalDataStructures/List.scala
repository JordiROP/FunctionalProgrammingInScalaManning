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

  private def product(doubles: List[Double]): Double =
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
  @tailrec
  private def drop[A](as: List[A], n: Int): List[A] =
      if n == 0 then as
      else as match
        case Nil => Nil
        case Cons(_, t) => drop(t, n-1)

  // Exercise 3.5
  @tailrec
  private def dropWhile[A](as: List[A], f:A => Boolean): List[A] = as match
      case Nil => Nil
      case Cons(h, t) =>
        if f(h) then dropWhile(t, f)
        else as

  private def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))

  // Exercise 3.6
  private def init[A](as: List[A]): List[A] =
    as match
      case Nil => sys.error("Empty")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))

  private def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, f(x, foldRight(xs, acc, f)))

  def sumViaFoldRight(ns: List[Int]): Any =
    foldRight(ns, 0, (x, y) => x + y)

  def productViaFoldRight(ns: List[Double]): Any =
    foldRight(ns, 1.0, _ * _)

  // Exercise 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0, (_ , y) => 1 + y)

  // Exercise 3.10
  @tailrec
  private def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B =
    as match
      case Nil => acc
      case Cons(x, xs) => foldLeft(xs, f(acc, x), f)

  // Exercise 3.11
  def sumViaFoldLeft(ns: List[Int]): Any =
    foldRight(ns, 0, _ +  _)

  def productViaFoldLeft(ns: List[Double]): Any =
    foldRight(ns, 1.0, _ * _)

  def lengthViaFoldLeft[A](as: List[A]): Int =
    foldLeft(as, 0, (y, _) => y + 1)

  // Exercise 3.12
  private def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A], (h, tl) => Cons(tl, h))

  // Exercise 3.13
  def foldRightInTermsOfFoldLeft[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(as), acc, (b, a) => f(a, b))

  def foldLeftInTermsOfFoldRight[A, B](as: List[A], acc: B, f: (B, A) => B): B =
    foldRight(as, (b: B) => b, (a, g) => b => g(f(b, a)))(acc)

  // Exercise 3.14
  def appendInTermsOfFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2, Cons(_, _))

  // Exercise 3.15
  def concat[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A], append)

  // Exercise 3.16
  def sum1ToIntList(as: List[Int]): List[Int] =
    foldRight(as, Nil: List [Int], (i, acc) => Cons(i + 1, acc))

  // Exercise 3.17
  def doubleListToStringList(as: List[Double]): List[String] =
    foldRight(as, Nil: List[String], (d, acc) => Cons(d.toString, acc))

  // Exercise 3.18
  def map[A, B](as: List[A], f: A=>B): List[B] =
    foldRight(as, Nil:List[B], (h, acc) => Cons(f(h), acc))

  // Exercise 3.19
  def filter[A](as: List[A], f: A =>Boolean): List[A] =
    foldRight(as, Nil:List[A], (elem, acc) => if f(elem) then Cons(elem, acc) else acc)

  // Exercise 3.20
  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B], (e, acc) => append(f(e), acc))

  // Exercise 3.21
  def flatMapFilter[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, a => if f(a) then List(a) else Nil)

  // Exercise 3.22
  private def sumValuesByPos(a: List[Int], b:List[Int]): List[Int] =
     (a, b) match
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(ah, at), Cons(bh, bt)) => Cons(ah+bh, sumValuesByPos(at, bt))

  // Exercise 3.22
  private def applyFuncByPos[A](a: List[A], b: List[A], f: (A, A) => A): List[A] =
    (a, b) match
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah, bh), applyFuncByPos(at, bt, f))

  // Exercise 3.22 (book solution)
  private def zipWith[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] =
    (a, b) match
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah, bh), zipWith(at, bt, f))
