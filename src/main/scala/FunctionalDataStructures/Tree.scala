package FunctionalDataStructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int =
    this match
      case Leaf(_) => 1
      case Branch(l, r) => 1 + l.size + r.size

  def depth: Int =
    this match
      case Leaf(_) => 0
      case Branch(l, r) => 1 + l.depth.max(r.depth)

  def map[B](f: A => B): Tree[B] =
    this match
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](f: A => B, g: (B, B) => B): B =
    this match
      case Leaf(v) => f(v)
      case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))

object Tree:
  extension (t: Tree[Int]) def firstPositive: Int =
    t match
      case Leaf(i) => i
      case Branch(l, r) =>
        val lpos = l.firstPositive
        if lpos > 0 then lpos else r.firstPositive

  extension (t: Tree[Int]) def maximum: Int =
    t match
      case Leaf(v) => v
      case Branch(l, r) =>
        l.maximum.max(r.maximum)
