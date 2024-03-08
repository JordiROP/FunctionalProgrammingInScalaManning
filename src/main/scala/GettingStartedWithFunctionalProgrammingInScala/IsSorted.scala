package GettingStartedWithFunctionalProgrammingInScala

object IsSorted:

  private def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean =
    @annotation.tailrec
    def recIsSorted(pos: Int): Boolean =
      if as.isEmpty || as.length == pos + 1 then true
      else if gt(as(pos), as(pos + 1)) then false
      else recIsSorted(pos+1)

    recIsSorted(0)


  @main def printIsSorted(): Unit =
    println(isSorted(Array(1, 2, 3), _>_))
    println(isSorted(Array(1, 2, 1), _>_))
    println(isSorted(Array(3, 2, 1), _<_))
    println(isSorted(Array(1, 2, 3), _<_))