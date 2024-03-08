package GettingStartedWithFunctionalProgrammingInScala

object Fibonacci:

  private def fib(n: Int): Int =
    @annotation.tailrec
    def recFib(step: Int, prev: Int, current: Int): Int =
      if n == 0 || n == 1 then n
      else if n==step then current
      else recFib(step + 1, current, prev + current)

    recFib(1, 0, 1)

  @main def printFibonacci(): Unit =
    println(fib(23))