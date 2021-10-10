object chapter2 {
  // 2.1
  def fibo(n: Int): Int = {
    def recursive(x: Int): Int = {
      if (x <= 1) x
      else recursive(x - 1) + recursive(x - 2)
    }

    recursive(n)
  }

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (!ordered(as(n), as(n + 1))) false
      else if (n < as.length - 2)
        loop(n + 1)
      else true
    }

    loop(0)
  }

  // 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  // 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  // 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

  def main(args: Array[String]) = {
    println(fibo(3))
    val arr = Array(1, 2, 4, 4)

    def ordered(a: Int, b: Int): Boolean = {
      if (a < b) true
      else false
    }

    println(isSorted(arr, ordered))
  }
}
