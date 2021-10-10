object chapter3 {
  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }

    // 3.2
    def tail[A](list: List[A]): List[A] = list match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(_, y) => y
    }

    // 3.3
    def setHead[A](value: A, list: List[A]) = list match {
      case Nil => Cons(value, Nil)
      case Cons(_, y) => Cons(value, y)
    }

    // 3.4
    def drop[A](l: List[A], n: Int): List[A] = l match {
      case Nil => Nil
      case Cons(_, y) => if (n == 1) y else drop(y, n - 1)
    }

    // 3.5
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, y) => if (f(x)) y else dropWhile(y, f)
    }


    def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, y) => if (f(x)) y else dropWhile2(y)(f)
    }

    // 3.6
    def init[A](l: List[A]): List[A] = l match {
      case Cons(x, Cons(_, Nil)) => Cons(x, Nil)
      case Cons(x, y) => Cons(x, init(y))
    }

    def next[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }

    // (3.8)
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => /* println("foldRight="+x); */ f(x, foldRight(xs, z)(f))
    }

    // 3.9
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => /*println("foldLeft: "+ z + " , " + x); */ foldLeft(xs, f(z, x))(f)
    }

    // 3.13
    def fold_rl[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
      foldLeft(as, (x: B) => x)((g, a) => b => g(f(a, b)))(z)
    }

    // 3.11
    def sum_left[A](list: List[Int]) = {
      foldLeft(list, 0)(_ + _)
    }

    // 3.11
    def product_left[A](list: List[Int]) = {
      foldLeft(list, 1)(_ * _)
    }

    // 3.11
    def length_left[A](list: List[Int]) = {
      foldLeft(list, 0)((x, _) => 1 + x)
    }

    // 3.11
    def sum2(ns: List[Int]) = {
      foldRight(ns, 0)((x, y) => x + y)
    }

    // 3.11
    def product2(ns: List[Int]) = {
      foldRight(ns, 1)(_ * _)
    }

    // 3.9
    def length[A](as: List[A]): Int = {
      foldRight(as, 0)((_, y) => 1 + y)
    }

    // 3.12
    def reverse[A](l: List[A]) = {
      foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))
    }

    // 3.13
    def length_rl[A](as: List[A]): Int = {
      fold_rl(as, 0)((_, y) => 1 + y)
    }

    // 3.14
    def append[A](a1: List[A], a2: List[A]): List[A] = {
      foldRight(a1, a2)((x, xs) => Cons(x, xs))
    }

    // 3.15
    def flat[A](list: List[List[A]]) = {
      foldRight(list, Nil: List[A])(append)
    }

    // 3.16
    def ppluse(list: List[Int]) = {
      foldRight(list, Nil: List[Int])((x, xs) => Cons(x + 1, xs))
    }

    // 3.17
    def dmap(list: List[Double]) = {
      foldRight(list, Nil: List[String])((x, xs) => Cons(x.toString(), xs))
    }

    // 3.18
    def map[A, B](list: List[A])(f: A => B): List[B] = {
      foldRight(list, Nil: List[B])((x, xs) => Cons(f(x), xs))
    }

    // 3.19
    def filter[A](as: List[A])(f: A => Boolean): List[A] = {
      foldRight(as, Nil: List[A])((x, xs) => {
        if (f(x))
          Cons(x, xs)
        else
          xs
      })
    }

    // 3.20
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
      flat(map(as)(f))
    }

    // 3.21
    def flatFilter[A](as: List[A])(f: A => Boolean): List[A] = {
      flatMap(as)(l => {
        if (f(l))
          List(l)
        else
          Nil
      })
    }

    // 3.22
    def zipNum(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, zipNum(xs, ys))
    }

    // 3.23
    def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    }

    // 3.24
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      def checkSeq(node: List[A], s_node: List[A]): Boolean = (node, s_node) match {
        case (Nil, _) => true
        case (_, Nil) => true
        case (Cons(x, xs), Cons(y, ys)) => (x == y && checkSeq(xs, ys))
      }

      def loop(node: List[A]): Boolean = node match {
        case Nil => false
        case xs => if (checkSeq(xs, sub)) true else loop(next(xs))
      }

      loop(sup)
    }


  }


  sealed trait Tree[+A]

  case

  class Leaf[A](value: A) extends Tree[A]

  case

  class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    // 3.25
    def size[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

    // 3.26
    def maximum(t: Tree[Int]): Int = t match {
      case Leaf(x) => x
      case Branch(x, y) => maximum(x) max maximum(y)
    }

    // 3.27
    def depth[A](t: Tree[A]): Int = t match {
      case Leaf(x) => 1
      case Branch(x, y) => (depth(x) + 1) max (depth(y) + 1)
    }

    // 3.28
    def map1[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(x, y) => Branch(map1(x)(f), map1(y)(f))
    }

    // 3.29
    def fold[A, B](t: Tree[A])(f: (A => B))(g: (B, B) => B): B = t match {
      case Leaf(x) => f(x)
      case Branch(x, y) => g(fold(x)(f)(g), fold(y)(f)(g))
    }

    // 3.29
    def size_f[A](t: Tree[A]): Int = {
      fold(t)(_ => 1)(_ + _ + 1)
    }

    // 3.29
    def map_f[A, B](t: Tree[A])(f: A => B) = {
      fold(t)(x => Leaf(f(x)): Tree[B])((x, y) => Branch(x, y))
    }
  }

  def main(args: Array[String]) = {
    val list = List(1, 2, 3, 4)
    println(list)
    println(List.tail(list))
    println(List.setHead(5, list))
    println(List.drop(list, 1))
    println(List.dropWhile(list, (a: Int) => a == 3))
    println(List.dropWhile2(list)(a => a == 4))
    println(List.init(list))
    println(List.product2(List(1, 2, 3, 0, 2, 1, 2)))
    println(List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))

    println("length = " + List.length(list))
    println("length = " + List.length(List(5, 4, 3, 2, 1)))

    println("reverse = " + List.reverse(list))

    println("rlength = " + List.length_rl(List(5, 4, 3, 2, 1)))

    println("llength = " + List.length_left(list))
    println("llength = " + List.length_left(List(5, 4, 3, 2, 1)))
    println("append = " + List.append(List(1, 2, 3, 4), List(5, 6, 7, 8)))

    println("flat = " + List.flat(List(List(1, 2, 3), List(4, 5, 6))))
    println("ppluse = " + List.ppluse(List(1, 2, 3, 4)))
    println("dmap = " + List.dmap(List(1, 2, 3, 4)))
    println("map = " + List.map(List(1, 2, 3, 4))(x => x + 10))


    println("filter = " + List.filter(List(1, 2, 3, 4, 5, 6, 7, 8))(x => (x % 2 == 0)))

    println("flatMap = " + List.flatMap(List(1, 2, 3))(i => List(i, i)))

    println("flatFilter = " + List.flatFilter(List(1, 2, 3, 4, 5, 6))(x => x % 2 == 1))

    println("has subseq = " + List.hasSubsequence(List(1, 2, 3, 4, 5, 6, 7), List(2, 3)))
    println("has subseq = " + List.hasSubsequence(List(1, 2, 3, 4, 5, 6, 7), List(3, 2)))
    println("has subseq = " + List.hasSubsequence(List(1, 2, 3, 4, 5, 6, 7), List(7)))
    println("has subseq = " + List.hasSubsequence(List(1, 2, 3, 4, 5, 6, 7), List(8)))

    // 3.1
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    println(x)


    val tree = Branch(Branch(Leaf(2), Branch(Leaf(3), Leaf(10))), Leaf(1))

    println("tree size = " + Tree.size(tree))
    println("tree size_f = " + Tree.size_f(tree))

    println("tree max= " + Tree.maximum(tree))

    println("tree depth= " + Tree.depth(tree))

    println("tree map= " + Tree.map1(tree)(x => x + 1))
    println("tree map= " + Tree.map_f(tree)(x => x - 1))

  }
}
