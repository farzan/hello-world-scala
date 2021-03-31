package farzan.hello.list

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, p) => x + sum(p)
  }

  def product(doubles: List[Double]): Double = doubles match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("Empty list; no element")
      case Cons(_, t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => sys.error("Empty list; no head")
      case Cons(_, t) => Cons(h, t)
    }

  def drop[A](l: List[A], n: Int): List[A] = {
    @tailrec
    def loop(l: List[A], i: Int): List[A] =
      if (i == n) l
      else loop(tail(l), i + 1)
    loop(l, 0)
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("Empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  @tailrec
  def dropWhile2[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Cons(h, t) if f(h) => dropWhile2(t)(f)
      case _ => as
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(as: List[Int]): Int =
    foldRight(as, 0)((x, y) => x + y)

  def product2(as: List[Double]): Double =
    foldRight(as, 1.0)(_ * _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc) => acc + 1)

  def length2[A](as: List[A]): Int = {
    as match {
      case Nil => 0
      case Cons(_, t) => length2(t) + 1
    }
  }

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def reverse[A](as: List[A]): List[A] = {
    @tailrec
    def loop(xs: List[A], acc: List[A]): List[A] = {
      xs match {
        case Nil => Nil
        case Cons(h, Nil) => Cons(h, acc)
        case Cons(h, t) => loop(t, Cons(h, acc))
      }
    }
    loop(as, List[A]())
  }

  def reverse2[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((acc, a) => Cons(a, acc))

  // Exercise 3.13; I couldn't solve it. I event could not understand the solution :-D
//  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f:(A, B) => B): B =

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((a, b) => Cons(a, b))

  def append3[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(a1, a2)((b, a) => Cons(a, b))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, List[A]())(append)

  def addOne(l: List[Int]): List[Int] =
    l match {
      case Nil => Nil
      case Cons(h, Nil) => Cons(h + 1, Nil)
      case Cons(h, t) => Cons(h + 1, addOne(t))
    }

  def addOne2(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString.+('d'), t))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])(
      (h, t) =>
        if (f(h)) Cons(h, t)
        else t
    )

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a =>
      if (f(a)) List(a)
      else Nil
    )

  def addPairwise(l1: List[Int], l2: List[Int]): List[Int] =
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
    }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] =
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

}
