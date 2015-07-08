package strictness

import Stream._

sealed trait Stream[+A] {
  /** Optionally extract the head of a stream */
  def headOption: Option[A] =
    this match {
      case Empty =>  None
      case Cons(h, t) => Some(h())
    }

  /** Convert a Stream to a List, which will force its evaluation and let you look at it in the REPL */
  def toList: List[A] =
    this match {
      case Cons(h,t) => h() :: t().toList
      case _ => List()
    }

  /** Return the first n element of a Stream */
  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, t) if n == 1 => cons(h(), empty)
      case _ => empty
    }

  /** Skip the first n elements of a Stream */
  def drop(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 => t().drop(n - 1)
      case _ => empty
    }

  /** Return all starting elements of a Stream that match the given predicate */
  def takeWhile(f: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if f(h()) => cons(h(), t() takeWhile f)
      case _ => empty
    }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  /** Smart constructor for creating a nonempty stream */
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  /** Smart constructor for creating an empty stream of a particular type */
  def empty[A]: Stream[A] = Empty

  /** Convenient variable-argument method for constructing a Stream from multiple elements */
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}