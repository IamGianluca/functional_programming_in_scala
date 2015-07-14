package strictness

import Stream._

sealed trait Stream[+A] {
  /** Optionally extract the head of a Stream */
  def headOption: Option[A] =
    this match {
      case Empty =>  None
      case Cons(h, t) => Some(h())
    }

  /** Write headOption using foldRight */
  def headOption2: Option[A] =
    foldRight(None: Option[A])((h, t) => Some(h))

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

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), x) if n > 1 => Some((h(), (t(), x - 1)))
      case _ => None
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

  /** Write takeWhite using foldRight */
  def takeWhile2(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else empty)

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  /** Check whether an element matching a Boolean function exists in this stream */
  def exists(p: A => Boolean): Boolean =
    this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

  /** Write exists in terms of foldRight */
  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  /** Implement a general recursion */
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  /** Check that all elements in the Stream match a given predicate */
  def forAll(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  /** Function which accepts two streams and construct a new stream by passing a function over the corresponding
    *   pairwise elements */
  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  /** Differently from zipWith, this function should continue the traversal as long as either stream has more elements;
    *   it uses Option to indicate whether each stream has been exhausted */
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_, _))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
  unfold((this, s2)) {
    case (Empty, Empty) => None
    case (Cons(h1, t1), Empty) => Some(f(Some(h1()), Option.empty[B]) -> (t1(), empty[B]))
    case (Empty, Cons(h2, t2)) => Some(f(Option.empty[A], Some(h2())) -> (empty[A] -> t2()))
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
  }

  /** Check if one Stream is a prefix of another
    * Source: https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/laziness/Stream.scala */
  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined) forAll {
      case (h, h2) => h == h2
    }

  /** Return the Stream of suffixes of the input sequence, starting from the original Stream */
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) => if (f(h)) cons(h, t) else t)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, append(t)))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption
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

  /** Example of an infinite Stream */
  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  /** This is more efficient than `cons(a, constant(a))` since it's just one object referencing itself
    * Source: https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/laziness/Stream.scala */
  def constantViaLazyVal[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(a => Some((a, a)))

  /** Function which generates an infinite stream of integers, starting from n, then n + 1, n + 2, and so on */
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n + 1)))

  /** Function that generates the infinite stream of Fibonacci numbers
    * TODO: It seems fine to me but console says there is a type mismatch (?) */
  val fib = {
    def fibs(n: Int = 0, acc: Int = 1): Stream[Int] =
      cons(n, fibs(n + acc, n))
    fibs()
  }

  /** Implement fibs using unfold */
  def fibsViaUnfold(start: Int = 0, accumulator: Int = 1): Stream[Int] =
    unfold((start, accumulator)) {
      case(n, acc) => Some((n, (acc, n + acc)))
    }

  /** General stream building function
    * Source: https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/laziness/Stream.scala */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, t)) => cons(h, unfold(t)(f))
      case None => empty
    }
}