package Chapter_3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int =
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

  def product(ds: List[Double]): Double =
    ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

  /** Generalise sum and product functions */
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](ns: List[A]): Int =
    foldRight(ns, 0)((x, acc) => 1 + acc)

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  /** Remove the first element of a List. This function takes constant time */
  def tail[A](ds: List[A]): List[A] =
    ds match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, xs) => xs
    }

  /** Replace the first element of a List with a different value */
  def setHead[A](ds: List[A], h: A): List[A] =
    ds match{
      case Nil => sys.error("setHead on empty list")
      case Cons(_, xs) => Cons(h, xs)
    }

  /** Remove the first n elements from a list. Note that this function takes
    *   time proportionally only to the number of elements being dropped; we
    *   don't need to make a copy of the entire List */
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  /** Remove elements from the List prefix as long as they match the predicate */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  /** We could curry the dropWhile function in order not to have to specify in
    *   the anonymous function f the type of the argument. More generally, when
    *   a function definition contains multiple argument groups, type information
    *   flows from left to right across these argument groups */
  def dropWhileCurrying[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  /** Add the elements of one list to the end of another */
  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  /** Return a List consisting of all but the last element of a List */
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  /** Our implementation of foldRight is not tail-recursive and will return
    *   a StackOverflowError for large lists (we say is not stack-safe).
    *   foldLeft is a tail-recursive implementation of foldRight */
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def sum3(as: List[Int]) =
    foldLeft(as, 0)((x, y) => x + y)

  def product3(as: List[Int]) =
    foldLeft(as, 1.0)((x, y) => x * y)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  /** Write foldRight in terms of foldLeft. This is useful because it lets us
    *   implement foldRight tail-recursively, which means it works even for
    *   large lists without overflowing the stack */
  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  /** Implement append in terms of foldLeft */
  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((h, acc) => Cons(h, acc))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  /** Concatenate a list of lists into a single list. Its runtime should be
    *   linear in the total length of all lists */
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  /** Transform a list of integers by adding 1 to each element */
  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((h, t) => Cons(h+1, t))

  /** Turn each value in a List[Double] into a String */
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((h, t) => Cons(h.toString, t))

  /** Generalise modifying each element in a list while maintaining the
    *   structure of the list. We use the foldRightViaFoldLeft to avoid stack-overflow */
  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(l, Nil:List[B])((h, t) => Cons(f(h), t))

  /** Remove elements from a list unless they satisfy a given predicate */
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRightViaFoldLeft(l, Nil:List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  /** Works like a map except that the function given will return a list
    *   instead of a single result, and that list should be inserted in the
    *   final resulting list */
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  /** Use flatMap to implement filter */
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(h => if (f(h)) List(h) else Nil)

  /** Function which accepts two lists and construct a new list by adding corresponding
    *   elements */
  def addPairWise(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairWise(t1, t2))
    }

  /** Generalise addPairWise to other data types and operations */
  def zipWith[A](a: List[A], b: List[A])(f: (A, A) => A): List[A] =
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  /** Check whether a List contains another List as a sub-sequence */
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match {
      case (Cons(h1, t1), Cons(h2, t2)) => if (h1 == h2) hasSubsequence(t1, t2) else hasSubsequence(t1, sub)
      case (Nil, Nil) => true
      case (Nil, _) => false
      case (_, Nil) => true
    }
}