package error_handling

sealed trait Option[+A] {
  /** Transform result inside an Option, if it exist */
  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }

  /** Return the result inside the Some case of the Option, or if the Option
    *   is None, return the given default value */
  def getOrElse[B >: A](default: => B): B =
    this match {
      case None => default
      case Some(a) => a
    }

  /** Similar to map, except that the function we provide to transform the
    *   result can itself fail */
  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  /** Similar to getOrElse except that we return another Option if the first
    *   is undefined */
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  /** Convert successes into failures if the successful values don't match
    *   the given predicate */
  def filter(f: A => Boolean): Option[A] =
    this match {
      case Some(a) if f(a) => this
      case _ => None
    }

  /** TO-DO: Understand map2 implementation: Why does this even compile?
    *   The f(aa, bb) shouldn't comply with the map() design. The map() function
    *   in fact applies a function f: A => B and not f: (A, B) => C */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  def map2_using_for_comprehension[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  /** Combines a list of Options into one Option containing a list of all
    *   the Some values in the original list. If the original list contains
    *   None even once, the result of the function should be None; otherwise
    *   the result should be Some with a list of all the values */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => None
      case h :: t => h flatMap(hh => sequence(t) map (hh :: _))
    }

  /** Map over a list using a function that might fail, returning None if
    *   applying it to any element of the list returns None */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /** Thanks to flatMap we can construct a computation with multiple
    *   stages, any of which might fail, and the computation will abort
    *   as soon as the first failure is encountered */
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
}

