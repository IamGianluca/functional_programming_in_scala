package Chapter_3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /** Count the number of nodes (leaves and branches) in a tree
    *   My initial implementation is a bit rudimental but does */
  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  /** Returns the maximum element in a Tree[Int] */
  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(n) => n
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  /** Return the maximum path length from the root of a tree to any leaf */
  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 0
      case Branch(l,r) => 1 + (depth(l) max depth(r))
    }
}