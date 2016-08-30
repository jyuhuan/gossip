package me.yuhuan.gossip.core

import scala.reflect.ClassTag

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
trait OrderedSet[X] extends Seq[X] { self =>

  implicit def ordering: Ordering[X]

  def subsetOf(that: OrderedSet[X]): Boolean = {
    //TODO: replace with efficient algorithm
    self forall that.contains
  }

  def ⊆(that: OrderedSet[X]) = self subsetOf that

  def supersetOf(that: OrderedSet[X]): Boolean = {
    //TODO: replace with efficient algorithm
    that forall self.contains
  }

  def ⊇(that: OrderedSet[X]) = self supersetOf that

  def union(that: OrderedSet[X])(implicit ct: ClassTag[X]): OrderedSet[X] = {
    ArrayOrderedSet(self ++ that: _*)
  }

  def ⋃(that: OrderedSet[X])(implicit ct: ClassTag[X]) = self union that

//  def intersect(that: SortedSeq[X])(implicit ct: ClassTag[X]): SortedSeq[X] = {
//
//  }

  def ⋂(that: OrderedSet[X])(implicit ct: ClassTag[X]) = self intersect that

  /**
    * @return A sorted sequence resulted from merging with another sorted sequence.
    */
  def mergedWith[Y >: X: ClassTag: Ordering](that: OrderedSet[Y]): OrderedSet[Y] = {
    //TODO: replace the stupid implementation with a more efficient merging algorithm.
    ArrayOrderedSet((self.toSet ++ that.toSet).toSeq: _*)
  }

}
