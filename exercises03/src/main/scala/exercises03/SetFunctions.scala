package exercises03

object SetFunctions {
  type Set[A] = A => Boolean

  def contains[A](s: Set[A], elem: A): Boolean = s(elem)
  def singletonSet[A](elem: A): Set[A] = Set(elem)

  def union[A](s: Set[A], t: Set[A]): Set[A] = el => s(el) || t(el)

  def intersect[A](s: Set[A], t: Set[A]): Set[A] = el => s(el) && t(el)

  def diff[A](s: Set[A], t: Set[A]): Set[A] = el => s(el) && !t(el)

  def symmetricDiff[A](s: Set[A], t: Set[A]): Set[A] = el => (s(el) && !t(el)) || (!s(el) && t(el))

  def filter[A](s: Set[A], p: A => Boolean): Set[A] = el => s(el) && p(el)

  def cartesianProduct[A, B](as: Set[A], bs: Set[B]): Set[(A, B)] = {

  }
}
