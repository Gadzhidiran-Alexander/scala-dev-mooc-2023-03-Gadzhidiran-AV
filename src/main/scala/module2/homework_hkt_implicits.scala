package module2

import scala.language.implicitConversions

object homework_hkt_implicits {

  import Converters._

  def tupleF[F[_], A, B](fa: F[A], fb: F[B])
                        (implicit convA: F[A] => Bindable[F, A], convB: F[B] => Bindable[F, B]): F[(A, B)] = {
    tupleBindable(convA(fa), convB(fb))
  }

  def callOpt[X, Y](x: Option[X], y: Option[Y]): Option[(X, Y)] = {
    tupleF(x, y)
  }

  def callList[X, Y](x: List[X], y: List[Y]): List[(Any, Any)] = {
    tupleF(x, y)
  }

  //noinspection Duplicates
  trait Bindable[F[_], A] {
    def map[B](f: A => B): F[B]
    def flatMap[B](f: A => F[B]): F[B]
  }

  private def tupleBindable[F[_], A, B](fa: Bindable[F, A], fb: Bindable[F, B]): F[(A, B)] = {
    fa.flatMap(a => fb.map((a,_)))
  }

  object Converters {
    //noinspection Duplicates
    implicit def optBindable[A](opt: Option[A]): Bindable[Option, A] = new Bindable[Option, A] {
      override def map[B](f: A => B): Option[B] = opt.map(f)

      override def flatMap[B](f: A => Option[B]): Option[B] = opt.flatMap(f)
    }

    implicit def listBindable[A](list: List[A]): Bindable[List, A] = new Bindable[List, A] {
      override def map[B](f: A => B): List[B] = list.map(f)

      override def flatMap[B](f: A => List[B]): List[B] = list.flatMap(f)
    }
  }

}
