package catstypes.homework

trait Monad[F[_]] extends Functor[F] { self =>
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def point[A](a: A): F[A]

  def flatten[A](fa: F[F[A]]): F[A] = flatMap(fa)(v => v)
}

object Monad {

  implicit val option: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(a => f(a))

    override def point[A](a: A): Option[A] = Option(a)

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(a => f(a))
  }

  implicit val list: Monad[List] = new Monad[List] {
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(a => f(a))

    override def point[A](a: A): List[A] = List(a)

    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(a => f(a))
  }

  implicit val set: Monad[Set] = new Monad[Set] {
    override def flatMap[A, B](fa: Set[A])(f: A => Set[B]): Set[B] = fa.flatMap(a => f(a))

    override def point[A](a: A): Set[A] = Set(a)

    override def map[A, B](fa: Set[A])(f: A => B): Set[B] = fa.map(a => f(a))
  }

  def apply[F[_]](implicit ev: Monad[F]): Monad[F] = ev

  implicit class MonadOps[A](a: A) {
    def pure[F[_]](implicit ev: Monad[F]): F[A] = ev.point(a)
  }

}