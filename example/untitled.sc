trait Functor[F[_]]{
  def map[A, B](fa: F[A])( f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F] {
  def ap[A,B](ff: F[A => B])(fa: F[A]): F[B]
  def pure[A](a: A): F[A]
  override def map[A, B](fa: F[A])(f: A=> B) = ap(pure(f))(fa)
}

sealed trait Optional[+A] extends Product with Serializable
case class Some[A](a: A) extends Optional[A]
case object None extends Optional[Nothing]

object Optional {
  def apply[A](a: A) = if(a == null) None else Some(a)
}

implicit val optionFunctor = new Functor[Optional] {
  override def map[A, B](fa: Optional[A])( f: A => B) = fa match {
    case None => None
    case Some(x) => Some(f(x))
  }
}

implicit class FunctorOptional[A](a: Optional[A]){
  def map[B](f: A => B) = implicitly[Functor[Optional]].map(a)(f)
}

val x = Optional(12)
val y = Optional(null)


val functor = implicitly[Functor[Optional]]

functor.map(x)(_ * 100)
x.map(_ * 100)



