package net.katsstuff.bukkit.katlib.util

import scala.concurrent.{ExecutionContext, Future}

import cats.syntax.all.*
import cats.{Eval, Monad, StackSafeMonad, Traverse}

case class FutureOrNow[+A](value: Either[Future[A], Eval[A]]) {

  def asFuture: Future[A] = value.map(e => Future.successful(e.value)).merge

  def map[B](f: A => B)(using ExecutionContext): FutureOrNow[B] = FutureOrNow(value.bimap(_.map(f), _.map(f)))

  def flatMap[B](f: A => FutureOrNow[B])(using ExecutionContext): FutureOrNow[B] = value match
    case Left(value)  => FutureOrNow.fromFuture(value.flatMap(f.andThen(_.asFuture)))
    case Right(value) => f(value.value)

  def foreach(f: A => Unit)(using ExecutionContext): Unit = value match
    case Left(fut)   => fut.foreach(f)
    case Right(eval) => f(eval.value)

  def zip[B](that: FutureOrNow[B]): FutureOrNow[(A, B)] = (value, that.value) match
    case (Right(e1), Right(e2)) => FutureOrNow.fromEval(e1.flatMap(a => e2.map(b => (a, b))))
    case (_, _)                 => FutureOrNow.fromFuture(this.asFuture.zip(that.asFuture))

  def isNow: Boolean = value.isRight
  
  def memoize: FutureOrNow[A] = value match
    case Left(_) => this
    case Right(eval) => FutureOrNow.fromEval(eval.memoize)
}
object FutureOrNow {

  def fromEval[A](eval: Eval[A]): FutureOrNow[A] = FutureOrNow(Right(eval))

  def now[A](value: A): FutureOrNow[A] = FutureOrNow(Right(Eval.now(value)))

  def fromFuture[A](value: Future[A]): FutureOrNow[A] = FutureOrNow(Left(value))

  def sequence[A, M[_]: Traverse](in: M[FutureOrNow[A]])(using ExecutionContext): FutureOrNow[M[A]] =
    val allNow = in.foldLeft(true)(_ && _.isNow)

    if allNow then fromEval(Eval.later(in.map(_.value.getOrElse(sys.error("Forall wrong")).value)))
    else fromFuture(in.traverse(_.asFuture))

  // No idea if this is actually stack safe or not
  given (using ExecutionContext): Monad[FutureOrNow] with StackSafeMonad[FutureOrNow] with {
    override def pure[A](value: A): FutureOrNow[A] = now(value)

    override def map[A, B](fa: FutureOrNow[A])(f: A => B): FutureOrNow[B] = fa.map(f)

    override def flatMap[A, B](fa: FutureOrNow[A])(f: A => FutureOrNow[B]): FutureOrNow[B] = fa.flatMap(f)
  }
}
