import scala.continuations._
import scala.continuations.ControlContext._

sealed abstract class vc[a]
case class pV[a](x: List[Pair[Double, vc[a]]]) extends vc[a]
case class V[a](x: a) extends vc[a]
case class C[a](x: () => pV[a]) extends vc[a]

object s_hansei extends Application
{
  type Monadic[+U, C[_]] =
  {
    def flatMap[V](f: U => C[V]): C[V]
  }

  class Reflective[+A, C[_]](xs: Monadic[A, C])
  {
    def reflect[B](): A @cps[C[B], C[B]] =
    {
      shift { k: (A => C[B]) => xs.flatMap(k) }
    }
  }

  /* The State monad was gleefully ripped off from scalaz, and is therefore due to Tony Morris */
  sealed trait State[S, +A]
  {
    def apply(s: S): (S, A)

    def map[B](f: A => B): State[S, B] =
      State.state(apply(_) match
      {
	    case (s, a) => (s, f(a))
	  })

	def flatMap[B](f: A => State[S, B]): State[S, B] =
	  State.state(apply(_) match
	  {
	    case (s, a) => f(a)(s)
	  })
	  
	def !(s: S) = apply(s)._2

	def ~>(s: S) = apply(s)._1

	def withs(f: S => S): State[S, A] = State.state(f andThen (apply(_)))
  }

  object State
  {
    def state[S, A](f: S => (S, A)) =
      new State[S, A]
	  {
	    def apply(s: S) = f(s)
	  }

	def init[S] = state[S, S](s => (s, s))

	def modify[S](f: S => S) = init[S] flatMap (s => state(_ => (f(s), ())))
  }

  def test1() =
  {
    reset
	{
	  shift
	  {
	    k: (Int=>Int) => k(k(k(7)))
	  } + 1
	} * 2
  }

  def foo() =
  {
    1 + shift{k: (Int=>Int) => k(k(k(7)))}
  }

  def bar() =
  {
    foo() * 2
  }

  def baz() =
  {
    reset(bar())
  }

  def pv_unit[a](x : a): pV[a] = pV(List((1.0, V(x))))

  def dist[a](ch : List[Pair[Double, a]]) : a @cps[pV[a], List[(Double, C[a])]] =
  {
    shift
    {
	  k: (a => pV[a]) => 
	  ch map {x: Pair[Double, a] => (x._1, C (() => k(x._2)))}
	}
  }

  def reify0[a](m: (() => a @cps[pV[a], pV[a]])) : pV[a] = { reset (pv_unit (m ())) }

  override def main(args : Array[String]) =
  {
    println(test1())		// result 20
    println(baz())			// result 70
  }
}
