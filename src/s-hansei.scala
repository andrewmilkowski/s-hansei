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

  class Reflective[+A, C[_]](xs: Monadic[A,C])
  { 
    def reflect[B](): A @cps[C[B], C[B]] =
    { 
	  shift { k:(A => C[B]) => xs.flatMap(k) }
	}
  } 

  implicit def reflective[A](xs:Iterable[A]) = new Reflective[A,Iterable](xs) 

 /* The State monad was gleefully ripped off from scalaz, and is therefore due to Tony Morris
    with surgical modifications due to James Iry. */
	sealed trait State[S, +A]
	{
  	def apply(s: S): (S, A)

  	def map[B](f: A => B): State[S, B] = State.state(apply(_) match
		{
    	case (s, a) => (s, f(a))
  	})

  	def flatMap[B](f: A => State[S, B]): State[S, B] = State.state(apply(_) match
		{
    	case (s, a) => f(a)(s)
		})
  
  	def !(s: S) = apply(s)._2

  	def ~>(s: S) = apply(s)._1

  	def withs(f: S => S): State[S, A] = State.state(f andThen (apply(_)))
	}

	object State
	{
  	def state[S, A](f: S => (S, A)) = new State[S, A]
		{
    	def apply(s: S) = f(s)
  	}

  	def init[S] = state[S, S](s => (s, s))

  	def modify[S](f: S => S) = init[S] flatMap (s => state(_ => (f(s), ())))

  	def unit[S,A](value : A) = state{x : S => (x, value)}
 
  	def put[S](newState : S) = modify[S]{x : S => newState}

  	def get[S] = state{x : S => (x, x)}
  }

/*
	class Reflective[S, +A](xs: State[S, A])
  {
    def reflect[B](): A @cps[State[S, B], State[S, B]] =
	{ 
	  shift { k:(A => State[S, B]) => xs.flatMap(k) }
	} 
  }

  implicit def reflective[S, A](xs:State[S, A]) = new Reflective(xs)
*/

  def test1() : Int =
  {
	  reset( shift { k: (Int => Int) => k(k(k(7))) } + 1 ) * 2
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

  def test3() =
  {
    reset
    { 
      val left = List("x","y","z") 
	  val right = List(4,5,6) 
	  List((left.reflect[Any], right.reflect[Any])) 
    } 
    // result: cartesian product
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
    println(test3())
  }
}
