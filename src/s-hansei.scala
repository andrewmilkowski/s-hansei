import scala.continuations._
import scala.continuations.ControlContext._

sealed abstract class vc[a]
case class pV[a](x: List[Pair[Double, vc[a]]]) extends vc[a]
case class V[a](x: a) extends vc[a]
case class C[a](x: () => pV[a]) extends vc[a]

object s_hansei extends Application
{
  trait MonadCompanion[M[_]]
  {
    // returns the computation that yields the given value 
	implicit def result[A](x: A): M[A]
  }

  trait Monad[A, M[_]]
  {
    implicit val meta: MonadCompanion[M]  
	import meta._
		  
    // chains this computation with the next one, `f`
    def >>=[B](f: A => M[B]): M[B] 
		  
		  
    def map[B](f: A => B): M[B]        = >>= {x => result(f(x))}
    def flatMap[B](f: A => M[B]): M[B] = >>=(f)
  }

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

 /* The State monad was gleefully ripped off from Adriaan Moors. */
  trait StateMonadCompanion[M[_], State] extends MonadCompanion[M]
  {
    def apply[A](fun: State => (A, State)): M[A]

    implicit def result[A](x: A): M[A] = apply((x, _))
  
    def update(f: State => State): M[State] = apply{s: State => (s, f(s))}
    def set(s: State): M[State] = update(x => s)
    def fetch = update(x => x)
  }

  trait StateMonad[A, M[x] <: StateMonad[x, M, State], State] extends Monad[A, M]
  {
    implicit override val meta: StateMonadCompanion[M, State]
    import meta._
  
    val initState: State
    val stateTrans: State => (A, State)

    def >>=[B](f: A => M[B]) = apply
      { currState => 
        val (a, nextState) = stateTrans(currState); f(a).stateTrans(nextState)
      }
        
    def run = stateTrans(initState)._1
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
