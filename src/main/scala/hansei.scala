package hansei

import scala.collection.generic.BuilderFactory

import scala.continuations._
import scala.continuations.ControlContext._

import scalaz.State
import scalaz.State._

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

  implicit def reflective_list[A](xs:List[A]) = new
  { // 2.8 collections have a more complicated flatMap mechanism
    def reflect[B](implicit bf: BuilderFactory[B, List[B], List[A]]): A @cps[List[B], List[B]] =
    {
      shift
      { k:(A => List[B]) =>
	       xs.flatMap(k)
	  }
	}
  }

/*
  implicit def reflective[A](s:State[A]) = new
  {
    def reflect[B] =
    {
      shift
      {
    	k:(A => State[B]) =>
          s.flatMap(k)
      }
    }
  }
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
    println(test3())		// List((x,4), (x,5), (x,6), (y,4), (y,5), (y,6), (z,4), (z,5), (z,6))
  }
}
