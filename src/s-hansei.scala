import scala.continuations._
import scala.continuations.ControlContext._

sealed abstract class vc[a]
case class pV[a](x: List[Pair[Double, vc[a]]]) extends vc[a]
case class V[a](x: a) extends vc[a]
case class C[a](x: () => pV[a]) extends vc[a]

object s_hansei extends Application
{
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
