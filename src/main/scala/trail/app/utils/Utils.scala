/*
 * Copyright (C) 2016  Nikos Katzouris
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package trail.app.utils

import java.io.{File, FileWriter}
import java.util.UUID

import scala.annotation.tailrec
import scala.math.BigInt
import scala.util.Random
import java.nio.file

/**
  * Created by nkatz at 4/12/19
  */

object Utils {

  def dumpToFile(input: Any, file: String = "", howTowrite: String = "overwrite") = {

      /* Write an iterable to file. Usage:
     * listToFile(new File("example.txt")) { p => data.foreach(p.println) }
     */
      def listToFile(f: java.io.File, mode: String)(op: java.io.PrintWriter => Unit) {

        val p = mode match {
          case "append" => new java.io.PrintWriter(new FileWriter(f, true))
          case "overwrite" => new java.io.PrintWriter(new FileWriter(f, false))
          case _ => new java.io.PrintWriter(new FileWriter(f, false)) // default is overwrite
        }
        try { op(p) } finally { p.close() }
      }

    val writeTo = {
      if (file == "") File.createTempFile(s"temp-${System.currentTimeMillis()}-${UUID.randomUUID.toString}", "asp")
      else {
        val f = new File(file)
        if (f.exists()) f
        else {
          val path = java.nio.file.Paths.get(file)
          java.nio.file.Files.createFile(path).toFile
        }
      }
    }

    val deleteOnExit = if (file == "") true else false

    val mode = if (file == "") "overwrite" else howTowrite

    input match {
      case in: Iterable[String] => listToFile(writeTo, mode) { p => in.foreach(p.println) }
      case in: String => listToFile(writeTo, mode) { p => Vector(in).foreach(p.println) }
    }

    if (deleteOnExit) writeTo.deleteOnExit()
    writeTo
  }

  def lined(msg: String) = s"\n$msg\n${"-" * msg.length}"

  def time[R](codeBlock: => R): (R, Double) = {
    val t0 = System.nanoTime()
    val result = codeBlock
    val t1 = System.nanoTime()
    val totalTime = (t1 - t0) / 1000000000.0
    (result, totalTime)
  }

  def mean(s: List[Double]) = s.foldLeft(0.0)(_ + _) / s.size

  def deviation(s: List[Double], mean: Double) = {
    val diffs = s map (x => math.abs(x - mean))
    this.mean(diffs)
  }

  def combinations(n: Int, k: Int) = {
    if (n >= k) factorial(n) / (factorial(k) * factorial(n - k)) else BigInt(0)
  }

  def factorial(x: BigInt): BigInt = {
      @tailrec
      def f(x: BigInt, acc: BigInt): BigInt = {
        if (x == 0) acc else f(x - 1, x * acc)
      }
    f(x, 1)
  }

  def f1Score(_tps: Int, _fps: Int, _fns: Int) = {
    val tps = _tps.toDouble
    val fps = _fps.toDouble
    val fns = _fns.toDouble
    val f = (x: Double) => if (x == 0.0) 0.0001 else x

    val precision = f(tps) / (f(tps) + f(fps))
    val recall = f(tps) / (f(tps) + f(fns))
    val f1score = 2 * precision * recall / (precision + recall)
    (precision, recall, f1score)
  }

  def f11Score(x: (Int, Int, Int)) = f1Score(x._1, x._2, x._3)

  /**
    * Randomly draw N distinct elements from a vector
    */
  def sampleN(N: Int, sampleFrom: Vector[Any]) = {
      @tailrec
      def sampleN(N: Int, sampleFrom: Vector[Any], sample: Vector[Any]): Vector[Any] = {
        sample.length match {
          case N => sample
          case _ =>
            val newValue = Random.shuffle(sampleFrom).head
            val newSample = if (!sample.contains(newValue)) sample :+ newValue else sample
            sampleN(N, sampleFrom, newSample)
        }
      }
    sampleN(N, sampleFrom, Vector())
  }

  def underline(x: String) = {
    val l = x.length
    val u = (for (i <- 1 to l) yield "-").mkString("")
    s"$u\n$x\n$u"
  }

  def underlineUpper(x: String) = {
    val l = x.length
    val u = (for (i <- 1 to l) yield "-").mkString("")
    s"$u\n$x\n"
  }

  def underlineStars(x: String) = {
    val l = x.length
    val u = (for (i <- 1 to l) yield "*").mkString("")
    s"$u\n$x\n$u"
  }

}

object CombinatorialOps {

  /**
    * From https://gist.github.com/vkostyukov/9015987
    * Usage:
    * import CombinatorialOps._
    * val v = List(1, 2, 3).xvariations(2)
    */

  implicit class CombinatorialList[A](l: List[A]) {

    /**
      * A pre-calculated size of given list.
      */
    val xsize = l.size

    /**
      * Generates the combinations of this list with given length 'n'. The order
      * doesn't matter.
      *
      * The total number of k-combinations on n-length set might be calculated
      * as follows:
      *
      *                  C_k,n = n!/k!(n - k)!
      *
      * Time - O(C_k,n)
      * Space - O(C_k,n)
      */
    def xcombinations(n: Int): List[List[A]] =
      if (n > xsize) Nil
      else l match {
        case _ :: _ if n == 1 => l.map(List(_))
        case hd :: tl => tl.xcombinations(n - 1).map(hd :: _) ::: tl.xcombinations(n)
        case _ => Nil
      }

    /**
      * Generates all the subsets of this list. The order doesn't matter.
      *
      * The total number of subsets might be obtained from variations formula:
      *
      *                  S_n = sum(i=1..n) {C_i,n} = 2 ** n
      * Time - O(S_n)
      * Space - O(S_n)
      */
    def xsubsets: List[List[A]] =
      (2 to xsize).foldLeft(l.xcombinations(1))((a, i) => l.xcombinations(i) ::: a)

    /**
      * Generates the variations of this list with given length 'n'. The order
      * does matter.
      *
      * The total number of variations might be calculated as follows:
      *
      *                   V_k,n = n!/(n - k)!
      *
      * Time - O(V_k,n)
      * Space - O(V_k,n)
      */
    def xvariations(n: Int): List[List[A]] = {
        def mixmany(x: A, ll: List[List[A]]): List[List[A]] = ll match {
          case hd :: tl => foldone(x, hd) ::: mixmany(x, tl)
          case _ => Nil
        }

        def foldone(x: A, ll: List[A]): List[List[A]] =
          (1 to ll.length).foldLeft(List(x :: ll))((a, i) => (mixone(i, x, ll)) :: a)

        def mixone(i: Int, x: A, ll: List[A]): List[A] =
          ll.slice(0, i) ::: (x :: ll.slice(i, ll.length))

      if (n > xsize) Nil
      else l match {
        case _ :: _ if n == 1 => l.map(List(_))
        case hd :: tl => mixmany(hd, tl.xvariations(n - 1)) ::: tl.xvariations(n)
        case _ => Nil
      }
    }

    /**
      * Generates all permutations of this list. The order does matter.
      *
      * The total number of permutations might be calculated as follows:
      *
      *                 P_n = V_n,n = n!
      *
      * Time - O(n!)
      * Space - O(n!)
      */
    def xpermutations: List[List[A]] = xvariations(xsize)
  }
}
