package me.tongfei.probe
/**
  * This package enables string matching using string interpolators.
  * @author Tongfei Chen (ctongfei@gmail.com).
  */
package object util {

  def format(d: Double): String = f"$d%.4f"

  implicit class StringMatch(ctx: StringContext) {

    /** Enables the "sm" string interpolator. */
    object sm {

      def unapplySeq(s: String): Option[List[String]] = {
        val rx = ctx.parts.mkString("(.*)")
        rx.r.unapplySeq(s)
      }

    }

  }

}
