package edu.jhu.hlt.probe

package object util {

  def format(d: Double): String = {
    val s1 = s"$d"
    val s2 = f"$d%.6f"
    if (s1.length > s2.length) s2 else s1
  }

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
