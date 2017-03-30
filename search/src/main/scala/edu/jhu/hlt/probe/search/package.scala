package edu.jhu.hlt.probe

/**
 * @author Tongfei Chen
 */
package object search {
  implicit class StringMatch(ctx: StringContext) {

    /** Enables the "sm" string interpolator. */
    object sm {

      def unapplySeq(s: String): Option[List[String]] = {
        val rx = ctx.parts.map(_.replace("(", "\\(").replace(")", "\\)")).mkString("(.*)")
        rx.r.unapplySeq(s)
      }

    }

  }
}
