package symsim

import org.typelevel.paiges.Doc
import org.typelevel.paiges.Style

// TODO: this could go to a presentation module (together with graphs, etc), if not upstream

/** Pretty print a rectangular matrix, with column and row titles */
def tabulate[A] (pad: Char, sep: String, rows: List[List[A]],
  sepPad: Option[String] = None, sepJoint: Option[String] = None): Doc =

  require (rows.nonEmpty, "The [rows] argument should be non-empty")
  require (rows.head.nonEmpty, "Each row in [rows] should be non-empty")
  val nCols = rows.head.length
  require (rows.map { _.length }.forall { _ == nCols })
  require (
    (sepPad.isDefined && sepJoint.isDefined) ||
    (!sepPad.isDefined && !sepJoint.isDefined),
    "Either both sepPad and sepJoint are given or none of them!" )

  val table = rows.map { r => r.map (_.toString) }
  val tableT = table.transpose
  val widths: List[Int] = tableT.map { col => col.map (_.length).max }

  val separator = for
    dash <- sepPad
    joint <- sepJoint
  yield Doc.intercalate(
    Doc.str (joint),
    widths.map { w => Doc.str (dash.repeat (w)) }
  ) + Doc.line

  val headings = Doc.intercalate (
    Doc.str (sep),
    table
     .head
     .zip (widths)
     .map { (s,w) => Doc.str (s.padTo (w, pad)) }
    ).style (Style.Ansi.Attr.Bold) + Doc.line

  def fmt (r: List[String]): Doc =
    Doc.intercalate (
      Doc.str (sep),
      r.zip (widths)
       .zipWithIndex
       .map { case ((s,w),i) =>
         if i == 0
         then Doc.str (s.padTo (w, pad)).style (Style.Ansi.Attr.Bold)
         else Doc.str (s.padTo (w, pad)).style (Style.Ansi.Attr.Faint) }
    ) + Doc.line

  val rowDocs: Doc = table.tail.map (fmt).reduce { _ + _ }
  headings + separator.getOrElse (Doc.empty) + rowDocs
