package ai4p.typings

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("marked", "marked")
object Marked extends js.Object:
  
  // Standard block parser (wraps in <p> tags)
  def parse(markdown: String, options: js.UndefOr[js.Object] = js.undefined): String = js.native
  
  // Modern inline parser (does NOT wrap in <p> tags)
  def parseInline(markdown: String, options: js.UndefOr[js.Object] = js.undefined): String = js.native

  // Access to internal Lexer classes for advanced token handling
  val Lexer: js.Dynamic = js.native
  val Parser: js.Dynamic = js.native
