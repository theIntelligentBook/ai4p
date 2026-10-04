package ai4p.doingstuff.widgets

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.Thenable.Implicits.thenable2future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

/** The bits of the global `puter` object (loaded from https://js.puter.com/v2/ in index.html) we use. */
@js.native
@JSGlobal("puter")
private object PuterGlobal extends js.Object {
  val ai: js.Dynamic = js.native
}

/**
 * A thin facade over puter.js, which gives browser pages free, no-backend access to a hosted LLM -
 * handy for a slide deck, which has no server of its own to hide an API key behind. The first call in
 * a session may pop up a permission dialog from Puter; after that it just works.
 */
object Puter {

  /** Sends one prompt to puter.ai.chat and resolves with the model's reply text. */
  def chat(prompt: String): Future[String] =
    Try(PuterGlobal.ai.chat(prompt).asInstanceOf[js.Thenable[js.Dynamic]]) match
      case scala.util.Success(thenable) =>
        val fut: Future[js.Dynamic] = thenable
        fut.map(extractText)
      case scala.util.Failure(e) =>
        Future.failed(new Exception("Couldn't reach puter.ai (check your connection, or that js.puter.com isn't blocked)", e))

  /** puter.ai.chat's response has a custom toString() giving just the message text, but we fall
    * back to the OpenAI-shaped `message.content` field in case a given model/route differs. */
  private def extractText(resp: js.Dynamic): String =
    if js.isUndefined(resp) || resp == null then ""
    else if js.typeOf(resp) == "string" then resp.asInstanceOf[String]
    else
      val content = resp.selectDynamic("message").selectDynamic("content")
      if !js.isUndefined(content) && content != null then content.asInstanceOf[String]
      else resp.toString()
}
