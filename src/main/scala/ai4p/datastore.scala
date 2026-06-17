package ai4p

import com.wbillingsley.veautiful.PushVariable
import org.scalajs.dom.HttpMethod
import org.scalajs.dom.RequestInit
import org.scalajs.dom.Headers

import scala.scalajs.js
import scala.scalajs.js.Thenable.Implicits.thenable2future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js.JSON

case class ServerLink(
    server:String,
    token:String, 
    name:Option[String] = None,
    email:Option[String] = None
) {

    def call(json:scalajs.js.Dictionary[js.Any]) = {
        for 
            r <- org.scalajs.dom.fetch(server, new RequestInit {
                method = HttpMethod.POST
                headers = js.Dictionary(
                    "Accept" -> "application/json",
                    "Content-Type" -> "application/json",
                    "Authorization" -> ("Bearer " + token)
                )
                body = js.JSON.stringify(json)                
            })
            text <- {
                r.status match {
                    case 200 => r.text():Future[String]
                    case 404 => Future.failed(new NoSuchElementException("Not found")) // NotFound
                    case 500 => Future.failed(RuntimeException(r.statusText)) // Internal Server Error
                    case x => Future.failed(IllegalArgumentException(s"Could not match response code $x. ${r.statusText}"))
                }
            }    
            resp = JSON.parse(text)
        yield resp
    }


}

val serverLink = PushVariable[Option[ServerLink]](None) { _ => () } 

def whoami() = {
     
}



def parseLink(fragment:String) = {

    fragment match {
        case s"${_}server=${url};token=${token}" => 
            val sl = ServerLink(url, token)
            serverLink.value = Some(sl)
            sl.call(js.Dictionary("call" -> "whoami"))
            
        case _ => 
            ()
    }

}