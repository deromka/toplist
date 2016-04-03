package org.roma.yad2

import com.ning.http.client.Response
import java.util.concurrent.{TimeUnit, Future}
import org.joda.time.DateTime

import scala.concurrent.{Await, Promise}
import scala.concurrent.duration.Duration


/**
 * Created by roma on 5/13/14.
 */

object Execute
object State
case class Credentials(username : String, password : String)
case class State(nextBounceTime : Option[DateTime])
case class Login(username : String, password : String)
case class Bounce(username : String, password : String)
abstract class Data(data : Any)
{
  def getData() : String
}
case class StringData(data : String) extends Data(data)
{
  def getData() : String = data
}
case class ResponseData(data : Response) extends Data(data)
{
  def getData() : String = data.getResponseBody
}
case class FutureResponseData(data : dispatch.Future[Response]) extends Data(data)
{
   def getData() : String = Await.result(data, Duration(1, TimeUnit.SECONDS)).getResponseBody
}

object Data
{
   def apply(data : String) = StringData(data)
   def apply(data : Response) = ResponseData(data)
   def apply(data : dispatch.Future[Response]) = FutureResponseData(data)
   def unapply(data : Data) : String =
   {
      data.getData()
   }
}


