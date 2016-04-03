package org.roma.yad2

import akka.util.Timeout
import akka.actor._
import akka.pattern.ask
import org.scalatra.ScalatraServlet
import com.typesafe.scalalogging.slf4j.LazyLogging
import java.util.concurrent.TimeUnit
import dispatch.Defaults.executor
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import com.ning.http.client.Response
import java.util.Date
import org.joda.time.DateTime

class HttpHandlerService extends ScalatraServlet with LazyLogging {

  implicit val timeout = Timeout(120, TimeUnit.SECONDS)

  logger.debug("Creating actors ...")

  val makeTopActor = ActorSystem().actorOf(Props[BouncerActor])
  val actorReceiver = ActorSystem().actorOf(Props(classOf[ActorReceiver], this))

  logger.debug("done.")

  private var nextBounceDate : Option[DateTime] = None

  private var username : String = _
  private var password : String = _

  object ActorReceiver {
    def apply() = new ActorReceiver()
  }

  class ActorReceiver extends Actor
  {
    def receive() =
    {
      case State(nextBounceTime) =>
        logger.info("Got next bounce time - " + nextBounceTime)
        schedule(nextBounceTime)
    }
  }


  get("/")
  {
    index
  }

  get("/state")
  {
     logger.debug("Got state: " + nextBounceDate)
     <html><body>Next Bounce Date - {nextBounceDate}</body></html>
  }

  get("/bounce") {
      <html><body>{loginForm}</body></html>
  }


  post("/loginForm") {

        username = params("username")
        password = params("password")
        logger.debug("Sending to makeTopActor ...")
        val futureData = makeTopActor.ask(Bounce(username, password))
        logger.debug("Done. Waiting for response ...")
        val nextBounceTimeOption = handleResponse(futureData)
        schedule(nextBounceTimeOption)
        <html><body>Next Bounce Time: {nextBounceTimeOption} </body></html>
  }



 def handleResponse(futureData : Future[Any]) : Option[DateTime] =
 {

   logger.debug("Handling response ...")
   val nextBounceTime = try
   {
     val responseData = Await.result(futureData, Duration(120, TimeUnit.SECONDS))
     logger.debug("Received response.")
     responseData match
     {
       case State(Some(nextBounceTime))  => logger.debug("Received next bounce time: " + nextBounceTime); nextBounceTime
       case other @ _ => logger.error("Using default time, Other data type received: " + other); BouncerActor.getDefaultNextBounceDate()
     }
   }
   catch
     {
       case e @ ( _ : java.util.concurrent.TimeoutException |  _ : InterruptedException) =>
         logger.error("Timed out: " + e.getMessage);
         BouncerActor.getDefaultNextBounceDate()
       case e : Exception =>
         logger.error("Other Error: " + e.getMessage);
         BouncerActor.getDefaultNextBounceDate()
     }
   logger.info("Next Bounce Time: " + nextBounceTime);
   Option(nextBounceTime)
 }


  lazy val index =
    <html>
      <body>
        <h1>Welcome to <i>bouncer</i> on <i>scalatra</i>!</h1>
        <p>Defined resources:</p>
        <ul>
          <li><a href="/bounce">Bounce Now</a></li>
        </ul>
      </body>
    </html>




  lazy val loginForm =
          <section class="loginform cf">
          <form name="login" action="/loginForm" method="post" accept-charset="utf-8">
            <ul>
              <li>
                <label for="usermail">Email</label>
                <input type="email" name="username" placeholder="yourname@email.com" required="true" />
              </li>
              <li>
                <label for="password">Password</label>
                <input type="password" name="password" placeholder="password" required="true" />
              </li>
              <li>
                <input type="submit" value="Bounce!" />
              </li>
              </ul>
            </form>
          </section>


  def schedule(periodOption: Option[DateTime]) = {

    logger.info("Rescheduling - " + periodOption)

    val periodInMillis = periodOption match
    {
      case Some(period) => period.getMillis - DateTime.now().getMillis
      case None => logger.debug("Period is none."); BouncerActor.getDefaultNextBounceDate().getMillis - DateTime.now().getMillis
    }

    val duration = new FiniteDuration(periodInMillis, TimeUnit.MILLISECONDS)
    ActorSystem().scheduler.scheduleOnce(duration)(makeTopActor.tell(Bounce(username, password), actorReceiver));
    nextBounceDate = Option(DateTime.now().plus(periodInMillis))
    logger.info("Rescheduled with delay: " + periodInMillis + " millis, Time = " + nextBounceDate.get)
  }

}

