
import akka.actor.{Props, ActorRef, ActorSystem}
import akka.io.IO
import org.eclipse.jetty.servlet.DefaultServlet
import org.scalatra.servlet.ScalatraListener
import org.eclipse.jetty.webapp.WebAppContext
import org.eclipse.jetty.server.Server
import com.typesafe.scalalogging.slf4j.LazyLogging

/**
 * Hello world!
 *
 */
object BouncerLauncher extends App with LazyLogging {

  try {

    logger.info("Bouncer is starting up ...")

    val port = if(System.getenv("PORT") != null) System.getenv("PORT").toInt else 8080

    logger.info("Using port " + port)

    val server = new Server(port)
    val context = new WebAppContext()
    context setContextPath "/"
    context.setResourceBase("src/main/webapp")
    context.addEventListener(new ScalatraListener)
    context.addServlet(classOf[DefaultServlet], "/")

    server.setHandler(context)

    server.start

    logger.info("Bouncer is running.")

    server.join

  }
  catch
    {
      case t : Throwable => logger.error("ERROR", t)
    }
}
