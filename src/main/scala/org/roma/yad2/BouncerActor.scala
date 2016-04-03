package org.roma.yad2

import akka.actor.{ActorRef, ActorSystem, ActorLogging, Actor}
import akka.util.Timeout
import java.util.concurrent.{ExecutorService, Executors, TimeUnit}
import scala.util.{Random, Success, Failure}
import dispatch._
import com.typesafe.scalalogging.slf4j.LazyLogging
import com.ning.http.client._
import com.ning.http.client.AsyncHandler.STATE
import com.ning.http.client.cookie.Cookie
import scala.collection.JavaConversions._
import scala.concurrent.{ExecutionContext, Await}
import scala.concurrent.duration.Duration
import java.io.{InputStream, BufferedReader}
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import org.htmlcleaner.{TagNode, HtmlCleaner}
import java.util.{Calendar, Date}
import java.text.SimpleDateFormat
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

/**
 * Created by roma on 5/13/14.
 */
class BouncerActor extends Actor with LazyLogging {

  implicit val timeout: Timeout = Timeout(30, TimeUnit.SECONDS)
  implicit val system: ActorSystem = ActorSystem()
  import system.dispatcher // implicit execution context


  def receive() =
  {

    case Bounce(username, password) =>
      logger.debug("Bouncing ...")
      sender() ! State(Option(bounce(username, password)))
      logger.debug("Done.")

    //case Login(username, password) => sender ! Data(login(username, password))
  }



  def bounce(username : String, password : String) =
  {
      // login first
      val loginResponseFuture = login(username, password)
      val sessionFuture = loginResponseFuture map { loginResponse =>
        logger.trace("Login Response: " + loginResponse.getResponseBody)
        Session(loginResponse)
      }
      val session = Await.result(sessionFuture, Duration(30, TimeUnit.SECONDS))
      BouncerActor.waitRandomTime() // simulate the user and wait
      val orderRes= gotoOrder(session.getLocation(), session)
      val orderLinks = BouncerActor.parseActiveOrdersLinks(orderRes)
      logger.debug("Order links: " + orderLinks.mkString(", "))
      // simulate the user and go over the link one by one (NOT in parallel)
      val dateTimes = orderLinks.map ( orderLink => {

        BouncerActor.waitRandomTime()
        val itemsPage = gotoUrl(session, orderLink, session.getLocation())
        val orders = BouncerActor.parseAllItemsInOrder(itemsPage)
        orders.filter(BouncerActor.isBounceNeeded(_)).foreach {

          order => try{
            bounceItem(session, order.orderLink, orderLink)
          }
          catch {
            case e : Exception => logger.error("Failed bouncing order: " + order + ", due to " + e.getMessage)
          }
        }
        BouncerActor.getNextMinimumBounceDate(orders)
      })

      val nextBounceTime = if (dateTimes != null && !dateTimes.isEmpty) {
        dateTimes.minBy( _.getMillis)
      }
      else
      {
        val nextBounceDate = BouncerActor.getDefaultNextBounceDate()
        logger.debug(s"Did not find order links, will use default time for scheduling " + BouncerActor.MINIMUM_MINUTES_FOR_BOUNCE + " minutes at: " + nextBounceDate)
        nextBounceDate
      }

       nextBounceTime
  }



  def bounceAllItemsInOrder(session : Session, orderLink : String) =
  {

  }

  def gotoUrl(session : Session, page : String, referer : String) =
  {
    logger.info("Going to page " + page + " ...")

    // add headers
    val headers = Map(("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"),
      ("Origin", "http://www.yad2.co.il"),
      ("Connection", "keep-alive"),
      ("Host", "my.yad2.co.il"),
      ("Cache-Control", "max-age=0"),
      ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/34.0.1847.116 Safari/537.36"),
      ("Referer", referer),
      ("Accept-Encoding", "gzip,deflate,sdch"),
      ("Accept-Language", "en-GB,en-US;q=0.8,en;q=0.6,he;q=0.4,ru;q=0.2"),
      (session.getSessionCookie()._1, session.getSessionCookie()._2))

    // create request
    val request = url(page) <:< headers
    //val stream = Http(request OK as.Response(_.getResponseBodyAsStream)).apply()
    val res = Http(request OK as.Response( a => a )).apply()
    val contentEncoding = res.getHeader("Content-Encoding")
    val response = if (contentEncoding != null && contentEncoding.trim=="gzip") {
      logger.debug("Content-Encoding is GZIP")
      BouncerActor.decompressGzip(res.getResponseBodyAsStream)
    }
    else
    {
      logger.debug("No special Content-Encoding using raw.")
      res.getResponseBody
    }
    logger.trace("URL response from page '" + page + "': \n" + response)
    logger.debug("Got response from page '" + page)
    response
  }

  def bounceItem(session : Session, itemLinkOpt : Option[String], refererLink : String) =
  {

    //val page = """http://my.yad2.co.il/MyYad2/MyOrder/CarDetails.php?CarID=3156496&Up=u"""
    //val result = gotoUrl(session, itemLink, """http://my.yad2.co.il/MyYad2/MyOrder/CarDetails.php?CarID=3156496""")
    itemLinkOpt match {
      case Some(itemLink) =>

        val fullLink = """http://my.yad2.co.il/MyYad2/MyOrder/""" + itemLink + "&Up=u"
        logger.info("Bouncing item on full link " + fullLink + " ...")
        val result = gotoUrl(session, fullLink, refererLink)

        val linkOpt = BouncerActor.findBoundLinkOnOrdersPage(result)
        linkOpt match {
          case Some(link) =>

            BouncerActor.waitRandomTime()
            gotoUrl(session, """http://my.yad2.co.il""" + link, """http://my.yad2.co.il/MyYad2/MyOrder/""" + itemLink)

            logger.info("Finished Bounce.")
            true
          case None => logger.error("Bounce link was not found."); false
        }
      case None => logger.info("Skipped bouncing, a given link was none."); false
    }
  }

  def gotoOrder(location : String, session : Session) =
  {
    logger.info("Going to order page " + location + " ...")

    val locationUrl = if (location == null || location.isEmpty)
    {
      logger.warn("A given location is null or empty!, using mitigation url '" + BouncerActor.MITIGATION_HOME_URL+ "'")
      BouncerActor.MITIGATION_HOME_URL
      //throw new IllegalArgumentException("A given location is null or empty!")
    }
    else
    {
        logger.debug("Using Location URL from login response: " + location)
       location
    }


      // dispatch dedicated thread pool and connection pool
      //val pool: ExecutorService = Executors.newFixedThreadPool(4)
      //val httpClient: Http = Http.configure(_.setAllowPoolingConnection(true).setFollowRedirects(true).setConnectionTimeoutInMs(1000).setExecutorService(pool))


      // add headers
      val headers = Map(("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"),
        ("Origin", "http://www.yad2.co.il"),
        ("Connection", "keep-alive"),
        ("Host", "my.yad2.co.il"),
        ("Cache-Control", "max-age=0"),
        ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/34.0.1847.116 Safari/537.36"),
        ("Referer", "http://www.yad2.co.il/"),
        ("Accept-Encoding", "gzip,deflate,sdch"),
        ("Accept-Language", "en-GB,en-US;q=0.8,en;q=0.6,he;q=0.4,ru;q=0.2"),
        (session.getSessionCookie()._1, session.getSessionCookie()._2))

      // create request
      val request = url( """http://my.yad2.co.il/""" + locationUrl) <:< headers
      //val stream = Http(request OK as.Response(_.getResponseBodyAsStream)).apply()
      //val result = BouncerActor.decompressGzip(stream)

      val res = Http(request OK as.Response( a => a )).apply()
      val contentEncoding = res.getHeader("Content-Encoding")
      val response = if (contentEncoding != null && contentEncoding.trim=="gzip") {
        logger.debug("Content-Encoding is GZIP")
        BouncerActor.decompressGzip(res.getResponseBodyAsStream)
      }
      else
      {
        logger.debug("No special Content-Encoding using raw.")
        res.getResponseBody
      }

      logger.trace("Response: " + response)
      logger.info("Finished order. ")
      response

  }



  def login(username : String, password : String) =
  {
     logger.info("Logging in with Username = " + username + " ...")

    // add headers
    val headers = Map(("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"),
                      ("Origin", "http://www.yad2.co.il"),
                      ("Content-Type", "application/x-www-form-urlencoded"),
                      ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/34.0.1847.116 Safari/537.36"),
                      ("Referer", "http://www.yad2.co.il/"),
                      ("Accept-Encoding", "gzip,deflate,sdch"),
                      ("Accept-Language", "en-GB,en-US;q=0.8,en;q=0.6,he;q=0.4,ru;q=0.2"))

    val params = Map(("Username", username), ("Password", password))

     // create request
     val request = url("""http://my.yad2.co.il/login.php""") << (params) <:< (headers)
     val futureResponse = Http(request)

     futureResponse

  }

}



object BouncerActor extends LazyLogging
{
   val LOCATION_REF = """(.*)location.href='([^"]*)'(.*)""".r
   val ACTIVE_ORDER_REF = """<a class="my2Active " href="(.*)">""".r
   val ORDER_LINK_PREFIX = "<a class=\"my2Active \" href=\""
   val ORDER_ID_PATTERN = """\(document\.getElementById\("TxtID([^"]*)"\)(.*)""".r
   val ORDER_LINK_PATTERN = """javascript:show_me\('([^']*)'(.*)""".r
   val MINIMUM_MINUTES_FOR_BOUNCE = 241 // minutes
   val MINUTES_RANDOM_INTERVAL = 30 // minutes
   val MITIGATION_HOME_URL = """MyYad2/MyOrder/index.php"""

   val WAIT_MILLIS_MIN = 2000
   val WAIT_MILLIS_MAX = 10000

    // create an instance of HtmlCleaner
    val html = new HtmlCleaner();

   def waitRandomTime() =
   {
     val millisToWait = Random.nextInt(WAIT_MILLIS_MAX) + WAIT_MILLIS_MIN
     logger.info("[Random Wait] Waiting " + millisToWait + " ms ...")
     Thread.sleep(millisToWait)
     logger.info("[Random Wait] Done.")
   }

  def getDefaultNextBounceDate() : DateTime =
  {
     DateTime.now().plusMinutes(MINIMUM_MINUTES_FOR_BOUNCE)
  }

  def getNextMinimumBounceDate(orders : Seq[OrderItem]) : DateTime =
  {
    logger.debug("Getting next bounce date ...")
    if (orders != null)
    {
      val ordersDates = orders.filterNot(isBounceNeeded(_)).flatMap( order => order.lastBounceTime )
      if (ordersDates != null && !ordersDates.isEmpty) {
        val lastMinBounceDate =ordersDates.minBy(_.getMillis)
        logger.debug("Last Min Bounce Date: " + lastMinBounceDate)
        val randomMinutes = Random.nextInt(MINUTES_RANDOM_INTERVAL)
        logger.debug("Chosen random minutes: " + randomMinutes)
        val date1 = lastMinBounceDate.plusMinutes(randomMinutes).plusMinutes(MINIMUM_MINUTES_FOR_BOUNCE)
        logger.debug(s"Found next bounce date at: " + date1)
        date1
      }
      else
      {
        val nextBounceDate = getDefaultNextBounceDate()
        logger.debug(s"Did not find next bounce date, will set it by default in $MINIMUM_MINUTES_FOR_BOUNCE minutes at: " + nextBounceDate)
        nextBounceDate
      }
    }
    else{
      val nextBounceDate = getDefaultNextBounceDate()
      logger.debug(s"Did not find next bounce date, will set it by default in $MINIMUM_MINUTES_FOR_BOUNCE minutes at: " + nextBounceDate)
      nextBounceDate
    }
  }

   def findBoundLinkOnOrdersPage(page : String) =
   {
     logger.debug("Getting bounce link from orders page ...")
     val lineOp = page.lines.find(line => line.contains("location.href") && line.contains("Up="))

     val ret = lineOp match {
       case Some(line) => line match {
         case BouncerActor.LOCATION_REF(_, link, _) =>
           logger.debug("Found bounce link: " + link)
           Some(link)
         case _ => logger.error("Could not bounce, reference link was not found!"); None
       }
       case None => logger.error("Could not bounce, did not find a suitable line!"); None
     }
     ret
   }

   def isBounceNeeded(order :OrderItem) =
   {
      logger.debug("Is Bounce Needed for Order - " + order)
      order.lastBounceTime match {
        case Some(lastBounceDate) => logger.debug("Last Bounce Date: " + lastBounceDate)
          val minutesBeforeBounce = MINIMUM_MINUTES_FOR_BOUNCE
          logger.debug ("Minimum minutes before bounce since last bounce: " + minutesBeforeBounce)
          val nextBounceDate = Calendar.getInstance()
          nextBounceDate.setTimeInMillis(lastBounceDate.getMillis)
          nextBounceDate.add(Calendar.MINUTE, minutesBeforeBounce)
          logger.debug ("Next Bounce date: " + nextBounceDate.getTime)
          val now = Calendar.getInstance()
          val needBounce = now.after(nextBounceDate)
          logger.debug("Need Bounce: " + needBounce)
          needBounce

        case None => logger.debug("There is no last bounce date, trying to bounce now"); true
      }
   }

   def parseAllItemsInOrder(orderHtml : String) : Seq[OrderItem] =
   {
      val tags = html.clean(orderHtml)

      val activeTags = tags.getElementListByAttValue("id", "ActiveLink", true, true)


      val orderItems : Seq[OrderItem] = activeTags.flatMap(parseItemInOrder(_))
      logger.debug("Order Items: " + (if (orderItems != null) {orderItems.mkString(", ")} else {"none"}) )
      orderItems
   }

   def parseItemInOrder(tag : TagNode) : Option[OrderItem] =
   {
       logger.debug("Parsing order item " + tag + " ....")

       val tagString = tag.getAttributeByName("onMouseOut")
       val orderItem : Option[OrderItem] = tagString match
       {
         case ORDER_ID_PATTERN(id, _) =>
         {
           logger.debug(s"item id $id")
           val fullId = "Txt_" + id
           val itemString = tag.getElementListByAttValue("id", fullId, true, false).map( _.getText ).mkString(", ")
           val date = getBounceLastDate(tag, id)
           val orderLink = getBounceLink(tag, id)
           Some(OrderItem(id, itemString, orderLink, date, true))
         }
         case _ => None
       }
       logger.debug("Done - " + orderItem)
       orderItem
   }

   def getBounceLastDate(tag : TagNode, id : String) =
   {
     logger.debug("Getting last bounce date and time ... ")
     val dateTimeSt : String = List(getBounceLastTime(tag, "startdate_", id), getBounceLastTime(tag, "starttime_", id)).flatMap( x => x ).mkString(" ").trim()
     logger.debug("Date Time String = " + dateTimeSt)
     try {
       val date = DateTime.parse(dateTimeSt, DateTimeFormat.forPattern("dd/MM/yyyy HH:mm"));
       logger.debug("Date = " + date)
       Some(date)
     }
     catch {
       case e : IllegalArgumentException => logger.error("Failed parsing last bounce date: " + e.getMessage); None
     }
   }

  def getBounceLink(tag : TagNode, id : String) : Option[String] =
  {
    logger.debug("Getting bounce link " + tag.toString + "...")
    val arr = tag.getElementsByAttValue("id", "Txt_" + id, false, false)
    val link = if (arr != null && arr.length > 0)
    {
        val att = arr(0).getAttributeByName("onclick")
        att match
        {
          case ORDER_LINK_PATTERN(foundLink, _) => Some(foundLink)
          case _ => logger.error("Did not get the order link"); None
        }
    }
    else
    {
      None
    }

    logger.debug("Parsed bounce link " + link)
    link

  }


   def getBounceLastTime(tag : TagNode, elemName : String, id : String) : Option[String] =
   {
     logger.debug("Getting last bounce time " + tag.toString + "...")
     var lastBounceTimeArr = tag.getElementsByAttValue("id", elemName + id, false, false)
     if (lastBounceTimeArr != null && lastBounceTimeArr.length == 0)
     {
       lastBounceTimeArr = tag.getElementsByAttValue("id", elemName, false, false)
     }
     val lastTimeSt = if (lastBounceTimeArr != null && lastBounceTimeArr.length > 0)
     {
       Some(String.valueOf(lastBounceTimeArr(0).getText))
     }
     else
     {
       None
     }
     logger.debug("Parsed last bounce time " + lastTimeSt)
     lastTimeSt

   }


   def parseActiveOrdersLinks(ordersHtml : String) =
   {
     logger.debug("Parsing active orders links ...")
     logger.trace("Orders Html: " + ordersHtml)
     val linesIt = ordersHtml.lines.filter( line => line.contains(ORDER_LINK_PREFIX))

     val links = new ArrayBuffer[String]

     linesIt.foreach { line =>
       var index = 0
       while ( { index = line.indexOf(ORDER_LINK_PREFIX, index); index != -1 }  ) {
            val endIndex = line.indexOf("\">", index)
            if (endIndex != -1) {
              val link = line.substring(ORDER_LINK_PREFIX.length + index, endIndex)
              links.add(link)
              index+=link.length
            }
       }
     }
      links

   }


   def decompressGzip(stream : InputStream) : String =
   {
     logger.debug("Decompressing GZIP response ...")
     val reader = new BufferedReader(
       new java.io.InputStreamReader(
         new java.util.zip.GZIPInputStream(stream)))

     var line = ""
     val stb = new StringBuilder()
     while( {line = reader.readLine() ;line != null }){
       stb.append(line).append("\n")
     }
     val res = stb.toString()
     logger.debug("Successfullt decompressed GZIP.")
     res
   }
}


case class OrderItem(id : String, value : String, orderLink : Option[String], lastBounceTime : Option[DateTime], isActive : Boolean)

case class Session(response : Response) extends LazyLogging
{
  logger.debug("Getting session from a given response headers - " + response.getHeaders)

  // Response
  /*Date: Mon, 12 May 2014 18:05:54 GMT
  Server: Apache
  Expires: Thu, 19 Nov 1981 08:52:00 GMT
  Cache-Control: no-store, no-cache, must-revalidate, post-check=0, pre-check=0
  Pragma: no-cache
  set-cookie: SPSI=b681845dcbd6d11c96b628c1f3cd097d ; path=/; domain=.yad2.co.il
  set-cookie: UTGv2=D-h4170210cacb955beb74bec6602caec7ed33 ; expires=Tue, 12 May 2015 18:05:54 GMT; path=/; domain=.yad2.co.il
  Set-Cookie: USERNAME=romande%40gmail.com; expires=Mon, 09-Jun-2014 18:05:54 GMT; path=/; domain=.yad2.co.il
  Set-Cookie: USER_NAME=Roman; expires=Mon, 09-Jun-2014 18:05:54 GMT; path=/; domain=.yad2.co.il
  Set-Cookie: id=a7127e8a5acdb66405e28bfb28eb93cc; expires=Tue, 13-May-2014 18:05:54 GMT; path=/; domain=.yad2.co.il
  Set-Cookie: uci=8a7711da96773a7da75b340d13f360e1; expires=Mon, 09-Jun-2014 18:05:54 GMT; path=/; domain=.yad2.co.il
  Set-Cookie: sid=5898e86f145623d90d86a229759c507b; expires=Tue, 13-May-2014 18:05:54 GMT; path=/; domain=.yad2.co.il
  Set-Cookie: login=201405122105; expires=Tue, 13-May-2014 18:05:54 GMT; path=/; domain=.yad2.co.il
  Set-Cookie: logout=deleted; expires=Sun, 12-May-2013 18:05:53 GMT; path=/; domain=.yad2.co.il
  Location: MyYad2/MyOrder/index.php
  Content-Length: 0
  Keep-Alive: timeout=15, max=100
  Connection: Keep-Alive
  Content-Type: text/html; charset=windows-1255*/



  private val location = response.getHeader("Location")
  private val sessionCookie = "resolution_width=1050; PHPSESSID=" + Cookies.getCookie("sid", response.getCookies) +
  "; yad2upload=301989898.20480.0000; CLIENT_WIDTH_DIR=1146; MAIN_WIDTH_DIR=1146; SaveSearch_CustID=fcb2747104606;" +
  " COOKIES_login_plasma=20140509; __utma=143340477.1592382675.1373148148.1399915253.1399917735.24; __utmb=143340477.4.10.1399917735; __utmc=143340477; __utmz=143340477.1399917735.24.5.utmcsr=yad2.co.il|utmccn=(referral)|utmcmd=referral|utmcct=/; _ga=GA1.3.1592382675.1373148148;" +
  " SPSI=" + Cookies.getCookie("SPSI", response.getCookies) + "; UTGv2=" + Cookies.getCookie("UTGv2", response.getCookies) +
  "; USERNAME=" + Cookies.getCookie("USERNAME", response.getCookies) + "; USER_NAME=" + Cookies.getCookie("USER_NAME", response.getCookies) +
  "; id=" + Cookies.getCookie("id", response.getCookies) + "; uci=" + Cookies.getCookie("uci", response.getCookies) +
   "; sid=" + Cookies.getCookie("sid", response.getCookies) + "; login=" + Cookies.getCookie("login", response.getCookies) +
  "; " + Cookies.getCookieNameValueWhichStartsWith("TS", response.getCookies)

  logger.debug("Session Cookie: " + sessionCookie)

  def getSessionCookie() =
  {
    ("Cookie", sessionCookie)
  }

  def getLocation() = location

}

object Cookies
{
  def getCookie(name : String, cookies : java.util.List[Cookie]) =
  {
     cookies.find( cookie => cookie.getName == name ) match
     {
       case Some(value) => value.getValue
       case None => ""
     }
  }

  def getCookieNameValueWhichStartsWith(name : String, cookies : java.util.List[Cookie]) =
  {
    cookies.find( cookie => cookie.getName startsWith name) match
    {
      case Some(foundCookie) => foundCookie.getName + "=" + foundCookie.getValue
      case None => ""
    }
  }
}
