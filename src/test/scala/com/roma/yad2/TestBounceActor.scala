package com.roma.yad2

import org.junit.Test
import com.ning.http.client._
import com.ning.http.client.providers.netty.NettyResponse
import com.ning.http.client.providers.apache.{ApacheResponseStatus, ApacheResponse}
import java.net.URI
import com.ning.http.client.providers.jdk.{ResponseHeaders, JDKResponse, ResponseStatus}
import org.roma.yad2.{OrderItem, BouncerActor, Cookies, Session}
import scala.io.{Codec, Source}
import java.io.File
import java.util.{TimeZone, Calendar, Date}
import org.joda.time.DateTime

/**
 * Created by roma on 5/24/14.
 */
class TestBounceActor {


  @Test
  def testIsBounceNeededTrue()
  {
    val lastBounce = DateTime.now().minusMinutes(350)
    val order = OrderItem("111", "aaa", Some("Yad2Details.php?SalesID=8906469"), Some(lastBounce), true)
    assert(BouncerActor.isBounceNeeded(order)==true)
  }

  @Test
  def testIsBounceNeededFalse()
  {
    val lastBounce = DateTime.now().minusMinutes(50)
    val order = OrderItem("111", "aaa", Some("Yad2Details.php?SalesID=8906469"), Some(lastBounce), true)
    assert(BouncerActor.isBounceNeeded(order)==false)
  }

  @Test
  def testParseActiveOrderLinks()
  {
    val source = scala.io.Source.fromFile("./orders.html")
    val ordersHtml = source.mkString
    source.close()

    val links = BouncerActor.parseActiveOrdersLinks(ordersHtml)
    assert(links.size == 2)
    assert(links(0).equals("http://my.yad2.co.il/MyYad2/MyOrder/Car.php"))
    assert(links(1).equals("http://my.yad2.co.il/MyYad2/MyOrder/Yad2.php"))
  }

  @Test
  def testGetNextBounceDate()
  {
    val date1 = DateTime.now().minusMinutes(50)

    val date2 = DateTime.now().minusMinutes(245)

    val date3 = DateTime.now().minusMinutes(100)

    val order1 = OrderItem("111", "aaa", Some("Yad2Details.php?SalesID=8906469"), Some(date1), true)
    val order2 = OrderItem("112", "aab", Some("Yad2Details.php?SalesID=8906470"), Some(date2), true)
    val order3 = OrderItem("113", "aac", Some("Yad2Details.php?SalesID=8906471"), Some(date3), true)

    val orders = List(order1, order2, order3)

    val nextBounceDate = BouncerActor.getNextMinimumBounceDate(orders)
    val upperTime = date3.plusMinutes(BouncerActor.MINIMUM_MINUTES_FOR_BOUNCE).plusMinutes(BouncerActor.MINUTES_RANDOM_INTERVAL)
    assert(nextBounceDate.isAfter(date3.plusMinutes(BouncerActor.MINIMUM_MINUTES_FOR_BOUNCE)) && nextBounceDate.isBefore(upperTime))

  }

  @Test
  def testParseActiveItemsInOrderPage()
  {
    val source = scala.io.Source.fromFile("./yad2Orders.html")(Codec.apply("windows-1255"))
    val ordersHtml = source.mkString
    source.close()

     val orders = BouncerActor.parseAllItemsInOrder(ordersHtml)
    assert(orders.size == 2)
    assert(orders(0).id=="8906643")
    assert(orders(0).orderLink.get=="Yad2Details.php?SalesID=8906643")
    assert(orders(1).id=="8906469")
    assert(orders(1).orderLink.get=="Yad2Details.php?SalesID=8906469")
  }

  @Test
  def testParseActiveItemsInOrderPageCars()
  {
    val source = scala.io.Source.fromFile("./carsOrders.html")(Codec.apply("windows-1255"))
    val carsHtml = source.mkString
    source.close()

    val orders = BouncerActor.parseAllItemsInOrder(carsHtml)
    assert(orders.size == 1)
    assert(orders(0).id=="3156496")
    assert(orders(0).orderLink.get=="CarDetails.php?CarID=3156496")
    assert(orders(0).lastBounceTime.isDefined)
    val cal = new DateTime(2014, 5, 27, 15, 6, 00, 00)
    val actualDate = orders(0).lastBounceTime.get
    assert(actualDate.compareTo(cal)==0)
  }

  @Test
  def testSession()
  {

    val status = new ResponseStatus(new URI("MyYad2/MyOrder/index.php"),null, null)

    val headers = new HttpResponseHeaders(new URI("MyYad2/MyOrder/index.php"),null) {
      override def getHeaders: FluentCaseInsensitiveStringsMap = {
        val map = new FluentCaseInsensitiveStringsMap()

        map.add("Date", "Mon, 12 May 2014 18:05:54 GMT")
        map.add("Server", "Apache")
        map.add("Expires", "Thu, 19 Nov 1981 08:52:00 GMT")
        map.add("Cache-Control", "no-store, no-cache, must-revalidate, post-check=0, pre-check=0")
        map.add("Pragma", "no-cache")
        map.add("set-cookie", "SPSI=b681845dcbd6d11c96b628c1f3cd097d ; path=/; domain=.yad2.co.il")
        map.add("set-cookie", "UTGv2=D-h4170210cacb955beb74bec6602caec7ed33 ; expires=Tue, 12 May 2015 18:05:54 GMT; path=/; domain=.yad2.co.il")
        map.add("Set-Cookie", "USERNAME=romande%40gmail.com; expires=Mon, 09-Jun-2014 18:05:54 GMT; path=/; domain=.yad2.co.il")
        map.add("Set-Cookie", "USER_NAME=Roman; expires=Mon, 09-Jun-2014 18:05:54 GMT; path=/; domain=.yad2.co.il")
        map.add("Set-Cookie", "id=a7127e8a5acdb66405e28bfb28eb93cc; expires=Tue, 13-May-2014 18:05:54 GMT; path=/; domain=.yad2.co.il")
        map.add("Set-Cookie", "uci=8a7711da96773a7da75b340d13f360e1; expires=Mon, 09-Jun-2014 18:05:54 GMT; path=/; domain=.yad2.co.il")
        map.add("Set-Cookie", "sid=5898e86f145623d90d86a229759c507b; expires=Tue, 13-May-2014 18:05:54 GMT; path=/; domain=.yad2.co.il")
        map.add("Set-Cookie", "login=201405122105; expires=Tue, 13-May-2014 18:05:54 GMT; path=/; domain=.yad2.co.il")
        map.add("Set-Cookie", "logout=deleted; expires=Sun, 12-May-2013 18:05:53 GMT; path=/; domain=.yad2.co.il")
        map.add("Location", "MyYad2/MyOrder/index.php")
        map.add("Content-Length", "0")
        map.add("Keep-Alive", "timeout=15, max=100")
        map.add("Connection", "Keep-Alive")
        map.add("Content-Type", "text/html; charset=windows-1255")
        map
      }
    }

    val response = new JDKResponse(status, headers, null)

    val session = Session(response)

    assert(Cookies.getCookie("USER_NAME", response.getCookies) == "Roman")
    assert(session.getSessionCookie()._2 contains("USERNAME=romande%40gmail.com"))
    assert(Cookies.getCookie("sid", response.getCookies) == "5898e86f145623d90d86a229759c507b")
  }

  @Test
  def testSessionWithTS()
  {

    val status = new ResponseStatus(new URI("MyYad2/MyOrder/index.php"),null, null)

    val headers = new HttpResponseHeaders(new URI("MyYad2/MyOrder/index.php"),null) {
      override def getHeaders: FluentCaseInsensitiveStringsMap = {
        val map = new FluentCaseInsensitiveStringsMap()

        map.add("Date", "Mon, 12 May 2014 18:05:54 GMT")
        map.add("Server", "Apache")
        map.add("Expires", "Thu, 19 Nov 1981 08:52:00 GMT")
        map.add("Cache-Control", "no-store, no-cache, must-revalidate, post-check=0, pre-check=0")
        map.add("Pragma", "no-cache")
        map.add("set-cookie", "SPSI=b681845dcbd6d11c96b628c1f3cd097d ; path=/; domain=.yad2.co.il")
        map.add("set-cookie", "UTGv2=D-h4170210cacb955beb74bec6602caec7ed33 ; expires=Tue, 12 May 2015 18:05:54 GMT; path=/; domain=.yad2.co.il")
        map.add("Set-Cookie", "USERNAME=romande%40gmail.com; expires=Mon, 09-Jun-2014 18:05:54 GMT; path=/; domain=.yad2.co.il")
        map.add("Set-Cookie", "USER_NAME=Roman; expires=Mon, 09-Jun-2014 18:05:54 GMT; path=/; domain=.yad2.co.il")
        map.add("Set-Cookie", "id=a7127e8a5acdb66405e28bfb28eb93cc; expires=Tue, 13-May-2014 18:05:54 GMT; path=/; domain=.yad2.co.il")
        map.add("Set-Cookie", "uci=8a7711da96773a7da75b340d13f360e1; expires=Mon, 09-Jun-2014 18:05:54 GMT; path=/; domain=.yad2.co.il")
        map.add("Set-Cookie", "sid=5898e86f145623d90d86a229759c507b; expires=Tue, 13-May-2014 18:05:54 GMT; path=/; domain=.yad2.co.il")
        map.add("Set-Cookie", "login=201405122105; expires=Tue, 13-May-2014 18:05:54 GMT; path=/; domain=.yad2.co.il")
        map.add("Set-Cookie", "logout=deleted; expires=Sun, 12-May-2013 18:05:53 GMT; path=/; domain=.yad2.co.il")
        map.add("Set-Cookie", "TSdd5cf4=0e2db8389669bc98da327ce656e098f344c53f2ca2266f0b539742a89c5eca8559043867763a2f23efdb8218e56b716e2592f6bdc0d043c1c0f09c070d9b58e1f2c6a81e160eee9dd65f0863154330d6659128d0b9c2efaa84ca01f1ca85ab0359043867ebaadfd9ca2e95eb6ebce7fc1761e9909cffdfa716d9bc2f46ce9707ffffffffc92c1249a28dfd26aa9c2ddcffffffff; Path=/; Domain=.my.yad2.co.il")
        map.add("Location", "MyYad2/MyOrder/index.php")
        map.add("Content-Length", "0")
        map.add("Keep-Alive", "timeout=15, max=100")
        map.add("Connection", "Keep-Alive")
        map.add("Content-Type", "text/html; charset=windows-1255")
        map
      }
    }

    val response = new JDKResponse(status, headers, null)

    val session = Session(response)

    assert(Cookies.getCookie("USER_NAME", response.getCookies) == "Roman")
    assert(session.getSessionCookie()._2 contains("USERNAME=romande%40gmail.com"))
    assert(Cookies.getCookie("sid", response.getCookies) == "5898e86f145623d90d86a229759c507b")
    assert(Cookies.getCookieNameValueWhichStartsWith("TS", response.getCookies) == "TSdd5cf4=0e2db8389669bc98da327ce656e098f344c53f2ca2266f0b539742a89c5eca8559043867763a2f23efdb8218e56b716e2592f6bdc0d043c1c0f09c070d9b58e1f2c6a81e160eee9dd65f0863154330d6659128d0b9c2efaa84ca01f1ca85ab0359043867ebaadfd9ca2e95eb6ebce7fc1761e9909cffdfa716d9bc2f46ce9707ffffffffc92c1249a28dfd26aa9c2ddcffffffff")
  }

  @Test
  def testBounceFindBounceLink()
  {
    val source = scala.io.Source.fromFile("./allCarOrders.html")(Codec.apply("windows-1255"))
    val ordersHtml = source.mkString
    source.close()

    val linkOpt = BouncerActor.findBoundLinkOnOrdersPage(ordersHtml)
    assert(linkOpt.isDefined == true)
    assert(linkOpt.get=="/MyYad2/MyOrder/CarDetails.php?CarID=3156496&Up=u")
  }


  @Test
  def testBounce1()
  {
     val stRes = """<HTML>
                   	<HEAD>
                   	<META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=windows-1255">
                   	<SCRIPT LANGUAGE="JavaScript">
                   	<!--
                   	function GoURL(){
                   		location.href='/MyYad2/MyOrder/CarDetails.php?Up=2&CarID=3156496';
                   	}
                   	//-->
                   	</SCRIPT>
                           <BODY onLoad="setTimeout('GoURL()', 2000);">
                               <TABLE height="90%" width="700" border="0" align="center" bgcolor="#F8EFEA">
                                   <TD height="100%" valign="middle" align="center">
                                                       </TD>
                               </table>
                           </BODY>
                       </HTML>"""

    val lineOp = stRes.lines.find( line => line.contains("location.href"))

    lineOp match
    {
      case Some(line) => line match
        {
          case BouncerActor.LOCATION_REF(_, link, _) =>
            assert(link == "/MyYad2/MyOrder/CarDetails.php?Up=2&CarID=3156496")
          case _ =>  assert(false, "Could not bounce, reference link was not found!")
        }
      case None => assert(false, "Could not bounce, did not find a suitable line!")
    }
  }

}
