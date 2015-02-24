package controllers

import java.text.SimpleDateFormat
import java.util.{TimeZone, Locale, Date}

import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.{JsString, JsNumber, JsValue, Json}
import play.api.libs.ws.WS
import play.api.mvc._

import scala.concurrent.Future

object Application extends Controller {
  private val lgtmRegex = """value="([^"]+)" class="form-control" id="imageUrl"""".r

  def lgtm = Action.async { implicit req =>
    req.body.asJson.map {json =>
      val postId = (json \ "post" \ "id").as[Long]
      WS.url("http://www.lgtm.in/g").get().map { response =>
        lgtmRegex.findFirstMatchIn(response.body).map(_.group(1)).get
      }.map { url =>
        Ok(Json.obj(
          "message" -> url,
          "replyTo" -> postId
        ))
      }
    }.getOrElse {
      Future.successful(BadRequest)
    }
  }

  def weather = Action.async { implicit req =>
    req.body.asJson.map { json =>
      val post = json \ "post"
      val postId = (post \ "id").as[Long]
      val q = (post \ "message").as[String].replaceAll("@[-\\w]+\\+", "").trim
      WS.url(s"http://api.openweathermap.org/data/2.5/weather").withQueryString("q" -> q).get().map { response =>
        val json = response.json
        val message = if (json \ "cod" == JsNumber(BigDecimal("200"))) {
          val city = "In " + (json \ "name").as[String] + ", " + (json \ "sys" \ "country").as[String]
          val weather = (json \ "weather").as[Seq[JsValue]].head
          val description = (weather \ "description").as[String]
          val icon = (weather \ "icon").as[String]
          s"$city - $description\nhttp://openweathermap.org/img/w/$icon.png"
        } else {
          (json \ "message").as[String]
        }
        Ok(Json.obj("message" -> message, "replyTo" -> postId))
      }
    }.getOrElse {
      Future.successful(BadRequest)
    }
  }

  def weatherForecast = Action.async { implicit req =>
    req.body.asJson.map { json =>
      val post = json \ "post"
      val postId = (post \ "id").as[Long]
      val q = (post \ "message").as[String].replaceAll("@[-\\w]+\\+", "").trim
      WS.url(s"http://api.openweathermap.org/data/2.5/forecast/daily").withQueryString("q" -> q).get().map { response =>
        val json = response.json
        val message = if (json \ "cod" == JsString("200")) {
          val cityJson = (json \ "city")
          val start = "In " + (cityJson \ "name").as[String] + ", " + (cityJson \ "country").as[String] + "\n"
          val formatter = new SimpleDateFormat("EEE, MMM d", Locale.ENGLISH)
          (json \ "list").as[Seq[JsValue]].map { data =>
            val date = formatter.format(new Date((data \ "dt").as[Long] * 1000))
            val weather = (data \ "weather").as[Seq[JsValue]].head
            val description = (weather \ "description").as[String]
            val emoji = (weather \ "main").as[String] match {
              case "Clear" => " :sunny:"
              case "Rain" => " :umbrella:"
              case "Clouds" => " :cloud:"
              case "Snow" => " :snowflake:"
              case _ => ""
            }
            s"$date - $description$emoji"
          }.mkString(start, "\n", "")
        } else {
          (json \ "message").as[String]
        }
        Ok(Json.obj("message" -> message, "replyTo" -> postId))
      }
    }.getOrElse {
      Future.successful(BadRequest)
    }
  }

  def localtime() = Action.async { implicit req =>
    req.body.asJson.map { json =>
      val post = json \ "post"
      val postId = (post \ "id").as[Long]
      val address = (post \ "message").as[String].replaceAll("@[-\\w]+\\+", "").trim
      WS.url("http://maps.googleapis.com/maps/api/geocode/json").withQueryString("address" -> address).get().flatMap { response =>
        val location = (response.json \ "results" \\ "geometry")(0) \ "location"
        val now = System.currentTimeMillis()
        WS.url("https://maps.googleapis.com/maps/api/timezone/json")
          .withQueryString(
            "location" -> Seq("lat", "lng").map(location \ _).mkString(","),
            "timestamp" -> (now / 1000).toString
          ).get().map { response =>
          val json = response.json
          val format = new SimpleDateFormat("hh:mm aaa 'on' MMM d", Locale.ENGLISH)
          format.setTimeZone(TimeZone.getTimeZone("UTC"))

          val date = new Date(now + Seq("rawOffset", "dstOffset").map(e => (json \ e).as[Int]).sum * 1000)

          val message = (json \ "timeZoneName").as[String] + "\n" + format.format(date).toLowerCase
          Ok(Json.obj("message" -> message, "replyTo" -> postId))
        }
      }
    }.getOrElse {
      Future.successful(BadRequest)
    }
  }
}
