
import java.security.PublicKey
import java.sql.Blob

import org.json4s.native.Serialization
import org.json4s.native.Serialization._
import org.json4s.{DefaultJsonFormats, ShortTypeHints}
import spray.httpx.SprayJsonSupport
import spray.json.{JsonFormat, DefaultJsonProtocol}
import spray.httpx.SprayJsonSupport._

import akka.actor.ActorRef

case class Feed (
                  message : String
                 )


case class PageData (
                        pageID : Int,
                        userType : String,
                        about : String,
                        can_checkin : Boolean,
                        can_post : Boolean,
                        category : String,
                        location : String,
                        phone : String,
                        feed : List[Feed]
                      )

case class User (
                    userID : Int,
                    userName : String,
                    friendList : List[Int],
                    pageList : List[PageData]
)

case class ImageUploaded(imageID: Int,
                         blob: String)
case class UserImage (
                       user : User,
                       imageList : List[ImageUploaded]
                       )
case class EncodedSignature( data : Array[Byte])

object User extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val feedFormat = jsonFormat1(Feed.apply)
  implicit val pageFormat = jsonFormat9(PageData.apply)
  implicit val userFormat = jsonFormat4(User.apply)
  implicit val imageUploadedFormat = jsonFormat2(ImageUploaded)
  implicit val userImageFormat = jsonFormat2(UserImage)
  implicit val encodedSignature = jsonFormat1(EncodedSignature)


}