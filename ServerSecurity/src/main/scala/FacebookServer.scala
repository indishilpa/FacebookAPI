import java.net.URLDecoder
import java.security._
import java.security.spec.{X509EncodedKeySpec, PKCS8EncodedKeySpec}
import javax.crypto.{Cipher, SecretKey}
import org.apache.commons.codec.binary.Base64
import spray.json._
import spray.httpx.unmarshalling._
import akka.actor.{Props, Actor, ActorSystem}
import spray.routing._
import spray.http._
import akka.pattern.ask
import akka.util.Timeout
import sun.misc.BASE64Decoder
import scala.concurrent.duration._
import spray.httpx.SprayJsonSupport._

import scala.util.Random


object serverRSAKey{
  var keys  = getKeys()
  val serverPrivateKey = keys.getPrivate()
  val serverPublicKey = keys.getPublic()

  def getKeys(): KeyPair = {
    val keyGen = KeyPairGenerator.getInstance("RSA")
    keyGen.initialize(1024)
    val keys = keyGen.genKeyPair()
    keys
  }

  def toS(priv:PrivateKey):String = {
    new String(Base64.encodeBase64(priv.getEncoded))
  }

  def toS(pub:PublicKey):String = {
    new String(Base64.encodeBase64(pub.getEncoded))
  }
}

object clientData{
  var userPublicKeys = new Array[PublicKey](10000)
  var tokenSet = new Array[String](10000)
  var isAuthorised = new Array[Boolean](10000)
  var lastLogin = new Array[Long](10000)
  val max_session_period = 1800000

}
object FacebookServer extends App with SimpleRoutingApp{
  implicit val actorSystem = ActorSystem()

  import  actorSystem.dispatcher
  implicit val timeout = Timeout(100.second)
  var fd = List(Feed("Mission to Pluto !!"))
  var plentyOfUsers = List[User]()//List( User(userID = 5000,userName="abc",friendList = List(2003), pageList = List(PageData(5000,"indivi","myself",false,true,"NA","gnv","352745", feed = fd))),
                            //User(userID = 2003,userName="xyz",friendList = List(5000), pageList = List(PageData(2003,"Company","Org",false,true,"NA","Chicago","352745", feed = List(Feed("Travelling to Mars !!"))))))

  var userImageList = List[UserImage]()
  var actorMap = Map[Int, String]()
/*  for ( i <- 0 to (clientData.isAuthorised.length - 1)) {
    clientData.isAuthorised(i)=true
  }*/


   //Initialize current time to 0 as user never logged in
   for (i <- 0 to (clientData.lastLogin.length - 1)) {
    clientData.lastLogin(i)=0
  }
  import User._
  def getJson(route: Route):Route = {
    get{
      respondWithMediaType(MediaTypes.`application/json`){
        route
      }
    }
  }

  lazy val helloActor = actorSystem.actorOf(Props(new HelloActor()))
  lazy val burnActor = actorSystem.actorOf(Props(new BurnActor()))
  lazy val helloRoute = get {
    path("hello"){
      complete{
        "Welcome to FaceBook"
      }
    }
  }
  lazy val helloRoute2 = get {
    path("List"){
      ctx=>helloActor!ctx
    }
  }
  lazy val burnRoute = get {
    path("burn" / "remaining"){
      complete{
        (burnActor ? RemainingBurningTime)
          .mapTo[Int]
          .map(s=>s"The remaining burning time is $s")
      }
    }
  }
  startServer(interface="localhost", port=8080){
    helloRoute ~
      helloRoute2 ~
      burnRoute~
      getJson {
        path("list" / "all") {
          complete {
            plentyOfUsers
          }
        }
      }~
      get{
        path("user" / "fetchName"){
          parameters('userId.as[Int]) { (userId) =>
            complete {
                if (true==clientData.isAuthorised(userId)) {
                  if (clientData.lastLogin(userId) == 0 || System.currentTimeMillis()-clientData.lastLogin(userId)<clientData.max_session_period){
                    clientData.lastLogin(userId) = System.currentTimeMillis()
                   val myIndex = plentyOfUsers.indexWhere(_.userID == userId)
                   if (myIndex != -1) {
                     var myData = plentyOfUsers(myIndex)
                     myData.userName
                   }
                   else {
                     "Invalid user ID " + userId + "for fetching username"
                   }
                 }
                else{
                     clientData.isAuthorised(userId)= false
                     clientData.lastLogin(userId) = 0
                     "UserId "+userId+"  session expired. Re - Authenticate"
                  }
              }
              else{
                  "Your UserID "+userId+" is not Authorized"
              }
            }
          }
        }
      }~
      get{
        path("user" / "getAnyPageId"){
          parameters('userId.as[Int]) { (userId) =>
            complete {
              if (true==clientData.isAuthorised(userId)){
              var r = new Random
              val myIndex = plentyOfUsers.indexWhere(_.userID == userId)
              if (myIndex != -1) {
                var myData = plentyOfUsers(myIndex).pageList
                if (myData.length != 0) {
                  myData(r.nextInt(myData.length)).pageID.toString
                }
                else {
                  "PageList is empty"
                }
              }
              else {
                "Invalid user ID " + userId + "for fetching getAnyPageId"
              }
            }
            else {
                  "Your UserID "+userId+" is not Authorized"
              }
          }
         }
        }
      }~
      post{
        path("user" / "authenticate"){
          parameters('userId.as[Int], 'pubKey) { (userId, pubKey) =>
            complete{
                  var token : String = "abc"// generateTokenForUser(userId)              
                  var etoken = encrypRSA(token,toPub(pubKey))
            //  println("authenticated etoken value : "+etoken)
              etoken
            }
          }
        }
      }~
      post{
        path("user" / "authorize"){
          parameters('userId.as[Int]) { (userId) =>
            complete{
              var token : String = generateTokenForUser(userId)
              clientData.tokenSet(userId)=token
           //   println("token :"+clientData.tokenSet(userId))
              clientData.tokenSet(userId)
            }
          }
        }
      }~
      post{
        path("user" / "verifyMe") {
          entity(as[EncodedSignature]) { (sign : EncodedSignature) =>

          parameters('userId.as[Int]) { (userId) =>
            complete {
              if (clientData.tokenSet(userId)!=null){
                if (digitalVerifySig((clientData.tokenSet(userId)).getBytes, clientData.userPublicKeys(userId), sign.data)) {
                  clientData.isAuthorised(userId)=true          
                  "Authorised"
                }
                else {
               //   println("UNAuthorized")
                  "UnAuthorised"
                }
            }
              else{
                "UnAuthorised"
              }
            }
          }
        }
        }
      }~
      post{
        path("user" / "sendPubKey") {
          headerValueByName("public-key") { pubKey =>
            parameters('userId.as[Int]) { (userId) =>
              complete{
                var token : String = "abc"// generateTokenForUser(userId)
                //println("userId "+userId+" pubKey fina  "+toPub(pubKey))
                clientData.userPublicKeys(userId) = toPub(pubKey)
                actorMap += (userId -> pubKey)
                "OK"
              }
            }
          }
        }
      }~
      get{
        path("user" / "getServerPublicKey"){
            complete {
              //No Authentication Needed. Will Give this to everyone.
              var pubK = toS(serverRSAKey.serverPublicKey)
              println("getServerPublicKey "+pubK)
              pubK
            }

        }
      }~
      get{
        path("user" / "getUserPublicKey"){
          parameters('userId.as[Int]) { (userId) =>
            complete{              
         //     println("getUserPublicKey : userId : " + userId + " " + actorMap(userId))
              actorMap(userId)
            }
          }
        }
      }~
      post{
        //This route is not being  used
        path("user" / "verifyUser"){
          parameters('userId.as[Int], 'pubKey) { (userId, pubKey) =>
            complete{
              var token : String = generateTokenForUser(userId)
              var etoken = encrypRSA(token,toPub(pubKey))
              "Authorised"
            }
          }
        }
      }~
      get{
      path("user" / "getMyFriendId"){
        parameters('userId.as[Int]) { (userId) =>
          complete {
            if(true==clientData.isAuthorised(userId)){
            var r = new Random
            val myIndex = plentyOfUsers.indexWhere(_.userID == userId)
            if (myIndex != -1) {
              var myfrnds = plentyOfUsers(myIndex).friendList
              if (myfrnds.length != 0) {
                myfrnds(r.nextInt(myfrnds.length)).toString
              } else {
                "No friends for this user with user id : " + userId
              }

            }
            else {
              "Invalid user ID " + userId + "for fetching getMyFriendId"
             }
            }
            else
            {
              "Your UserID "+userId+" is not Authorized"
            }
           }
          }
        }
    }~
      get{
        path("user" / "viewFriendWallPosts"){
          parameters('myUserId.as[Int],'friendUserId.as[Int]) { (myUserId,friendUserId) =>
            complete{
              if(true==clientData.isAuthorised(myUserId)){
              var r = new Random
              val fIndex = plentyOfUsers.indexWhere(_.userID==friendUserId)
              if (fIndex != -1) {
                var pListIndex = plentyOfUsers(fIndex).pageList.indexWhere(_.pageID==friendUserId)
                if(pListIndex != -1){
                    var xlist = plentyOfUsers(fIndex).pageList(pListIndex).feed
                    var friendFeeds = List[String]()
                    for(i <- 0 to xlist.length - 1){                    
                      friendFeeds = xlist(i).message :: friendFeeds 
                    }
                    friendFeeds
                  }
                else {
                  "Could not fetch proper pListIndex"
                }
              }
              else {
                 "Could not fetch proper pListIndex"
              }
            }
            else
            {
              "Your UserID "+myUserId+" is not Authorized"
            }
            }
          }
        }
      }~
      post{
        path("user" / "postOnWall") {
          entity(as[Feed]) { (feed : Feed) =>
            parameters('friendId.as[Int]) { (friendId) =>
              complete {
                val myIndex = plentyOfUsers.indexWhere(_.userID == friendId)
                if (myIndex != -1){
                var myData = plentyOfUsers(myIndex)
                var pgListIndex = plentyOfUsers(myIndex).pageList.indexWhere(_.pageID == friendId)
                  if(pgListIndex != -1){
                    var pfpage = plentyOfUsers(myIndex).pageList(pgListIndex)
                    var currfeed = plentyOfUsers(myIndex).pageList(pgListIndex).feed
                    currfeed = feed :: currfeed

                    var page = PageData(pfpage.pageID,pfpage.userType,pfpage.about,pfpage.can_checkin,pfpage.can_post,pfpage.category,pfpage.location,pfpage.phone,currfeed)
                    val newUser = User(myData.userID, myData.userName, friendList = myData.friendList, pageList = page::myData.pageList.filter(_.pageID != friendId))
                    plentyOfUsers = plentyOfUsers.filter(_.userID != friendId)
                    plentyOfUsers = newUser :: plentyOfUsers
                    newUser
                  }
                  else
                    {
                      "pgList index is invalid"
                    }

                }
                else{
                  "postOnWall failed for " + friendId
                }
              }
            }
          }
        }
      }~
      get{
        path("user" / "readUserPage"){
          parameters('userId.as[Int],'pageId.as[Int]) { (userId,pageId) =>
            complete{
              if(true==clientData.isAuthorised(userId)){
              val myIndex = plentyOfUsers.indexWhere(_.userID==userId)
              if (myIndex != -1) {
                var myData = plentyOfUsers(myIndex).pageList
                var pageList = myData.filter(_.pageID==pageId)
                if(pageList.isEmpty==false) {
                  pageList
                }
                else {
                  "PageId not found"+pageId
                }
              }
              else {
                "Invalid user ID "+userId+"for fetching username"
              }
            }
            else
            {
              "Your UserID "+userId+" is not Authorized"
            }


          }
          }
        }
      }~
      put{
        path("user" / "createPage") {
          entity(as[User]) { (user : User) =>
            parameters('userId.as[Int]) { (userId) =>
              complete {
              if(true==clientData.isAuthorised(userId)){
                val myIndex = plentyOfUsers.indexWhere(_.userID == userId)
                if (myIndex != -1){
                var myData = plentyOfUsers(myIndex)
                var newpageList = List[PageData]()
                newpageList = myData.pageList ::: user.pageList
                val newUser = User(myData.userID, myData.userName, friendList = myData.friendList, pageList = newpageList)
                if (myIndex != -1) {
                  plentyOfUsers = plentyOfUsers.filter(_.userID != userId)
                  plentyOfUsers = newUser :: plentyOfUsers
                }
                else {
                  "Create Page List failed for " + userId
                }
                newUser
               }
               else{
                  "Create Page List failed for " + userId
                }
              }
              else
              {
                "Your UserID "+userId+" is not Authorized"
              }
             }
            }
          }
        }
      }~
      put{
        path("user" / "updatePage") {
          entity(as[User]) { (user : User) =>
            parameters('userId.as[Int]) { (userId) =>
              complete {
               if(true==clientData.isAuthorised(userId)){
                val myIndex = plentyOfUsers.indexWhere(_.userID==userId)
                if(myIndex != -1) {
                  var myData = plentyOfUsers(myIndex)
                  var newpageList = List[PageData]()
                  newpageList =  user.pageList
                  val newUser = User(myData.userID, myData.userName, friendList = myData.friendList, pageList = newpageList)
                    plentyOfUsers = plentyOfUsers.filter(_.userID != userId)
                    plentyOfUsers = newUser :: plentyOfUsers
                    newUser
                }
                else {
                  "Update Page List failed for "+userId
                }
              }
              else
              {
                "Your UserID "+userId+" is not Authorized"
              }
              }
            }
          }
        }
      }~
      put{
        path("user" / "deletePage") {
          parameters('userId.as[Int], 'pageId.as[Int]) { (userId, pageId) =>
            complete {
              if(true==clientData.isAuthorised(userId)){
              val myIndex = plentyOfUsers.indexWhere(_.userID==userId)

              if(myIndex != -1){
                var myData = plentyOfUsers(myIndex)
                var myPgList = plentyOfUsers(myIndex).pageList
                val newPgList = plentyOfUsers(myIndex).pageList.filter(_.pageID!=pageId)
                val newUser = User(myData.userID, myData.userName, friendList = myData.friendList, pageList = newPgList)
                plentyOfUsers = plentyOfUsers.filter(_.userID != userId)
                plentyOfUsers = newUser :: plentyOfUsers

                plentyOfUsers
              }
              else{
                "delete page failed myIndex "+myIndex+" userId "+userId+" pageId "+pageId
              }
            }
            else
            {
              "Your UserID "+userId+" is not Authorized"
            }
            }
          }
        }
      }~
      put{

        path("user" / "updateProfile") {
          entity(as[User]) { (user : User) =>{
          parameters('userId.as[Int], 'pageId.as[Int]) { (userId, pageId) =>
            complete {
              val myIndex = plentyOfUsers.indexWhere(_.userID == userId)
              if(true==clientData.isAuthorised(userId)){
              if (myIndex != -1) {
                var myData = plentyOfUsers(myIndex)
                var myPgList = plentyOfUsers(myIndex).pageList
                var newPgList = plentyOfUsers(myIndex).pageList.filter(_.pageID != pageId)
                newPgList = user.pageList(0) :: newPgList
                val newUser = User(myData.userID, myData.userName, friendList = myData.friendList, pageList = newPgList)
                plentyOfUsers = plentyOfUsers.filter(_.userID != userId)
                plentyOfUsers = newUser :: plentyOfUsers

                newUser
              }
              else
                {"updateProfile failed"}
            }
            else
            {
              "Your UserID "+userId+" is not Authorized"
            }

            }
          }
        }
        }
        }
      }~
      get{
        path("silicon" / IntNumber / "details"){ index =>
          complete{ "OK"
          }
        }
      }~
      post{
        path("user" / "add"){
          entity(as[UserImage]) { someUserImage: UserImage =>
            plentyOfUsers = someUserImage.user :: plentyOfUsers
            userImageList = someUserImage :: userImageList
            complete{
            if(true==clientData.isAuthorised(someUserImage.user.userID)){
              plentyOfUsers
            }
            else
            {
              "Your UserID "+someUserImage.user.userID+" is not Authorized"
            }
            }
          }
        }
      }~
      put{
        path("user" / IntNumber / "addPhotoToAlbum"){ userid =>
          entity(as[ImageUploaded]) { (postPhoto) =>
            complete {
            if(true==clientData.isAuthorised(userid)){
              val ind = userImageList.indexWhere(_.user.userID==userid)
              if(ind != -1) {
                var tempList = userImageList(ind).imageList
                tempList = postPhoto :: tempList
                var newUserImage = UserImage(user = userImageList(ind).user, imageList = tempList)
                userImageList = userImageList.filter(_.user.userID != userid)
                userImageList = newUserImage :: userImageList
                "Photo added to album sucess"
              }
              else{
                "Failed adding photo to album"
              }
            }
            else
            {
              "Your UserID "+userid+" is not Authorized"
            }
            }

          }
        }
      }~
      get{
        path("user" / IntNumber / "getAlbum") { (userId) =>
          complete {
            if(true==clientData.isAuthorised(userId)){
            val ind = userImageList.indexWhere(_.user.userID == userId)
            if(ind >= 0){
            var tempList = userImageList(ind).imageList

            val r = new scala.util.Random

            if (tempList.length != 0) {
              var imageIndex = r.nextInt(tempList.length)
              var image = tempList.indexWhere(_.imageID == tempList(imageIndex).imageID)
              tempList(image).blob
            } else {
              "Nothing to show here :("
            }
          }
            else{
              "Nothing to show"
            }
          }
          else
          {
            "Your UserID "+userId+" is not Authorized"
          }
        }
        }
      }~
     get{
      path("user" / IntNumber / "images") { userId =>
        complete {
          if(true==clientData.isAuthorised(userId)){
          val r = new scala.util.Random
          val ind = userImageList.indexWhere(_.user.userID == userId)
          if (ind >= 0){
          var tempList = userImageList(ind).imageList

          if (tempList.length != 0) {
            var imageIndex = r.nextInt(tempList.length)
            tempList(imageIndex).imageID.toString()
          } else {
            "Nothing to show here :("
          }
        }
        else{
            "Nothing to show"
          }
        }
        else
        {
          "Your UserID "+userId+" is not Authorized"
        }
      }
      }
    }~
      delete {
        path("user" / IntNumber / "deleteImage" / IntNumber) {  (userid, imageid) =>
          complete {
            if(true==clientData.isAuthorised(userid)){
            val ind = userImageList.indexWhere(_.user.userID==userid)
            if(ind != -1) {
              var tempList = userImageList(ind).imageList
              tempList = tempList.filter(_.imageID != imageid)

            var newUserImage = UserImage(user = userImageList(ind).user, imageList = tempList)

              userImageList = userImageList.filter(_.user.userID != userid)
              userImageList = newUserImage :: userImageList
              "Image Delete Success"
            }
            else{
              "Image Delete Failed"
            }
          }
          else
          {
            "Your UserID "+userid+" is not Authorized"
          }

        }
        }
      }~
      delete {
        path("user" / IntNumber / "deleteAlbum" ) {  (userid) =>
          val ind = userImageList.indexWhere(_.user.userID==userid)
          var tempList = List[ImageUploaded]()

          complete {
            if(true==clientData.isAuthorised(userid)){
            if(ind != -1) {
            var newUserImage = UserImage(user = userImageList(ind).user, imageList = tempList)

              userImageList = userImageList.filter(_.user.userID != userid)
              userImageList = newUserImage :: userImageList
              "Album Delete Success"
            }
            else{
              "Album Delete Failed"
            }
          }
          else
          {
            "Your UserID "+userid+" is not Authorized"
          }
          }
        }
      }~
      put{
        path("user" / "addFriend"){
          parameters('myId.as[Int],'frndUserId.as[Int]) { (myId,frndUserId) =>
            complete{
              if(true==clientData.isAuthorised(myId)){
              //Add friend to initiator
              val myIndex = plentyOfUsers.indexWhere(_.userID==myId)
              if(myIndex != -1) {
                var myData = plentyOfUsers(myIndex)
                var myFriends = myData.friendList
                var myNewFriends = List[Int]()
                if (myFriends.contains(frndUserId) != true) {

                  myNewFriends = myFriends ::: (frndUserId :: myNewFriends)
                  val newUser = User(myData.userID, myData.userName, friendList = myNewFriends, pageList = myData.pageList)
                  if (myIndex != -1) {
                    plentyOfUsers = plentyOfUsers.filter(_.userID != myId)
                    plentyOfUsers = newUser :: plentyOfUsers
                    plentyOfUsers
                  }
                  else {
                    "Adding to myFriends failed"
                  }
                  //Add initiator to friend
                  val friendIndex = plentyOfUsers.indexWhere(_.userID == frndUserId)
                  if (friendIndex != -1){
                  var friendData = plentyOfUsers(friendIndex)
                  var friendFrdlist = friendData.friendList
                  var friendNewFrdlist = List[Int]()
                  if (friendFrdlist.contains(myId) != true) {
                    friendNewFrdlist = friendFrdlist ::: (myId :: friendNewFrdlist)
                    val frndUser = User(friendData.userID, friendData.userName, friendList = friendNewFrdlist, pageList = friendData.pageList)
                    if (friendIndex != -1) {
                      plentyOfUsers = plentyOfUsers.filter(_.userID != frndUserId)
                      plentyOfUsers = frndUser :: plentyOfUsers
                      plentyOfUsers
                    }
                    else {
                      "Adding to friendNewFrdlist Failed"
                    }
                  }
                 }
                }
              }
              plentyOfUsers
            }
            else
            {
              "Your UserID "+myId+" is not Authorized"
            }
            }
          }
        }
      }~
      put{
        path("user" / "deleteFriend"){
          parameters('myId.as[Int],'frndUserId.as[Int]) { (myId,frndUserId) =>
            complete{
              if(true==clientData.isAuthorised(myId)){
              val myIndex = plentyOfUsers.indexWhere(_.userID==myId)
              if(myIndex != -1) {
              var myData = plentyOfUsers(myIndex)
              var myFriends = myData.friendList
              var myNewFriends = List[Int]()
              if(myFriends.contains(frndUserId)==true) {

                myNewFriends = myFriends.filter(x => x != frndUserId)

                val newUser = User(myData.userID, myData.userName, friendList = myNewFriends, pageList = myData.pageList)
                if (myIndex != -1) {
                  plentyOfUsers = plentyOfUsers.filter(_.userID != myId)
                  plentyOfUsers = newUser :: plentyOfUsers
                  plentyOfUsers
                }
                else {
                  "Adding to myFriends failed"
                }
                //Add initiator to friend
                val friendIndex = plentyOfUsers.indexWhere(_.userID == frndUserId)
                if(myIndex != -1) {
                var friendData = plentyOfUsers(friendIndex)
                var friendFrdlist = friendData.friendList
                var friendNewFrdlist = List[Int]()
                if (friendFrdlist.contains(myId) == true) {
                  friendNewFrdlist = friendFrdlist.filter(x => x != myId)
                  val frndUser = User(friendData.userID, friendData.userName, friendList = friendNewFrdlist, pageList = friendData.pageList)
                  if (friendIndex != -1) {
                    plentyOfUsers = plentyOfUsers.filter(_.userID != frndUserId)
                    plentyOfUsers = frndUser :: plentyOfUsers
                    plentyOfUsers
                  }
                  else {
                    "Adding to friendNewFrdlist Failed"
                  }
                }
               }
               }
              }
              plentyOfUsers
            }
            else{
              "Your UserID "+myId+" is not Authorized"
            }
           }
          }
        }
      }~
      delete {
        path("user" / "delete") {
          parameters("user_id".as[Int]) { (user_id) =>
            plentyOfUsers = plentyOfUsers.filter(_.userID != user_id)
            complete {
              plentyOfUsers
            }
          }
        }
      }~
      put{
        path("user" / "update"/ "phone"){
          parameters("user_id".as[Int],"pageid".as[Int],"newphone") { (user_id,pageid,newphone) =>
            complete{
            if(true==clientData.isAuthorised(user_id)){
              val ind = plentyOfUsers.indexWhere(_.userID==user_id)
              var pind = plentyOfUsers(ind).pageList.indexWhere(_.pageID==pageid)
              var user1 = plentyOfUsers(ind)
              var u1pl= plentyOfUsers(ind).pageList(pind)
              val newUser = User(user1.userID,user1.userName,user1.friendList, pageList = List(PageData(u1pl.pageID,u1pl.userType,u1pl.about,u1pl.can_checkin,u1pl.can_post,u1pl.category,u1pl.location,newphone, feed = u1pl.feed)))
              if(pind != -1) {
                plentyOfUsers = plentyOfUsers.filter(_.userID != user_id)
                plentyOfUsers = newUser :: plentyOfUsers
                plentyOfUsers
              }
              else{
                "Update Failed"
              }
              //plentyOfUsers(ind).pageList(pind)
              /*plentyOfUsers = someUser :: plentyOfUsers
                someUser
                val ind = plentyOfUsers.indexWhere(_.userID==6)
                val x = plentyOfUsers(ind).pageList(0).pageID
                s" UserID12 = 6 at : ${ind} pageList 0 ${x}"*/
            }
            else
            {
              "Your UserID "+user_id+" is not Authorized"
            }
            }
          }
        }
      }

  }


  def generateTokenForUser(uid: Int) : String = {
    val  AB = "abcdefghijklmnopqrstuvwxyz"
    val r = new scala.util.Random
    val sb = new StringBuilder
    for(i <- 1 to 8)
      sb.append(AB.charAt(r.nextInt(AB.length())))
    var gentoken= sb.toString()
    clientData.tokenSet(uid) = gentoken
    return clientData.tokenSet(uid)
  }

  def toS(priv:PrivateKey):String = {
    new String(Base64.encodeBase64(priv.getEncoded))
  }

  def toPub23(s:String):PublicKey  = {
    KeyFactory.getInstance("RSA").generatePublic(new X509EncodedKeySpec(Base64.decodeBase64(s)))
  }
  def toS(pub:PublicKey):String = {
    new String(Base64.encodeBase64(pub.getEncoded))
  }

  def toPriv(s:String):PrivateKey = {
    KeyFactory.getInstance("RSA").generatePrivate(new PKCS8EncodedKeySpec(Base64.decodeBase64(s)))
  }

  def stringToPublicKey(s:String):PublicKey  = {
    KeyFactory.getInstance("RSA").generatePublic(new X509EncodedKeySpec(Base64.decodeBase64(s)))
  }
  def toPub(base64EncodedStringOfPublicKey: String): PublicKey = {
    val decoder = new BASE64Decoder()
    val c = decoder.decodeBuffer(base64EncodedStringOfPublicKey);
    val keyFact = KeyFactory.getInstance("RSA")
    val x509KeySpec: X509EncodedKeySpec = new X509EncodedKeySpec(c)
    keyFact.generatePublic(x509KeySpec)
  }

  def encrypRSA(data : String , key : PublicKey) : String = {
    val AesCipher = Cipher.getInstance("RSA/ECB/PKCS1Padding")
    AesCipher.init(Cipher.ENCRYPT_MODE, key)
    val evalue =  AesCipher.doFinal(data.getBytes)
    return new String(Base64.encodeBase64(evalue))
  }


  def digitalVerifySig(data : Array[Byte], key : PublicKey ,sig : Array[Byte]):Boolean= {
    try {
      var signer: Signature = Signature.getInstance("SHA256withRSA")
      signer.initVerify(key)
      signer.update(data)
      return (signer.verify(sig))
    }
    catch{
      case _: Throwable => println("Ignoring NonFatal Exception")
        return false
    }

  }
  class HelloActor extends Actor {
    override def receive = {
      case ctx: RequestContext =>
        ctx.complete("Welcome to pseudo facebook")
    }
  }

  class BurnActor extends Actor{
    val remainingTime =10
    override def receive = {
      case RemainingBurningTime =>
        sender ! remainingTime
    }
  }

  object RemainingBurningTime
}