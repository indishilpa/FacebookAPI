import javax.crypto.spec.{SecretKeySpec, IvParameterSpec}
import java.io.{File, PrintWriter, FileOutputStream}
import java.net.{URLEncoder, URL}
import java.nio.file.{Paths, Files}
import java.security._
import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}
import javax.crypto.spec.SecretKeySpec
import javax.xml.bind.DatatypeConverter
import akka.actor.{ActorRef, Actor, Props, ActorSystem}
import akka.util.Timeout
import org.apache.commons.codec.binary.Base64
import spray.client.pipelining._
import spray.http.{HttpHeader, HttpCookie, HttpResponse, HttpRequest}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Random}
import scala.concurrent.duration._
import spray.client.pipelining.{sendReceive, _}
import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}
import javax.crypto.spec.SecretKeySpec
import javax.crypto.{Cipher, KeyGenerator, SecretKey}
import akka.pattern.ask
import java.io.FileOutputStream
import java.net.URL
import java.nio.file.{Paths, Files}
import spray.http.HttpHeaders.Cookie
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.Security;
import java.security.Signature
import java.security.MessageDigest

case class InitClientSystem()
case class RegisterNewUser(uid : Int)
case class UpdateFriendListOnBoot()
case class AddFriend(myUserId: Int , friendUserId: Int)
case class DeleteFriend(myUserId: Int , friendUserId: Int)
case class DeleteFriendsOps(myUserId: Int)
case class CreatePagesForUsers()
case class CreatePage(myUserId : Int, pageData : PageData)
case class DeletePage(userId : Int, pageId : Int)
case class UpdatePage(userId : Int,pageId: Int,pageData:PageData)
case class AddNewPageToUser(userId : Int)
case class GeneratePageDataandUpdatePage(userId : Int)
case class ReadPageData(userId : Int, pageId:Int)
case class CreateUserProfile(userId : Int , userName : String)
case class UpdateUserProfile(userId : Int)
case class ViewFriendsAlbumsPhotosWallPosts(myUserId: Int)
case class PostOnFriendsWall(myUserId: Int , friendId :Int = -1)
case class SimulateUserInteraction()
case class ViewMyProfilePicture(userId : Int)
case class DeleteProfilePicture(userId : Int)
case class CreateAlbum(userId : Int)
case class ViewAlbum(userId : Int)
case class DeleteAlbum(userId : Int )
case class InformServerWithPublicKey(uid:Int)

case class getEncryptedSymKey(pubKey: PublicKey, symKey: SecretKey)
case class Keys(pubKey: PublicKey, privKey: PrivateKey)
case class Info(symKey: String, value: String)
case class GetServerPublicKey()
case class InitiateUserAuthorization(uid:Int)

object Auth {
  var token = new Array[String](10000)
  var serverKey : PublicKey = null
}


object keyDB{
  var publicKeyMap = Map[Int,PublicKey]()
}

class FaceBookUser(aesKeyList: List[SecretKey], keyPair: Keys, actorId:Int, totalUsers: Int) extends Actor
{
  var pipeline = sendReceive
  var r = new Random
  import User._
  implicit val timeout = Timeout(3000 seconds)
  
  var selfSymFeed = aesKeyList(0)
  var selfSymAlbum = aesKeyList(1)
  var selfSymProfile = aesKeyList(2)

  var selfPub = keyPair.pubKey
  var selfPriv = keyPair.privKey
  var checkPublicKeys = Map[Int, String]()

  def receive = {


    case RegisterNewUser(uid : Int) =>    {
      val prefix: String = "face"
      val userName: String =  userNameGenerator(8)
      val userId = uid
      var r = new Random
      var myFriends = List[Int]()
      var profileData = createUserProfile(userId,userName)
      var newUser = User(userID = userId,userName =userName , friendList = myFriends, pageList = List(profileData))
      userSignUp(newUser)
    }

    case InformServerWithPublicKey(uid:Int)=>{
      informServerWithPublicKeyUtil(uid,keyPair.pubKey)
    }

    case InitiateUserAuthorization(uid:Int)=>{
 //     println("InitiateUserAuthorization  UserId "+uid)
      authUtil(uid)
    }
    case GetServerPublicKey() =>{
      getServerPublicKey()
    }

    case getEncryptedSymKey(pubKey: PublicKey, selfSym: SecretKey) => {
      val rsa = Cipher.getInstance("RSA")

      rsa.init(Cipher.ENCRYPT_MODE, pubKey)
      var esym = rsa.doFinal(selfSym.getEncoded)

      sender ! new String(Base64.encodeBase64(esym))
    }

    case UpdateFriendListOnBoot() => {

      var numFriends = r.nextInt(totalUsers/2 + 1)
      var myFriends = List[Int]()

      for(i <- 0 to totalUsers-1) {
        if(i < totalUsers-11) {
          addFriendUtil(i + 1, i + r.nextInt(10))
          addFriendUtil(i + 1, i + r.nextInt(10))
        }
        else {
          addFriendUtil(i + 1, i - r.nextInt(10))
          addFriendUtil(i + 1, i - r.nextInt(10))
        }

      }

      Thread.sleep(5000L)
    }

    case AddFriend(myUserId: Int , friendUserId: Int) => {
      addFriendUtil(myUserId,friendUserId)
    }

    case DeleteFriend(myUserId: Int , friendUserId: Int) => {
      deleteFriendUtil(myUserId, friendUserId)
    }

    case DeleteFriendsOps(myUserId: Int) =>{


      val friendId = getOneOfFriendId(myUserId)
      if(friendId > 0)
      {
        self!DeleteFriend(myUserId,friendId)
      }
      else{
        println("No friends for user "+myUserId)
      }
    }
    case CreatePage(myUserId : Int, pageData : PageData)=>{
      createPageUtil(myUserId,pageData)
    }

    case DeletePage(userId : Int, pageId : Int)=>{
      deletePageUtil(userId,pageId)
    }

    case ReadPageData(userId : Int, pageId:Int) => {
      readPageDataUtil(userId,pageId)
    }
    case ViewFriendsAlbumsPhotosWallPosts(myUserId: Int) =>
    {
      var friendId  = getOneOfFriendId(myUserId)
      println("Will view wall friendId  "+friendId + " myUserId "+myUserId)
      if(friendId > 0) {
        viewFriendsWallPostsUtil(myUserId,friendId)     
        viewProfilePictureUtil(friendId)
        viewAlbum(friendId)
      }
    }
    case PostOnFriendsWall(myUserId: Int, friendId :Int) =>
    {
      if(friendId == -1)
      {
        var friendId  = getOneOfFriendId(myUserId)
        if(friendId > 0){
          postOnFriendsWallUtil(friendId)
        }
      }
      else{
        postOnFriendsWallUtil(friendId)
      }

    }
    case AddNewPageToUser(myUserId : Int)=> {

      val bools = List(true,false)
      val categoryList = List("Public", "Private")
      val userTypeList = List("Company", "LocalBusiness", "Artist/Band", "Cause/Community")
      val aboutTypeList = List("We are a corporate", "Grocery", "Eminem", "Cancer Awareness Campaign")
      val locationList = List("Chicago", "NewYork", "New Delhi", "Sydney", "Thailand", "Buenos Aires", "Istanbul")
      var randid = r.nextInt(totalUsers - 1)
      while(randid==myUserId){
        randid = r.nextInt(totalUsers - 1)
      }

      val userType = userTypeList(r.nextInt(userTypeList.length))
      val pageData = PageData(randid, userTypeList(r.nextInt(userTypeList.length)),
        aboutTypeList(userTypeList.indexWhere(_.contentEquals(userType))),
        bools(r.nextInt(bools.length)), bools(r.nextInt(bools.length)), categoryList(r.nextInt(categoryList.length))
        ,locationList(r.nextInt(locationList.length)), userPhoneGenerator(), feed = List(Feed(encrypAES("myfeed", selfSymFeed))))


      self ! CreatePage(myUserId,pageData)

    }

    case UpdatePage(userId : Int,pageId: Int,pageData:PageData)=> {
      updatePageUtil(userId,pageId,pageData)
    }

    case GeneratePageDataandUpdatePage(myUserId : Int)=> {

      val bools = List(true,false)
      val categoryList = List("Public", "Private")
      val userTypeList = List("Company", "LocalBusiness", "Artist/Band", "Cause/Community")
      val aboutTypeList = List("We are a corporate", "Grocery", "Eminem", "Cancer Awareness Campaign")
      val locationList = List("Chicago", "NewYork", "New Delhi", "Sydney", "Thailand", "Buenos Aires", "Istanbul")

      var pageid = getUserPageId(myUserId)  //Gives a random page from list of pages of user.

      if(pageid != -1){
        val userType = userTypeList(r.nextInt(userTypeList.length))

        val pageData = PageData(pageid, userTypeList(r.nextInt(userTypeList.length)),
          aboutTypeList(userTypeList.indexWhere(_.contentEquals(userType))),
          bools(r.nextInt(bools.length)), bools(r.nextInt(bools.length)), categoryList(r.nextInt(categoryList.length))
          ,locationList(r.nextInt(locationList.length)), userPhoneGenerator(), feed = List(Feed(encrypt(selfPub, "myfeed", selfSymFeed).value)))

        self ! UpdatePage(myUserId,pageid,pageData)
      }
    }

    case UpdateUserProfile(userId : Int)=>{
      updateUserProfile(userId)
    }

    case ViewMyProfilePicture(userId : Int) =>{
      println("Will view my image userID "+userId)
      viewProfilePictureUtil(userId)
    }

    case ViewAlbum(userId : Int) => {
      viewAlbum(userId)
    }
    case CreateAlbum(userId : Int) => {
      createAlbumUtil(userId)
    }

    case DeleteProfilePicture(userId : Int) =>{
      deleteProfilePictureUtil(userId)
    }

    case DeleteAlbum(userId : Int ) =>{
      deleteAlbumUtil(userId)
    }
  }

  def encrypt(pub: PublicKey, value: String, symKey : SecretKey): Info = {
    val rsa = Cipher.getInstance("RSA")

    rsa.init(Cipher.ENCRYPT_MODE, pub)
    var esym = rsa.doFinal(symKey.getEncoded)

    val AesCipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    val secretKey = new SecretKeySpec(symKey.getEncoded, "AES")
    val iv = Array[Byte](1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3)
    AesCipher.init(Cipher.ENCRYPT_MODE, secretKey, new IvParameterSpec(iv));
//    AesCipher.init(Cipher.ENCRYPT_MODE, symKey);

    val evalue =  AesCipher.doFinal(value.getBytes)
    Info( new String(Base64.encodeBase64(esym)) , new String(Base64.encodeBase64(evalue)))
  }

  def decryptSelf(value: String, symKey: SecretKey): String = {
    try {
      val evalue = Base64.decodeBase64(value.getBytes)
      val AesCipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
      val skeySpec = new SecretKeySpec(symKey.getEncoded, "AES")
      val iv = Array[Byte](1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3)
      AesCipher.init(Cipher.DECRYPT_MODE, skeySpec, new IvParameterSpec(iv))
    //  AesCipher.init(Cipher.DECRYPT_MODE, skeySpec)

      val bvalue = AesCipher.doFinal(evalue)
      new String(bvalue)
    }
    catch{
      case _: Throwable => println("Ignoring NonFatal Exception")
      return ""
    }
  }

  def decrypt(value: String, otherSym: String ): String = {
    try{
      val esym = Base64.decodeBase64(otherSym.getBytes)
      val evalue = Base64.decodeBase64(value.getBytes)
      val rsa = Cipher.getInstance("RSA")
      rsa.init(Cipher.DECRYPT_MODE, keyPair.privKey)
      
      val sym = rsa.doFinal(esym)
   
      val AesCipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
          
      val iv = Array[Byte](1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3)
      val skeySpec = new SecretKeySpec(sym, "AES")
      AesCipher.init(Cipher.DECRYPT_MODE, skeySpec, new IvParameterSpec(iv))
  //    AesCipher.init(Cipher.DECRYPT_MODE, skeySpec)

      val bvalue = AesCipher.doFinal(evalue)
      new String(bvalue)
    }catch{
      case _: Throwable => println("Ignoring NonFatal Exception")
      return ""
    }
  }


  def getServerPublicKey(): Boolean = {

    var result = pipeline(Get("http://localhost:8080/user/getServerPublicKey"))
    result.foreach { response =>
      Auth.serverKey = toPub(response.entity.asString)
      true
    }
    true
  }

  def md5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes)
  }

  def getUserPublicKey(userid: Int): PublicKey = {
    val timeout2 = 3000.seconds
    var result = pipeline(Get("http://localhost:8080/user/getUserPublicKey?userId=" + userid))
    val publicKey = Await.result(result, timeout2)
    var checkKey = new String(md5(publicKey.entity.asString))

    if (!checkPublicKeys.contains(userid)){
      checkPublicKeys += (userid -> checkKey) 
      return toPub(publicKey.entity.asString)
    }
    else if (checkPublicKeys(userid) == checkKey) {
      return toPub(publicKey.entity.asString)
    }
    else{
      println(" ALARM !!! MITM ATTACK ")
      return null
    }
  }

  def informServerWithPublicKeyUtil(myUserId: Int,key : PublicKey): Unit = {

    val pipeline2 = (
      addHeader("public-key", toS(key))
        ~> sendReceive
      )
    //println("myUserId "+myUserId+" pubKey "+key)
    var result = pipeline2(Post("http://localhost:8080/user/sendPubKey?userId=" + myUserId))
    result.foreach { response =>
      //println("informServerWithPublicKeyUtil "+response.entity.asString)
    }
  }

  def authUtil(myUserId: Int): Unit = {
    var ret = false
    val timeout2 = 500.seconds
    var URL = "http://localhost:8080/user/authorize?userId=" + myUserId
    val pipeline: HttpRequest => Future[HttpResponse] = sendReceive ~> unmarshal[HttpResponse]
    val f: Future[HttpResponse] = pipeline(Post(URL))

    f onComplete {
      case Success(response)=>
        Auth.token(myUserId) = response.entity.asString
        if (Auth.token(myUserId) != null){
          var digitalSignature: Array[Byte] = digitallySignData((Auth.token(myUserId)).getBytes, keyPair.privKey)

          var result = pipeline(Post("http://localhost:8080/user/verifyMe?userId=" + myUserId, EncodedSignature(digitalSignature)))
          result.foreach { response =>       
            if ((response.entity.asString).equals("Authorised")) {
              println("User "+myUserId+" is authenticated")
            }else{
              println("User " + myUserId + " is unauthenticated")
            }
          }
        }
      case Failure(_) => println("No response")
    }

  }

  def digitallySignData(data : Array[Byte], key: PrivateKey): Array[Byte]= {
    var signer : Signature  = Signature.getInstance("SHA256withRSA")
    signer.initSign(key)
    signer.update(data)
    return (signer.sign())
  }

  def digitalVerifySig(data : Array[Byte], key : PublicKey ,sig : Array[Byte]):Boolean= {
    var signer : Signature  = Signature.getInstance("SHA256withRSA")
    signer.initVerify(key)
    signer.update(data)
    return (signer.verify(sig))

  }

  def toS(priv:PrivateKey):String = {
    new String(Base64.encodeBase64(priv.getEncoded))
  }

  def toStr(str:String):String = {
    new String(str)
  }
  def toS(pub:PublicKey):String = {
    new String(Base64.encodeBase64(pub.getEncoded))
  }

  def toPriv(s:String):PrivateKey = {
    KeyFactory.getInstance("RSA").generatePrivate(new PKCS8EncodedKeySpec(Base64.decodeBase64(s)))
  }

  def toPub(s:String):PublicKey  = {
    KeyFactory.getInstance("RSA").generatePublic(new X509EncodedKeySpec(Base64.decodeBase64(s)))
  }

 
  def encrypAES(data : String , key : SecretKey) : String = {
    val AesCipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    val secretKey = new SecretKeySpec(key.getEncoded, "AES")
    val iv = Array[Byte](1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3)
    AesCipher.init(Cipher.ENCRYPT_MODE, secretKey, new IvParameterSpec(iv));
//    AesCipher.init(Cipher.ENCRYPT_MODE, key);

   /*  AesCipher.init(Cipher.ENCRYPT_MODE, key,
        new IvParameterSpec(new byte[16]));*/
    val evalue =  AesCipher.doFinal(data.getBytes)
    return new String(Base64.encodeBase64(evalue))
  }

  def encrypRSA(data : String , key : PublicKey) : String = {
    val AesCipher = Cipher.getInstance("RSA")
    AesCipher.init(Cipher.ENCRYPT_MODE, key)
    val evalue =  AesCipher.doFinal(data.getBytes)
    return new String(Base64.encodeBase64(evalue))
  }

  def decryptRSA(data : String , key : PrivateKey) : String = {
    val AesCipher = Cipher.getInstance("RSA/ECB/PKCS1Padding")
    AesCipher.init(Cipher.DECRYPT_MODE, key)
    val evalue =  AesCipher.doFinal(data.getBytes)
    return new String(Base64.encodeBase64(evalue))
  }

  def decrypAES(data : String , key : SecretKey) : String = {
    val AesCipher = Cipher.getInstance("AES")
    AesCipher.init(Cipher.DECRYPT_MODE, key)
    val evalue =  AesCipher.doFinal(data.getBytes)
    return new String(Base64.encodeBase64(evalue))
  }

  def viewAlbum(userId : Int): Unit = {

    println("View album for "+userId)
    var result = pipeline(Get("http://localhost:8080/user/" + userId + "/getAlbum"))
    result.foreach { response =>
      var imageString = response.entity.asString

      if (!imageString.equals("")){
        if (userId == actorId){
          imageString = decryptSelf(imageString, selfSymAlbum)
          if (!imageString.equals("")){
            var data = Base64.decodeBase64(imageString)
            var stream = new FileOutputStream("temp" + userId + ".png")
            stream.write(data)
            println(s"Image "+"temp"+userId+".png"+" stored at :"+System.getProperty("user.dir"))
          }
        }else{
          var peer = context.system.actorFor("akka://Client/user/" + userId)
          val fresponse: Future[String] = (peer ? getEncryptedSymKey(selfPub, selfSymAlbum)).mapTo[String]

          fresponse.onComplete { e7 =>
            var otherSym = e7.get
            println(" encrypted image for userid " + userId)
            imageString = decrypt(imageString, otherSym)

            if (!imageString.equals("")){
              var data = Base64.decodeBase64(imageString)
              var stream = new FileOutputStream("temp" + userId + ".png")
              stream.write(data)
              println(s"Image "+"temp"+userId+".png"+" stored at :"+System.getProperty("user.dir"))
            }
          }
        }
      }
    }
  }

  def deleteAlbumUtil(userId : Int ) ={
    var result = pipeline(Delete("http://localhost:8080/user/" + userId + "/deleteAlbum"))
    result.foreach { response =>
      println(s"Delete Album with status ${response.status}")
    }

  }

  def createAlbumUtil(userId: Int) ={

    var mUrl = List (System.getProperty("user.dir")+"\\img13.png", System.getProperty("user.dir")+"\\img14.png", System.getProperty("user.dir")+"\\img15.png")
    var inUrl = r.nextInt(mUrl.length)

    var httpData = Files.readAllBytes(Paths.get(mUrl(inUrl)))
    val bytes64 = Base64.encodeBase64(httpData)
    val streamInString = new String(bytes64)

    var encryptedImage = encrypt(selfPub, streamInString, selfSymAlbum).value
  //  println("Encrypted image : " + encryptedImage)

    var cIndex = userId
    while (cIndex == userId ) {
      cIndex = r.nextInt(1000)
    }
    var photo = ImageUploaded(imageID = cIndex, blob = encryptedImage)

    var result = pipeline(Put("http://localhost:8080/user/" + userId + "/addPhotoToAlbum", photo))
    result.foreach { response =>
     // println(s"Create Album success status ${response.status}")
    }
  }

  def deleteProfilePictureUtil(userId: Int) = {

    println("Deleting Profile picture for "+userId)
    val result = pipeline(Delete("http://localhost:8080/user/" + userId + "/deleteImage/" + userId))
    result.foreach { response =>
      println(s"Request completed with status ${response.status} ")
    }
  }

  def viewProfilePictureUtil(myUserId : Int) ={
    val timeout2 = 3000.seconds
    val pipeline: HttpRequest => Future[HttpResponse] = sendReceive ~> unmarshal[HttpResponse]
    var result = pipeline(Get("http://localhost:8080/user/" + myUserId + "/getAlbum" ))
    var imageString = Await.result(result, timeout2).entity.asString
    if (!imageString.equals("")){
      if (myUserId ==actorId){
      
        imageString = decryptSelf(imageString, selfSymProfile)
        if (!imageString.equals("")){
          var data = Base64.decodeBase64(imageString)
          var stream = new FileOutputStream("temp" + myUserId + ".png")
          stream.write(data)
          println(s"Image "+"temp"+myUserId+".png"+" stored at :"+System.getProperty("user.dir"))
        }
      }else{
        var peer = context.system.actorFor("akka://Client/user/" + myUserId)
        val fresponse: Future[String] = (peer ? getEncryptedSymKey(selfPub, selfSymProfile)).mapTo[String]

        fresponse.onComplete { e7 =>
          var otherSym = e7.get
          println("viewing profile image of user : " + myUserId)
          imageString = decrypt(imageString, otherSym)
          if (!imageString.equals("")){
            var data = Base64.decodeBase64(imageString)
            var stream = new FileOutputStream("temp" + myUserId + ".png")
            stream.write(data)
            println(s"Image "+"temp"+myUserId+".png"+" stored at :"+System.getProperty("user.dir"))
          }
        }
      }
    }
  }

  def createProfileImage(u :User, imageId : Int): ImageUploaded ={

    var mUrl = List (System.getProperty("user.dir")+"\\img13.png", System.getProperty("user.dir")+"\\img14.png", System.getProperty("user.dir")+"\\img15.png")

    var inUrl = r.nextInt(mUrl.length)
    var httpData = Files.readAllBytes(Paths.get(mUrl(inUrl)))
    val bytes64 = Base64.encodeBase64(httpData)
    val streamInString = new String(bytes64)
    var encryptedImage = encrypt(selfPub, streamInString, selfSymProfile).value
   // println("sending : " + encryptedImage)
    var photo = ImageUploaded(imageID = imageId, blob = encryptedImage)
    return photo
  }

  def userSignUp(u : User): Unit = {

    val photo = createProfileImage(u, u.userID)
    var imList = List(photo)

    var userImg : UserImage = UserImage(user= u, imageList = imList)
    val result =  pipeline(Post("http://localhost:8080/user/add", userImg))
    result.foreach { response =>
   //   println(s"User "+u.userID+s" signup completed with status ${response.status}")
    }
  }

  def addFriendUtil(myUserId: Int , friendUserId: Int):Unit ={

    var result = pipeline(Put("http://localhost:8080/user/addFriend?myId=" + myUserId + "&frndUserId=" + friendUserId))
    result.foreach { response =>
   //   println(s"Add Friend completed with status ${response.status} ")
    }
  }

  def deleteFriendUtil(myUserId: Int , friendUserId: Int):Unit ={
    var result = pipeline(Put("http://localhost:8080/user/deleteFriend?myId=" + myUserId + "&frndUserId=" + friendUserId))
    result.foreach { response =>
      println(s"Delete Friend Success")
    }
  }

  def createUserProfile(userId:Int, userName : String) : PageData = {

    val bools = List(true,false)
    val categoryList = List("Public", "Private")
    val userTypeList = List("Individual", "Company", "LocalBusiness", "Artist/Band", "Cause/Community")
    val aboutTypeList = List("I am individual", "We are a corporate", "Grocery", "Eminem", "Cancer Awareness Campaign")
    val locationList = List("Chicago", "NewYork", "New Delhi", "Sydney", "Thailand", "Buenos Aires", "Istanbul")

    var pageId = userId
    val userType = userTypeList(r.nextInt(userTypeList.length))

    val pageData = PageData(pageId, userTypeList(r.nextInt(userTypeList.length)),
      aboutTypeList(userTypeList.indexWhere(_.contentEquals(userType))),
      bools(r.nextInt(bools.length)), bools(r.nextInt(bools.length)), categoryList(r.nextInt(categoryList.length))
      ,locationList(r.nextInt(locationList.length)), userPhoneGenerator(), feed = List(Feed(encrypt(selfPub, "Welcome "+userName+" to FaceBook!!", selfSymFeed).value)))

    pageData
  }

  def updateUserProfile(userId : Int) : Unit ={

    val bools = List(true,false)
    val categoryList = List("Public", "Private")
    val userTypeList = List("Individual", "Company", "LocalBusiness", "Artist/Band", "Cause/Community")
    val aboutTypeList = List("I am individual", "We are a corporate", "Grocery", "Eminem", "Cancer Awareness Campaign")
    val locationList = List("Chicago", "NewYork", "New Delhi", "Sydney", "Thailand", "Buenos Aires", "Istanbul")

    var pageId = userId
    val userType = userTypeList(r.nextInt(userTypeList.length))

    val pageData = PageData(pageId, userTypeList(r.nextInt(userTypeList.length)),
      aboutTypeList(userTypeList.indexWhere(_.contentEquals(userType))),
      bools(r.nextInt(bools.length)), bools(r.nextInt(bools.length)), categoryList(r.nextInt(categoryList.length))
      ,locationList(r.nextInt(locationList.length)), userPhoneGenerator(), feed = List(Feed(encrypt(selfPub, "Its a sunny day today!!", selfSymFeed).value)))

    updateUserProfileUtil(userId,pageData)

  }
  def updateUserProfileUtil(userId: Int , pageData: PageData):Unit ={

    /* Hack added as was unable to pass PageData directly hence passing page data as User induced value.
       Error:(115, 30) could not find implicit value for evidence parameter of type spray.httpx.marshalling.Marshaller[PageData]
       var result = pipeline(Put("http://localhost:8080/user/createPage",pageData))*/

    var newUser =  User(userId, "dummy", friendList = List[Int](), pageList = List(pageData))
    var result = pipeline(Put("http://localhost:8080/user/updateProfile?userId="+userId+"&pageId="+userId,newUser))
    result.foreach { response =>
     // println(s"profile page updated with status ${response.status} and content ${response.entity.asString}")
    }
  }

  def createPageUtil(userId: Int , pageData: PageData):Unit ={

    /* Hack added as was unable to pass PageData directly hence passing page data as User induced value.
       Error:(115, 30) could not find implicit value for evidence parameter of type spray.httpx.marshalling.Marshaller[PageData]
       var result = pipeline(Put("http://localhost:8080/user/createPage",pageData))*/

    var newUser =  User(userId, "dummy", friendList = List[Int](), pageList = List(pageData))
    println("creating page for :"+userId+" Page Data "+pageData)
    var result = pipeline(Put("http://localhost:8080/user/createPage?userId="+userId,newUser))
    result.foreach { response =>
      println(s"Page created with status ${response.status} ")
    }
  }

  def deletePageUtil(userId: Int , pageId :Int):Unit ={

    println("deleting page for :"+userId+"pageId"+pageId)
    var result = pipeline(Put("http://localhost:8080/user/deletePage?userId="+userId+"&pageId="+pageId))
    result.foreach { response =>
      println(s"Delete Page with status ${response.status}")
    }
  }

  def updatePageUtil(userId: Int , pageId: Int, pageData: PageData):Unit ={

    /* Hack added as was unable to pass PageData directly hence passing page data as User induced value.
       Error:(115, 30) could not find implicit value for evidence parameter of type spray.httpx.marshalling.Marshaller[PageData]
       var result = pipeline(Put("http://localhost:8080/user/createPage",pageData))*/

    var newUser =  User(userId, "dummy", friendList = List[Int](), pageList = List(pageData))
    var result = pipeline(Put("http://localhost:8080/user/updatePage?userId="+userId+"&pageId"+pageId,newUser))
    result.foreach { response =>
      println(s"Update page with status ${response.status}")
    }
  }

  def readPageDataUtil(userId: Int,pageId: Int): Unit = {

    var result = pipeline(Get("http://localhost:8080/user/readUserPage?userId="+userId+"&pageId="+pageId))
    result.foreach { response =>
      println(s"Read page with status ${response.status} ")
    }
  }

  def getUserPageId(userId:Int): Int = {
    val timeout2 = 3000.seconds
    var URL = "http://localhost:8080/user/getAnyPageId?userId="+userId
    val pipeline: HttpRequest => Future[HttpResponse] = sendReceive ~> unmarshal[HttpResponse]

    val f: Future[HttpResponse] = pipeline(Get(URL))
    val page = Await.result(f, timeout2)
    var sr = page.entity.asString

    try {
      val pageId = sr.toInt
      return pageId
    }
    catch{
      case ex : NumberFormatException => {
        println(ex.getMessage)
        return -1
      }
    }
  }

  def postOnFriendsWallUtil(friendId : Int): Unit = {


    val samplefeedList = List("Hi !! How have you been ?", "Happy Birthday !!!", "Happy Halloween !!!")
    var otherPub = getUserPublicKey(friendId)
    if (otherPub != null){
      var feed = Feed(encrypt(otherPub, samplefeedList(r.nextInt(samplefeedList.length)), selfSymFeed).value)

      var result = pipeline(Post("http://localhost:8080/user/postOnWall?friendId="+friendId, feed))
      result.foreach { response =>
        println(s"Posting on friends wall status ${response.status} ")
      }
    }
  }

  def getOneOfFriendId(userId:Int): Int = {
    var URL = "http://localhost:8080/user/getMyFriendId?userId="+userId
    val timeout2 = 3000.seconds
    val pipeline: HttpRequest => Future[HttpResponse] = sendReceive ~> unmarshal[HttpResponse]

    if(userId>0) {
      val f: Future[HttpResponse] = pipeline(Get(URL))
      val res = Await.result(f, timeout2)
      var str = res.entity.asString

      try {
        val friendId = str.toInt
        return friendId
      }
      catch {
        case ex: NumberFormatException => {
          println(ex.getMessage)
          return -1
        }
      }
    }
    else {
      return -1
    }
  }

  def viewFriendsWallPostsUtil(myUserId: Int,friendUserId: Int): Unit = {

    var result = pipeline(Get("http://localhost:8080/user/viewFriendWallPosts?myUserId="+myUserId+"&friendUserId="+friendUserId))
    result.foreach { response =>
      var peer = context.system.actorFor("akka://Client/user/" + + friendUserId)
      val fresponse: Future[String] = (peer ? getEncryptedSymKey(selfPub, selfSymFeed)).mapTo[String]

      fresponse.onComplete { e7 =>
        var otherSym = e7.get

        var sr = response.entity.asString
        if (!sr.contains("Could not fetch proper pListIndex") || !sr.contains("Your UserID")) {
          //println("Friend wall post : " + response.entity.asString)
          sr = sr.substring(1, sr.length() - 1)

          var arr = sr.split(',').map(_.trim)
          for (i <- 0 to arr.length - 1) {
            if (arr(i) != "") {
              var feedString = decrypt(arr(i), otherSym)
              println(s"Friend : " + friendUserId + ": wall post  content : " + feedString)
            }
          }
        }
      }
    }
  }

  def userNameGenerator( length: Int ) : String = {
    val  AB = "abcdefghijklmnopqrstuvwxyz"
    val r = new scala.util.Random
    val sb = new StringBuilder
    for(i <- 1 to length)
      sb.append(AB.charAt(r.nextInt(AB.length())))
    sb.toString()
  }

  def userPhoneGenerator(length: Int = 10 ) : String = {
    val  num = "123456789"
    val r = new scala.util.Random
    val sb = new StringBuilder
    for(i <- 1 to length)
      sb.append(num.charAt(r.nextInt(num.length())))
    sb.toString()
  }

  def truncateBefore(s: String, p: String) = {
    s.substring(s.indexOf(p) + p.length, s.length)
  }

  def truncateAfter(s: String, p: String) = {
    s.substring(0, s.indexOf(p) + p.length-1)
  }


}
class ClientUnitOperation(numofUsers : Int) extends Actor {
  var numOfFacebookUsers: Int = numofUsers
  var actors: Array[ActorRef] = null
  var pipeline = sendReceive
  var aesKeyList : List[SecretKey] = List[SecretKey]()
  var keyPair : Keys = null
  val keyGen: KeyGenerator = KeyGenerator.getInstance("AES")
  keyGen.init(128)
  val keyGenPair = KeyPairGenerator.getInstance("RSA")
  keyGenPair.initialize(1024)
  implicit val timeout = Timeout(3000 seconds)

  implicit val system = ActorSystem("Client")
  def receive = {
    case InitClientSystem() => {

      actors = new Array[ActorRef](numOfFacebookUsers)

      /*Create FaceBook Users*/
      for (i <- 0 to numOfFacebookUsers - 1) {
        aesKeyList  = generateAesKey()
        keyPair  = getKeys()
        keyDB.publicKeyMap+= ((i+1) -> keyPair.pubKey)
        actors(i) = system.actorOf(Props(new FaceBookUser(aesKeyList, keyPair, (i+1), numOfFacebookUsers)), (i+1).toString)
      }

      //Register each user on FaceBook domain
      for (i <- 0 to numOfFacebookUsers-1) {
        actors(i) ! RegisterNewUser(i+1)
      }

      //actors(0)!GetServerPublicKey()
      for (i <- 0 to numOfFacebookUsers - 1) {
        actors(i)!InformServerWithPublicKey(i+1)
      }

      for (i <- 0 to numOfFacebookUsers-1) {
        actors(i)!InitiateUserAuthorization(i+1)
      }

      //Update friend list for each user
      context.system.scheduler.schedule(30 seconds, 5000 seconds) {
        actors(0) ! UpdateFriendListOnBoot()
      }


    }

    case SimulateUserInteraction() => {


      /*Simulation Distribution*/
      /*A sets of users Update their Profiles */
      for (i <- 0 to numOfFacebookUsers*15/100) {
        actors(i)!UpdateUserProfile(i+1)
      }

      /*Randomly update Pages associated with a user*/
      for (i <- 0 to numOfFacebookUsers*27/100) {
        actors(i)!GeneratePageDataandUpdatePage(i+1)
      }

/*      /*View Profile Picture  */
      for (i <- 0 to  numOfFacebookUsers*25/100) {
        actors(i)!ViewMyProfilePicture(i+1)
      }
*/
      /*Create Album  */
      for (i <- 0 to numOfFacebookUsers*25/100) {
        actors(i)!CreateAlbum(i+1)
      }
  
      /*Users post on their friends wall*/
      for (i <- 0 to numOfFacebookUsers*22/100){
        actors(i)!PostOnFriendsWall(i+1)
      }
  

        /*Users see their friends wall*/
        for (i <- 0 to numOfFacebookUsers*44/100) {
          actors(i)!ViewFriendsAlbumsPhotosWallPosts(i+1)
        }
/*
      /*Deleting friends from x` list*/
      for (i <- 0 to numOfFacebookUsers*1/100) {
        actors(i)!DeleteFriendsOps(i+1)
      }

      /*View Album  */
      for (i <- 0 to (numOfFacebookUsers)*15/100) {
        actors(i)!ViewAlbum(i+1)
      }
*/
      /*Delete Profile Picture */
      for (i <- 0 to (numOfFacebookUsers)*15/100) {
        actors(i)!DeleteProfilePicture(i+1)
      }

      /*Delete Album */
      for (i <- 0 to (numOfFacebookUsers)*15/100) {
        actors(i)!DeleteAlbum(i+1)

      }

    }

    case CreatePagesForUsers() => {

      val userTypeList = List("Company","LocalBusiness","Artist/Band","Cause/Community")
      val aboutTypeList = List("We are a corporate" ,"Grocery","Eminem","Cancer Awareness Campaign")
      val categoryList = List("Public","Private")
      val locationList = List ("Chicago","NewYork","New Delhi","Sydney","Thailand","Buenos Aires","Istanbul")
      var pageidList = List[Int]()

      var r = new Random
      var randid = r.nextInt(numOfFacebookUsers-1)
      for (i <- 0 to numOfFacebookUsers - 1) {
        while (pageidList.contains(randid)){
          randid=r.nextInt(numOfFacebookUsers-1)
        }
        pageidList = randid :: pageidList

        val pageid  = randid
        val userType = userTypeList(r.nextInt(userTypeList.length))
        val about = aboutTypeList(userTypeList.indexWhere(_.contentEquals(userType)))
        val cancheckin = true
        val canpost = true
        val catgry = categoryList(r.nextInt(categoryList.length))
        val loc = locationList(r.nextInt(locationList.length))
        val phone = userPhoneGenerator()
        var fd = List(Feed("myfeed"))
        val pageData = PageData(pageid,userType,about,cancheckin,canpost,catgry,loc,phone,feed=fd)

        //actors(i)!CreatePage(i+1,pageData)
      }

      Thread.sleep(5000L)
    }

  }

  def generateAesKey(): List[SecretKey] ={
    var aesKeyList = List(keyGen.generateKey, keyGen.generateKey, keyGen.generateKey)
    aesKeyList
  }

  def generateRSAKeyPair(): KeyPair ={
    val keyPairGen: KeyPairGenerator = KeyPairGenerator.getInstance("RSA")
    keyPairGen.initialize(1024)
    keyPairGen.genKeyPair
  }

  def getUsernameFromUserId(userId : Int): Unit = {
    val result =  pipeline(Get("http://localhost:8080/user/fetchName?userId="+userId))
    result.foreach { response =>
      val uname = response.entity.asString
     // println(s"Request completed with status ${response.status} and content username: "+uname)
    }
  }

  def userPhoneGenerator(length: Int = 10 ) : String = {
    val  num = "123456789"
    val r = new scala.util.Random
    val sb = new StringBuilder
    for(i <- 1 to length)
      sb.append(num.charAt(r.nextInt(num.length())))
    sb.toString()
  }

  def getKeys(): Keys = {
    val keys = keyGenPair.genKeyPair()
    Keys(keys.getPublic, keys.getPrivate)
  }
}

object FaceBookClientSimulator extends App{

  var numOfUsers = 2000
  implicit val timeout = Timeout(3000 seconds)

  /*Read number of facebook users to be registered for simulation*/
  if (args.length != 1) {
    println("Default "+numOfUsers+" FaceBook users is being created !!!")
  }
  else{
    numOfUsers = args(0).toInt
    println(numOfUsers+" FaceBook users is being created !!!")
  }
  /*Create Simulator System and Simulator Actor Boss*/
  val System = ActorSystem("ClientSimulatorSystem")
  val clientSimulator = System.actorOf(Props(new ClientUnitOperation(numOfUsers)),name="ClientSimulatorBoss")

  /*Init FBUser or Client System*/
  println("System Bootup .....")
  clientSimulator!InitClientSystem()

  println("Initiating User Simulation now ...")
  println("Few moments to set up simulation  ...")
  //Let Boot Complete then intiate User Simulation
  System.scheduler.schedule(40 seconds, 5000 seconds) {
    clientSimulator!SimulateUserInteraction()
  }
}
