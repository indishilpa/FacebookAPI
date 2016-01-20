import java.net.{URLDecoder, URLEncoder}
import java.nio.Buffer
import java.nio.charset.StandardCharsets
import java.security._
import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}
import java.util.{Timer, Calendar}
import javax.crypto.spec.SecretKeySpec
import javax.crypto.{Cipher, KeyGenerator, SecretKey}
import org.apache.commons.codec.binary.Base64

object Crypto {

  case class Keys(pub: PublicKey, priv: PrivateKey)
  case class Info(symKey: String, value: String)

  def getKeys(): Keys = {
    val keyGen = KeyPairGenerator.getInstance("RSA")
    keyGen.initialize(2048)
    val keys = keyGen.genKeyPair()
    Keys(keys.getPublic, keys.getPrivate)
  }

  def encrypt(pub: PublicKey, value: String): Info = {
    val keyGen = KeyGenerator.getInstance("AES")
    // AES 256 not available without java crypto extension
    // following line prints 128 on OSX
    //println(Cipher.ge tMaxAllowedKeyLength("AES"))
    keyGen.init(128)
    val sym:SecretKey = keyGen.generateKey()

    val rsa = Cipher.getInstance("RSA")
    rsa.init(Cipher.ENCRYPT_MODE, pub)
    val esym = rsa.doFinal(sym.getEncoded)

    val AesCipher = Cipher.getInstance("AES")
    AesCipher.init(Cipher.ENCRYPT_MODE, sym)
    val evalue =  AesCipher.doFinal(value.getBytes)
    Info(new String(Base64.encodeBase64(esym)),new String(Base64.encodeBase64(evalue)))
  }

  def decrypt(priv: PrivateKey, info: Info): String = {
    val esym = Base64.decodeBase64(info.symKey.getBytes)
    val evalue = Base64.decodeBase64(info.value.getBytes)
    val rsa = Cipher.getInstance("RSA")
    rsa.init(Cipher.DECRYPT_MODE, priv)
    val sym = rsa.doFinal(esym)
    val AesCipher = Cipher.getInstance("AES")
    AesCipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(sym,"AES"))
    val bvalue = AesCipher.doFinal(evalue)
    new String(bvalue)
  }

  object newTask extends Timer{

    def printTime():Unit= {
      println(System.currentTimeMillis())
    }
  }

  def main(args:Array[String]) {
    var curr = System.currentTimeMillis()
    println(curr)

    while(true)
      {
         var timenow = System.currentTimeMillis()
         if(timenow - curr > 3000)
           {
              println(" 3 secs over ")
              newTask.printTime;
              return
           }
      }
  }


  def toS(priv:PrivateKey):String = {
    new String(Base64.encodeBase64(priv.getEncoded))
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

}