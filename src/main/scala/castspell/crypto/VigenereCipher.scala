package castspell.crypto

import scala.collection.mutable.StringBuilder

/**
 * Implements Ceasar cipher (shift cipher)
 *
 * TODO https://class.coursera.org/cryptography-004/lecture/111
 */
object VigenereCipher {
  /**
   * Encrypt message by shifting code of each character by each key character
   *
   * @param key encryption key
   * @param message message to encrypt
   * @return encrypted message
   */
  def encrypt(key: String, message: String): String = {

    def encryptIter(key: String, message: String, pos: Int, encrypted:StringBuilder):StringBuilder =
      if(pos == message.length)
        encrypted
      else
        encryptIter(key,
          message,
          pos+1,
          encrypted.append( ShiftCipher.encrypt(
              keyCharAt(key, pos),
              message.charAt(pos) ) ) )

    encryptIter(key, message, 0, new StringBuilder())
      .toString
  }

  def keyCharAt(key: String, pos:Int):Int =
    ShiftCipher.normalize(charAtRoundedPosition(key, pos))

  def charAtRoundedPosition(key: String, pos:Int): Char = key.charAt(pos % key.length)

  /**
   * Decrypt message using key
   *
   * @param key encryption key
   * @param message encrypted message
   * @return decrypted message
   */
  def decrypt(key: String, message: String): String = {
    def decryptIter(key: String, message: String, pos: Int, decrypted: StringBuilder): StringBuilder =
      if (pos == message.length)
        decrypted
      else
        decryptIter(key,
          message,
          pos + 1,
          decrypted.append(ShiftCipher.decrypt(
            keyCharAt(key, pos),
            message.charAt(pos))))

    decryptIter(key, message, 0, new StringBuilder())
      .toString
  }

}
