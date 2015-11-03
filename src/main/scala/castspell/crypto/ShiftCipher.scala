package castspell.crypto

/**
 * Implements Ceasar cipher (shift cipher)
 *
 * https://en.wikipedia.org/wiki/Caesar_cipher
 *
 * Currently implementation does not handle other characters than a-z
 * (no upper case, no white space, no period
 * or any other special characters)
 */
object ShiftCipher {

  private val FirstLetter:Int = 'a'

  private val NumberOfLetters: Int = 26

  /**
   * Encrypt message by shifting code of each character by key
   *
   * @param key encryption key
   * @param message message to encrypt
   * @return encrypted message
   */
  def encrypt(key: Int, message: String): String =
    message.map( c => encrypt(key, c) )

  def encrypt(key: Int, character: Char): Char =
    denormalize((normalize(character) + key) % NumberOfLetters)

  def normalize(character:Char):Integer =
    character - FirstLetter

  def denormalize(characterCode:Int):Char =
    (characterCode + FirstLetter).toChar

  /**
   * Decrypt message using key
   *
   * @param key encryption key
   * @param message encrypted message
   * @return decrypted message
   */
  def decrypt(key: Int, message: String): String =
    message.map( c => decrypt(key, c) )

  def decrypt(key: Int, character: Char): Char = {
    denormalize((normalize(character) + NumberOfLetters - key) % NumberOfLetters)
  }

  /**
   * Check if given key is valid
   *
   * @param key key to validate
   * @return true if key is valid otherwise false
   */
  def isValidKey(key:Int) =
    (key >= 0) && (key <= 25)
}
