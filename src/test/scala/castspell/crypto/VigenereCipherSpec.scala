package castspell.crypto

import org.scalatest.{MustMatchers, FunSpec}

class VigenereCipherSpec extends FunSpec with MustMatchers  {

  val cipher = VigenereCipher

  describe("charAtRoundedPosition") {
    it("return first char at 0") {
      cipher.charAtRoundedPosition("cipher", 0) mustBe 'c'
    }

    it("return second char at position n plus 1") {
      cipher.charAtRoundedPosition("ci", 2) mustBe 'c'
    }
  }

  describe("keyCharAt") {
    it("return first char at 0") {
      cipher.keyCharAt("cipher", 0) mustBe 2
    }

    it("return second char at position n plus 1") {
      cipher.keyCharAt("ci", 2) mustBe 2
    }
  }

  describe("encypt") {
    it("for zero shift leave massage not changed") {
      cipher.encrypt("a", "cryptoisfun") mustBe "cryptoisfun"
    }

    it("for 1 key shift replace each letter with next one") {
      cipher.encrypt("ab", "abcd") mustBe "acce"
    }

    it("for 1 key shift replace a with z") {
      cipher.encrypt("ba", "zaa") mustBe "aab"
    }
  }

  describe("decrypt") {
    it("for zero zero shift leave massage not changed") {
      cipher.decrypt("aa", "cryptoisfun") mustBe "cryptoisfun"
    }

    it("for 12 shit every second latter with next one") {
      cipher.decrypt("ab", "acce") mustBe "abcd"
    }

    it("for 21 decrypt aab into zaa") {
      cipher.decrypt("ba", "aab") mustBe "zaa"
    }
  }

}
