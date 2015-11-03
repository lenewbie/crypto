package castspell.crypto

import org.scalatest.{MustMatchers, FunSpec}

class ShiftCipherSpec extends FunSpec with MustMatchers {

  val cipher = ShiftCipher

  describe("normalize") {
    it("of a is 0") {
      cipher.normalize('a') mustBe 0
    }

    it("of z is 25") {
      cipher.normalize('z') mustBe 25
    }
  }

  describe("denormalize") {
    it("of a is 0") {
      cipher.denormalize(0) mustBe 'a'
    }

    it("of z is 25") {
      cipher.denormalize(25) mustBe 'z'
    }
  }

  describe("Key") {
    it("is valid when is greater or equal 0") {
      cipher.isValidKey(-1) mustBe false
      cipher.isValidKey(0) mustBe true
      cipher.isValidKey(1) mustBe true
    }

    it("must be smaller or equals 25") {
      cipher.isValidKey(25) mustBe true
      cipher.isValidKey(26) mustBe false
    }
  }

  describe("encypt") {
    it("for zero shift leave massage not changed") {
      cipher.encrypt(0, "cryptoisfun") mustBe "cryptoisfun"
    }

    it("for 1 key shift replace each letter with next one") {
      cipher.encrypt(1, "abcd") mustBe "bcde"
    }

    it("for 1 key shift replace a with z") {
      cipher.encrypt(1, "z") mustBe "a"
    }
  }

  describe("decrypt") {
    it("for zero shift leave massage not changed") {
      cipher.decrypt(0, "cryptoisfun") mustBe "cryptoisfun"
    }

    it("for 1 key shift replace each letter with next one") {
      cipher.decrypt(1, "bcde") mustBe "abcd"
    }

    it("for 1 key shift replace a with z") {
      cipher.decrypt(1, "a") mustBe "z"
    }
  }

}
