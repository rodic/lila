package lila.user

import org.specs2.mutable.Specification

class TwoFactorRecoveryCodesTest extends Specification {

  "when code set is provided" >> {
    "constructor uses provided set" >> {
      val codes = Set("code1", "code2")
      val recoveryCodes = TwoFactorRecoveryCodes(codes)
      recoveryCodes.codes must_== codes
    }
  }
  "when code set is not provided" >> {
    val randomRecoveryCodes = TwoFactorRecoveryCodes()
    "constructor creates random set" >> {
      randomRecoveryCodes.codes must have size 16
    }
    "codes consist of ten random char strings" >> {
      randomRecoveryCodes.codes must containPattern("[a-zA-Z0-9]{10}")
    }
  }

  "when wrapped in an option" >> {
    "and is None" >> {
      val none: Option[TwoFactorRecoveryCodes] = None
      "mapCodes returns an empty sequence" >> {
        none.mapCodes(identity) must be empty
      }
      "isNotEmpty returns false" >> {
        none.isNotEmpty must beFalse
      }
    }

    "and is Some" >> {
      "and does not have codes" >> {
        val someEmpty = Some(TwoFactorRecoveryCodes(Set()))
        "mapCodes returns an empty sequence" >> {
          someEmpty.mapCodes(identity) must be empty
        }
        "isNotEmpty returns false" >> {
          someEmpty.isNotEmpty must beFalse
        }
      }
      "and have codes" >> {
        val codes = Set("code1", "code2")
        val some = Some(TwoFactorRecoveryCodes(codes))
        "mapCodes returns the correct sequence" >> {
          some.mapCodes(identity) must_== codes.toSeq
        }
        "isNotEmpty returns true" >> {
          some.isNotEmpty must beTrue
        }
      }
    }
  }
}
