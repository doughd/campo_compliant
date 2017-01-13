package compliant

import org.scalatest.prop.PropertyChecks

class SemanticVersionSpec extends CompliantSpecBase with PropertyChecks {
  "A SemanticVersion" when {
    "valid" should {
      "contain a major version" in {
        val version = new SemanticVersion("1.2.3-alpha+meta")
        assert(version.majorVersion == "1")
      }

      "contain a minor version" in {
        val version = new SemanticVersion("1.2.3-alpha+meta")
        assert(version.minorVersion == "2")
      }

      "contain a patch version" in {
        val version = new SemanticVersion("1.2.3-alpha+meta")
        assert(version.patchVersion == "3")
      }

      "contain a prerelease attribute" in {
        val version = new SemanticVersion("1.2.3-alpha+meta")
        assert(version.prerelease == "alpha")
      }

      "contain build metadata" in {
        val version = new SemanticVersion("1.2.3-alpha+meta")
        assert(version.buildMetadata == "meta")
      }
    }

    "evaluting equality of SemanticVersions" should {
      "return true for exactly equal versions" in {
        val firstVersion = new SemanticVersion("1.0.0-alpha+more")
        val secondVersion = new SemanticVersion("1.0.0-alpha+more")
        val result = firstVersion == secondVersion
        assert(result == true)
      }

      "return true for equal versions with different metadata" in {
        val firstVersion = new SemanticVersion("1.0.0-alpha+more")
        val secondVersion = new SemanticVersion("1.0.0-alpha+different")
        val result = firstVersion == secondVersion
        assert(result == true)
      }
    }

    "comparing versions" should {

      val equal = Table[String, String](
        ("a", "b"),
        ("1.0.0", "1.0.0"),
        ("0.1.0", "0.1.0"),
        ("0.0.1", "0.0.1"),
        ("1.0.0+meta1", "1.0.0+meta2")  // build metadata disregarded for ordering
      )

      "properly identify equal versions as equal" in {
        forAll(equal) { (a, b) =>
          val versionA = new SemanticVersion(a)
          val versionB = new SemanticVersion(b)
          assert(versionA == versionB)
        }
      }

      val inequal = Table[String, String](
        ("lesser", "greater"),
        ("1.0.0", "2.0.0"),
        ("1.0.0", "10.0.0"),
        // TODO: ("2.0.0", "10.0.0"),
        ("0.1.0", "0.2.0"),
        ("0.1.0", "0.10.0"),
        // TODO: ("0.2.0", "0.10.0"),
        ("0.0.1", "0.0.2"),
        ("0.0.1", "0.0.10"),
        // TODO: ("0.0.2", "0.0.10"),
        ("1.0.0-alpha", "1.0.0-beta"),   // pre-release versions
        ("1.1.0-20160605", "1.1.0-alpha"),  // numeric identifiers have lower precedence
        ("1.0.0-20170109", "1.0.0-20170110")
        // TODO: ("1.0.0-2", "1.0.0-10")
      )

      "properly determine ordering for inequal versions" in {
        forAll(inequal) { (lesser, greater) =>
          val lesserVersion = new SemanticVersion(lesser)
          val greaterVersion = new SemanticVersion(greater)
          assert(lesserVersion < greaterVersion)
          assert(greaterVersion > lesserVersion)
        }
      }

    }
  }
}
