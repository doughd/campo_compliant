package compliant

import org.scalatest.prop.PropertyChecks

class SemanticVersionSpec extends CompliantSpecBase with PropertyChecks {
  "A SemanticVersion" when {
    "invalid" should {

      val badVersions = Table[String](
        (""),
        ("1"),
        ("1.0"),
        ("1.1.1.1"),
        ("1.1.1a"),
        ("1.1.1-01"),
        ("1a.alpha"),
        ("v1.0.0"),
        (" 1.0.0"),
        ("1.0.0 alpha"),
        ("1.0.0+alpha_1.4"),
        ("1.0.0-beta_2"),
        ("1.3.5-00"),
        ("1.3.7-001"),
        ("1.2.3-.."),
        ("1.2.3-1..5"),
        ("1.2.3-1.5."),
        ("1.5.2082-1.0.00"),
        ("1.5.200283-1.09"),
        ("15.27.390+build1+build2")
      )

      "reject bad values" in {
        forAll(badVersions) { (a) =>
          assertThrows[IllegalArgumentException]{
            new SemanticVersion(a)
          }
        }
      }
    }

    "valid" should {

      val goodVersions = Table[String](
        ("1.0.0"),
        ("1.2.3"),
        ("999.999.999"),
        ("1.0.0-alpha"),
        ("1.5.7-alpha.47"),
        ("1.5.7-alpha57"),
        ("1.5.99-5407"),
        ("1.583.8903-5470-.3.342+000-432.30.000.00-0000"),
        ("1.2.3--------+----------"),
        ("1.2.3-alpha.-.-.-4-4-5+build.47.00.-.----.0")
      )

      "accept good values" in {
        forAll(goodVersions) { a =>
          assert(new SemanticVersion(a) != null)
        }
      }

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
        assert(version.prerelease == Some("alpha"))
      }

      "contain build metadata" in {
        val version = new SemanticVersion("1.2.3-alpha+meta")
        assert(version.buildMetadata == Some("meta"))
      }

      "does not contain build metadata" in {
        val version = new SemanticVersion("1.2.3-alpha")
        assert(version.buildMetadata == None)
      }

      "does not contain prerelease" in {
        val version = new SemanticVersion("1.2.3+build1")
        assert(version.prerelease == None)
      }

      "does not contain prerelease or build metadata" in {
        val version = new SemanticVersion("1.2.3")
        assert(version.buildMetadata == None)
        assert(version.buildFields() == List())
        assert(version.prerelease == None)
        assert(version.prereleaseFields() == List())
      }

      "contains prerelease and build fields" in {
        val version = new SemanticVersion("1.2.3-alpha.2018.09a+build4.000.1")
        assert(version.prerelease == Some("alpha.2018.09a"))
        assert(version.prereleaseFields() == List("alpha", "2018", "09a"))
        assert(version.buildMetadata == Some("build4.000.1"))
        assert(version.buildFields() == List("build4", "000", "1"))
      }
    }

    "evaluting equality of SemanticVersions" should {
      "return true for exactly equal versions" in {
        assert(new SemanticVersion("1.0.0-alpha+more") == new SemanticVersion("1.0.0-alpha+more"))
      }

      "return true for equal versions with different metadata" in {
        assert(new SemanticVersion("1.0.0-alpha+more") == new SemanticVersion("1.0.0-alpha+different"))
      }
    }

    "comparing versions" should {

      val equal = Table[String, String](
        ("a", "b"),
        ("1.0.0", "1.0.0"),
        ("0.1.0", "0.1.0"),
        ("0.0.1", "0.0.1"),
        ("1.0.0-alpha.1.1.1", "1.0.0-alpha.1.1.1"),
        ("1.0.0-alpha.1.1.1+999", "1.0.0-alpha.1.1.1+r2.3u.0.0"),
        ("1.0.0-alpha.1.1.1+v3", "1.0.0-alpha.1.1.1"),
        ("1.0.0-alpha.0.1.0", "1.0.0-alpha.0.1.0+0000"),
        ("1.0.0+meta1", "1.0.0+meta2")  // build metadata disregarded for ordering
      )

      "properly identify equal versions as equal" in {
        forAll(equal) { (a, b) =>
          assert(new SemanticVersion(a) == new SemanticVersion(b))
        }
      }

      val inequal = Table[String, String](
        ("lesser", "greater"),
        ("1.0.0", "2.0.0"),
        ("1.0.0", "10.0.0"),
        ("2.0.0", "10.0.0"),
        ("0.1.0", "0.2.0"),
        ("0.1.0", "0.10.0"),
        ("0.2.0", "0.10.0"),
        ("0.0.1", "0.0.2"),
        ("0.0.1", "0.0.10"),
        ("0.0.2", "0.0.10"),
        ("1.0.0-alpha", "1.0.0-beta"),   // pre-release versions
        ("1.1.0-20160605", "1.1.0-alpha"),  // numeric identifiers have lower precedence
        ("1.0.0-20170109", "1.0.0-20170110"),
        ("1.0.0-2", "1.0.0-10"),

        // Tests from semver.org site
        ("1.0.0", "2.0.0"),
        ("2.0.0", "2.1.0"),
        ("2.1.0", "2.1.1"),
        ("1.0.0-alpha", "1.0.0-alpha.1"),
        ("1.0.0-alpha.1", "1.0.0-alpha.beta"),
        ("1.0.0-alpha.beta", "1.0.0-beta"),
        ("1.0.0-beta", "1.0.0-beta.2"),
        ("1.0.0-beta.2", "1.0.0-beta.11"),
        ("1.0.0-beta.11", "1.0.0-rc.1"),
        ("1.0.0-rc.1", "1.0.0"),

        // New
        ("1.0.0-pre.1.1.2.alpha.9", "1.0.0-pre.1.1.2.alpha.40"),
        ("1.0.0-pre.1.1.2.alpha.9", "1.0.0-pre.1.1.2.alpha.-"),
        ("1.0.0-pre.1.1.2.alpha9", "1.0.0-pre.1.1.2.alpha9.5"),
        ("1.0.0-pre.9999+build-1-1", "1.0.0-pre.-9999"),  // numeric is lower
        ("1.0.0-pre.9999", "1.0.0-pre.a"),  // numeric is lower
        ("1.0.0--pre9999", "1.0.0-pre9999+v47"),
        ("2.0.14-1.1.2", "2.0.14-1.2+13u3"),
        ("1.0.0-9999", "1.0.0-000alpha1"),

        // -1 should be parsed as a non-numeric identifier rather than the int -1
        // Non-numeric identifiers have a higher precedence than numeric.
        ("1.0.0-9999", "1.0.0--1"),
        ("1.0.0-0", "1.0.0--"),
        ("1.0.0-9", "1.0.0-000alpha1"),
        ("1.0.0-9", "1.0.0--"),
        ("1.0.0-1.9", "1.0.0-1.-"),
        ("1.0.0-1.1234", "1.0.0-1.123-0"),
        ("1.0.0--alpha.9999", "1.0.0--alpha.-"),

        // build metadata can have leading zeroes
        ("2.0.14-1.1", "2.0.14-1.1.2+0000"),

        // '-' is valid per spec as a prerelease or build metadata field.
        ("2.0.14-1.1.2", "2.0.14-1.1.-+-"),

        // Spec never limited the size of numeric values.
        // Use numbers > 2^64 to make sure this library is compliant.
        ("4722366482869645213696.4722366482869645213696238.47223664828696452999923802-alpha.4722366482869645213696.0", "4722366482869645213696.4722366482869645213696238.47223664828696452999923802-alpha.4722366482869645213696.1.4722366482869645213696"),
        ("1000000000000000000000.4722366482869645213696238.47223664828696452999923802-alpha.4722366482869645213696.1.4722366482869645213696", "4722366482869645213699.4722366482869645213696238.47223664828696452999923802-alpha.4722366482869645213696.0")
      )

      "properly determine ordering for inequal versions" in {
        forAll(inequal) { (lesser, greater) =>
          val lesserVersion = new SemanticVersion(lesser)
          val greaterVersion = new SemanticVersion(greater)
          assert(lesserVersion < greaterVersion)
          assert(greaterVersion > lesserVersion)
          assert(lesserVersion != greaterVersion)
          assert(greaterVersion != lesserVersion)
        }
      }

    }
  }
}
