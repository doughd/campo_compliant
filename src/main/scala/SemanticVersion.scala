package compliant

import scala.util.matching.Regex

// This implementation should be able to parse https://semver.org (version 2.0.0) version strings.
object SemanticVersion {
  // major = \d+ (but must not start with '0' unless only '0')
  // minor = same restrictions as major
  // patch = same restrictions as major
  // pre-release = '-' followed by dot separated fields of [0-9A-Za-z-]+ (but if all numeric, must not start with 0 unless '0')
  // build-metadata = '+' followed by dot separated fields of [0-9A-Za-z-]+.  No restrictions on leading zeroes.
  val re: Regex = """^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-((?:(?:0|0[0-9A-Za-z-]*[A-Za-z-][0-9A-Za-z-]*|[1-9A-Za-z-][0-9A-Za-z-]*)\.)*(?:0|0[0-9A-Za-z-]*[A-Za-z-][0-9A-Za-z-]*|[1-9A-Za-z-][0-9A-Za-z-]*)))?(?:\+((?:[0-9A-Za-z-]+\.)*[0-9A-Za-z-]+))?$""".r

  def apply(versionString: String): SemanticVersion = {
    new SemanticVersion(versionString)
  }
}

object Version {
  def apply(versionString: String): SemanticVersion = {
    new SemanticVersion(versionString)
  }
}

class SemanticVersion(val versionString: String) extends Ordered[SemanticVersion] {
  val fullVersionString = versionString

  val (majorVersion, minorVersion, patchVersion, prerelease, buildMetadata) = {
    versionString match {
      case SemanticVersion.re(major, minor, patch, pre, meta) =>
        (major, minor, patch, Option(pre), Option(meta))
      case _ =>
        throw new IllegalArgumentException(versionString)
    }
  }

  def ==(otherVersionString: String): Boolean = {
    this == new SemanticVersion(otherVersionString)
  }

  def ==(otherVersion: SemanticVersion): Boolean = {
    compare(otherVersion) == 0
  }

  def versions(): Seq[String] = {
    List(this.majorVersion, this.minorVersion, this.patchVersion)
  }

  // We don't compare build metadata for precedence purposes.
  def compare(that: SemanticVersion): Int = {
    this.versions.zip(that.versions).map { case (a, b) => compareNumeric(a, b) }.dropWhile(_ == 0).headOption match {
      case None    => this.prereleaseCompare(that)
      case Some(r) => r
    }
  }

  // This takes advantage of the fact that our numeric input is restricted (no leading zeroes or signs)
  def compareNumeric(left: String, right: String): Int = {
    left.length.compare(right.length) match {
      case 0 => left.compare(right)
      case r => r
    }
  }

  def isAllDigits(s: String): Boolean = s.find(!_.isDigit) == None

  // All numeric field has lower precedence than non-numeric field.
  // This is only used by prerelease fields.  Build metadata isn't compared.
  def fieldCompare(left: String, right: String): Int = {
    if (isAllDigits(left)) {
      if (isAllDigits(right)) {
        compareNumeric(left, right)
      } else {
        -1
      }
    } else {
      if (isAllDigits(right)) {
        1
      } else {
        left.compare(right)
      }
    }
  }

  def buildFields(): Seq[String] = {
    this.buildMetadata match {
      case None    => List()
      case Some(r) => r.split("\\.")
    }
  }

  def prereleaseFields(): Seq[String] = {
    this.prerelease match {
      case None    => List()
      case Some(r) => r.split("\\.")
    }
  }

  // Not having a prerelease identifier has higher precedence than having one.
  // Otherwise, the larger number of fields wins after comparison
  def prereleaseCompare(that: SemanticVersion): Int = {
    val thisArray = this.prereleaseFields()
    val thatArray = that.prereleaseFields()

    if (thisArray.isEmpty || thatArray.isEmpty) {
      -thisArray.length.compare(thatArray.length)
    } else {
      thisArray.zip(thatArray).map { case (a, b) => fieldCompare(a, b) }.dropWhile(_ == 0).headOption match {
        case None    => thisArray.length.compare(thatArray.length)
        case Some(r) => r
      }
    }
  }

}
