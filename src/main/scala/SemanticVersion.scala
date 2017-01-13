package compliant

import scala.util.matching.Regex

object SemanticVersion {

  val re: Regex = """(\d+)\.(\d+)\.(\d+)(-([^+]+))?(\+(.+))?""".r

}

class SemanticVersion(val versionString: String) extends Ordered[SemanticVersion] {

  val fullVersionString: String = versionString.trim.replaceAll("\\s", "")

  val (majorVersion, minorVersion, patchVersion, prerelease, buildMetadata) = {
    fullVersionString match {
      case SemanticVersion.re(major, minor, patch, _, pre, _, meta) =>
        (major, minor, patch, Option(pre).getOrElse(""), Option(meta).getOrElse(""))
      case _ =>
        throw new IllegalArgumentException(versionString)
    }
  }

  def versionStringWithoutBuildMetadata : String = {
    this.fullVersionString.split("\\+").head
  }

  def ==(otherVersionString: String) : Boolean = {
    val otherVersion = new SemanticVersion(otherVersionString)
    this == otherVersion
  }

  def ==(otherVersion: SemanticVersion) : Boolean = {
    this.versionStringWithoutBuildMetadata == otherVersion.versionStringWithoutBuildMetadata
  }

  def compare(that: SemanticVersion) = {
    val majorVersionDiff = this.majorVersion.toInt - that.majorVersion.toInt
    val minorVersionDiff = this.minorVersion.toInt - that.minorVersion.toInt
    val patchVersionDiff = this.patchVersion.toInt - that.patchVersion.toInt
    val prereleaseVersionDiff =
      if (this.prerelease.isEmpty && !that.prerelease.isEmpty) {
        1
      } else if (!this.prerelease.isEmpty && that.prerelease.isEmpty) {
        -1
      } else {
        this.prerelease.compare(that.prerelease)
      }

    if (this.versionStringWithoutBuildMetadata == that.versionStringWithoutBuildMetadata) {
      0
    } else if (majorVersionDiff != 0) {
      majorVersionDiff
    } else if (minorVersionDiff != 0) {
      minorVersionDiff
    } else if (patchVersionDiff != 0) {
      patchVersionDiff
    } else {
      prereleaseVersionDiff
    }
  }
}
