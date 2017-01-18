  package compliant

  import scala.util.matching.Regex
  import scala.collection.mutable.ListBuffer

  object SemanticVersion {

    val re: Regex = """(\d+)\.(\d+)\.(\d+)(-([^+]+))?(\+(.+))?""".r

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
      val prereleaseVersionDiff = this.prereleaseCompare(that)

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

    def prereleaseCompare(that: SemanticVersion): Int = {
      if (this.prerelease.isEmpty && !that.prerelease.isEmpty) {
        return 1
      } else if (!this.prerelease.isEmpty && that.prerelease.isEmpty) {
        return -1
      } else {
        val thisArray = Chunkify.chunkify(this.prerelease)
        val thatArray = Chunkify.chunkify(that.prerelease)
        var currentDiff = 0
        var currentIndex = 0

        while (currentDiff == 0 && currentIndex < thisArray.length) {
          val currentThis = thisArray(currentIndex)
          val currentThat = thatArray(currentIndex)

          if (Chunkify.isAllDigits(currentThis) && Chunkify.isAllDigits(currentThat)) {
            currentDiff = currentThis.toInt.compare(currentThat.toInt)
          } else {
            currentDiff = currentThis.compare(currentThat)
          }

          currentIndex = currentIndex + 1
        }

        // TODO: Find a way to not use return statements
        if (currentDiff == 0) {
          if (thisArray.length == thatArray.length) {
            return 0
          } else if (thisArray.length < thatArray.length) {
            return 1
          } else {
            return -1
          }
        } else {
          return currentDiff
        }
      }
    }

  }

  // Helper functions for chunking prerelease strings
  // TODO: Consider moving this to another file
  object Chunkify {

    def chunkify(string: String) = {
      var chunks = new ListBuffer[String]()
      var currentChunk = new StringBuilder
      var currentChunkType = ""
      var i = 0

      while (i < string.length) {
        var c = string(i)

        val thisChunkType = {
          if (isDigit(c.toString)) {
            "digit"
          } else if (isAlpha(c.toString)) {
            "alpha"
          } else if (isPunctuation(c.toString)) {
            "punctuation"
          } else {
            "unknown"
          }
        }

        // Beginning of the string or beginning of a new chunk
        if (currentChunkType == "" || currentChunkType == thisChunkType) {
          currentChunk += c
          currentChunkType = thisChunkType
        } else {
          chunks += currentChunk.toString
          currentChunk = new StringBuilder
          currentChunk += c
          currentChunkType = thisChunkType
        }

        // Make sure to append the final chunk when
        // we reach the end of the string
        if (i == string.length - 1) {
          chunks += currentChunk.toString
        }

        i += 1
      }  // end string.map

      chunks.toList
    }

    def isAllDigits(s: String) = {
      s.length == s.filter(_.isDigit).length
    }

    def isDigit(s: String) = {
      if (Array("0", "1", "2", "3", "4", "5", "6", "7", "8", "9").contains(s)) {
        true
      } else {
        false
      }
    }

    def isPunctuation(s: String) = {
      if (Array(".", "-", "+").contains(s)) {
        true
      } else {
        false
      }
    }

    def isAlpha(s: String) = {
      val alphaArray = Array(
        "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l",
        "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x",
        "y", "z")

      if (alphaArray.contains(s.toLowerCase)) {
        true
      } else {
        false
      }
    }

  }
