package imagemanipulation

trait FileSystemOperations {
  import ammonite.ops._

  val originalImgDir = ammonite.ops.pwd / 'OriginalImages
  val listed = ls! originalImgDir
  val manipulatedImgDir = cwd / 'ManipulatedImages
  val generatedImgDir = cwd / 'GeneratedImages

  def copyFreshImages_!(srcDir: Path, targetDir: Path): LsSeq = {
    val oldImages = ls! targetDir
    oldImages.map{ rm! _ }
    mkdir! targetDir
    for ( file <- (ls! srcDir) ) { cp.into(file, targetDir)  }
    ls! targetDir
  }

  def generateFreshImages_!(targetDir: Path) = {
    mkdir! targetDir
    val oldImages = ls! targetDir
    oldImages.map{ rm! _ }
  }
}


