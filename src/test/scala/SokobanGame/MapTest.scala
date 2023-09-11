package SokobanGame

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfter
import SokobanGame.Sokoban.Board
import java.nio.file.{Files, Paths}
import java.io.PrintWriter



class MapTest extends AnyFlatSpec with Matchers with BeforeAndAfter {

  val map = new SokobanGame.Map()

  def createTempFile(content: String, extension: String): String = {
    val tempFile = Files.createTempFile("temp", extension).toFile
    new PrintWriter(tempFile) {
      write(content)
      close()
    }
    tempFile.getAbsolutePath
  }



  "listMaps" should "return empty array when directory does not exist" in {
    val files = map.listMaps("nonexistent_directory")
    files shouldBe empty
  }

  it should "list all files in a directory" in {
    val tempDir = Files.createTempDirectory("tempDir").toString
    createTempFile("content1", ".txt")
    createTempFile("content2", ".txt")
    val files = map.listMaps(tempDir)
    files should have length 2
  }

  it should "not list sub-directories" in {
    val tempDir = Files.createTempDirectory("tempDir").toString
    Files.createTempDirectory(Paths.get(tempDir), "subDir")
    val files = map.listMaps(tempDir)
    files shouldBe empty
  }

  "readFileAsString" should "read the correct content of a file" in {
    val tempFile = createTempFile("hello world", ".txt")
    val content = map.readFileAsString(tempFile)
    content shouldBe "hello world"
  }

  it should "return empty string for non-existing file" in {
    val content = map.readFileAsString("nonexistent_file.txt")
    content shouldBe ""
  }

  "SetNewBoard" should "correctly set the level string from a board" in {
    val board: Board = Array(
      Array(Space, Target),
      Array(Player, Box)
    )
    map.SetNewBoard(board)
    map.getLevel() shouldBe "SpaceTarget\nPlayerBox\n"
  }

}
