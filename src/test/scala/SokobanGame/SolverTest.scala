package SokobanGame



import SokobanGame.Sokoban.Board
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers



class SolverTest extends AnyFlatSpec with Matchers {



  "Solver" should "find the player's position" in {
    val solver = new Solver
    val grid = Array(
      Array[Tile](Space, Space, Space),
      Array[Tile](Space, Player, Space),
      Array[Tile](Space, Space, Space)
    )



    solver.findPlayerPosition1(grid) shouldBe Point(1, 1)
  }



  it should "find all box positions" in {
    val solver = new Solver
    val grid =
      Array(
        Array[Tile]
          (Box, Space, Space),
        Array[Tile](
          Space, Box, Space),
        Array[Tile](
          Space, Space, Box)
    )



    solver.findBoxes(grid) shouldBe Set(Point(0, 0), Point(1, 1), Point(2, 2))
  }



  it should "find all target positions" in {
    val solver = new Solver
    val grid = Array(
      Array[Tile](Target, Space, Space),
      Array[Tile](Space, Target, Space),
      Array[Tile](Space, Space, Target)
    )



    solver.findTargets(grid) shouldBe Set(Point(0, 0), Point(1, 1), Point(2, 2))
  }

  "getMoves" should "return the correct moves for a simple board" in {
    val solver = new Solver
    val board: Board = Array(
      Array[Tile](Space,  Wall,   Space,   Space),
      Array[Tile](Space, Player, Space,   Space),
      Array[Tile](Space, Box,    Target,  Space),
      Array[Tile](Space, Space,  Space,   Space)
    )
    val moves = solver.getMoves(board)
    val expectedMoves = List("l", "d", "r")
    moves shouldEqual expectedMoves
  }

  it should "return 'No solution found' for an unsolvable board" in {
    val solver = new Solver
    val board: Board = Array(
      Array[Tile](Wall, Wall, Wall, Wall),
      Array[Tile](Wall, Player, Wall, Wall),
      Array[Tile](Wall, Box, Wall, Wall),
      Array[Tile](Wall, Wall, Wall, Wall)     )
    val moves = solver.getMoves(board)
    moves shouldEqual List("No solution found")   }
}