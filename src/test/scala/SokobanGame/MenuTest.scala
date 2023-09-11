package SokobanGame

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers



class MenuTest extends AnyFlatSpec with Matchers {


  "Menu" should "execute the correct function based on user input" in {
    val menu = new Menu


    var outputMessage = ""
    var functionExecuted = false


    def sampleFunction(): Unit = {
      functionExecuted = true
    }


    val options: List[menu.DescriptionAndFunction] = List(
      ("Option 1", sampleFunction)
    )


    menu.chooseAndExecuteFunction(
      options,
      () => 1, // mock user input
      message => outputMessage = message
    )


    functionExecuted shouldBe true
  }


  it should "display error message for invalid input" in {
    val menu = new Menu


    var outputMessage = ""


    val options: List[menu.DescriptionAndFunction] = List(
      ("Option 1", () => {})
    )


    menu.chooseAndExecuteFunction(
      options,
      () => 10, // mock invalid user input
      message => outputMessage = message
    )


    outputMessage shouldBe "Pogresan izbor!"
  }
}