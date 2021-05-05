import io.circe.parser.decode
import work.arudenko.markov.discrete.DiscreteMarkovChain
import work.arudenko.markov.MarkovSerializer._
import work.arudenko.markov.TextBasedMarkovChain._
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers._
import TgChatMarkov.MarkovGen
import better.files._
import java.io.{File => JFile}

class TextGenSpec extends AnyFlatSpec with should.Matchers {

  "Markov chain" should "generate model and return value" in{
    TgChatMarkov.runGen(
      MarkovGen(
        "src/test/resources/bbc_tg_chat_export.json".toFile,
        "src/test/resources/out.json".toFile,
        Some(Seq("BBC News"))
      )
    )
    val chain = decode[DiscreteMarkovChain[String]]("src/test/resources/out.json".toFile.contentAsString).toTry.get
    println(chain.walkTxt())
  }

  it should "generate text of various length without failing" in {
    val chain = decode[DiscreteMarkovChain[String]](Resource.getAsString("out.json")).toTry.get
    println(chain.walkTxt())
    println(chain.walkTxt(Some(10)))
    println(chain.walkTxt(Some(15)))
    println(chain.walkTxt(Some(20)))
    println(chain.walkTxt(Some(30)))
  }


}
