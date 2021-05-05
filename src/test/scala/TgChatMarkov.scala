import com.typesafe.scalalogging.Logger
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import work.arudenko.markov.MarkovSerializer._
import work.arudenko.markov.TextBasedMarkovChain

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}
import better.files._
import java.io.{File => JFile}

/**
 * example object that generates markov chain form telegram chat exported as .json file
 */
object TgChatMarkov {

  private val logger: Logger = Logger("TgChatMarkov")
  def getTextsFromArrayElement(arrayElement: ACursor,users:Option[Iterable[String]]): Iterable[String] = {

    @tailrec
    def getTextsFromArrayElement(arrayElement: ACursor, previous:List[String]):List[String] ={
      arrayElement.focus match {
        case Some(value) =>
          val messageType = value.findAllByKey("type").head.asString.get
          if(messageType == "message")
          {
            val from = value.findAllByKey("from").head.asString
            users.map(_.exists(user=>from.contains(user))).flatMap(exists => if (exists) None else Some())
            match {
              case Some(_) =>
                getTextsFromArrayElement(arrayElement.right, previous)
              case None =>
                val text = value.findAllByKey("text").head.asString
                text match {
                  case Some(value) =>
                    if (value.nonEmpty && !value.isBlank)
                      getTextsFromArrayElement(arrayElement.right, value :: previous)
                    else
                      getTextsFromArrayElement(arrayElement.right, previous)
                  case None => getTextsFromArrayElement(arrayElement.right, previous)
                }
            }
          }else{
            getTextsFromArrayElement(arrayElement.right,previous)
          }
        case None => previous
      }
    }
    getTextsFromArrayElement(arrayElement,Nil)
  }

  case class MarkovGen(in:File,out:File,names:Option[Seq[String]])

  def runGen(options:MarkovGen): Unit = {
    logger.info("reading tg log file")
    logger.info("parsing json content")
    val parseResult = parse(options.in.contentAsString)
    logger.info("teaching markov chain")
    val parsedExtract = parseResult.toTry.get
    parsedExtract.hcursor.downField("chats").downField("list").focus
      .orElse{logger.info("chats list not found, looks like singe chat extract"); None}
      .map(_.hcursor.downArray).map(cursor=> (cursor,cursor.focus))

    val texts = parsedExtract.hcursor.downField("messages").focus.orElse{logger.error("messages field not found"); None}
      .map(messages=>getTextsFromArrayElement(messages.hcursor.downArray,options.names))
      .map(_.filter(_.nonEmpty))

    val chain = texts.map(texts=>TextBasedMarkovChain.teach(texts.toSeq)).get
    logger.info("chain generation finished, converting to json")
    val jsonChain = chain.asJson
    logger.info("writing to file")
    options.out.write(jsonChain.noSpaces)
  }


  implicit class loggableTry[T](target:Try[T]){
    def toOptionLogged:Option[T] = {
      target match {
        case Failure(exception) => logger.error(exception.toString); None
        case Success(value) =>Some(value)
      }
    }
  }

}
