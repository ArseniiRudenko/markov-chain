package work.arudenko.markov

import edu.stanford.nlp.pipeline.{CoreDocument, StanfordCoreNLP}
import work.arudenko.markov.discrete.DiscreteMarkovChainTeachable

import java.util.Properties
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.Try


object TextBasedMarkovChain{
  // set up pipeline properties
  lazy val props: Properties = {
    val p =  new Properties()
    // set the list of annotators to run
    p.setProperty("annotators", "tokenize,ssplit")
    p
  }

  lazy val propsSentences: Properties = {
    val p =  new Properties()
    // set the list of annotators to run
    p.setProperty("annotators", "tokenize,ssplit")
    p.setProperty("ssplit.isOneSentence","true")
    p
  }

  // build pipeline
  lazy val pipelineText = new StanfordCoreNLP(props)

  lazy val pipelineSentences = new StanfordCoreNLP(propsSentences)


  def teach(sentences:Seq[String]):MarkovChain[String] = {
    val docs = sentences.map(s=>new CoreDocument(s))
    docs.foreach(doc=>Try(pipelineSentences.annotate(doc)))
    val wordSeries = docs.flatMap(doc=>if(doc.sentences()!=null)doc.sentences().asScala else Nil).map(cs=>cs.tokensAsStrings().asScala)
    wordSeriesToModel(wordSeries)
  }

  def teach(text:String):MarkovChain[String] = {

    // create a document object
    val doc = new CoreDocument(text)
    // annotate
    pipelineText.annotate(doc)
    // display sentences
    val wordSeries =
      doc
        .sentences()
        .asScala
        .map(cs=>cs.tokensAsStrings().asScala)
    wordSeriesToModel(wordSeries)
  }

  def wordSeriesToModel(wordSeries: Iterable[Iterable[String]]):MarkovChain[String] = {
    wordSeries
      .foldLeft[MarkovChainTeachable[String]](DiscreteMarkovChainTeachable[String]())(
        (chain,sentence)=>{
          if(sentence.size>=2) {
            val withStartAndEnd = chain.addStart(sentence.head).addEnd(sentence.last)
            sentence
              .sliding(2)
              .foldLeft(withStartAndEnd)(
                (newChain, wordPair) => newChain.addEvent(wordPair.head, wordPair.tail.head)
              )
          }else if(sentence.size==1) {
            val word = sentence.head
            if(word.nonEmpty && !word.isBlank && !word.matches("\\W+"))
              chain.addStart(word).addEnd(word)
            else
              chain
          }else{
            chain
          }
        }
      ).materializeModel
  }

  implicit class TextBasedMarkovChain(val chain: MarkovChain[String]) extends AnyVal{
    def walkTxt(targetLength: Option[Int] = None, startNode: Option[StartFrom[String]]=None): String ={
      val res = chain.walk(targetLength,startNode)
      res.foldLeft("")((text,elem)=>{
        if(elem.matches("[!\\._\\-,:'@\\?\\) ]+") || text.endsWith("-"))
          text+elem
        else {
          s"$text $elem"
        }
      })
    }
  }

}
