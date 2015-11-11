import org.scalatest._

import PorterStemmer._


class PorterStemmerSpec extends FlatSpec with Matchers {


  info("Running porter stemmer building blocks tests...")
  
  info("Testing PorterStemmer.measure implementation...")
  "Measure" should "return number of VC in a word" in {
    PorterStemmer.measure("TR") should be (0)
    PorterStemmer.measure("EE") should be (0)
    PorterStemmer.measure("TREE") should be (0)
    PorterStemmer.measure("Y") should be (0)
    PorterStemmer.measure("BY") should be (0)
    PorterStemmer.measure("TROUBLE") should be (1)
    PorterStemmer.measure("OATS") should be (1)
    PorterStemmer.measure("TREES") should be (1)
    PorterStemmer.measure("IVY") should be (1)
    PorterStemmer.measure("TROUBLES") should be (2)
    PorterStemmer.measure("PRIVATE") should be (2)
    PorterStemmer.measure("OATEN") should be (2)
    PorterStemmer.measure("ORRERY") should be (2)
  }

  "Step 1a" should "eliminate proper s sufixes" in {
    PorterStemmer.step1a("CARESSES") should be ("CARES")
    PorterStemmer.step1a("PONIES") should be ("PONI")
    PorterStemmer.step1a("TIES") should be ("TI")
    PorterStemmer.step1a("CARESS") should be ("CARES")
    PorterStemmer.step1a("CATS") should be ("CAT")
  }

  "Step 1b" should "eliminate proper ed, ing sufixes" in {
    PorterStemmer.step1b("FEED") should be ("FE")
    PorterStemmer.step1b("AGREED") should be ("AGREE")
    PorterStemmer.step1b("PLASTERED") should be ("PLASTER")
    PorterStemmer.step1b("BLED") should be ("BLED")
    PorterStemmer.step1b("MOTORING") should be ("MOTOR")
    PorterStemmer.step1b("SING") should be ("SING")
  }

  "Step 1c" should "replace y with i if condiftion *v* is satisfied" in {
    PorterStemmer.step1c("HAPPY") should be ("HAPPI")
    PorterStemmer.step1c("SKY") should be ("SKY")
  }

  "Step 2" should "eliminate" in {
    PorterStemmer.step2("RELATIONAL") should be ("RELATE")
    PorterStemmer.step2("CONDITIONAL") should be ("CONDITION")
    PorterStemmer.step2("RATIONAL") should be ("RATIONAL")
    PorterStemmer.step2("VALENCI") should be ("VALENCE")
    PorterStemmer.step2("HESITANCI") should be ("HESITANCE")
    PorterStemmer.step2("DIGITIZER") should be ("DIGITIZE")
    PorterStemmer.step2("CONFORMABLI") should be ("CONFORMABLE")
    PorterStemmer.step2("RADICALLI") should be ("RADICAL")
    PorterStemmer.step2("DIFFERENTLI") should be ("DIFFERENT")
    PorterStemmer.step2("VILELI") should be ("VILE")
    PorterStemmer.step2("ANALOGOUSLI") should be ("ANALOGOUS")
    PorterStemmer.step2("VIETNAMIZATION") should be ("VIETNAMIZE")
    PorterStemmer.step2("PREDICATION") should be ("PREDICATE")
    PorterStemmer.step2("OPERATOR") should be ("OPERATE")
    PorterStemmer.step2("FEUDALISM") should be ("FEUDAL")
    PorterStemmer.step2("DECISIVENESS") should be ("DECISIVE")
    PorterStemmer.step2("HOPEFULNESS") should be ("HOPEFUL")
    PorterStemmer.step2("CALLOUSNESS") should be ("CALLOUS")
    PorterStemmer.step2("FORMALITI") should be ("FORMAL")
    PorterStemmer.step2("SENSITIVITI") should be ("SENSITIVE")
    PorterStemmer.step2("SENSIBILITI") should be ("SENSIBLE")
  }

  "Step 3" should "eliminate" in {
    PorterStemmer.step3("TRIPLICATE") should be ("TRIPLIC")
    PorterStemmer.step3("FORMATIVE") should be ("FORM")
    PorterStemmer.step3("FORMALIZE") should be ("FORMAL")
    PorterStemmer.step3("ELECTRICITI") should be ("ELECTRIC")
    PorterStemmer.step3("ELECTRICAL") should be ("ELECTRIC")
    PorterStemmer.step3("HOPEFUL") should be ("HOPE")
    PorterStemmer.step3("GOODNESS") should be ("GOOD")
  }

  "Step 4" should "eliminate" in {
    PorterStemmer.step4("REVIVAL") should be ("REVIV")
    PorterStemmer.step4("ALLOWANCE") should be ("ALLOW")
    PorterStemmer.step4("INFERENCE") should be ("INFER")
    PorterStemmer.step4("AIRLINER") should be ("AIRLIN")
    PorterStemmer.step4("GYROSCOPIC") should be ("GYROSCOP")
    PorterStemmer.step4("ADJUSTABLE") should be ("ADJUST")
    PorterStemmer.step4("DEFENSIBLE") should be ("DEFENS")
    PorterStemmer.step4("IRRITANT") should be ("IRRIT")
    PorterStemmer.step4("REPLACEMENT") should be ("REPLAC")
    PorterStemmer.step4("ADJUSTMENT") should be ("ADJUST")
    PorterStemmer.step4("DEPENDENT") should be ("DEPEND")
    PorterStemmer.step4("ADOPTION") should be ("ADOPT")
    PorterStemmer.step4("HOMOLOGOU") should be ("HOMOLOG")
    PorterStemmer.step4("COMMUNISM") should be ("COMMUN")
    PorterStemmer.step4("ACTIVATE") should be ("ACTIV")
    PorterStemmer.step4("ANGULARITI") should be ("ANGULAR")
    PorterStemmer.step4("HOMOLOGOUS") should be ("HOMOLOG")
    PorterStemmer.step4("EFFECTIVE") should be ("EFFECT")
    PorterStemmer.step4("BOWDLERIZE") should be ("BOWDLER")
  }

  "Step 5a" should "eliminate" in {
    PorterStemmer.step5a("PROBATE") should be ("PROBAT")
    PorterStemmer.step5a("RATE") should be ("RATE")
    PorterStemmer.step5a("CEASE") should be ("CEAS")
  }

  "Step 5b" should "eliminate" in {
    PorterStemmer.step5b("CONTROLL") should be ("CONTROL")
    PorterStemmer.step5b("ROLL") should be ("ROLL")
  }

}
