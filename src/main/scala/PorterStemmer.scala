
object PorterStemmer {

  def stem(word: String): String = {
    (step1a _ andThen step1b _ andThen step1c _
      andThen step2 _
      andThen step3 _
      andThen step4 _
      andThen step5a _ andThen step5b _)(word.toUpperCase)
  }

  def step1a(word: String): String = {
    applyRules(List((ident _, "SSES", "SS"),
                    (ident _, "IES", "I"),
                    (ident _, "SS", "SS"),
                    (ident _, "S", "")), word)
  }

  def step1b(word: String): String = {
    var rst = word
    rst = applyRule((x => measure(x) > 0, "EED", "EE"), rst)
    if (rst.endsWith("ED") && containsVowels(rst.substring(0, rst.length - 2)) ||
        rst.endsWith("ING") && containsVowels(rst.substring(0, rst.length - 3))) {
      rst = applyRules(List((containsVowels _, "ED", ""),
                            (containsVowels _, "ING", ""),
                            (ident _, "AT", "ATE"),
                            (ident _,"BL", "BLE"),
                            (ident _,"IZ", "IZE")), rst)
      if (endsWithCC(rst) && !(endsWith("L", rst) || endsWith("S", rst) || endsWith("Z", rst))) {
        rst = singleLetter(word)
      }
      if (measure(rst) == 1 && endsWithCVC(rst)) {
        rst = rst ++ "E"
      }

    }
    rst
  }

  def step1c(word: String): String = {
    applyRule((containsVowels _, "Y", "I"), word)
  }

  def step2(word: String): String = {
    applyRules(List((x => measure(x) > 0, "ATIONAL", "ATE"),
                    (x => measure(x) > 0, "TIONAL", "TION"),
                    (x => measure(x) > 0, "ENCI", "ENCE"),
                    (x => measure(x) > 0, "ANCI", "ANCE"),
                    (x => measure(x) > 0, "IZER", "IZE"),
                    (x => measure(x) > 0, "ABLI", "ABLE"),
                    (x => measure(x) > 0, "ALLI", "AL"),
                    (x => measure(x) > 0, "ENTLI", "ENT"),
                    (x => measure(x) > 0, "ELI", "E"),
                    (x => measure(x) > 0, "OUSLI", "OUS"),
                    (x => measure(x) > 0, "IZATION", "IZE"),
                    (x => measure(x) > 0, "ATION", "ATE"),
                    (x => measure(x) > 0, "ATOR", "ATE"),
                    (x => measure(x) > 0, "ALISM", "AL"),
                    (x => measure(x) > 0, "IVENESS", "IVE"),
                    (x => measure(x) > 0, "FULNESS", "FUL"),
                    (x => measure(x) > 0, "OUSNESS", "OUS"),
                    (x => measure(x) > 0, "ALITI", "AL"),
                    (x => measure(x) > 0, "IVITI", "IVE"),
                    (x => measure(x) > 0, "BILITI", "BLE")), word)
  }

  def step3(word: String): String = {
    applyRules(List((x => measure(x) > 0, "ICATE", "IC"),
                    (x => measure(x) > 0, "ATIVE", ""),
                    (x => measure(x) > 0, "ALIZE", "AL"),
                    (x => measure(x) > 0, "ICITI", "IC"),
                    (x => measure(x) > 0, "ICAL", "IC"),
                    (x => measure(x) > 0, "FUL", ""),
                    (x => measure(x) > 0, "NESS", "")), word)

  }

  def step4(word: String): String = {
    applyRules(List((x => measure(x) > 1, "AL", ""),
                    (x => measure(x) > 1, "ANCE", ""),
                    (x => measure(x) > 1, "ENCE", ""),
                    (x => measure(x) > 1, "ER", ""),
                    (x => measure(x) > 1, "IC", ""),
                    (x => measure(x) > 1, "ABLE", ""),
                    (x => measure(x) > 1, "IBLE", ""),
                    (x => measure(x) > 1, "ANT", ""),
                    (x => measure(x) > 1, "EMENT", ""),
                    (x => measure(x) > 1, "MENT", ""),
                    (x => measure(x) > 1, "ENT", ""),
                    (x => measure(x) > 1 && (x.endsWith("S") || x.endsWith("T")), "ION", ""),
                    (x => measure(x) > 1, "ION", ""),
                    (x => measure(x) > 1, "OU", ""),
                    (x => measure(x) > 1, "ISM", ""),
                    (x => measure(x) > 1, "ATE", ""),
                    (x => measure(x) > 1, "ITI", ""),
                    (x => measure(x) > 1, "OUS", ""),
                    (x => measure(x) > 1, "IVE", ""),
                    (x => measure(x) > 1, "IZE", "")), word)
  }

  def step5a(word: String): String = {
    var rst = word
    rst = applyRule((x => measure(x) == 1 && !endsWithCVC(x), "E", ""), rst)
    rst = applyRule((x => measure(x) > 1, "E", ""), rst)
    rst
  }

  def step5b(word: String): String = {
    var rst = word
    if (measure(rst) > 1 && endsWithCC(rst) && endsWith("L", rst)) {
      rst = singleLetter(rst)
    }
    rst
  }

  def isConsonantAt(pos: Int, word: String): Boolean = {
    if (0 <= pos && pos < word.length) {
      val l = word(pos)
      !(Set('A', 'E', 'I', 'O', 'U') contains l) && !(l == 'Y' && isConsonantAt(pos - 1, word))
    } else {
      false
    }
  }

  def isVowelAt(pos: Int, word: String): Boolean = {
    !isConsonantAt(pos, word)
  }

  def containsVowels(word: String): Boolean = {
    word.indices.exists(pos => isVowelAt(pos, word))
  }

  def endsWithCC(word: String): Boolean = {
    (word.length > 1) && (word(word.length - 1) == word(word.length - 2)) &&
    isConsonantAt(word.length - 1, word)
  }

  def endsWith(l: String, word: String): Boolean = {
    word.length > 1 && word.endsWith(l)
  }

  def endsWithCVC(word: String): Boolean = {
    val wzy = Set('w', 'x', 'y')
      (word.length > 2) && isConsonantAt(word.length - 1, word) && isConsonantAt(word.length - 3, word) &&
    isVowelAt(word.length - 2, word) && !wzy.contains(word(word.length - 1))
  }

  def singleLetter(word: String): String = {
    word.substring(0, word.length - 1)
  }

  def ident(word: String): Boolean = {
    true
  }

  def measure(word: String): Int = {
    word.indices.filter(pos => isVowelAt(pos, word) && isConsonantAt(pos + 1, word)).length
  }

  def applyReplacement(word: String, suffix: String, replacement: String): String = {
    word.substring(0, word.length - suffix.length) ++ replacement
  }

  def applyRule(rule: (String => Boolean, String, String), word: String): String = {
    val (fn, suffix, replacement) = rule
    if (word.endsWith(suffix)) {
      val stem = word.substring(0, word.length - suffix.length)
      if (fn(stem)) {
        return applyReplacement(word, suffix, replacement)
      }
    }
    word
  }

  def applyRules(rules: List[(String => Boolean, String, String)], word: String): String = {
    rules.foldLeft(word)((rst, rule)  => applyRule(rule, rst))
  }

}
