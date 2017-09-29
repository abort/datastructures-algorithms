import org.scalatest.{FunSuite, Matchers}

import scala.collection.JavaConverters._

class TestDiff extends FunSuite with Matchers {
  val instance = new SuffixTreeJava()
  def validateEquality(input : String) : Unit = {
    val a = SuffixTrie.computeSuffixTrie(input).toSet
    val b = instance.computeSuffixTreeEdges(input).asScala.toSet
    a should contain theSameElementsAs b
    //assert(a == b)
  }

  test("GTGGG$") {
    validateEquality("GTGGG$")
  }
  test("CACAC$") {
    validateEquality("CACAC$")
  }
  test("GGGCTCAGCCAGG$") {
    validateEquality("GGGCTCAGCCAGG$")
  }
  test("AAACAC$") {
    validateEquality("AAACAC$")
  }
  test("AAAA$") {
    validateEquality("AAAA$")
  }
  test("ATTGTCACAACTGCGAGCGGCTAAATTT$") {
    validateEquality("ATTGTCACAACTGCGAGCGGCTAAATTT$")
  }
  test("smaller1") {
    validateEquality("TAATTAATT$")
  }
  test("smaller") {
    validateEquality("CTAAATTTGGTAATTCTGGTAAAC$")
  }
  test("CTAAATTTGGTAATTCTGCCGGTAAAC$") {
    validateEquality("CTAAATTTGGTAATTCTGCCGGTAAAC$")
  }
  test("ATTGTCACAACTGCGAGCGGCTAAATTTGGTAATTCTGCCGGTAAACGAAAACAACGACCT$") {
    validateEquality("ATTGTCACAACTGCGAGCGGCTAAATTTGGTAATTCTGCCGGTAAACGAAAACAACGACCT$")
  }
}
