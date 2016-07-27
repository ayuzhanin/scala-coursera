  /**
    * Streams and functional generators
    */

trait Generator[+T] {
  outer =>
  def generate: T
  def map[S] (f: T => S):  Generator[S] = new Generator[S] {
    override def generate: S = f(outer.generate)
  }
  def flatMap[S] (f: T => Generator[S]): Generator[S] = {
    f(generate)
  }
}

val integers = new Generator[Int] {
  val rand = new java.util.Random
  override def generate = rand.nextInt
}

val booleans = integers map (x => x>0)

def createPairs[T, U](t: Generator[T], u: Generator[U]): Generator[(T,U)] = {
  t flatMap( x => u map(y => (x,y)))
}

def createSingle[T](x: T): Generator[T] = new Generator[T] {
  override def generate = x
}

def createChooser[T](low: Int, high: Int): Generator[Int] = {
  integers map {x => {
    val y = if (x > 0) x else -x
    low + y % (high - low)
  }}
}

def createOneOf[T](xs: T*): Generator[T] = {
  createChooser(0, xs.length - 1) map (index => xs(index))
}

def list: Generator[List[Int]] = {
  for {
    isEmpty <- booleans
    list <- if (isEmpty) emptyListGenerator else nonEmptyListGenerator
  } yield list
}

integers.generate
booleans.generate
def pair = createPairs(booleans, integers)
pair.generate

def single = createSingle(true)
single.generate

def oneOf = createOneOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
oneOf.generate

def emptyListGenerator = createSingle(Nil)

def nonEmptyListGenerator = {
  integers flatMap (head => list map (tail => head :: tail))
}

emptyListGenerator.generate

nonEmptyListGenerator.generate

list.generate

def isPrime (x: Int): Boolean = {
  if (x == 1 || x == 2) true
  else 2 until x forall { p => (x % p) != 0}
}

isPrime(7)
isPrime(6)

((1 until 100).toStream filter isPrime)(1)
