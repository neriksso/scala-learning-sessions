/**
  Exercise Sheet 1 of the SPAN Scala Sessions.

  The content in this sheet is relevant to Weeks 1, 2, 3 and 4 of the "Functional Programming Principles
  in Scala" course covered in the Scala Developer Roadmap.
  */


/**
1) Lists & String Operators
  Calculate the sum (as an integer) of all numbers that are in the "numbers" list.
  This is possible using only 4 function-calls and no custom logic.
  */
val numbers = List(List("123", "456", "7"), List("10", "34", "9"), List("32", "54y"))

val result: Int = numbers.flatten.flatMap(_.toIntOption).sum

assert(result == (123+456+7+10+34+9+32))


/**
2) Functions as Objects
  Implement the following functions:
  a) sumInts(), that should sum a list of integers that it takes as argument
  b) multiplyInts(), that should multiply a list of integers it takes as argument
  c) mathsEngine(), that should be able to take a list of integers (List[Int]) as well as any
     other function (sumInts() or multiplyInts()) and apply that function a given list on integers.
  */
def sumInts(ints: List[Int]): Int = ints.sum

def multiplyInts(ints: List[Int]): Int = ints.product

def mathsEngine(ints: List[Int], mathFunction: List[Int] => Int): Int = {
  mathFunction(ints)
}

assert(15 == mathsEngine(List(1,2,3,4,5), sumInts))
assert(6 == mathsEngine(List(1,2,3), multiplyInts))


/**
3) Polymorphism
  Context: Assume a Predator's average speed is determined simply by the Predator's
  age and max speed (maxSpeed / age).

  If a human is faster than a predator's average speed, the human can "eat"
  the predator. Else, if the predator is faster than the human, the human will "be eaten".

  Implement the function eatOrBeEaten() that takes as parameters
  a) the speed of a specific human, and b) any Predator of a specific
  age (eg. Lion(20)), and returns "eat" if the human is fater than the Predator or "be eaten"
  if the predator is faster than the human.
  */
trait Predator {
  val maxSpeed: Int
  def age: Int
  def avgSpeed: Int = maxSpeed / age
}
case class Cheetah(age: Int) extends Predator {
  override val maxSpeed: Int = 20
}
case class Lion(age: Int) extends Predator {
  override val maxSpeed: Int = 30
}

def eatOrBeEaten(speedOfHuman: Int, predator: Predator): String = {
  //take a human's speed & a predator as function parameters
  if (speedOfHuman > predator.avgSpeed) "eat"
  else "be eaten"
}

assert(eatOrBeEaten(2, Lion(30)).contentEquals("eat"))
assert(eatOrBeEaten(1, Lion(30)).contentEquals("be eaten"))
assert(eatOrBeEaten(1, Cheetah(30)).contentEquals("eat"))


/**
4) For-Comprehension
  Using For-Comprehension, implement the function sumOptionsFor(). The function takes 3 optional
  integers (Option[Int]) as arguments, and should sums the value of the these integers.
  * If only two (or one) integers exist, they should still be summed.

  To understand the power of Scala's For-Comprehension, do the same but using Map & Flatmap. Implement
  this in the sumOptionsMap() function.
  */
def sumOptionsFor(num1Opt: Option[Int], num2Opt: Option[Int], num3Opt: Option[Int]): Option[Int] = {
  //implement using For-Comp.
  for {
    num1 <- num1Opt
    num2 <- num2Opt
    num3 <- num3Opt
  }  yield num1 + num2 + num3
}

assert(sumOptionsFor(Some(1), Some(2), Some(3)).contains(6))
assert(sumOptionsFor(Some(1), Some(2), None).isEmpty)
assert(sumOptionsFor(Some(1), None, None).isEmpty)
assert(sumOptionsFor(None, None, None).isEmpty)

def sumOptionsMap(num1Opt: Option[Int], num2Opt: Option[Int], num3Opt: Option[Int]): Option[Int] = {
  //implement using map & flatmap
  num1Opt.flatMap(x1 => num2Opt.flatMap(x2 => num3Opt.map(x3 => x1 + x2 +x3)))
}

assert(sumOptionsMap(Some(1), Some(2), Some(3)).contains(6))
assert(sumOptionsMap(Some(1), Some(2), None).isEmpty)
assert(sumOptionsMap(Some(1), None, None).isEmpty)
assert(sumOptionsMap(None, None, None).isEmpty)


/**
5) Case Classes, Pattern Matching and Options *note, this is Week 4 content of Functional Programming
  Principles in Scala course.

  Context: An 'Ingestion' row is extracted from the ADSS database. Based on previous ingestion activity,
  this Ingestion can have 1) only a rule, 2) only an outcome, 3) both of them, or 4) neither of them.
  using pattern matching, implement the interpretRow() function so that it return the required Actions
  that must be performed on the Ingestion it receives.

  Apply the functions below to determine the required actions per Ingestion, based on the
  following Ingestion data scenarios.
  When the Ingestion has:
  a) Only a Rule             -> use useRule()
  b) Only an Outcome         -> use useOutcome()
  c) Both a Rule and Outcome -> use useBoth()
  d) Any other scenario      -> no Action to be returned
  */
case class Rule(name: String)
case class Outcome(result: String)
case class Ingestion(rule: Option[Rule], outcome: Option[Outcome])
case class Action(doThis: String)

def useRule(rule: Rule): List[Action] = {
  //DO NOT IMPLEMENT THIS FUNCTION, ONLY USE THIS FUNCTION IN interpretRow()
  ???
}
def useOutcome(rule: Outcome): List[Action] = {
  //DO NOT IMPLEMENT THIS FUNCTION, ONLY USE THIS FUNCTION IN interpretRow()
  ???
}
def useBoth(rule: Rule, outcome: Outcome): List[Action] = {
  //DO NOT IMPLEMENT THIS FUNCTION, ONLY USE THIS FUNCTION IN interpretRow()
  ???
}

def interpretRow(row: Ingestion): List[Action] = {
  //Implement this function using pattern matching
  row match {
    case Ingestion(Some(rule), None) => useRule(rule)
    case Ingestion(None, Some(outcome)) => useOutcome(outcome)
    case Ingestion(Some(rule), Some(outcome)) => useBoth(rule, outcome)
    case _ => List.empty
  }
}