import com.asyaminor.functional.book.datastructures.{Cons, List, Nil}

sealed trait Animal

case class Dog(name: String) extends Animal

case class Bird(age: Int) extends Animal

val listAnimals : List[Dog] = List(Dog("berkecan"), Dog("black"))
val animals: List[Animal] = listAnimals

val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y case Cons(h, t) => h + List.sum(t)
  case _ => 101
}

val someList = List(1, 2, 3, 4, 5)

List.tail(someList)