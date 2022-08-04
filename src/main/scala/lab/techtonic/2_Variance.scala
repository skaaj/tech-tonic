package lab.techtonic

import zio._

// COVARIANT +A (producer)
// CONTRAVARIANT -A (consumer)
// INVARIANT A

class Animal
class Dog extends Animal
class Cat extends Animal

class MyList[+A] // MyList = COVARIANT
val listOfAnimals: MyList[Animal] = new MyList[Dog]

trait Vet[-A] {
  def heal(animal: A): Boolean
}

def heal(vet: Vet[Dog], dog: Dog): Boolean = vet.heal(dog)

val dogVet: Vet[Dog] = new Vet[Animal] {
  override def heal(animal: Animal): Boolean = true
}

abstract class VetSeller[-A] {
  def heal(animal: A): Boolean
  def sell[B <: A](): B
}

val seller: VetSeller[Animal] = new VetSeller[Animal] {
  override def heal(animal: Animal): Boolean = true
  override def sell[B <: Animal](): B = ???
}
val dogSeller: VetSeller[Dog] = seller
val aDog: Dog = dogSeller.sell()

@main def varianceMain() =
  println(dogVet.heal(new Dog))
  val animalVeto: Vet[Animal] = new Vet { def heal(animal: Animal) = true }
  val dogVeto: Vet[Dog] = animalVeto

  //val maybeAnimal: Option[Animal] = None
  //val maybeDog: Option[Dog] = maybeAnimal

  heal(dogVeto, new Dog)
  heal(animalVeto, new Dog)