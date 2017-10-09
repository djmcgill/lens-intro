package example

import monocle.macros.GenLens


object Seventh extends RunSeventh with App {
  run()
}

// Monocle
trait RunSeventh {
  // Lenses annotation
  object Address {
    val houseNumberLens = GenLens[Address](_.houseNumber)
    val townLens = GenLens[Address](_.town)
  }
  case class Address(houseNumber: Int, street: String, town: String, postcode: String)

  object Person {
    val addressLens = GenLens[Person](_.address)
  }
  case class Person(name: String, address: Address) {
    import Person._
    import Address._

    def incrementHouseNumber(): Person = addressLens.composeLens(houseNumberLens).modify(_ + 1)(this)
    def concatToTown(city: String): Person = addressLens.composeLens(townLens).modify(_.concat(city))(this)
  }

  val egPerson = Person (
    name = "DMC",
    address = Address (
      houseNumber = 3,
      street = "STREET_NAME",
      town = "TOWN_NAME",
      postcode = "POSTCODE"
    )
  )

  def run() = {
    import Person._
    import Address._

    println(egPerson)
    println(egPerson.incrementHouseNumber())
    println(egPerson.concatToTown(", LONDON"))

    println(
      addressLens.composeLens(townLens).set("Islington")(egPerson)
    )
  }

}
