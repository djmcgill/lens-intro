package example

object Third extends RunThird with App {
  run()
}

trait RunThird {
  trait Lens[A, B] { self =>
    def get(a: A): B

    def modify(a: A, b2b: B => B): A = {
      val oldB = get(a)
      val newB = b2b(oldB)
      set(a, newB)
    }

    def set(a: A, b: B): A = modify(a, _ => b)

    def join[C](subLens: Lens[B, C]): Lens[A, C] = new Lens[A, C] {
      override def get(a: A): C = {
        val b: B = self.get(a)
        subLens.get(b)
      }

      override def modify(a: A, c2c: C => C): A = {
        val modifyB: B => B = b => subLens.modify(b, c2c)
        self.modify(a, modifyB)
      }
    }
  }


  case class Address(houseNumber: Int, street: String, town: String, postcode: String)
  object Address {
    val houseNumberLens: Lens[Address, Int] = new Lens[Address, Int] {
      override def get(address: Address): Int = address.houseNumber
      override def set(address: Address, newHouseNumber: Int): Address = address.copy(houseNumber = newHouseNumber)
    }


    val townLens: Lens[Address, String] = new Lens[Address, String] {
      override def get(address: Address): String = address.town
      override def set(address: Address, newTown: String): Address = address.copy(town = newTown)
    }
  }

  object Person {
    val addressLens: Lens[Person, Address] = new Lens[Person, Address] {
      override def get(person: Person): Address = person.address
      override def set(person: Person, newAddress: Address): Person = person.copy(address = newAddress)
    }
  }
  case class Person(name: String, address: Address) {
    import Person._
    import Address._

    def incrementHouseNumber(): Person = addressLens.join(houseNumberLens).modify(this, houseNumber => houseNumber + 1)
    def concatToTown(city: String): Person = addressLens.join(townLens).modify(this, town => town.concat(city))
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

  def run(): Unit = {
    import Address._
    import Person._

    println(egPerson)
    println(egPerson.incrementHouseNumber())
    println(egPerson.concatToTown(", LONDON"))

    println(
      addressLens.join(townLens).set(egPerson, "Islington")
    )
  }

}
