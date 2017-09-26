package example

object Master extends RunThird with App {
  run()
}

trait RunThird {
  trait Lens[A, B] { self =>
    def get(a: A): B
    def set(a: A, b: B): A = modify(a, _ => b)
    def modify(a: A, b2b: B => B): A = {
      val newB = b2b(get(a))
      set(a, newB)
    }

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

  trait WithLens[A] { self: A =>
    def get[B](lens: Lens[A, B]): B = lens.get(self)
    def set[B](lens: Lens[A, B])(newB: B): A = lens.set(self, newB)
    def modify[B](lens: Lens[A, B])(b2b: B => B): A = lens.modify(self, b2b)
  }

  case class Address(houseNumber: Int, street: String, town: String, postcode: String) extends WithLens[Address] {
    def getHouseNumber: Int = houseNumber
    def setHouseNumber(newHouseNumber: Int) = this.copy(houseNumber = newHouseNumber)
    def modifyHouseNumber(f: Int => Int) = this.setHouseNumber(f(this.getHouseNumber))
  }
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
  case class Person(name: String, address: Address) extends WithLens[Person] {
    import Person._

    def incrementHouseNumber(): Person =
      this.copy(
        address = this.address.copy(
          houseNumber = this.address.houseNumber + 1
        )
      )

    def concatToTown(city: String): Person =
      this.copy(
        address = this.address.copy(
          town = this.address.town.concat(city)
        )
      )

    def incrementHouseNumber2(): Person =
      Person(
        name = this.name,
        address = Address (
          houseNumber = this.address.houseNumber + 1,
          street = this.address.street,
          town = this.address.town,
          postcode = this.address.postcode
        )
      )

    def getAddress: Address = address
    def setAddress(newAddress: Address) = this.copy(address = newAddress)
    def modifyAddress(f: Address => Address) = this.setAddress(f(this.getAddress))

    def incrementHouseNumber3() = modifyAddress (address =>
      address.modifyHouseNumber (houseNumber =>
        houseNumber + 1
      )
    )

    import Address._
    def incrementHouseNumber4() = addressLens.join(houseNumberLens).modify(this, houseNumber => houseNumber + 1)
    def concatToTown2(city: String) = addressLens.join(townLens).modify(this, town => town.concat(city))

    def incrementHouseNumber5() = this.modify(addressLens.join(houseNumberLens))(houseNumber => houseNumber + 1)
    def concatToTown3(city: String) = this.modify(addressLens.join(townLens))(town => town.concat(city))
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

    println(egPerson.incrementHouseNumber3())
    println(egPerson.incrementHouseNumber4())
    println(egPerson.concatToTown2(", LONDON"))

    println(
      egPerson.set(
        addressLens.join(townLens)
      )("Islington")
    )
  }

}
