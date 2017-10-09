package example

object First extends RunFirst with App {
  run()
}

// Basic case-class implementation
trait RunFirst {
  case class Address(houseNumber: Int, street: String, town: String, postcode: String)

  case class Person(name: String, address: Address) {

    def incrementHouseNumber(): Person =
      this.copy(
        address = this.address.copy(
          houseNumber = this.address.houseNumber + 1
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

    def concatToTown(city: String): Person =
      this.copy(
        address = this.address.copy(
          town = this.address.town.concat(city)
        )
      )
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
    println(egPerson)
    println(egPerson.incrementHouseNumber())
    println(egPerson.incrementHouseNumber2())
    println(egPerson.concatToTown(", LONDON"))
  }
}
