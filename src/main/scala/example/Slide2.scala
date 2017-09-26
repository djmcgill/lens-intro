package example

object Second extends RunSecond with App {
  run()
}

trait RunSecond {
  case class Address(houseNumber: Int, street: String, town: String, postcode: String) {
    def getHouseNumber: Int = houseNumber
    def setHouseNumber(newHouseNumber: Int): Address = this.copy(houseNumber = newHouseNumber)
    def modifyHouseNumber(f: Int => Int): Address = this.setHouseNumber(f(this.getHouseNumber))

    def getTown: String = town
    def setTown(newTown: String): Address = this.copy(town = newTown)
    def modifyTown(f: String => String): Address = this.setTown(f(this.getTown))
  }

  case class Person(name: String, address: Address) {
    def getAddress: Address = address
    def setAddress(newAddress: Address): Person = this.copy(address = newAddress)
    def modifyAddress(f: Address => Address): Person = this.setAddress(f(this.getAddress))

    def incrementHouseNumber(): Person = modifyAddress (address =>
      address.modifyHouseNumber (houseNumber =>
        houseNumber + 1
      )
    )

    def concatToTown(city: String): Person = modifyAddress (address =>
      address.modifyTown(town =>
        town.concat(city)
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
    println(egPerson.concatToTown(", LONDON"))
  }
}
