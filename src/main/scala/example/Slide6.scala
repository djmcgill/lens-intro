package example

object Sixth extends RunSixth with App {
  run()
}

// Bonus
trait RunSixth {
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

  // What if you wanted to change the type of a field?
  def fstLens[A, B]: Lens[(A, B), A] = ???
  def sndLens[A, B]: Lens[(A, B), B] = ???

  def caadr[A, B, C, D]: Lens[(((A, B), C), D), B] = fstLens.join(fstLens.join(sndLens))

  // What if the element doesn't exist?
  def lensAt[A](i: Int): Lens[Array[A], A] = ???
  //  def safeLensAt[A](i: Int): Lens[Array[A], Option[A]] maybe? What happens if you set None?



  def run() = {
    val tuple = ((("foo", 1), Nil), 0.1)
    println(
      caadr.get(tuple)
    )
    println(
      caadr.modify(tuple, (n: Int) => n-1)
    )
    val array = Array(1, 2, 3)
    println(
      lensAt(2).set(array, 5)
    )
  }

}
