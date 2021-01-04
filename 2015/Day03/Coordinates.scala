case class Coordinates(
    var x: Int = 0,
    var y: Int = 0
) {
    override def toString(): String = "(" + x + ", " + y + ")";
}