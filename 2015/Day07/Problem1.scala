object Problem1 extends App {
    val circuit = buildCircuit(scala.io.Source.fromFile(args(0)).getLines);
    println(circuit(args(1))());

    private def buildCircuit(input: Iterator[String]): scala.collection.mutable.Map[String, ()=> Int] = {
        return CircuitBuilder.build(input);
    }
}