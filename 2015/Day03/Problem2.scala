object Problem2 extends App {

    println(
        (args(0).grouped(2).map(_.head).scanLeft(new Coordinates(0, 0))((l, v) => newLocation(l, v)) ++
        args(0).drop(1).grouped(2).map(_.head).scanLeft(new Coordinates(0, 0))((l, v) => newLocation(l, v)))
        .toList.distinct.length);


    def newLocation(l: Coordinates, v: Char) : Coordinates = {
        return new Coordinates(
            l.x + (if (v == '<') -1 else if( v == '>') 1 else 0),
            l.y + (if (v == '^') -1 else if( v == 'v') 1 else 0));
    }
}