object Problem1 extends App {

    println(args(0).scanLeft(new Coordinates(0, 0))((l, v) => newLocation(l, v)).distinct.length);

    def newLocation(l: Coordinates, v: Char) : Coordinates = {
        return new Coordinates(
            l.x + (if (v == '<') -1 else if( v == '>') 1 else 0),
            l.y + (if (v == '^') -1 else if( v == 'v') 1 else 0));
    }
}