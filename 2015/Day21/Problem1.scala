class Player(val HitPoints: Int, val Damage: Int, val Armor: Int)

object Problem1 extends App {

    val fightFactors =
        EquipementStore.getEquipement()
        .map(e => e.foldLeft((0, 0, 0))((t, v) => (t._1 + v._2, t._2 + v._3, t._3 + v._4))) // fold equipement to (cost, damage, armor)
        .groupBy(f => (f._2, f._3))         // group by damage and armor
        .map(t => t._2.sortBy(_._1).head)   // get the cheapest from each group
        .toSeq.sortBy(_._1)                 // sort by cost

    val boss = getBoss(scala.io.Source.fromFile(args(0)).getLines);

    println(fightFactors.find(ff => fight(new Player(100, ff._2, ff._3), boss)))

    private def fight(hero: Player, boss: Player) = {
        turnsToWin(hero, boss) <= turnsToWin(boss, hero)
    }

    private def turnsToWin(attacker: Player, defender: Player) = {
        val damagePerTurn = Math.max(1, attacker.Damage - defender.Armor)

        Math.ceil(defender.HitPoints / damagePerTurn).toInt
    }

    private def getBoss(inputLines: Iterator[String]): Player = {
        val inputPattern = """(.+): (\d+)""".r

        val input = inputLines.map(l => l match {
            case inputPattern(n, v) => (n, v.toInt)
        }).toMap

        new Player(input("Hit Points"), input("Damage"), input("Armor"))
    }
}