class Player(var HitPoints: Int, val Damage: Int, var Mana: Int) {

    def copy(): Player = {
        new Player(HitPoints, Damage, Mana)
    }
}

object Problem1 extends App {
    val spellMap = Map("Magic Missile" -> 53, "Drain" -> 73, "Shield" -> 113, "Poison" -> 173, "Recharge" -> 229)

    // println(battle(new Player(10, 0, 250), new Player(13, 8, 0), List("Poison", "Magic Missile")))
    // println(battle(new Player(10, 0, 250), new Player(14, 8, 0),
        // Array("Recharge", "Shield", "Drain", "Poison", "Magic Missile")))

    val boss = getBoss(scala.io.Source.fromFile(args(0)).getLines)
    val wizard = new Player(50, 0, 500)
    println(findMinCostToWin(wizard, boss))

    //println(findMinCostToWin(new Player(10, 0, 250), new Player(14, 8, 0)))

    private def findMinCostToWin(wizard: Player, boss: Player): Int = {
        var spellNames = Array("Magic Missile", "Drain", "Shield", "Poison", "Recharge")
        var spells = Array.fill(32)(0)

        var minCost = Int.MaxValue

        do
        {
            val spellSeq = spells.map(spellNames(_)).toArray

            val result = battle(wizard.copy, boss.copy, spellSeq);
            minCost = Math.min(minCost, result._1)
            if (result._1 != Int.MaxValue) {
                println(result)
            }

            // this way we can skip all the spell combinations that are worse than the current one
            increment(spells, result._2)
            (result._2 + 1 to spells.size - 1).foreach(spells(_) = 0)
        }
        while(spells.sum != 0)

        return minCost
    }

    private def increment(spells: Array[Int], position: Int): Unit = {
        if (position == -1) {
            return;
        }

        spells(position) = (spells(position) + 1) % 5

        if (position > 0 && (spells(position) == 2 || spells(position) == 3) &&
            ((Math.max(0, position - 2) to position - 1).foldLeft(false)((t, v) => t || spells(v) == spells(position)))) {

            increment(spells, position)
        }

        // TODO: A check for concurrent recharge is missing

        if (spells(position) == 0) {
            increment(spells, position - 1)
        }
    }

    private def battle(wizard: Player, boss: Player, spells: Array[String]): (Int, Int) = {
        var roundNumber = 0
        while (round(roundNumber, wizard, boss, spells)) {
            roundNumber += 1
        //    println
        }

        // println(s"${wizard.HitPoints} ${boss.HitPoints}")

        if (wizard.HitPoints > 0 && boss.HitPoints <= 0) {
            return (spells.take(roundNumber + 1).map(spellMap(_)).sum, roundNumber)
        }
        else {
            return (Int.MaxValue, roundNumber)
        }
    }

    private def round(roundNumber: Int, wizard: Player, boss: Player, spells: Array[String]): Boolean = {
        playerTurn(roundNumber, wizard, boss, spells) && bossTurn(roundNumber, wizard, boss, spells)
    }

    private def playerTurn(roundNumber: Int, wizard: Player, boss: Player, spells: Array[String]): Boolean = {
/*
        println("-- Player turn --")
        println(s"- Player has ${wizard.HitPoints} hit points, x armor, ${wizard.Mana} mana")
        println(s"- Boss has ${boss.HitPoints} hit points")
*/

        // Uncomment this to solve the second part of the puzzle (level hard)
        // wizard.HitPoints -= 1

        if (wizard.HitPoints <= 0 || wizard.Mana < spellMap(spells(roundNumber))) {
            return false;
        }

        val lastingSpells = spells.slice(roundNumber - 3, roundNumber)
//        println(s"lasting spells: ${lastingSpells}")

        if (lastingSpells.contains("Poison")) {
//            println("Poison deals 3 damage;")
            boss.HitPoints -= 3
        }

        if ((lastingSpells.contains("Recharge")) && (lastingSpells.size < 3 || lastingSpells.head != "Recharge")) {
//            println(s"Recharge provides 101 mana")
            wizard.Mana += 101
        }

//        println(s"Player casts ${spells(roundNumber)}")
        wizard.Mana -= spellMap(spells(roundNumber))

        spells(roundNumber) match {
            case "Magic Missile" => boss.HitPoints -= 4
            case "Drain" => { boss.HitPoints -= 2; wizard.HitPoints += 2 }
            case _ =>
        }

//        println
        return boss.HitPoints > 0
    }

    private def bossTurn(roundNumber: Int, wizard: Player, boss: Player, spells: Array[String]): Boolean = {
        val lastingSpells = spells.slice(roundNumber - 2, roundNumber + 1)
        var armor = if (lastingSpells.contains("Shield")) 7 else 0

/*
        println("-- Boss turn --")
        println(s"- Player has ${wizard.HitPoints} hit points, ${armor} armor, ${wizard.Mana} mana")
        println(s"- Boss has ${boss.HitPoints} hit points")
        println(s"lasting spells: ${lastingSpells}")
*/

        if (lastingSpells.contains("Poison")) {
            // println("Poison deals 3 damage;")
            boss.HitPoints -= 3
        }

        if (lastingSpells.contains("Recharge")) {
            // println(s"Recharge provides 101 mana")
            wizard.Mana += 101
        }

        if (boss.HitPoints > 0) {
            // println(s"Boss attacks for Math.max(1, ${boss.Damage} - ${armor}) = ${Math.max(1, boss.Damage - armor)} damage.")
            wizard.HitPoints -= Math.max(1, boss.Damage - armor)
        }

        // println

        return wizard.HitPoints > 0 && boss.HitPoints > 0
    }

    private def getBoss(inputLines: Iterator[String]): Player = {
        val inputPattern = """(.+): (\d+)""".r

        val input = inputLines.map(l => l match {
            case inputPattern(n, v) => (n, v.toInt)
        }).toMap

        new Player(input("Hit Points"), input("Damage"), 0)
    }
}
