import java.io.File
import kotlin.math.*

class Node(val value: Int, var next: Node?, var prev: Node?) {}

fun addNode(value: Int, predecessor: Node?): Node {
    if (predecessor == null) {
        var newNode = Node(value, null, null)
        newNode.next = newNode
        newNode.prev = newNode
        return newNode
    }
    var newNode = Node(value, next = predecessor.next, prev = predecessor.next!!.prev)
    predecessor.next!!.prev = newNode
    predecessor.next = newNode
    return newNode
}

fun removeNode(node: Node): Node {
    node.prev!!.next = node.next
    node.next!!.prev = node.prev
    return node
}

fun insertAfter(node: Node, predecessor: Node) {
    node.next = predecessor.next
    node.prev = predecessor
    predecessor.next!!.prev = node
    predecessor.next = node
}

fun insertBefore(node: Node, successor: Node) {
    node.next = successor
    node.prev = successor.prev!!
    successor.prev!!.next = node
    successor.prev = node
}

fun printList(head: Node) {
    var node = head
    do {
        print("${node.value} ")
        node = node.next!!
    } while (node != head)
    println()
}

fun getNode(startNode: Node, count: Int): Node {
    var n = startNode
    for (i in 0 until count) {
        n = n.next!!
    }
    return n
}

fun main(args: Array<String>) {
    val numbers = File(args[0]).readLines().map { it.toInt() }
    println(problem1(numbers))
}

fun problem1(numbers: List<Int>): Int {
    var nodes = mutableListOf<Node>()
    for (n in numbers) {
        nodes.add(addNode(n, nodes.lastOrNull()))
    }

    for (n in nodes) {
        if (n.value == 0) {
            continue
        }
        var tmp = removeNode(n)
        if (n.value > 0) {
            var count = n.value
            do {
                tmp = tmp.next!!
                count--
            } while (count > 0)
            insertAfter(n, tmp)
        } else {
            var count = -n.value
            do {
                tmp = tmp.prev!!
                count--
            } while (count > 0)
            insertBefore(n, tmp)
        }
    }
    val startNode = nodes.first { it.value == 0 }
    return getNode(startNode, 1000 % nodes.size).value +
            getNode(startNode, 2000 % nodes.size).value +
            getNode(startNode, 3000 % nodes.size).value
}
