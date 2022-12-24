import java.io.File
import kotlin.math.*

class Node(val value: Long, var next: Node?, var prev: Node?) {}

fun addNode(value: Long, predecessor: Node?): Node {
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
    val numbers = File(args[0]).readLines().map { it.toLong() }
    println(problem1(numbers))
    println(problem2(numbers))
}

fun problem1(numbers: List<Long>): Long {
    return solve(numbers, 1)
}

fun problem2(numbers: List<Long>): Long {
    return solve(numbers.map { it * 811589153L }, 10)
}

fun solve(numbers: List<Long>, reps: Int): Long {
    var nodes = mutableListOf<Node>()
    for (n in numbers) {
        nodes.add(addNode(n, nodes.lastOrNull()))
    }
    for (i in 0 until reps) {
        mixNodes(nodes)
    }

    val startNode = nodes.first { it.value == 0L }
    return getNode(startNode, 1000 % nodes.size).value +
            getNode(startNode, 2000 % nodes.size).value +
            getNode(startNode, 3000 % nodes.size).value
}

fun mixNodes(nodes: List<Node>) {
    for (n in nodes) {
        if (n.value == 0L) {
            continue
        }
        if (n.value > 0 && (n.value % (nodes.size - 1)) != 0L) {
            var tmp = removeNode(n)
            var count = n.value % (nodes.size - 1)
            do {
                tmp = tmp.next!!
                count--
            } while (count > 0)
            insertAfter(n, tmp)
        } else if (n.value < 0 && ((-n.value) % (nodes.size - 1)) != 0L) {
            var tmp = removeNode(n)
            var count = (-n.value) % (nodes.size - 1)
            do {
                tmp = tmp.prev!!
                count--
            } while (count > 0)
            insertBefore(n, tmp)
        }
    }
}
