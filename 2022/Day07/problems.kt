import java.io.File
import kotlin.collections.mutableListOf

class Node constructor(val name: String, var size: Int, val parent: Node?) {
    val childNodes = mutableListOf<Node>()
    fun isDir(): Boolean = childNodes.size > 0
}

fun main(args: Array<String>) {
    val lines = File(args[0]).readLines()
    val fs = buildFS(lines)
    println(problem1(fs))
    println(problem2(fs))
}

fun buildFS(commands: List<String>): Node {
    val root = Node("/", 0, null)
    var currentNode = root

    for (cmd in commands) {
        val components = cmd.split(" ")
        if (components[1].equals("cd")) {
            when (components[2]) {
                "/" -> currentNode = root
                ".." -> currentNode = currentNode.parent!!
                else -> {
                    val dirNode = Node(components[2], 0, currentNode)
                    currentNode.childNodes.add(dirNode)
                    currentNode = dirNode
                }
            }
        } else if (components[1].equals("ls")) {} else {
            if (components[0].equals("dir")) {
                // not needed
            } else {
                var fileNode = Node(components[1], components[0].toInt(), currentNode)
                currentNode.childNodes.add(fileNode)
            }
        }
    }
    setSize(root)
    return root
}

fun setSize(node: Node): Int {
    var size: Int = 0
    for (child in node.childNodes) {
        setSize(child)
        size += child.size
    }
    node.size = size + node.size
    return node.size
}

fun printTree(node: Node, indent: Int) {
    println("${" ".repeat(indent)}- ${node.name} (${node.size})")
    for (child in node.childNodes) {
        printTree(child, indent + 1)
    }
}

fun problem1(node: Node): Int {
    var size: Int = if (node.size > 100000) 0 else node.size
    for (child in node.childNodes.filter { it.isDir() }) {
        size += problem1(child)
    }
    return size
}

fun dirToDelete(node: Node, spaceToFree: Int, bestCandidate: Node): Node {
    var candidate =
            if (node.size > spaceToFree && node.size < bestCandidate.size) node else bestCandidate
    for (child in node.childNodes.filter { it.isDir() }) {
        candidate = dirToDelete(child, spaceToFree, candidate)
    }
    return candidate
}

fun problem2(node: Node): Int {
    val freeSpace = 70000000 - node.size
    val spaceToFree = 30000000 - freeSpace
    val dir = dirToDelete(node, spaceToFree, node)
    return dir.size
}
