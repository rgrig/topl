//  Inspired by
//    org.apache.tools.ant.Project(.tsort)
//  Sometimes you want nodes in a dag to be processed in topological order.

//  At the moment, it looks like it might be a significant challange to
//  check this property. Just a couple of TODOs:
//  TODO: How to handle the unbounded number of children?
//  TODO: Can we handle BFS?

class NodeIterator
  Bool hasNext()
  Node next()

class NodeList
  NodeIterator iterator()
  Unit add(Node n)

class Node
  var NodeList children

  Unit init()
    children := new

  Unit addChild(Node n)
    children.add(n)

  Unit process()

  Unit dfs()
    var NodeIterator i := children.iterator()
    var Bool hn
    do { hn := i.hasNext() } while hn
      var Node c := i.next()
      c.process()
    this.process()

main
  var Node n0 := new
  var Node n1 := new
  var Node n2 := new
  var Node n3 := new
  n0.init()
  n0.addChild(n1)
  n0.addChild(n2)
  n0.addChild(n3)
  n0.dfs()
