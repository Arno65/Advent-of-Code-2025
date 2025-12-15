//  Advent of Code 2025 - Day 11 part One and Two
//      Solutions in Swift
//      (Ter leering ende vermaeck...)
//
//      Part one:   The number of different paths leading from you to out is:   508
//      Part two:   The number of different paths visiting both dac and fft is: 315116216513280
//
// For a generic solution this code still eats lots of RAM :-(
// Like with the Haskell code there is a limit set on the depth, a maximum number of steps.
// With that it's about 18 times faster than my Haskell version and will work on machines 
// with only 8 Gb of RAM.
//
//  (cl) by Arno Jacobs, 2025-12-15

import Cocoa
 
// Helper for FFT -> DAC (the Haskell code takes the first 5 values > 0)
let maxDepth: Int       = 16
let filename: String    = "Code/Advent/AoC_2025/data/inputDay11_2025.txt"

// The read lines function
func readLines () -> [String] {
    var wiring: [String] = []
    if let dir = FileManager.default.urls (for: .documentDirectory, in: .userDomainMask).first
    {   let fileURL = dir.appendingPathComponent (filename)
        do {    let readGrid = try String (contentsOf: fileURL, encoding: .utf8)            
                let lines = readGrid.split (separator: "\n")
                for line in lines { wiring.append (String (line)) }
            }
        catch { print ("Read error?") }
    }
    return (wiring)
}

func uniqueNodes (_ allNodes: [String]) -> [String] {
    var uniqueNodes: [String] = [] 
    for node in allNodes {
        if !(uniqueNodes.contains (node)) { uniqueNodes.append(node) }
    }
    return (uniqueNodes)
}

func noColonAndSpaces (_ name: String) -> String {
    var rs: String = ""
    /// remove the colons
    for c in name { if (c != ":" && c != " ") { rs.append (c) }}
    return (rs)
}

func getAllNodes (_ wiring: [String]) -> [String] {
    var nodeNames: [String] = []
    /// First collect ALL nodes
    for connections in wiring {
        for inode in (connections.components (separatedBy: " ")) {
            let node = noColonAndSpaces (inode)
            if (node.count > 0) { nodeNames.append (node) }
        }
    }
    /// Create a list of unique nodes - sorted
    nodeNames = Array( Set (nodeNames)) 
    nodeNames.sort()
    return (nodeNames)
}

func getNodeIndex (_ nodeNames: [String], _ nodeName : String) -> Int16 {
    var ix:Int16 = 0
    for node in nodeNames {
        if (node == nodeName) { break }
        ix += 1
    }
    return ix
}

func getNodes (_ wiring: [String], _ names: [String] ) -> [Int16] {
    var indexes: [Int16] = []
    for nodes in wiring {
        let node = nodes.components (separatedBy: ":")[0]
        indexes.append (getNodeIndex (names, node))
    }
    return (indexes)
}

func getConnections(_ wiring: [String], _ names: [String] ) -> [[Int16]] {
    var connections: [[Int16]] = []
    for nodes in wiring {
        let connectionsList = nodes.components (separatedBy: ":")[1]
        let connectionNames = connectionsList.components (separatedBy: " ")
        var connectionIndexes: [Int16] = [] 
        for connectionName in connectionNames {
            if (connectionName != "") { 
                connectionIndexes.append (getNodeIndex (names, connectionName)) 
            }
        }
        connections.append (connectionIndexes)
    }
    return (connections) 
}

func nodeIndex (_ nodes: [Int16], _ node: Int16 ) -> Int {
    var ix: Int = 0
    for checkNode in nodes {
        if (checkNode == node) { break }
        ix += 1
    }
    return (ix)
}

func backwards (_ nodes: [Int16], _ connections: [[Int16]]) -> [[Int16]] {
    var backwardNodes: [[Int16]] = []
    var helper: [Int16] = []
    for fromNode in nodes {
        helper.removeAll ()
        var ix:Int = 0
        for checkNodes in connections {
            if (checkNodes.contains (fromNode)) { helper.append (nodes[ix]) }
            ix += 1
        }
        backwardNodes.append (helper)
    }
    return (backwardNodes)
}

func travel (_ startStep: Int16, _ end: Int16, _ nodes: [Int16], _ connections: [[Int16]] ) -> Int {
    var endCount: Int           = 0
    var currentSteps: [Int16]   = [startStep]
    var restarts: [Int16]       = []
    var nextSteps: [Int16]      = []
    var stepCounter: Int        = 0
    repeat {
        // get all the next steps
        nextSteps.removeAll ()
        for start in currentSteps {
            let ix = nodeIndex (nodes, start)
            nextSteps += connections[ix]
        }
        // check for end-points
        restarts.removeAll ()
        for start in nextSteps { 
            if (start == end)   { endCount += 1 } 
            else                { restarts.append (start) }
        }
        currentSteps = restarts
        stepCounter += 1
    } while (currentSteps.count != 0) && (stepCounter < maxDepth)
    return (endCount)
}

//  ------------- Main program -------------
//
func AOC2025_day11() {
    let wiring:    [String] = readLines()
    let nodeNames: [String] = getAllNodes(wiring)    
    let youIx: Int16 = getNodeIndex (nodeNames, "you")
    let svrIx: Int16 = getNodeIndex (nodeNames, "svr")
    let fftIx: Int16 = getNodeIndex (nodeNames, "fft")
    let dacIx: Int16 = getNodeIndex (nodeNames, "dac")
    let outIx: Int16 = getNodeIndex (nodeNames, "out")
    let nodes:             [Int16]  = getNodes (wiring, nodeNames)
    let nodesConnections: [[Int16]] = getConnections (wiring, nodeNames)

    print ("\nAdvent of Code 2025 - day 11  (Swift)\n")
    let partOne = travel (youIx, outIx, nodes, nodesConnections)
    print ("Part one: The number of different paths leading from you to out is:   \(partOne)")

    let dac2out = travel (dacIx, outIx, nodes, nodesConnections)
    let fft2dac = travel (fftIx, dacIx, nodes, nodesConnections)
    let svr2fft = travel (fftIx, svrIx, nodes, backwards(nodes, nodesConnections))
    print ("Part two: The number of different paths visiting both dac and fft is: \(dac2out * fft2dac * svr2fft )")

    print ("0K.\n")
}

AOC2025_day11 ()

// End of code

