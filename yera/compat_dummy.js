var Dom = {
    makeElementNode: function (ns,nm) {return {namespace: ns, nodeValue: nm}},
    makeTextNode: function (t) {return t},
    setAttribute: function (node, ns, nm, vl) {node [ns + ":" + nm] = vl},
    delAttribute: function (node, ns, nm) {delete node [ns + ":" + nm]}
}
    