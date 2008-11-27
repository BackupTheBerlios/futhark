var Dom = function () {
    var makeElementNode = document.createElementNS;

    var makeTextNode = document.createTextNode;

    var setAttribute = function (node, namespace, name, value) {
	return name == style && value instanceof Object ? 
	    for (var j in value) node.style [j] = value [j] :
	    node.setAttributeNS (namespace, name, value);
    }
    
    var delAttribute = function (node, namespace, name) {
	return name == style ? 
	    for (var j in node.style) node.style [j] = null:
	    node.removeAttributeNS (namespace, name);
    }

    return {
	makeElementNode: makeElementNode,
	makeTextNode: makeTextNode,
	setAttribute: setAttribute,
	delAttribute: delAttribute
    }
}
   
    

    