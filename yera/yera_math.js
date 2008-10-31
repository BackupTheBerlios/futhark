with (Yera) {
    var YeraMath = box (function () {
	with (unbox (YeraCore)) {
	    
	    var $unpi$un = box (function () {
		return Math.PI;
	    });

	    var pi = box (function () {
		return unbox (lift0) ($unpi$un);
	    });

	    var $une$un = box (function () {
		return Math.E;
	    });

	    var e = box (function () {
		return unbox (lift0) ($une$un);
	    });

	    var $unminValue$un = box (function () {
		return Number.MIN_VALUE;
	    });

	    var minValue = box (function () {
		return unbox (lift0) ($unminValue$un);
	    });

	    var $unmaxValue$un = box (function () {
		return Number.MAX_VALUE;
	    });

	    var maxValue = box (function () {
		return unbox (lift0) ($unmaxValue$un);
	    });
	    
	    var $unNaN$un = box (function () {
		return Number.NaN;
	    });

	    var NaN = box (function () {
		return unbox (lift0) ($unNaN$un);
	    });

	    var $uninfinity$un = box (function () {
		return Number.POSITIVE_INFINITY;
	    });

	    var infinity = box (function () {
		return unbox (lift0) ($uninfinity$un);
	    });

	    var abs$un = box (function () {
		return function (x) {
		    return Math.abs (unbox (x));
		}
	    });

	    var abs = box (function () {
		return unbox (lift1) (abs$un);
	    });

	    var sin$un = box (function () {
		return function (x) {
		    return Math.sin (unbox (x));
		}
	    });

	    var sin = box (function () {
		return unbox (lift1) (sin$un);
	    });

	    var cos$un = box (function () {
		return function (x) {
		    return Math.cos (unbox (x));
		}
	    });

	    var cos = box (function () {
		return unbox (lift1) (cos$un);
	    });
	    
	    var tan$un = box (function () {
		return function (x) {
		    return Math.tan (unbox (x));
		}
	    });

	    var tan = box (function () {
		return unbox (lift1) (tan$un);
	    });
	    
	    var asin$un = box (function () {
		return function (x) {
		    return Math.asin (unbox (x));
		}
	    });

	    var asin = box (function () {
		return unbox (lift1) (asin$un);
	    });

	    var acos$un = box (function () {
		return function (x) {
		    return Math.acos (unbox (x));
		}
	    });

	    var acos = box (function () {
		return unbox (lift1) (acos$un);
	    });
	    
	    var atan$un = box (function () {
		return function (x) {
		    return Math.atan (unbox (x));
		}
	    });

	    var atan = box (function () {
		return unbox (lift1) (atan$un);
	    });

	    var atan2$un = box (function () {
		return function (x) {
		    return Math.atan2 (unbox (x));
		}
	    });

	    var atan2 = box (function () {
		return unbox (lift1) (atan2$un);
	    });

	    var exp$un = box (function () {
		return function (x) {
		    return Math.exp (unbox (x));
		}
	    });

	    var exp = box (function () {
		return unbox (lift1) (exp$un);
	    });

	    var log$un = box (function () {
		return function (x) {
		    return Math.log (unbox (x));
		}
	    });

	    var log = box (function () {
		return unbox (lift1) (log$un);
	    });

	    var ceil$un = box (function () {
		return function (x) {
		    return Math.ceil (unbox (x));
		}
	    });

	    var ceil = box (function () {
		return unbox (lift1) (ceil$un);
	    });

	    var floor$un = box (function () {
		return function (x) {
		    return Math.floor (unbox (x));
		}
	    });

	    var floor = box (function () {
		return unbox (lift1) (floor$un);
	    });

	    var round$un = box (function () {
		return function (x) {
		    return Math.round (unbox (x));
		}
	    });

	    var round = box (function () {
		return unbox (lift1) (round$un);
	    });
	    
	    var $unsqrt$un = box (function () {
		return function (x) {
		    return function (y) {
			return Math.sqrt (unbox (x), unbox (y));
		    }
		}
	    });

	    var sqrt = box (function () {
		return unbox (lift2) ($unsqrt$un);
	    });

	    var $unmax$un = box (function () {
		return function (x) {
		    return function (y) {
			return Math.max (unbox (x), unbox (y));
		    }
		}
	    });

	    var max = box (function () {
		return unbox (lift2) ($unmax$un);
	    });

	    var $unmin$un = box (function () {
		return function (x) {
		    return function (y) {
			return Math.min (unbox (x), unbox (y));
		    }
		}
	    });

	    var min = box (function () {
		return unbox (lift2) ($unmin$un);
	    });

	    var $unpow$un = box (function () {
		return function (x) {
		    return function (y) {
			return Math.pow (unbox (x), unbox (y));
		    }
		}
	    });

	    var pow = box (function () {
		return unbox (lift2) ($unpow$un);
	    });


	    
	    return {

		$unpi$un: $unpi$un,
		$une$un: $une$un, 
		$unmaxValue$un: $unmaxValue$un,
		$unminValue$un: $unminValue$un,
		$unNaN$un: $unNaN$un,
		$uninfinity$un: $uninfinity$un,

		pi:pi,
		e:e, 
		maxValue:maxValue,
		minValue:minValue,
		NaN:NaN,
		infinity: infinity,
		
	 	abs$un: abs$un,
		sin$un: sin$un,
		cos$un: cos$un,
		tan$un: tan$un,
		asin$un: asin$un,
		acos$un: acos$un,
		atan$un: atan$un,
		atan2$un :atan2$un,
		exp$un: exp$un,
		log$un: log$un,
		ceil$un: ceil$un,
		floor$un: floor$un,
		round$un: round$un,

		abs:abs,
		sin:sin,
		cos:cos,
		tan:tan,
		asin:asin,
		acos:acos,
		atan:atan,
		atan2:atan2,
		exp:exp,
		log:log,
		ceil:ceil,
		floor:floor,
		round:round,

		$unsqrt$un: $unsqrt$un,
		$unmin$un: $unmin$un,
		$unmax$un: $unmax$un,
		$unpow$un: $unpow$un,

		sqrt:sqrt,
		min:min,
		max:max,
		pow:pow
	    };
	}
    });
}
	