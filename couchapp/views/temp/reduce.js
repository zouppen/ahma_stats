// Combines values into global (all-time) temperature readings

function combine(key, values) {
    "use strict";

    var i;
    var total = { sum: 0,
		  n: 0,
		  min: Number.POSITIVE_INFINITY,
		  max: Number.NEGATIVE_INFINITY,
		  start_time: Number.POSITIVE_INFINITY,
		  end_time: 0
		};

    for (i=0; i<values.length; i++) {
	var cur = values[i][key];

	// If it contains no measurements, do not tamper values at all
	if (cur === null) continue;

	// Minimum and maximum race
	if (cur.min < total.min) {
	    total.min = cur.min;
	    total.min_time = cur.min_time;
	}
	if (cur.max > total.max) {
	    total.max = cur.max;
	    total.max_time = cur.max_time;
	}

	// Average calculation
	total.sum += cur.sum;
	total.n += cur.n;

	// Range calculation
	if (cur.start_time < total.start_time) total.start_time = cur.start_time;
	if (cur.end_time > total.end_time) {
	    total.end_time = cur.end_time;
	    total.end_value = cur.end_value;
	}
    }

    if (total.n === 0) return null;
    return total;
}

function (key, values, rereduce) {
    "use strict";
    return { box: combine("box",values),
	     in: combine("in",values),
	     out: combine("out",values)
	   };
}
