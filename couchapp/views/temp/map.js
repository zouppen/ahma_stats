// This maps a measurement document into singleton which contains
// maximum and minimum values, sum and n (for average), etc.

function build(ts,value) {
    "use strict";
    if (value === undefined) return null;
    return { min: value,       // Global minimum
	     max: value,       // Global maximum
	     n: 1,             // Number of samples
	     sum: value,       // Sum of temperatures
	     min_time: ts,     // Time of global minimum
	     max_time: ts,     // Time of global maximum
	     start_time: ts,   // First timestamp
	     end_time: ts,     // Last timestamp
	     end_value: value  // Last value recorded
	 };
}

function(doc) {
    "use strict";
    var ts = doc.timestamp;
    emit(ts, { box: build(ts,doc.temp_box),
               in: build(ts,doc.temp_in),
               out: build(ts,Math.min(doc.temp_north,doc.temp_south))
	     });
}
