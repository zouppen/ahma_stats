function build(ts,value) {
  var n = isNaN(value) ? 0 : 1;
  return { min: value, max: value, n: n, sum: value, min_time: ts, max_time: ts };
}

function(doc) {
  var ts = doc.timestamp;
  emit(ts, {box: build(ts,doc.temp_box),
            in: build(ts,doc.temp_in),
            out: build(ts,Math.min(doc.temp_north,doc.temp_south))});
}
