function combine(key, values) {
  var total = {sum: 0, n: 0, min: Number.POSITIVE_INFINITY, max: Number.NEGATIVE_INFINITY};
  for (var i=0; i<values.length; i++) {
    var cur = values[i][key];
    if (cur.n == 0) continue;
    if (cur.min < total.min) {
      total.min = cur.min;
      total.min_time = cur.min_time;
    }
    if (cur.max > total.max) {
      total.max = cur.max;
      total.max_time = cur.max_time;
    }
    total.sum += cur.sum;
    total.n += cur.n;
  }
  return total;
}

function (key, values, rereduce) {
  return {box: combine("box",values), in: combine("in",values), out: combine("out",values)};
}
