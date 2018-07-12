function luxGraph(luxData, svgLux) {

  var x = d3.scaleTime().range([0, width]);
  var y = d3.scaleLinear().range([height, 0]);

  var valueLine = d3.line()
                    .x(function(d) { return x(d.datetime); })
                    .y(function(d) { return y(d.lux); });

  x.domain(d3.extent(data.datetime, function(d) { return d; }));
  y.domain([0, (d3.max(data.lux, function(d) { return d; }) + 3) ]);

  svgLux.append('path')
    .data([luxData])
    .attr('class', 'line')
    .attr("d", valueLine);

  svgLux.append("g")
        .attr("transform", "translate(0," + height + ")")
        .call(d3.axisBottom(x));

  svgLux.append("g")
    .call(d3.axisLeft(y));
}
