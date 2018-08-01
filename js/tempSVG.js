function tempGraph(tempData, svgTemp) {

  var x = d3.scaleTime().range([0, width]);
  var y = d3.scaleLinear().range([height, 0]);

  var valueLine = d3.line()
                    .x(function(d) { return x(d.datetime); })
                    .y(function(d) { return y(d.temp); });

  x.domain(d3.extent(data.datetime, function(d) { return d; }));
  y.domain([0, (d3.max(data.temp, function(d) { return d; }) + 3) ]);

  svgTemp.append('path')
    .data([tempData])
    .attr('class', 'line')
    .attr("d", valueLine);

  svgTemp.append("g")
        .attr("transform", "translate(0," + height + ")")
        .call(d3.axisBottom(x));

  svgTemp.append("g")
    .call(d3.axisLeft(y));
}
