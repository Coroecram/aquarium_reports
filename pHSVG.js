function phGraph(phData, svgPH) {

  var x = d3.scaleTime().range([0, width]);
  var y = d3.scaleLinear().range([height, 0]);

  var valueLine = d3.line()
                    .x(function(d) { return x(d.datetime); })
                    .y(function(d) { return y(d.pH); });

  x.domain(d3.extent(data.datetime, function(d) { return d; }));
  y.domain([0, (d3.max(data.pH, function(d) { return d; }) + 0.3) ]);

  svgpH.append('path')
    .data([phData])
    .attr('class', 'line')
    .attr("d", valueLine);

  svgpH.append("g")
        .attr("transform", "translate(0," + height + ")")
        .call(d3.axisBottom(x));

  svgpH.append("g")
    .call(d3.axisLeft(y));
}
