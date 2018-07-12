function luxGraph() {
  var valueLine = d3.line()
                    .x(function(d) { return x(d.datetime); })
                    .y(function(d) { return y(d.lux); });

  var svgLux = d3.select('#lux')
                  .attr('width', width + margin.left + margin.right)
                  .attr('height', height + margin.top + margin.bottom)
                .append('g')
                  .attr('transform', 'translate(' + margin.left + ',' + margin.top + ')');

  // console.log(dater);

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
