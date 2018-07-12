function phGraph(phData) {
  var valueLine = d3.line()
                    .x(function(d) { return x(d.datetime); })
                    .y(function(d) { return y(d.ph); });

  var svgpH = d3.select('#ph')
                  .attr('width', width + margin.left + margin.right)
                  .attr('height', height + margin.top + margin.bottom)
                .append('g')
                  .attr('transform', 'translate(' + margin.left + ',' + margin.top + ')');

  // console.log(dater);

  x.domain(d3.extent(data.datetime, function(d) { return d; }));
  y.domain([0, (d3.max(data.ph, function(d) { return d; }) + 3) ]);

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
