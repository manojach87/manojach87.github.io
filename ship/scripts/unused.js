

function sortBars(ships,x,y){

    var x0 = x.domain(ships.sort(function(a, b) { return b.value - a.value;})
        .map(function(d) { return d[colorBy]; }))
        .copy();

    var svg = d3.select("#bar").select("svg");

    svg.selectAll(".bar")
        .sort(function(a, b) { return x0(a[colorBy]) - x0(b[colorBy]); });
    svg.selectAll(".bar")
    // .transition()
    // .delay(500)
        .attr("transform", function(d){
            return "translate("  + x0(d[colorBy]) + ", " +  y(d.value) + ")";});
    svg.select("#xAxis")
        .call(d3.axisBottom(x0))
};

function getHeaders(arr) {
    arr.forEach(function (row,i) {
        if(i==0) console.log(Object.keys(row));
    });
}


function drawLegend(legend){

    // draw legend colored rectangles
    legend.append("rect")
        .attr("x", width - 18)
        .attr("width", 18)
        .attr("height", 18)
        .style("fill", color)
        .on("mouseover", function(d){
            highlightCircle(d);
        })
        .on("mouseout",function(d){
            d3.selectAll("circle").transition().duration(200).style("opacity", 1);
        });

    // draw legend text
    legend.append("text")
        .attr("x", width - 24)
        .attr("y", 9)
        .attr("dy", ".35em")
        .style("text-anchor", "end")
        .style("font-size","10px")
        .text(function(d) { return d;})

}

function changeColorCategories(){
    console.log(arguments.callee.name);
    var x = document.getElementById("colorMenu").value;
    console.log(x);
    colorBy = x;

    var lbl = document.getElementById("idSidebarRight");
    console.log(lbl);
    var lblSvg = document.getElementById("svg");
    //lbl.removeChild(lblSvg)
    //d3.selectAll("#idSidebarRight").selectAll("svg").remove();

    svg.selectAll(".dot")
        .transition()
        .duration(500)
        .attr("cx", xMap)
        .attr("cy", yMap)
        .style("fill", function(d) {
            console.log(d);
            return color(cValue(d));})
        .attr("class", function(d){
            return "circle_" + cValue(d);
        })
    legend.transition().duration(500).data(color.domain())
        .enter().append("g")
        .attr("class", "legend")
        .attr("transform", function(d, i) {return "translate(0," + i * 20 + ")"; });

    // draw legend colored rectangles
    legend.append("rect")
        .attr("x", width - 18)
        .attr("width", 18)
        .attr("height", 18)
        .style("fill", color)
        .on("mouseover", function(d){
            highlightCircle(d);
        })
        .on("mouseout",function(d){
            d3.selectAll("circle").transition().duration(200).style("opacity", 1);
        });

    // draw legend text
    legend.append("text")
        .attr("x", width - 24)
        .attr("y", 9)
        .attr("dy", ".35em")
        .style("text-anchor", "end")
        .style("font-size","10px")
        .text(function(d) { return d;})

};