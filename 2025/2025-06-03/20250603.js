function chart(data) {
  // Set the dimensions of the chart
  const width = 600;
  const height = 400;
  const marginTop = 60;
  const marginRight = 20;
  const marginLeft = 20;
  const marginBottom = 30;
  const iconSize = 24;

  // Select the chart container and clear any existing content
  const chartContainer = d3.select("#chart");

  const x = d3
    .scaleLinear()
    .domain([0.5, 10.5])
    .range([marginLeft, width - marginRight]);

  const y = d3
    .scaleLinear()
    .domain([0.5, 10.5])
    .range([height - marginBottom, marginTop]);

  // Create the SVG container for the new pie chart
  const svg = chartContainer
    .append("svg")
    .attr("viewBox", `0 0 ${width} ${height}`)
    .attr("preserveAspectRatio", "xMidYMid meet")
    .style("width", "100%")
    .style("height", "auto")
    .append("g");

  // Plot goes here
  d3.xml("book-solid.svg").then((iconXml) => {
  const baseIconNode = iconXml.documentElement;
  
  data.forEach((d) => {
    const svgNode = baseIconNode.cloneNode(true);

    d3.select(svgNode)
      .attr("x", x(d.x) - iconSize / 2)
      .attr("y", y(d.y) - iconSize / 2)
      .attr("width", iconSize)
      .attr("height", iconSize)
      .style("fill", d.colour)
      .style("opacity", 1);

    svg.node().appendChild(svgNode);
  });
});

  // Title
  svg.append("text")
      .attr("text-anchor", "left")
      .attr("y", 20)
      .attr("x", marginLeft)
      .text("Title.")
      .style("font-size", "16px")
      .style("font-weight", "bold");

  // Subtitle
  svg.append("text")
      .attr("text-anchor", "left")
      .attr("y", 40)
      .attr("x", marginLeft)
      .text("Subtitle")
      .style("font-size", "12px");

  // Caption
  svg.append("text")
      .attr("text-anchor", "left")
      .attr("y", height - 10)
      .attr("x", marginLeft)
      .text("Data: ")
      .style("font-size", "10px");
}

d3.csv("data.csv", (d) => ({
    x: +d.x,
    y: +d.y,
    colour: d.colour,
})).then((data) => {
  chart(data);
});
