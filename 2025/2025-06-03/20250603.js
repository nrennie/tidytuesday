function chart(data) {
  // Set the dimensions of the chart
  const width = 900;
  const height = 1200;
  const marginTop = 140;
  const marginRight = 20;
  const marginLeft = 20;
  const marginBottom = 40;
  const iconSize = 68;

  // Select the chart container
  const chartContainer = d3.select("#chart");

  const x = d3
    .scaleLinear()
    .domain([0.5, 10.5])
    .range([width - marginRight, marginLeft, ]);

  const y = d3
    .scaleLinear()
    .domain([0.5, 10.5])
    .range([height - marginBottom, marginTop]);

  // Create the SVG container
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
  svg
    .append("foreignObject")
    .attr("x", marginLeft)
    .attr("y", 10)
    .attr("width", 850)
    .attr("height", 100)
    .append("xhtml:div")
    .style("font-size", "32px")
    .style("font-weight", "bold")
    .style("line-height", "1.4")
    .style("text-align", "left")
    .html(
      "Majority of Project Gutenberg works are in <span style='color: #A30000;'>English</span>"
    );

  // Subtitle
  svg
    .append("foreignObject")
    .attr("x", marginLeft)
    .attr("y", 60)
    .attr("width", 850)
    .attr("height", 100)
    .append("xhtml:div")
    .style("font-size", "26px")
    .style("line-height", "1.4")
    .style("text-align", "left")
    .html(
      "Project Gutenberg is a digital library offering free access to over 70,000 public domain books, including classic literature and historical texts."
    );

  // Caption
  svg
      .append("foreignObject")
      .attr("x", marginLeft)
      .attr("y", height - 40)
      .attr("width", 900)
      .attr("height", 100)
      .append("xhtml:div")
      .style("font-size", "26px")
      .style("line-height", "1.4")
      .style("text-align", "left")
      .html(
        "<b>Data:</b> Project Gutenberg | <b>Graphic</b>: Nicola Rennie (<i class='fa-brands fa-github'></i> GitHub)"
      );
  
}

d3.csv("data.csv", (d) => ({
  x: +d.x,
  y: +d.y,
  colour: d.colour,
})).then((data) => {
  chart(data);
});
