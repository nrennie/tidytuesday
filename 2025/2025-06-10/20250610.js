function chart(data) {
  // Set the dimensions of the chart
  const width = 800;
  const height = 650;
  const marginTop = 180;
  const marginRight = 20;
  const marginLeft = 45;
  const marginBottom = 70;

  // Functions
  const tooltipFormat = d3.timeFormat("%d %b, %Y");

  // Select the chart container
  const chartContainer = d3.select("#chart");

  data.forEach((d) => {
    d.start = new Date(d.start);
    d.end = new Date(d.end);
  });

  // Scales
  const x = d3
    .scaleTime()
    .domain([d3.min(data, (d) => d.start), d3.max(data, (d) => d.end)])
    .range([marginLeft, width - marginRight]);

  const y = d3
    .scaleLinear()
    .domain([0, d3.max(data, (d) => d.n)])
    .nice()
    .range([height - marginBottom, marginTop]);

  const color = d3
    .scaleOrdinal()
    .domain(["Democratic", "Republican"])
    .range(["#0000ff", "#e50000"]);

  // Create the SVG container
  const svg = chartContainer
    .append("svg")
    .attr("viewBox", `0 0 ${width} ${height}`)
    .attr("preserveAspectRatio", "xMidYMid meet")
    .style("width", "100%")
    .style("height", "auto")
    .append("g");

  // Axes
  svg
    .append("g")
    .attr("transform", `translate(0,${height - marginBottom})`)
    .call(d3.axisBottom(x));

  svg
    .append("g")
    .attr("transform", `translate(${marginLeft},0)`)
    .call(d3.axisLeft(y));

  // Bars
  svg
    .selectAll("rect")
    .data(data)
    .enter()
    .append("rect")
    .attr("class", "tooltipText")
    .attr("x", (d) => x(d.start))
    .attr("y", (d) => y(d.n))
    .attr("width", (d) => x(d.end) - x(d.start))
    .attr("height", (d) => height - marginBottom - y(d.n))
    .attr("fill", (d) => color(d.party))
    .attr("fill-opacity", 0.6)
    .attr("stroke", (d) => color(d.party));

  // Annotation with arrow

  // Tooltips
  const tip = d3.select("svg").node().parentNode;
  const tooltipDiv = d3
    .select(tip)
    .append("div")
    .attr("class", "tooltip")
    .style("opacity", 0);

  d3.selectAll(".tooltipText")
    .on("mouseover", function (event, d) {
      const tooltipWidth = tooltipDiv.node().offsetWidth;
      const tooltipHeight = tooltipDiv.node().offsetHeight;

      let left = event.clientX + 10;
      let top = event.clientY - tooltipHeight - 10;

      const windowWidth = window.innerWidth;
      const windowHeight = window.innerHeight;

      if (left + tooltipWidth > windowWidth) {
        left = windowWidth - tooltipWidth - 10;
      }
      if (top < 0) {
        top = event.clientY + 10;
      }

      tooltipDiv
        .style("display", "block")
        .style("opacity", 0.75)
        .html(
          "<b>" +
            d.name +
            "</b><br>" +
            tooltipFormat(d.start) +
            " - " +
            tooltipFormat(d.end) +
            "<br>Nominations: " +
            d.n
        )
        .style("position", "absolute")
        .style("font-size", "16px")
        .style("left", left + "px")
        .style("top", top + "px");
    })
    .on("mouseout", function (event, d) {
      tooltipDiv.style("opacity", 0).style("display", "none");
    });

  // Title
  svg
    .append("foreignObject")
    .attr("x", marginLeft)
    .attr("y", 10)
    .attr("width", 750)
    .attr("height", 100)
    .append("xhtml:div")
    .style("font-size", "24px")
    .style("font-weight", "bold")
    .style("line-height", "1.4")
    .style("text-align", "left")
    .html(
      "President <span style='color:#e50000;'>Ronald Reagan</span> nominated the most judges."
    );

  // Subtitle
  svg
    .append("foreignObject")
    .attr("x", marginLeft)
    .attr("y", 40)
    .attr("width", 750)
    .attr("height", 130)
    .append("xhtml:div")
    .style("font-size", "20px")
    .style("line-height", "1.3")
    .style("text-align", "left")
    .html(
      "Data includes judges who were presidentially appointed during good behavior who have served since 1789 on the U.S. District Courts, the U.S. Courts of Appeals, the Supreme Court of the United States, the former U.S. Circuit Courts, and the federal judiciary's courts of special jurisdiction; though only data from Truman onwards is shown here."
    );

  // Caption
  svg
    .append("foreignObject")
    .attr("x", marginLeft)
    .attr("y", height - 40)
    .attr("width", 750)
    .attr("height", 100)
    .append("xhtml:div")
    .style("font-size", "18px")
    .style("line-height", "1.4")
    .style("text-align", "left")
    .html(
      "<b>Data:</b> Federal Judicial Center | <b>Graphic</b>: Nicola Rennie (<i class='fa-brands fa-github'></i> nrennie)"
    );
}

d3.csv("data.csv", (d) => ({
  name: d.name,
  start: new Date(d.start),
  end: new Date(d.end),
  party: d.party,
  n: +d.n,
})).then((data) => {
  chart(data);
});
