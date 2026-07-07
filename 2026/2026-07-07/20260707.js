let config = {
    "graphicDataURL": "data.csv",
    "groupColumn": "Weight class", // name of the column used for row grouping
    "colTypes": "httnnnn", // d = date, n = number, t = text, h = html (first entry = name column, then group is excluded)
    "iconColumn": ["Status"],
    "circleRadius": 14, // diameter of circle in px
    "colourPalette": ["#7F055F", "#E4BB25", "#197176", "#555555"],
    "legend": false, // false = no legend, "auto" for just category names
    "legendText": [], // Only used if "legend": "custom"
    "nullText": "-",
    "numberFormat": ",.0f",
    "freezeFirstColumn": false,
    "firstColWidth": 150, // only applied if freeze true
    "tableDescription": "The main message is summarised by the table title and the data behind the table is available to download below.", // Must update for screenreaders
    "minColWidth": 40,
    "maxColWidth": 150,
    "paginate": true, // true = split table into pages
    "numRows": 10, // number of data rows shown per page when paginate is true
    "filters": ["Status", "Country of birth", "Weight class"], // array of column names to filter on, or null for no filters
    "sparklineCols": null, // either null, or an array of 2 column names e.g. ["2020", "2023"] giving the inclusive range of columns to render as a single sparkline column instead of printed values
    "sparklineColTitle": "", // header title for the sparkline column. Only used if sparklineCols is not null
    "sparklineColour": "#151C28", // stroke/dot colour for the sparkline. Only used if sparklineCols is not null
    "arrowCols": null, // either null, or an array of column names. For these columns, positive numbers get an up triangle, negative numbers get a down triangle, 0 prints "No change", and missing values print "No data". Column is centre-aligned.
    "arrowPalette": ["#151C28"] // 1 or 2 colours. If 1, all arrows are this colour. If 2, first = positive arrows, second = negative arrows. Only used if arrowCols is not null
}

let container = d3.select('#table-container');
let tableData;
let pymChild = null;
let currentPage = 0;
let activeFilters = {};

/* =========================================================
   MAIN RENDER FUNCTION
   ========================================================= */
function drawTable() {

    container.html("");

    const categoryColumn = config.iconColumn;
    const categoryColumns = Array.isArray(categoryColumn)
        ? categoryColumn
        : [categoryColumn];

    const legendMode = config.legend ?? "auto";

    const arrowCols = Array.isArray(config.arrowCols) ? config.arrowCols : null;

    let categoryScale = null;
    let categories = [];
    let legendItemsData = [];

    /* =========================================================
       CATEGORY + LEGEND DATA PREP
    ========================================================= */
    if (categoryColumn) {

        const rawCategories = Array.from(
            new Set(
                tableData.flatMap(d =>
                    categoryColumns
                        .map(col => (d[col] ?? "").toString().trim())
                        .filter(v => v !== "")
                )
            )
        );

        if (legendMode === "custom" && Array.isArray(config.legendText)) {
            legendItemsData = config.legendText;
            categories = legendItemsData.map(d => d.value);
        } else {
            categories = rawCategories;
            legendItemsData = categories;
        }

        categoryScale = d3.scaleOrdinal()
            .domain(categories)
            .range(config.colourPalette);
    }

    /* ---------------------------------------------
       NORMALISE COLUMN TYPES + NUMBER FORMATTERS
       --------------------------------------------- */
    const colTypes = config.colTypes.split("");

    const numberFormatters = (() => {
        // single format → apply to all numeric columns
        if (typeof config.numberFormat === "string") {
            return colTypes.map(t =>
                t === "n" ? d3.format(config.numberFormat) : null
            );
        }

        // array of formats → per column
        if (Array.isArray(config.numberFormat)) {
            return colTypes.map((t, i) => {
                const fmt = config.numberFormat[i];
                return (t === "n" && fmt) ? d3.format(fmt) : null;
            });
        }

        return [];
    })();


    /* =========================================================
       LEGEND
    ========================================================= */
    if (categoryColumn && legendMode !== false && legendItemsData.length) {

        const legend = container.append("div")
            .attr("class", "table-legend");

        const legendItems = legend.selectAll(".legend-item")
            .data(legendItemsData)
            .enter()
            .append("div")
            .attr("class", "legend-item");

        legendItems.append("span")
            .style("width", config.circleRadius + "px")
            .style("height", config.circleRadius + "px")
            .style("border-radius", "50%")
            .style("display", "inline-block")
            .style("margin-right", "6px")
            .style("background-color", d =>
                legendMode === "custom" ? categoryScale(d.value) : categoryScale(d)
            );

        legendItems.append("span")
            .attr("class", "legend--text")
            .text(d => {
                if (legendMode === "custom") {
                    return d.description
                        ? `${d.value}${d.description}`
                        : d.value;
                }
                return d;
            });

        legend.append("div")
            .attr("class", "legend--extraText")
            .text("where GNI is Gross National Income, the total income earned by a country's residents and businesses.");
    }


    /* =========================================================
       FILTERS
    ========================================================= */
    let filteredData = tableData;

    if (Array.isArray(config.filters) && config.filters.length) {

        const filterContainer = container.append("div")
            .attr("class", "table-filters");

        config.filters.forEach(col => {

            const options = Array.from(
                new Set(
                    tableData
                        .map(d => (d[col] ?? "").toString().trim())
                        .filter(v => v !== "")
                )
            ).sort();

            const filterRow = filterContainer.append("div")
                .attr("class", "filter-row");

            filterRow.append("label")
                .attr("class", "filter-label")
                .attr("for", `filter-${col}`)
                .text("Select " + col.toLowerCase());

            const select = filterRow.append("select")
                .attr("class", "filter-select")
                .attr("id", `filter-${col}`)
                .on("change", function () {
                    activeFilters[col] = this.value;
                    currentPage = 0;
                    drawTable();
                });

            select.append("option")
                .attr("value", "")
                .text(`All`);

            select.selectAll("option.filter-option")
                .data(options)
                .enter()
                .append("option")
                .attr("class", "filter-option")
                .attr("value", d => d)
                .text(d => d);

            select.property("value", activeFilters[col] || "");
        });

        filteredData = tableData.filter(row =>
            config.filters.every(col => {
                const selected = activeFilters[col];
                if (!selected) return true;
                return (row[col] ?? "").toString().trim() === selected;
            })
        );
    }

    /* =========================================================
       SCROLL WRAPPER
       ========================================================= */
    const scrollWrapper = container.append("div")
        .attr("class", "table-scroll")
        .style("--min-col-width", config.minColWidth + "px")
        .style("--max-col-width", config.maxColWidth + "px");

    const table = scrollWrapper.append("table");

    table.append("caption")
        .attr("class", "visually-hidden")
        .text(config.tableDescription);

    /* =========================================================
       COLUMN STRUCTURE
       ========================================================= */
    const allColumns = tableData.columns;
    const groupColumn = config.groupColumn || "group";
    const dataColumns = allColumns.filter(c => c !== groupColumn);
    const nameColumn = dataColumns[0];
    const valueColumns = dataColumns.slice(1);

    // colTypes[0] corresponds to the name column, so value columns
    // are offset by 1 when looking up their type / formatter.
    const valueColTypeOffset = 1;

    const grouped = d3.group(filteredData, d => d[groupColumn]);

    /* ---------------------------------------------
       SPARKLINE SETUP
       Collapses a contiguous range of value columns into a
       single sparkline column, sharing one x/y scale across
       every row so trends are visually comparable.
       --------------------------------------------- */
    let sparklineRange = null;

    if (Array.isArray(config.sparklineCols) && config.sparklineCols.length === 2) {
        const startIdx = valueColumns.indexOf(config.sparklineCols[0]);
        const endIdx = valueColumns.indexOf(config.sparklineCols[1]);

        if (startIdx !== -1 && endIdx !== -1) {
            const lo = Math.min(startIdx, endIdx);
            const hi = Math.max(startIdx, endIdx);
            sparklineRange = {
                startIdx: lo,
                endIdx: hi,
                cols: valueColumns.slice(lo, hi + 1)
            };
        }
    }

    const sparklineColTitle = config.sparklineColTitle || "Trend";

    // columnBlocks: the list of "columns" actually rendered in the
    // table — a normal value column, or (where applicable) a single
    // block representing the whole sparkline range.
    const columnBlocks = [];

    for (let i = 0; i < valueColumns.length; i++) {
        if (sparklineRange && i === sparklineRange.startIdx) {
            columnBlocks.push({
                kind: "sparkline",
                cols: sparklineRange.cols,
                title: sparklineColTitle
            });
            i = sparklineRange.endIdx;
            continue;
        }
        columnBlocks.push({
            kind: "value",
            col: valueColumns[i],
            type: colTypes[i + valueColTypeOffset],
            i: i + valueColTypeOffset
        });
    }

    const sparklineSvgWidth = 150;
    const sparklineSvgHeight = 60;
    const sparklinePad = 2;

    let sparklineScales = null;

    if (sparklineRange) {

        const allSparklineValues = filteredData.flatMap(row =>
            sparklineRange.cols.map(col => {
                const raw = (row[col] ?? "").toString().trim();
                return raw === "" ? null : +raw;
            })
        ).filter(v => v !== null && !isNaN(v));

        const yDomain = allSparklineValues.length
            ? d3.extent(allSparklineValues)
            : [0, 1];

        if (yDomain[0] === yDomain[1]) {
            yDomain[0] -= 1;
            yDomain[1] += 1;
        }

        sparklineScales = {
            width: sparklineSvgWidth,
            height: sparklineSvgHeight,
            x: d3.scalePoint()
                .domain(sparklineRange.cols)
                .range([sparklinePad, sparklineSvgWidth - sparklinePad]),
            y: d3.scaleLinear()
                .domain(yDomain)
                .range([sparklineSvgHeight - sparklinePad, sparklinePad])
        };
    }

    /* =========================================================
       COLGROUP
       ========================================================= */
    const colgroup = table.append("colgroup");
    colgroup.append("col").attr("class", "col--text");

    columnBlocks.forEach(block => {

        if (block.kind === "sparkline") {
            colgroup.append("col").attr("class", "col--sparkline");
            return;
        }

        if (arrowCols && arrowCols.includes(block.col)) {
            colgroup.append("col").attr("class", "col--arrow");
            return;
        }

        const typeClass =
            block.type === "n" ? "col--number" :
                block.type === "d" ? "col--date" :
                    block.type === "h" ? "col--text" :
                        "col--text";

        colgroup.append("col").attr("class", typeClass);
    });

    /* =========================================================
       THEAD
       ========================================================= */
    const thead = table.append("thead");
    const headerRow = thead.append("tr");
    headerRow.append("td");

    headerRow.selectAll("th")
        .data(columnBlocks)
        .enter()
        .append("th")
        .attr("scope", "col")
        .attr("id", d => d.kind === "sparkline" ? "sparkline" : d.col)
        .attr("class", d => {
            if (d.kind === "sparkline") return "col--sparkline";
            if (arrowCols && arrowCols.includes(d.col)) return "col--arrow";
            return d.type === "n" ? "col--number" :
                d.type === "d" ? "col--date" :
                    "col--text";
        })
        .style("text-align", d =>
            (d.kind !== "sparkline" && arrowCols && arrowCols.includes(d.col)) ? "center" : null
        )
        .text(d => d.kind === "sparkline" ? d.title : d.col);

    /* =========================================================
       TBODY
       ========================================================= */
    const tbody = table.append("tbody");
    const colSpan = columnBlocks.length + 1;

    /* ---------------------------------------------
       BUILD FLAT LIST OF ROWS (ungrouped + grouped, in render order)
       so that pagination can be applied across the whole table.
       --------------------------------------------- */
    let flatRows = [];

    const ungroupedRows = [...grouped].find(([g]) => !g || g.trim() === "");

    if (ungroupedRows) {
        const [, rows] = ungroupedRows;
        rows.forEach((row, index) => {
            flatRows.push({
                kind: "ungrouped",
                row,
                rowId: `ungrouped-r${index + 1}`
            });
        });
    }

    for (const [groupName, rows] of grouped) {
        if (!groupName || groupName.trim() === "") continue;

        const groupId = groupName.toLowerCase().replace(/\s+/g, "");

        rows.forEach((row, index) => {
            flatRows.push({
                kind: "grouped",
                row,
                rowId: `${groupId}-r${index + 1}`,
                groupId,
                groupName
            });
        });
    }

    /* ---------------------------------------------
       PAGINATION
       --------------------------------------------- */
    const paginate = !!config.paginate;
    const numRows = config.numRows || flatRows.length;
    const totalPages = paginate
        ? Math.max(1, Math.ceil(flatRows.length / numRows))
        : 1;

    if (currentPage > totalPages - 1) currentPage = totalPages - 1;
    if (currentPage < 0) currentPage = 0;

    const pageRows = paginate
        ? flatRows.slice(currentPage * numRows, currentPage * numRows + numRows)
        : flatRows;

    /* ---------------------------------------------
       RENDER ROWS FOR THE CURRENT PAGE
       --------------------------------------------- */
    let lastGroupId = null;

    pageRows.forEach(item => {

        if (item.kind === "ungrouped") {
            lastGroupId = null;
        }

        if (item.kind === "grouped" && item.groupId !== lastGroupId) {

            const groupTh = tbody.append("tr")
                .append("th")
                .attr("id", item.groupId)
                .attr("scope", "colgroup")
                .attr("colspan", colSpan)
                .classed("colgroup-header", true);

            groupTh.append("span")
                .attr("class", "sticky-first-col")
                .style("min-width", (config.firstColWidth || 150) + "px")
                .text(item.groupName);

            lastGroupId = item.groupId;
        }

        const row = item.row;
        const rowId = item.rowId;
        const tr = tbody.append("tr");

        const rowTh = tr.append("th")
            .attr("id", rowId)
            .attr("scope", "row")
            .attr("class", "col--text");

        if (item.kind === "grouped") {
            rowTh.attr("headers", item.groupId);
        }

        const nameRaw = (row[nameColumn] ?? "").toString().trim();
        const nameType = colTypes[0];

        if (nameRaw === "") {
            rowTh.text(config.nullText);
        } else if (nameType === "h") {
            rowTh.html(nameRaw);
        } else if (nameType === "n" && !isNaN(nameRaw) && numberFormatters[0]) {
            rowTh.text(numberFormatters[0](+nameRaw));
        } else {
            rowTh.text(nameRaw);
        }

        const cells = tr.selectAll("td")
            .data(columnBlocks.map(block => {
                if (block.kind === "sparkline") {
                    return {
                        kind: "sparkline",
                        cols: block.cols,
                        title: block.title
                    };
                }
                return {
                    kind: "value",
                    col: block.col,
                    value: row[block.col],
                    type: block.type,
                    i: block.i
                };
            }))
            .enter()
            .append("td")
            .attr("headers", d => d.kind === "sparkline" ? `${rowId} sparkline` : `${rowId} ${d.col}`)
            .attr("class", d => {
                if (d.kind === "sparkline") return "col--sparkline";
                if (arrowCols && arrowCols.includes(d.col)) return "col--arrow";
                return d.type === "n" ? "col--number" :
                    d.type === "d" ? "col--date" :
                        "col--text"; // includes "h"
            })
            .style("text-align", d =>
                (d.kind !== "sparkline" && arrowCols && arrowCols.includes(d.col)) ? "center" : null
            );

        cells.each(function (d) {
            const cell = d3.select(this);

            // Sparkline column: draw a shared-scale SVG line chart
            // plus a visually-hidden text description for screenreaders.
            if (d.kind === "sparkline") {

                const values = d.cols.map(col => {
                    const raw = (row[col] ?? "").toString().trim();
                    return {
                        col,
                        value: raw === "" ? null : +raw
                    };
                });

                const validValues = values.filter(v => v.value !== null && !isNaN(v.value));

                if (!validValues.length || !sparklineScales) {
                    cell.style("text-align", "center").text("No data");
                    return;
                }

                const svg = cell.append("svg")
                    .attr("class", "sparkline")
                    .attr("viewBox", `0 0 ${sparklineScales.width} ${sparklineScales.height}`)
                    .attr("width", sparklineScales.width)
                    .attr("height", sparklineScales.height)
                    .attr("aria-hidden", "true")
                    .attr("focusable", "false");

                const sparklineColour = config.sparklineColour || "#197176";

                const line = d3.line()
                    .defined(v => v.value !== null && !isNaN(v.value))
                    .x(v => sparklineScales.x(v.col))
                    .y(v => sparklineScales.y(v.value));

                svg.append("path")
                    .datum(values)
                    .attr("class", "sparkline-line")
                    .attr("fill", "none")
                    .attr("stroke", sparklineColour)
                    .attr("stroke-width", 1.5)
                    .attr("stroke-linecap", "round")
                    .attr("stroke-linejoin", "round")
                    .attr("d", line);

                const lastValidValue = validValues[validValues.length - 1];

                svg.append("circle")
                    .attr("class", "sparkline-point")
                    .attr("cx", sparklineScales.x(lastValidValue.col))
                    .attr("cy", sparklineScales.y(lastValidValue.value))
                    .attr("r", 2)
                    .attr("fill", sparklineColour);

                const descriptionText = values
                    .map(v => `${v.col}: ${v.value !== null && !isNaN(v.value) ? v.value : config.nullText}`)
                    .join(", ");

                cell.append("span")
                    .attr("class", "visually-hidden")
                    .text(`${d.title} — ${descriptionText}`);

                return;
            }

            // Arrow column: render an up/down triangle based on sign,
            // or "No change" / "No data" for zero/missing values.
            if (arrowCols && arrowCols.includes(d.col)) {

                const raw = (d.value ?? "").toString().trim();
                const numVal = raw === "" ? NaN : +raw;

                if (raw === "" || isNaN(numVal)) {
                    cell.text("No data");
                    return;
                }

                if (numVal === 0) {
                    cell.text("No change");
                    return;
                }

                const palette = Array.isArray(config.arrowPalette) && config.arrowPalette.length
                    ? config.arrowPalette
                    : ["#197176", "#7F055F"];

                const arrowColour = numVal > 0
                    ? palette[0]
                    : (palette[1] || palette[0]);

                const arrowSymbol = numVal > 0 ? "▲" : "▼";

                const formattedValue = (d.type === "n" && numberFormatters[d.i])
                    ? numberFormatters[d.i](numVal)
                    : raw;

                const wrapper = cell.append("span")
                    .style("display", "inline-flex")
                    .style("align-items", "center")
                    .style("justify-content", "center")
                    .style("gap", "4px");

                wrapper.append("span")
                    .attr("class", "arrow-icon")
                    .attr("aria-hidden", "true")
                    .style("color", arrowColour)
                    .style("font-size", "0.7em")
                    .text(arrowSymbol);

                wrapper.append("span")
                    .text(formattedValue + " points");

                return;
            }

            const raw = (d.value ?? "").toString().trim();

            if (raw === "") {
                cell.text(config.nullText);
                return;
            }

            if (categoryColumn && categoryColumns.includes(d.col)) {

                const wrapper = cell.append("span")
                    .style("display", "inline-flex")
                    .style("align-items", "center");

                wrapper.append("span")
                    .style("width", config.circleRadius + "px")
                    .style("height", config.circleRadius + "px")
                    .style("border-radius", "50%")
                    .style("display", "inline-block")
                    .style("margin-right", "6px")
                    .style("background-color", categoryScale(raw));

                wrapper.append("span")
                    .text(raw);

                return;
            }

            // HTML column
            if (d.type === "h") {
                cell.html(raw);
                return;
            }

            // Number formatting
            if (d.type === "n" && !isNaN(raw) && numberFormatters[d.i]) {
                cell.text(numberFormatters[d.i](+raw));
                return;
            }

            // Default text
            cell.text(raw);
        });

    });



    if (config.freezeFirstColumn) {
        scrollWrapper.classed("freeze-first-col", true);

        colgroup.select("col:first-child")
            .style("width", config.firstColWidth + "px" || "150px");

    }

    /* =========================================================
       PAGINATION CONTROLS (shown below the table)
       ========================================================= */
    if (paginate) {

        const pagination = container.append("div")
            .attr("class", "table-pagination");

        pagination.append("button")
            .attr("class", "pagination-prev")
            .attr("type", "button")
            .property("disabled", currentPage === 0)
            .text("Previous")
            .on("click", () => {
                if (currentPage > 0) {
                    currentPage--;
                    drawTable();
                }
            });

        pagination.append("span")
            .attr("class", "pagination-status")
            .text(`Page ${currentPage + 1} of ${totalPages}`);

        pagination.append("button")
            .attr("class", "pagination-next")
            .attr("type", "button")
            .property("disabled", currentPage >= totalPages - 1)
            .text("Next")
            .on("click", () => {
                if (currentPage < totalPages - 1) {
                    currentPage++;
                    drawTable();
                }
            });
    }

    if (pymChild) {
        pymChild.sendHeight();
    }

}

/* =========================================================
   LOAD DATA AND RENDER
   ========================================================= */
d3.csv(config.graphicDataURL).then(rawData => {
    tableData = rawData.map(d => ({ ...d }));
    tableData.columns = rawData.columns;
    pymChild = new pym.Child({
        renderCallback: drawTable
    });
});