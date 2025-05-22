<script>
    import {
        Plot,
        Dot,
        Cell,
        RuleX,
        RuleY,
        AxisX,
        AxisY,
        Pointer
    } from 'svelteplot';
    import csvWeather from '$lib/data/weather.csv?raw';
    import csvWater from '$lib/data/water_quality.csv?raw';
    import Papa from 'papaparse';

    const parsedWeather = Papa.parse(csvWeather, { header: true });
    const weather = parsedWeather.data;
    weather.forEach(row => {
        const [year, month, day] = row.date.split('-');
        row.date = new Date(Number(year), Number(month) - 1, Number(day));
        row.max_temp_C = parseFloat(row.max_temp_C);
        row.min_temp_C = parseFloat(row.min_temp_C);
        row.precipitation_mm = parseFloat(row.precipitation_mm);
    });

    const parsedWater = Papa.parse(csvWater, { header: true });
    const water = parsedWater.data;
    water.forEach(row => {
        row.water_temperature_c = parseFloat(row.water_temperature_c);
        row.enterococci_cfu_100ml = parseFloat(row.enterococci_cfu_100ml);
    });

    const tempBinSize = 5;
const cfuBinSize = 10;
const maxTemp = 30;
const maxCFU = 200;

const tempBins = [];
for (let t = 0; t < maxTemp; t += tempBinSize) {
    tempBins.push(t);
}
tempBins.push(`${maxTemp}+`);

const cfuBins = [];
for (let c = 0; c < maxCFU; c += cfuBinSize) {
    cfuBins.push(c);
}
cfuBins.push(`${maxCFU}+`);

const binCounts = new Map();
// Initialise all combinations with 0
tempBins.forEach(temp => {
    cfuBins.forEach(cfu => {
        const key = `${temp},${cfu}`;
        binCounts.set(key, 0);
    });
});

    // Count actual values
    water.forEach(row => {
        const rawTemp = parseFloat(row.water_temperature_c);
        const rawCFU = parseFloat(row.enterococci_cfu_100ml);

        if (isNaN(rawTemp) || isNaN(rawCFU)) return;

        const tempBin = rawTemp >= maxTemp
            ? `${maxTemp}+`
            : Math.floor(rawTemp / tempBinSize) * tempBinSize;

        const cfuBin = rawCFU >= maxCFU
            ? `${maxCFU}+`
            : Math.floor(rawCFU / cfuBinSize) * cfuBinSize;

        const key = `${tempBin},${cfuBin}`;
        binCounts.set(key, binCounts.get(key) + 1);
    });

    const heatmapData = Array.from(binCounts.entries()).map(([key, value]) => {
        const [x, y] = key.split(',').map(Number);
        return { x, y, value };
    });

    console.log(heatmapData)

</script>

<div>
    <Plot
        height=500
        title="Water temperature and quality"
        subtitle="Green tiles represent more common combinations, and pink tiles represent less combinations of bacteria levels and water temperature. Tempertaures around 20°C are common."
        caption="Source: Beachwatch" marginBottom={30}
        y={{ label: 'Water temperature (°C)', type: 'band',
       domain: heatmapData.map(d => d.x)
            .filter((v, i, self) => self.indexOf(v) === i) // remove duplicates
            .sort((a, b) => {
                const numA = parseFloat(a);
                const numB = parseFloat(b);
                const isPlusA = String(a).includes('+');
                const isPlusB = String(b).includes('+');
                if (isPlusA && isPlusB) return 0;
                if (isPlusA) return 1;
                if (isPlusB) return -1;
                return numA - numB;
            })
        }}
        x={{ label: 'Enterococci (cfu/100ml)', type: 'band',
            domain: heatmapData.map(d => d.y)
            .filter((v, i, self) => self.indexOf(v) === i) 
            .sort((a, b) => {
                const numA = parseFloat(a);
                const numB = parseFloat(b);
                const isPlusA = String(a).includes('+');
                const isPlusB = String(b).includes('+');
                if (isPlusA && isPlusB) return 0;
                if (isPlusA) return 1;
                if (isPlusB) return -1;
                return numA - numB;
            })
         }}
        color={{ type: 'quantile', scheme: 'PiYG' }}>
    <Cell
        data={heatmapData}
        x="y"
        y="x"
        fill="value"
        inset={0.5} />
    </Plot>
</div>

<div style="touch-action: none">
    <Plot 
        x={{label: 'Maximum temperature (°C)' }} y={{label: 'Precipitation (mm)', grid: true }}
        title="Weather at Sydney beaches"
        subtitle=""
        caption="Source: open-meteo.com"
        marginBottom={30}>
        <AxisX />
        <AxisY />
        <Dot data={weather} x="max_temp_C" y="precipitation_mm" fill="#2F4F4F" strokeWidth=0 opacity=0.5/>
        <Pointer
            data={weather} x="max_temp_C" y="precipitation_mm"
            maxDistance={30}>
            {#snippet children({ data })}
                {#if data.length > 0}
                    <RuleX {data} x="max_temp_C" opacity="0.6" stroke="#E30B5C"/>
                    <RuleY {data} y="precipitation_mm" opacity="0.6" stroke="#E30B5C"/>
                    <AxisX
                        data={data.map((d) => d.max_temp_C)}
                        tickFormat={(d) => d.toFixed()} />
                    <AxisY
                        data={data.map((d) => d.precipitation_mm)}
                        tickFormat={(d) => d.toFixed()} />
                {/if}
            {/snippet}
        </Pointer>
    </Plot>
</div>