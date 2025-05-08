<script>
    import {
        Plot,
        Dot,
        RuleX,
        RuleY,
        AxisX,
        AxisY,
        Pointer
    } from 'svelteplot';
    import csvText from '$lib/data/weather.csv?raw';
    import Papa from 'papaparse';

    const parsed = Papa.parse(csvText, { header: true });
    const weather = parsed.data;

    weather.forEach(row => {
        const [year, month, day] = row.date.split('-');
        row.date = new Date(Number(year), Number(month) - 1, Number(day));
        row.max_temp_C = parseFloat(row.max_temp_C);
        row.min_temp_C = parseFloat(row.min_temp_C);
        row.precipitation_mm = parseFloat(row.precipitation_mm);

    });

</script>

<div style="touch-action: none">
    <Plot x={{label: 'Maximum temperature (°C)' }} y={{label: 'Precipitation (mm)', grid: true }} title="Water quality and weather at Sydney beaches" subtitle="Beachwatch and their partners monitor water quality at swim sites to ensure that recreational water environments are managed as safely as possible so that as many people as possible can benefit from using the water." caption="Source: Beachwatch" marginBottom={30}>
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