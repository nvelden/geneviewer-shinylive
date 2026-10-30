// The coronavirus example: it draws its eleven genomes with links, measures on
// the links and the color bar in the legend, and the link controls move them
const puppeteer = require('puppeteer');
const assert = require('node:assert/strict');
(async () => {
  const browser = await puppeteer.launch({ headless: true });
  try {
    const page = await browser.newPage();
    const errors = []; page.on('pageerror', e => errors.push(e.message));
    await page.setViewport({ width: 1500, height: 1100 });
    await page.goto(process.env.GENEVIEWER_APP_URL || 'http://127.0.0.1:4848/');
    await page.waitForSelector('#gcChart .gene', { timeout: 60000 });
    await page.evaluate(() => Shiny.setInputValue('data-example', 'coronavirus', { priority: 'event' }));
    await page.waitForFunction(() =>
      document.querySelectorAll('#gcChart svg[data-cluster]').length === 11 &&
      document.querySelector('#gcChart').textContent.includes('Conserved core') &&
      document.querySelector('#gcChart .legend-color-bar'), { timeout: 60000 });

    const chart = await page.evaluate(() => ({
      genes: document.querySelectorAll('#gcChart .gene').length,
      links: document.querySelectorAll('#gcChart path.GeneLink').length,
      measures: document.querySelectorAll('#gcChart text.link-measure').length,
      legend: Array.from(document.querySelectorAll('#gcChart .legend-label'), t => t.textContent),
      genus: Array.from(document.querySelectorAll('#gcChart [data-component=clusterTitle] text'), t => t.textContent).filter(Boolean),
      slider: !!document.querySelector('#gcChart .geneviewer-zoom-slider'),
      cut: Array.from(document.querySelectorAll('#gcChart text.label')).filter(t => {
        if (t.style.display === 'none') return false;
        const svg = t.ownerSVGElement.getBoundingClientRect(), b = t.getBoundingClientRect();
        return b.top < svg.top - 0.5 || b.bottom > svg.bottom + 0.5;
      }).length,
      cutGenes: Array.from(document.querySelectorAll('#gcChart .gene')).filter(g => {
        const svg = g.ownerSVGElement.getBoundingClientRect(), b = g.getBoundingClientRect();
        return b.top < svg.top - 0.5 || b.bottom > svg.bottom + 0.5;
      }).map(g => g.__data__.gene),
      lowerTracks: Array.from(document.querySelectorAll('#gcChart .gene')).filter(g => g.__data__.geneTrack > 1).length,
      code: document.querySelector('#chartCode').textContent
    }));
    assert.equal(chart.genes, 148);
    assert.equal(chart.links, 53);
    assert.ok(chart.measures > 0, 'measures on the links');
    assert.ok(chart.legend.includes('spike (S)'), 'legend names with their gene');
    assert.deepEqual(chart.genus, ['Alphacoronavirus', 'Betacoronavirus', 'Gammacoronavirus', 'Deltacoronavirus']);
    assert.ok(chart.slider, 'zoom slider');
    assert.equal(chart.cut, 0, 'no label is cut off');
    assert.deepEqual(chart.cutGenes, [], 'no gene is cut off');
    assert.ok(chart.lowerTracks > 0, 'nested ORFs on the tracks below');
    assert.match(chart.code, /prevent_gene_overlap = "nested"/);
    assert.match(chart.code, /label_position = "link"/);
    assert.match(chart.code, /colorBarOptions = list\(position = "legend", legendSide = "right"\)/);

    // The color bar goes to a corner, and the measures back to the genes
    await page.evaluate(() => Shiny.setInputValue('links-colorBarPosition', 'topleft'));
    await page.waitForFunction(() => document.querySelector('#gcChart svg.color-bar') &&
      !document.querySelector('#gcChart .legend-color-bar'), { timeout: 30000 });
    await page.evaluate(() => Shiny.setInputValue('links-labelPosition', 'gene'));
    // With the gene, a measure is a second line of the gene label
    await page.waitForFunction(() => !document.querySelector('#gcChart text.link-measure') &&
      Array.from(document.querySelectorAll('#gcChart text.label')).some(t => t.textContent.includes('%')),
      { timeout: 30000 });

    assert.deepEqual(errors, []);
    console.log('Coronavirus example passed: genomes, links, measures on links, legend color bar, genus titles, no cut labels; controls move the bar and labels.');
  } finally {
    await browser.close();
  }
})().catch(e => { console.error(e); process.exit(1); });
