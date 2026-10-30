const puppeteer = require('puppeteer');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
// The zoom slider: the window is written in the code, kept when a setting
// draws the chart again, saved in a project and put back when it opens, and
// cleared by a double-click. The slider does not select or open chart elements.
(async () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'geneviewer-zoom-'));
  const browser = await puppeteer.launch({ headless: true });
  try {
    const page = await browser.newPage();
    await page.setViewport({ width: 1500, height: 1100 });
    const errors = [];
    page.on('pageerror', e => errors.push(e.message));
    await page.goto(process.env.GENEVIEWER_APP_URL || 'http://127.0.0.1:4848/');
    await page.waitForSelector('#gcChart .gene', { timeout: 60000 });
    const windowStyle = () => page.$eval('#gcChart .geneviewer-zoom-window', n => n.style.width);
    const state = () => page.evaluate(() => ({
      show: Shiny.shinyapp.$inputValues['zoom-show'],
      zoom: Shiny.shinyapp.$inputValues.gcChart_zoom,
      width: document.querySelector('#gcChart .geneviewer-zoom-window')?.style.width,
      code: (document.querySelector('#chartCode').textContent.match(/GC_zoom\([^)]*\)/) || [null])[0]
    }));
    // A failed wait shows where the zoom is
    const step = async (name, wait) => {
      try { await wait; } catch (e) { e.message += ` at "${name}": ` + JSON.stringify(await state()); throw e; }
    };
    const zoomCode = () => page.$eval('#chartCode', n => (n.textContent.match(/GC_zoom\([^)]*\)/) || [null])[0]);

    await page.evaluate(() => Shiny.setInputValue('zoom-show', true));
    await page.waitForSelector('#gcChart .geneviewer-zoom-slider');
    assert.equal(await windowStyle(), '100%');

    // Drag the end handle to the left; a click on the slider selects nothing
    const section = await page.evaluate(() => Shiny.shinyapp.$inputValues.section);
    const handle = await page.$('#gcChart [data-zoom-part=end]');
    const box = await handle.boundingBox();
    await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
    await page.mouse.down();
    await page.mouse.move(box.x - 300, box.y + box.height / 2, { steps: 8 });
    await page.mouse.up();
    await step('drag code', page.waitForFunction(() => /GC_zoom\(range = c\(0, 0\.\d+\)\)/.test(document.querySelector('#chartCode').textContent)));
    const width = await windowStyle();
    const code = await zoomCode();
    assert.notEqual(width, '100%');
    assert.equal(await page.evaluate(() => Shiny.shinyapp.$inputValues.section), section);
    assert.equal(await page.$$eval('#gcChart .geneviewer-selected', n => n.length), 0);

    // Another setting draws the chart again and keeps the window
    await page.evaluate(() => Shiny.setInputValue('scale-reverse', true));
    await page.waitForFunction(() => /reverse = TRUE/.test(document.querySelector('#chartCode').textContent));
    await new Promise(resolve => setTimeout(resolve, 500));
    assert.equal(await windowStyle(), width);
    assert.equal(await zoomCode(), code);

    // Save the project, then load an example: its settings turn the slider off
    await page.evaluate(() => {
      window.__saved = null;
      const create = URL.createObjectURL;
      URL.createObjectURL = blob => { blob.text().then(text => { window.__saved = text; }); return create(blob); };
      Shiny.setInputValue('data-saveProject', Date.now(), { priority: 'event' });
    });
    await page.waitForFunction(() => window.__saved !== null);
    const saved = await page.evaluate(() => window.__saved);
    assert.ok(JSON.parse(saved).zoom.range.length === 2);
    const file = path.join(dir, 'zoomed.geneviewer.json');
    fs.writeFileSync(file, saved);

    await page.evaluate(() => Shiny.setInputValue('data-example', 'erythromycin', { priority: 'event' }));
    await step('example', page.waitForFunction(() => document.querySelector('#gcChart')?.textContent.includes('Erythromycin A biosynthetic')));
    await step('example zoom', page.waitForFunction(() => !document.querySelector('#gcChart .geneviewer-zoom-slider')));
    await step('example code', page.waitForFunction(() => !/GC_zoom\(range/.test(document.querySelector('#chartCode').textContent)));

    // Opening the project puts its window back
    await (await page.$('input[type=file]')).uploadFile(file);
    await step('project zoom', page.waitForFunction(w => document.querySelector('#gcChart .geneviewer-zoom-window')?.style.width === w,
                                                    { timeout: 30000 }, width));
    await step('project code', page.waitForFunction(c => document.querySelector('#chartCode').textContent.includes(c), {}, code));

    // Zoomed in further, opening the same project again puts its window back,
    // in the chart, the code and a project saved again
    const end2 = await (await page.$('#gcChart [data-zoom-part=end]')).boundingBox();
    await page.mouse.move(end2.x + end2.width / 2, end2.y + end2.height / 2);
    await page.mouse.down();
    await page.mouse.move(end2.x - 200, end2.y + end2.height / 2, { steps: 8 });
    await page.mouse.up();
    await step('zoom further', page.waitForFunction(c => !document.querySelector('#chartCode').textContent.includes(c), {}, code));
    await (await page.$('input[type=file]')).uploadFile(file);
    await step('reopen zoom', page.waitForFunction(w => document.querySelector('#gcChart .geneviewer-zoom-window')?.style.width === w,
                                                   { timeout: 30000 }, width));
    await step('reopen code', page.waitForFunction(c => document.querySelector('#chartCode').textContent.includes(c), {}, code));
    await page.evaluate(() => {
      window.__saved = null;
      Shiny.setInputValue('data-saveProject', Date.now(), { priority: 'event' });
    });
    await page.waitForFunction(() => window.__saved !== null);
    assert.deepEqual(JSON.parse(await page.evaluate(() => window.__saved)).zoom, JSON.parse(saved).zoom);

    // Double-click shows everything, and the code leaves the range out
    await page.click('#gcChart .geneviewer-zoom-slider', { clickCount: 2 });
    await page.waitForFunction(() => document.querySelector('#gcChart .geneviewer-zoom-window').style.width === '100%');
    await page.waitForFunction(() => !/GC_zoom\(range/.test(document.querySelector('#chartCode').textContent));

    // Dragging keeps the preview where it is scrolled to: each step redraws
    // the chart, which empties it for a moment
    await page.setViewport({ width: 1500, height: 450 });
    await page.evaluate(() => Shiny.setInputValue('data-example', 'pqeb1', { priority: 'event' }));
    await step('pqeb1', page.waitForFunction(() => document.querySelectorAll('#gcChart svg[data-cluster]').length === 5 && document.querySelectorAll('#gcChart .gene').length > 300, { timeout: 60000 }));
    await page.evaluate(() => Shiny.setInputValue('zoom-show', true));
    await step('pqeb1 zoom', page.waitForSelector('#gcChart .geneviewer-zoom-slider', { timeout: 30000 }));
    await page.evaluate(() => document.querySelector('#gcChart .geneviewer-zoom-slider').scrollIntoView({ block: 'center' }));
    const scrollTop = () => page.$eval('.gv-canvas-area', n => n.scrollTop);
    const scrolled = await scrollTop();
    assert.ok(scrolled > 100);
    const end = await (await page.$('#gcChart [data-zoom-part=end]')).boundingBox();
    await page.mouse.move(end.x + 4, end.y + 4);
    await page.mouse.down();
    await page.mouse.move(end.x - 300, end.y + 4, { steps: 10 });
    await page.mouse.up();
    await new Promise(resolve => setTimeout(resolve, 1000));
    assert.notEqual(await windowStyle(), '100%');
    assert.equal(await scrollTop(), scrolled);

    assert.deepEqual(errors, []);
    console.log('Zoom regression passed: window in code, kept on redraw, saved and opened with a project, off after an example, cleared by a double-click, scroll kept while dragging.');
  } finally {
    await browser.close();
    fs.rmSync(dir, { recursive: true, force: true });
  }
})().catch(e => { console.error(e); process.exit(1); });
