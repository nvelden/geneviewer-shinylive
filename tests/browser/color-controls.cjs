const puppeteer = require('puppeteer');
const assert = require('node:assert/strict');
(async () => {
 const browser = await puppeteer.launch({headless: true});
 try {
  const page = await browser.newPage();
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  await page.setViewport({width: 1500, height: 1100});
  await page.goto('http://127.0.0.1:4848/');
  await page.waitForSelector('#gcChart .legend-label', {timeout: 60000});
  await page.click('#gcChart .legend-label');
  await page.waitForSelector('[data-sb-input-id="element-fontSize"] input', {visible: true});
  const size = await page.$eval('#gcChart .legend-label', n => parseFloat(getComputedStyle(n).fontSize));
  assert.equal(await page.$eval('[data-sb-input-id="element-fontSize"] input', n => Number(n.value)), size);
  assert.ok(!(await page.$eval('#chartCode', n => n.textContent)).includes('GC_element_style'));
  await page.focus('[data-sb-input-id="element-fontSize"] input');
  await page.keyboard.press('ArrowUp');
  await page.waitForFunction(expected => parseFloat(getComputedStyle(document.querySelector('#gcChart .legend-label')).fontSize) === expected, {}, size + 1);
  await page.click('[data-sb-input-id="element-reset"] button');
  await page.waitForFunction(expected => parseFloat(getComputedStyle(document.querySelector('#gcChart .legend-label')).fontSize) === expected, {}, size);
  await page.click('.gv-scope-option:nth-child(2)');
  await page.waitForFunction(() => Shiny.shinyapp.$inputValues.section === 'legend');
  const field = '[data-sb-input-id="legend-fill"]';
  await page.$eval(field, n => n.closest('.gv-color-field').querySelector('.gv-swatch[title="#2ca02c"]').click());
  await page.waitForFunction(() => Shiny.shinyapp.$inputValues['legend-fill'] === '#2ca02c');
  await page.waitForFunction(() => getComputedStyle(document.querySelector('#gcChart .legend-label')).fill === 'rgb(44, 160, 44)');
  const controls = await page.evaluate(() => [...document.querySelectorAll('.gv-color-field')].map(n => ({
   id: n.querySelector('[data-sb-input-id]')?.getAttribute('data-sb-input-id'),
   swatches: n.querySelectorAll('button.gv-swatch').length,
   custom: n.querySelectorAll('input[type="color"]').length
  })));
  assert.ok(controls.length >= 20);
  assert.ok(controls.every(c => c.swatches === 12 && c.custom === 1), JSON.stringify(controls));
  // Exercise every static swatch control through the same server update path.
  const ids = controls.map(c => c.id).filter(id => id && !id.startsWith('element-'));
  await page.evaluate(ids => ids.forEach(id => {
   const control = document.querySelector(`[data-sb-input-id="${id}"]`).closest('.gv-color-field');
   const picker = control.querySelector('input[type="color"]');
   picker.value = '#008000'; picker.dispatchEvent(new Event('change', {bubbles: true}));
  }), ids);
  await page.waitForFunction(ids => ids.every(id => Shiny.shinyapp.$inputValues[id] === '#008000'), {}, ids);
  assert.deepEqual(errors, []);
  console.log(`color controls browser test passed: ${ids.length} global fields, selected size value and increment, custom pickers`);
 } finally { await browser.close(); }
})().catch(error => {console.error(error); process.exit(1);});
