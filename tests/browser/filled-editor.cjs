const puppeteer = require('puppeteer');
const assert = require('node:assert/strict');
(async () => {
 const browser = await puppeteer.launch({headless: true});
 try {
  const page = await browser.newPage();
  await page.setViewport({width: 1500, height: 1100});
  const errors = []; page.on('pageerror', e => errors.push(e.message));
  await page.goto('http://127.0.0.1:4848/');
  await page.waitForSelector('#gcChart .gene', {timeout: 60000});
  const value = name => page.$eval(`[data-sb-input-id="element-${name}"] input`, n => n.value);
  await page.click('#gcChart .gene');
  await page.waitForSelector('[data-sb-input-id="element-strokeWidth"] input', {visible:true});
  const gene = await page.$eval('#gcChart .gene', n => ({...n.__drawn__, strokeWidth: parseFloat(getComputedStyle(n).strokeWidth)}));
  for (const name of ['markerHeight', 'arrowheadWidth', 'y', 'strokeWidth']) {
   if (gene[name] !== undefined) assert.equal(Number(await value(name)), gene[name], name);
  }
  assert.equal(await page.$eval('[data-sb-input-id="element-marker"] select', n => n.value), gene.marker);
  assert.ok(!(await page.$eval('#chartCode', n => n.textContent)).includes('GC_element_style'));
  await page.click('#element-fields .gv-swatch[title="#d62728"]');
  await page.waitForFunction(() => document.querySelector('#chartCode').textContent.includes('GC_element_style'));
  const code = await page.$eval('#chartCode', n => n.textContent);
  assert.ok(!code.includes('markerHeight ='), code);
  await page.evaluate(() => {
   Shiny.setInputValue('labels-label', 'name', {priority:'event'});
   Shiny.setInputValue('labels-show', true, {priority:'event'});
  });
  await page.waitForSelector('#gcChart text.label', {visible:true});
  await page.click('#gcChart text.label');
  await page.waitForSelector('[data-sb-input-id="element-text"] input', {visible:true});
  const label = await page.$eval('#gcChart text.label', n => ({text:n.__text__ ?? n.textContent, ...n.__drawn__, dx:n.dx.baseVal.getItem(0).value, dy:n.dy.baseVal.getItem(0).value}));
  assert.equal(await value('text'), label.text);
  for (const name of ['dx','dy','rotate']) assert.equal(Number(await value(name)), label[name], name);
  assert.ok(await value('fontSize'));
  assert.ok(await value('fill'));
  assert.notEqual(await page.$eval('[data-sb-input-id="element-fontWeight"] select', n => n.value), '__none__');
  await page.click('#gcChart .legend-marker');
  await page.waitForSelector('[data-sb-input-id="element-opacity"] input', {visible:true});
  assert.ok(await value('fill')); assert.ok(await value('strokeWidth')); assert.ok(await value('opacity'));
  await page.click('#gcChart .legend-label');
  await page.waitForSelector('[data-sb-input-id="element-text"] input', {visible:true});
  assert.equal(await value('text'), await page.$eval('#gcChart .legend-label', n=>n.textContent));
  assert.ok(await value('fontSize'));
  assert.deepEqual(errors, []);
  console.log('filled editor browser test passed: gene, label, legend marker and text values; only changed fields saved');
 } finally {await browser.close();}
})().catch(e=>{console.error(e);process.exit(1)});
