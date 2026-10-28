const puppeteer = require('puppeteer');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
(async () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'geneviewer-upload-'));
  const first = path.join(dir, 'first.csv'), second = path.join(dir, 'second.csv');
  fs.writeFileSync(first, 'start,end,name,cluster,group\n10,30,Uploaded one,A,Family A\n50,80,Uploaded two,A,Family A\n');
  fs.writeFileSync(second, 'start,end,name,cluster,group\n100,130,Uploaded three,B,Family B\n');
  const browser = await puppeteer.launch({ headless: true });
  try {
    const page = await browser.newPage();
    await page.setViewport({ width: 1500, height: 1100 });
    const errors = [];
    page.on('pageerror', e => errors.push(e.message));
    await page.goto(process.env.GENEVIEWER_APP_URL || 'http://127.0.0.1:4848/');
    await page.waitForSelector('#gcChart .gene', { timeout: 60000 });
    await page.evaluate(() => Shiny.setInputValue('data-example', 'erythromycin', { priority: 'event' }));
    await page.waitForFunction(() => document.querySelector('#gcChart')?.textContent.includes('Erythromycin A biosynthetic'));
    await page.waitForSelector('input[type=file]');
    await (await page.$('input[type=file]')).uploadFile(first);
    await page.waitForFunction(() => {
      const genes = [...document.querySelectorAll('#gcChart .gene')];
      return genes.length === 2 && genes.every(n => n.__data__.name.startsWith('Uploaded')) &&
        !document.querySelector('#gcChart').textContent.includes('Erythromycin');
    }, { timeout: 30000 });
    const state = await page.evaluate(() => ({
      inputs: Object.fromEntries(['scale-start', 'scale-end', 'titles-chart_title', 'data-start', 'data-end', 'data-group'].map(k => [k, Shiny.shinyapp.$inputValues[k]])),
      visible: [...document.querySelectorAll('#gcChart .gene')].every(n => {
        const a = n.getBoundingClientRect(), b = document.querySelector('#gcChart').getBoundingClientRect();
        return a.width > 0 && a.right > b.left && a.left < b.right;
      }),
      code: document.querySelector('#chartCode').textContent
    }));
    assert.equal(state.inputs['titles-chart_title'], '');
    assert.equal(state.inputs['data-group'], 'group');
    assert.ok(state.visible);
    assert.ok(!state.code.includes('erythromycin_cluster'));
    await page.evaluate(() => { Shiny.setInputValue('titles-chart_title', 'My uploaded data', { priority: 'event' }); Shiny.setInputValue('titles-chart_show', true, { priority: 'event' }); });
    await page.waitForFunction(() => document.querySelector('#gcChart').textContent.includes('My uploaded data'));
    await (await page.$('input[type=file]')).uploadFile(second);
    await page.waitForFunction(() => document.querySelectorAll('#gcChart .gene').length === 3);
    assert.ok(await page.$eval('#gcChart', n => n.textContent.includes('My uploaded data')));
    assert.deepEqual(errors, []);
    console.log('Upload regression passed: example title/range cleared, uploaded genes visible, mapping retained, appended files preserve settings.');
  } finally {
    await browser.close();
    fs.rmSync(dir, { recursive: true, force: true });
  }
})().catch(e => { console.error(e); process.exit(1); });
