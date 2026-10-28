const puppeteer = require('puppeteer');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
// Files added to an upload keep the user's settings
(async () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'geneviewer-append-'));
  const first = path.join(dir, 'first.csv'), second = path.join(dir, 'second.csv');
  fs.writeFileSync(first, 'start,end,name,cluster,group\n10,30,one,A,F1\n50,80,two,A,F1\n');
  fs.writeFileSync(second, 'start,end,name,cluster,group\n100,130,three,B,F2\n');
  const wait = ms => new Promise(r => setTimeout(r, ms));
  const browser = await puppeteer.launch({ headless: true });
  try {
    const page = await browser.newPage();
    await page.goto(process.env.GENEVIEWER_APP_URL || 'http://127.0.0.1:4848/');
    await page.waitForSelector('#gcChart .gene', { timeout: 60000 });
    await (await page.$('input[type=file]')).uploadFile(first);
    await page.waitForFunction(() => document.querySelectorAll('#gcChart .gene').length === 2, { timeout: 30000 });
    await page.waitForFunction(() => document.getElementById('gv-loading').hidden);
    const get = () => page.evaluate(() => ['tooltip-columns', 'data-group', 'titles-chart_title']
      .map(k => Shiny.shinyapp.$inputValues[k]));
    assert.deepEqual((await get())[0], ['start', 'end']);
    await page.evaluate(() => {
      Shiny.setInputValue('tooltip-columns', ['name']);
      Shiny.setInputValue('data-group', 'name');
      Shiny.setInputValue('titles-chart_title', 'Mine');
    });
    await wait(1500);
    await (await page.$('input[type=file]')).uploadFile(second);
    await page.waitForFunction(() => document.querySelectorAll('#gcChart .gene').length === 3, { timeout: 30000 });
    await page.waitForFunction(() => document.getElementById('gv-loading').hidden);
    await wait(1500);
    assert.deepEqual(await get(), [['name'], 'name', 'Mine']);
    console.log('append-upload ok');
  } finally {
    await browser.close();
  }
})().catch(e => { console.error(e); process.exit(1); });
