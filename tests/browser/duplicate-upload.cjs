const puppeteer = require('puppeteer');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
// A file uploaded a second time leaves the data as it was; the chart is still
// drawn for that load, so the loading cover goes away
(async () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'geneviewer-duplicate-'));
  const first = path.join(dir, 'first.csv'), second = path.join(dir, 'second.csv');
  fs.writeFileSync(first, 'start,end,name,cluster\n10,30,one,A\n50,80,two,A\n');
  fs.writeFileSync(second, 'start,end,name,cluster\n100,130,three,B\n');
  const browser = await puppeteer.launch({ headless: true });
  try {
    const page = await browser.newPage();
    await page.goto(process.env.GENEVIEWER_APP_URL || 'http://127.0.0.1:4848/');
    await page.waitForSelector('#gcChart .gene', { timeout: 60000 });
    for (const [file, genes] of [[first, 2], [second, 3], [second, 3]]) {
      await (await page.$('input[type=file]')).uploadFile(file);
      await page.waitForFunction(() => !document.getElementById('gv-loading').hidden, { timeout: 10000 });
      await page.waitForFunction(n => document.querySelectorAll('#gcChart .gene').length === n, { timeout: 30000 }, genes);
      await page.waitForFunction(() => document.getElementById('gv-loading').hidden, { timeout: 30000 });
    }
    console.log('duplicate-upload ok');
  } finally {
    await browser.close();
  }
})().catch(e => { console.error(e); process.exit(1); });
