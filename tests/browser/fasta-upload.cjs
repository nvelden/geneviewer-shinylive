const puppeteer = require('puppeteer');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
// FASTA files load as one cluster each, named after the file
(async () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'geneviewer-fasta-'));
  const first = path.join(dir, 'clusterA.fasta'), second = path.join(dir, 'clusterB.faa');
  fs.writeFileSync(first, [
    '>lcl|X_1 [gene=abcA] [protein=first protein] [location=100..400]', 'MKTAYIAKQR',
    '>lcl|X_2 [gene=abcB] [protein=second protein] [location=complement(500..900)]', 'MSDNELLKRL', ''
  ].join('\n'));
  fs.writeFileSync(second, [
    '>X_3 protein=third protein protein_id=ADX1.1 location=complement(<1..2247)', 'MKTAYIAKQR', ''
  ].join('\r\n'));
  const browser = await puppeteer.launch({ headless: true });
  try {
    const page = await browser.newPage();
    await page.goto(process.env.GENEVIEWER_APP_URL || 'http://127.0.0.1:4848/');
    await page.waitForSelector('#gcChart .gene', { timeout: 60000 });
    const input = await page.$('input[type=file]');
    const accept = await input.evaluate(el => el.accept);
    assert.match(accept, /\.fasta/);
    assert.match(accept, /\.faa/);
    await input.uploadFile(first);
    await page.waitForFunction(() => document.querySelectorAll('#gcChart .gene').length === 2, { timeout: 30000 });
    await page.waitForFunction(() => document.getElementById('gv-loading').hidden);
    await (await page.$('input[type=file]')).uploadFile(second);
    await page.waitForFunction(() => document.querySelectorAll('#gcChart .gene').length === 3, { timeout: 30000 });
    await page.waitForFunction(() => document.getElementById('gv-loading').hidden);
    const clusters = await page.evaluate(() =>
      [...new Set([...document.querySelectorAll('#gcChart [data-cluster]')].map(e => e.dataset.cluster))]);
    assert.deepEqual(clusters.sort(), ['clusterA', 'clusterB']);
    if (process.env.SCREENSHOT) await page.screenshot({ path: process.env.SCREENSHOT });
    console.log('fasta-upload ok');
  } finally {
    await browser.close();
  }
})().catch(e => { console.error(e); process.exit(1); });
