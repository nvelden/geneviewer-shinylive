const puppeteer = require('puppeteer');
const assert = require('node:assert/strict');
(async () => {
 const browser = await puppeteer.launch({headless:true});
 try {
  const page = await browser.newPage();
  const errors = []; page.on('pageerror', e => errors.push(e.message));
  await page.setViewport({width:1500,height:1100});
  await page.goto(process.env.GENEVIEWER_APP_URL || 'http://127.0.0.1:4848/');
  await page.waitForSelector('#gcChart .gene', {timeout:60000});
  await page.evaluate(() => {
   const inputs = {section:'links', 'links-show':true, 'links-source':'matching',
    'links-group':'class', 'links-measure':'none'};
   Object.entries(inputs).forEach(([key,value])=>Shiny.setInputValue(key,value,{priority:'event'}));
  });
  await page.waitForSelector('#gcChart path[data-component="links"]');
  await page.waitForSelector('[data-sb-input-id="links-useGroupColors"] [role="switch"]',{visible:true});
  if (await page.$eval('[data-sb-input-id="links-useGroupColors"] [role="switch"]', n=>n.getAttribute('aria-checked')) === 'true') {
   await page.click('[data-sb-input-id="links-useGroupColors"] [role="switch"]');
  }
  await page.waitForSelector('[data-sb-input-id="links-normalColor"]',{visible:true});
  for (const [name, color] of [['normalColor','#112233'],['invertedColor','#112233']]) {
   await page.evaluate((id,value)=>Shiny.setInputValue('gvColorPick',{id,value},{priority:'event'}),'links-'+name,color);
   await page.waitForFunction((name,color)=>document.querySelector(`[data-sb-input-id="links-${name}"] input`).value===color,{},name,color);
  }
  if (await page.$eval('[data-sb-input-id="links-useGroupColors"] [role="switch"]', n=>n.getAttribute('aria-checked')) !== 'true') {
   await page.click('[data-sb-input-id="links-useGroupColors"] [role="switch"]');
  }
  await page.waitForFunction(()=>Shiny.shinyapp.$inputValues['links-useGroupColors']===true);
  assert.equal(await page.$('[data-sb-input-id="links-lowColor"]'),null);
  await page.waitForSelector('[data-sb-input-id="links-colorBar"]',{hidden:true});
  const ids = ['normalColor','invertedColor','vmin','vmax'];
  for (const id of ids) await page.waitForSelector(`[data-sb-input-id="links-${id}"]`, {hidden:true});
  await page.click('[data-sb-input-id="links-useGroupColors"] [role="switch"]');
  for (const id of ids) await page.waitForSelector(`[data-sb-input-id="links-${id}"]`, {visible:true});
  assert.equal(await page.$eval('[data-sb-input-id="links-normalColor"] input', n=>n.value),'#112233');
  await page.waitForFunction(() => {
   const paths=[...document.querySelectorAll('#gcChart path[data-component="links"]')];
   return paths.length && paths.every(n=>getComputedStyle(n).fill==='rgb(17, 34, 51)');
  });
  await page.$eval('[data-sb-input-id="links-normalColor"]', n =>
   n.closest('.gv-color-field').querySelector('.gv-swatch[title="#9467bd"]').click());
  await page.waitForFunction(()=>[...document.querySelectorAll('#gcChart path[data-component="links"]')].some(n=>getComputedStyle(n).fill==='rgb(148, 103, 189)'));
  await page.click('[data-sb-input-id="links-useGroupColors"] [role="switch"]');
  await page.waitForSelector('[data-sb-input-id="links-normalColor"]',{hidden:true});
  await page.evaluate(()=>Shiny.setInputValue('links-source','alignment',{priority:'event'}));
  for (const id of ids) await page.waitForSelector(`[data-sb-input-id="links-${id}"]`, {visible:true});
  assert.deepEqual(errors,[]);
  console.log('link colors browser test passed: inactive fields hidden, custom values retained, fill updates, alignment colors available');
 } finally {await browser.close();}
})().catch(e=>{console.error(e);process.exit(1)});
