const puppeteer=require('puppeteer');
const assert=require('node:assert/strict');
(async()=>{
 const browser=await puppeteer.launch({headless:true, ...(process.env.PUPPETEER_EXECUTABLE_PATH ? {executablePath:process.env.PUPPETEER_EXECUTABLE_PATH} : {})});
 try {
 const page=await browser.newPage();await page.setViewport({width:1500,height:1100});
 const errors=[];page.on('pageerror',e=>errors.push(e.message));page.on('console',m=>{if(m.type()==='error')console.log('BROWSER',m.text())});
 await page.goto(process.env.GENEVIEWER_APP_URL || 'http://127.0.0.1:4848');await page.waitForSelector('#gcChart .gene',{timeout:60000});
 const section=()=>page.evaluate(()=>Shiny.shinyapp.$inputValues.section);
 const code=()=>page.evaluate(()=>document.querySelector('#chartCode')?.textContent || '');

 // One click selects a gene and opens it in the Selection section.
 await page.click('#gcChart .gene');
 await page.waitForFunction(()=>Shiny.shinyapp.$inputValues.section==='element');
 await page.waitForSelector('[data-sb-input-id="element-fill"] input',{visible:true});
 const row=await page.evaluate(()=>Shiny.shinyapp.$inputValues.gcChart_select.data.rowID);

 // A swatch applies right away, to the chart and the code.
 await page.click('#element-fields .gv-swatch[title="#d62728"]');
 await page.waitForFunction(r=>getComputedStyle(document.querySelector(`#gcChart .gene[rowID="${r}"]`)).fill==='rgb(214, 39, 40)',{},row);
 await page.waitForFunction(()=>document.querySelector('#chartCode')?.textContent.includes('#d62728'));
 assert.ok((await code()).includes('GC_element_style'));

 // The switch above the settings goes to all genes and back, from one place.
 const scope=async()=>page.$eval('.gv-scope',e=>{const r=e.getBoundingClientRect();return [r.top,r.left]});
 const before=await scope();
 await page.click('.gv-scope-option:nth-child(2)');
 await page.waitForFunction(()=>Shiny.shinyapp.$inputValues.section==='genes' && document.querySelector('.gv-scope-option:nth-child(2)[aria-pressed="true"]'));
 assert.deepEqual(await scope(),before);
 await page.click('.gv-scope-option:nth-child(1)');
 await page.waitForFunction(()=>Shiny.shinyapp.$inputValues.section==='element');

 // Other parts of the chart open their own settings.
 const pt=await page.$eval('#gcChart [data-component="sequence"] line',e=>{const r=e.getBoundingClientRect();return {x:r.left+r.width*0.35,y:r.top+r.height/2}});
 await page.mouse.click(pt.x,pt.y);
 await page.waitForFunction(()=>Shiny.shinyapp.$inputValues.section==='sequence');
 // Legend labels and markers use the same individual/all settings flow.
 await page.click('#gcChart .legend-label');
 await page.waitForFunction(()=>Shiny.shinyapp.$inputValues.section==='element' && Shiny.shinyapp.$inputValues.gcChart_select?.part==='label');
 await page.waitForFunction(()=>document.querySelector('.gv-element-card')?.textContent.includes('Legend label'));
 assert.equal(await page.$$eval('#gcChart .legend.unselected', es=>es.length),0);
 await page.click('#element-fields .gv-swatch[title="#9467bd"]');
 await page.waitForFunction(()=>getComputedStyle(document.querySelector('#gcChart .legend-label')).fill==='rgb(148, 103, 189)');
 await page.click('.gv-scope-option:nth-child(2)');
 await page.waitForFunction(()=>Shiny.shinyapp.$inputValues.section==='legend' && document.querySelector('.gv-scope')?.textContent.includes('All legend labels'));
 await page.click('.gv-scope-option:nth-child(1)');
 await page.waitForFunction(()=>Shiny.shinyapp.$inputValues.section==='element');
 await page.click('[data-sb-input-id="element-reset"] button');
 await page.waitForFunction(()=>getComputedStyle(document.querySelector('#gcChart .legend-label')).fill!=='rgb(148, 103, 189)');
 await page.click('#gcChart .legend-marker');
 await page.waitForFunction(()=>document.querySelector('.gv-element-card')?.textContent.includes('Legend marker'));
 await page.waitForSelector('[data-sb-input-id="element-strokeWidth"] input',{visible:true});
 assert.equal(await page.$('[data-sb-input-id="element-fontSize"] input'),null);
 await page.click('#element-fields .gv-swatch[title="#2ca02c"]');
 await page.waitForFunction(()=>getComputedStyle(document.querySelector('#gcChart .legend-marker')).fill==='rgb(44, 160, 44)');
 assert.ok((await code()).includes('part = "marker"'));
 await page.click('.gv-scope-option:nth-child(2)');
 await page.waitForFunction(()=>Shiny.shinyapp.$inputValues.section==='legend' && document.querySelector('.gv-scope')?.textContent.includes('All legend markers'));
 await page.click('.gv-scope-option:nth-child(1)');
 await page.waitForFunction(()=>Shiny.shinyapp.$inputValues.section==='element');
 await page.click('[data-sb-input-id="element-reset"] button');
 await page.waitForFunction(()=>getComputedStyle(document.querySelector('#gcChart .legend-marker')).fill!=='rgb(44, 160, 44)');
 // Viewing mode keeps legend visibility toggles, including across a redraw.
 await page.evaluate(()=>Shiny.setInputValue('editMode',false,{priority:'event'}));
 await page.waitForFunction(()=>!document.querySelector('#gcChart').classList.contains('geneviewer-selectable'));
 await page.click('#gcChart .legend-label');
 await page.waitForFunction(()=>document.querySelectorAll('#gcChart .legend.unselected').length===1);
 await page.click('#gcChart .legend-marker');
 await page.waitForFunction(()=>document.querySelectorAll('#gcChart .legend.unselected').length===0);
 await page.evaluate(()=>Shiny.setInputValue('editMode',true,{priority:'event'}));
 await page.waitForFunction(()=>document.querySelector('#gcChart').classList.contains('geneviewer-selectable'));

 // Back to the gene: its colour is in the field. Reset removes it.
 await page.click(`#gcChart .gene[rowID="${row}"]`);
 await page.waitForFunction(()=>document.querySelector('[data-sb-input-id="element-fill"] input')?.value==='#d62728');
 await page.click('[data-sb-input-id="element-reset"] button');
 await page.waitForFunction(()=>!document.querySelector('#chartCode')?.textContent.includes('GC_element_style'));
 await page.waitForFunction(()=>{const field=document.querySelector('[data-sb-input-id="element-fill"] input');return field && field.value!=='#d62728';});

 // Escape ends the selection.
 await page.$eval(`#gcChart .gene[rowID="${row}"]`,e=>e.focus());await page.keyboard.press('Escape');
 await page.waitForSelector('.gv-element-empty');
 assert.equal(await section(),'element');
 assert.deepEqual(errors,[]);
 console.log('element editor browser test passed');
 } finally { await browser.close(); }
})().catch(e=>{console.error(e);process.exit(1)});
