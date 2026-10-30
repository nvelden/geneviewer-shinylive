const puppeteer = require('puppeteer');
const assert = require('node:assert/strict');
(async()=>{
 const browser=await puppeteer.launch({headless:true});
 try {
  const page=await browser.newPage();const errors=[];
  page.on('pageerror',e=>errors.push(e.message));
  await page.setViewport({width:1600,height:1200});
  await page.goto(process.env.GENEVIEWER_APP_URL || 'http://127.0.0.1:4848/');
  await page.waitForSelector('#gcChart .gene',{timeout:60000});
  await page.evaluate(()=>{
   const values={'labels-label':'name','labels-show':true,'labels-adjustLabels':false,
    'titles-chart_title':'Chart title','titles-chart_subtitle':'Chart subtitle','titles-chart_show':true};
   Object.entries(values).forEach(([name,value])=>Shiny.setInputValue(name,value,{priority:'event'}));
  });
  const label='#gcChart text.label[rowID="1"]';
  const title='#gcChart [data-component="title"] [data-part="title"]';
  const subtitle='#gcChart [data-component="title"] [data-part="subtitle"]';
  await page.waitForSelector(label);await page.waitForSelector(title);
  await page.waitForFunction(()=>document.querySelector('#gcChart').classList.contains('geneviewer-draggable'));
  const source=await page.$eval('#gcChart svg[data-cluster="ophA"]',n=>n.getBoundingClientRect().toJSON());
  const target=await page.$eval('#gcChart svg[data-cluster="dbophA"]',n=>n.getBoundingClientRect().toJSON());
  const glyph=async selector=>page.$eval(selector,n=>{
   n.getBoundingClientRect();
   for(let i=0;i<n.getNumberOfChars();i++) {
    const r=n.getExtentOfChar(i),p=new DOMPoint(r.x+r.width/2,r.y+r.height/2).matrixTransform(n.getScreenCTM());
    if(document.elementFromPoint(p.x,p.y)?.closest('text')===n)return{x:p.x,y:p.y};
   }
   throw new Error('Moved text cannot be clicked: '+n.textContent);
  });
  const move=async(selector,dx,dy)=>{
   await page.waitForFunction(selector=>{const n=document.querySelector(selector);n.getBoundingClientRect();return n.getNumberOfChars()>0}, {},selector);
   const p=await glyph(selector);
   await page.$eval(selector,n=>{window.dragOriginal=n});
   await page.mouse.move(p.x,p.y);await page.mouse.down();
   await page.mouse.move(p.x+dx,p.y+dy,{steps:12});await page.mouse.up();
   await page.waitForFunction(selector=>document.querySelector(selector)!==window.dragOriginal,{},selector);
   try {
    await page.waitForFunction(selector=>!!document.querySelector(selector).closest('.geneviewer-move-overlay'),{timeout:10000},selector);
   } catch(error) {
    console.log('Unmoved text',await page.evaluate(selector=>({node:document.querySelector(selector).outerHTML,
      drawn:document.querySelector(selector).__drawn__,drag:Shiny.shinyapp.$inputValues.gcChart_drag,
      code:document.querySelector('#chartCode').textContent}),selector));throw error;
   }
  };
  const p=await glyph(label);
  await move(label,60,target.top+target.height*0.6-p.y);
  assert.ok(await page.$eval(label,n=>n.getBoundingClientRect().top)>source.bottom);
  assert.equal(await page.$eval(label,n=>n.closest('[data-cluster]').getAttribute('data-cluster')),'ophA');
  // The moved label can be grabbed again outside its original cluster SVG.
  await glyph(label);await move(label,20,5);await glyph(label);
  const t=await glyph(title);await move(title,30,target.top+target.height*0.3-t.y);await glyph(title);
  const s=await glyph(subtitle);await move(subtitle,-45,target.top+target.height*0.75-s.y);await glyph(subtitle);
  const exported=await page.evaluate(()=>{
   const el=document.querySelector('#gcChart');
   const documentSVG=new DOMParser().parseFromString(chartToSVG(el),'image/svg+xml');
   const text=documentSVG.querySelector('text[rowID="1"]');
   return {count:documentSVG.querySelectorAll('text[rowID="1"]').length,
    height:Number(text.closest('svg').getAttribute('height')),chartHeight:Math.ceil(el.getBoundingClientRect().height)};
  });
  assert.equal(exported.count,1);assert.ok(Math.abs(exported.height-exported.chartHeight)<1);
  const before=await page.$eval(label,n=>n.__drawn__.moveY);
  await page.$eval(label,n=>{window.resizeOriginal=n});
  await page.setViewport({width:1500,height:1200});
  await page.waitForFunction(selector=>document.querySelector(selector)!==window.resizeOriginal,{},label);
  await page.waitForFunction(selector=>!!document.querySelector(selector).closest('.geneviewer-move-overlay'),{},label);
  assert.equal(await page.$eval(label,n=>n.__drawn__.moveY),before);await glyph(label);
  assert.deepEqual(errors,[]);
  console.log('chart-wide drag browser test passed: cluster boundary, title/subtitle, re-grabbing, resize and SVG export');
 }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exit(1)});
