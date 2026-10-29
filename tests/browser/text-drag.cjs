const puppeteer = require('puppeteer');
const assert = require('node:assert/strict');
(async () => {
 const browser = await puppeteer.launch({headless:true});
 try {
  const page = await browser.newPage();
  const errors=[]; page.on('pageerror', e=>errors.push(e.message));
  await page.setViewport({width:1600,height:1200});
  await page.goto(process.env.GENEVIEWER_APP_URL || 'http://127.0.0.1:4848/');
  await page.waitForSelector('#gcChart .gene',{timeout:60000});
  await page.evaluate(()=>{
   const inputs={'labels-label':'name','labels-show':true,'labels-adjustLabels':false,
    'titles-chart_title':'Chart title','titles-chart_subtitle':'Chart subtitle','titles-chart_show':true,
    'titles-title_title':'Cluster title','titles-title_subtitle':'Cluster subtitle','titles-title_show':true,
    'titles-footer_title':'Cluster footer','titles-footer_subtitle':'Footer subtitle','titles-footer_show':true,
    'titles-label_title':'Cluster label','titles-label_subtitle':'Label subtitle','titles-label_show':true};
   Object.entries(inputs).forEach(([name,value])=>Shiny.setInputValue(name,value,{priority:'event'}));
  });
  const label='#gcChart text.label[rowID="1"]';
  const title='#gcChart [data-component="title"] [data-part="title"]';
  await page.waitForSelector(label); await page.waitForSelector(title);
  const drawn = selector => page.$eval(selector,n=>({...n.__drawn__}));
  const drag = async (selector,dx,dy,cancel=false) => {
   await page.$eval(selector,n=>n.scrollIntoView({block:"center",inline:"nearest"}));
   await page.waitForFunction(selector=>{
    const n=document.querySelector(selector);n.getBoundingClientRect();return n.getNumberOfChars()>0;
   },{},selector);
   const enabled=await page.$eval('#gcChart',n=>n.classList.contains('geneviewer-draggable'));
   // A redraw can replace the text while it is looked up; look again
   let point;
   for(let tries=0;!point;tries++) {
    try {
     await page.$eval(selector,n=>{window.dragTestOriginal=n});
     point=await page.$eval(selector,n=>{
      for(let i=0;i<n.getNumberOfChars();i++) {
       const r=n.getExtentOfChar(i);
       const p=new DOMPoint(r.x+r.width/2,r.y+r.height/2).matrixTransform(n.getScreenCTM());
       if(document.elementFromPoint(p.x,p.y)?.closest('text')===n) return {x:p.x,y:p.y};
      }
      throw new Error('No visible text character for '+n.textContent);
     });
    } catch(error) {
     if(tries>9) throw error;
     await new Promise(resolve=>setTimeout(resolve,300));
    }
   }
   await page.evaluate(()=>{
    window.lastDragDown=null;
    document.addEventListener('pointerdown',event=>{
     const n=event.target.closest('text');
     window.lastDragDown={tag:event.target.tagName,text:n?.textContent,part:n?.getAttribute('data-part'),pointerId:event.pointerId};
    },{once:true,capture:true});
   });
   await page.mouse.move(point.x,point.y);await page.mouse.down();
   await page.mouse.move(point.x+dx,point.y+dy,{steps:8});
   if(cancel) await page.keyboard.press('Escape');
   await page.mouse.up();
   if(enabled && !cancel) {
    await page.waitForFunction(selector=>document.querySelector(selector)!==window.dragTestOriginal,{timeout:15000},selector);
   }
  };
  // Clearing both position fields moves the element back, keeping its style
  const clearPosition=async()=>{
   for(const name of ['moveX','moveY']) {
    const field=`[data-sb-input-id="element-${name}"] input:not([data-shiny-no-bind-input])`;
    await page.waitForSelector(field,{visible:true});
    await page.click(field,{clickCount:3});await page.keyboard.press('Backspace');await page.keyboard.press('Tab');
   }
   await page.waitForFunction(()=>{const d=document.querySelector('#gcChart text.label[rowID="1"]').__drawn__;return d.moveX===0&&d.moveY===0;});
  };
  // Clicking and dragging are always on; the widget tells them apart.
  await page.waitForFunction(()=>document.querySelector('#gcChart').classList.contains('geneviewer-draggable'));
  // The chart may still redraw with the settings above
  for(let tries=0;;tries++) {
   try { await page.click(label); await page.waitForSelector('[data-sb-input-id="element-fill"] input',{visible:true,timeout:3000}); break; }
   catch(error) { if(tries>4) throw error; }
  }
  await page.click('#element-fields .gv-swatch[title="#d62728"]');
  await page.waitForFunction(()=>getComputedStyle(document.querySelector('#gcChart text.label[rowID="1"]')).fill==='rgb(214, 39, 40)');
  await drag(label,26,14);
  await page.waitForFunction(()=>Math.abs(document.querySelector('#gcChart text.label[rowID="1"]').__drawn__.moveX-26)<1);
  assert.ok(Math.abs((await drawn(label)).moveY-14)<1);
  await page.waitForFunction(()=>document.querySelector('#chartCode').textContent.includes('moveX'));
  await page.waitForFunction(()=>Number(document.querySelector('[data-sb-input-id="element-moveX"] input')?.value)>25);
  await drag(label,15,9,true);
  assert.ok(Math.abs((await drawn(label)).moveX-26)<1);
  await clearPosition();
  await page.waitForFunction(()=>document.querySelector('#gcChart text.label[rowID="1"]').__drawn__.moveX===0);
  assert.equal(await page.$eval(label,n=>getComputedStyle(n).fill),'rgb(214, 39, 40)');
  // Rotation and CSS scaling do not change the direction or saved SVG offsets.
  await page.waitForSelector('[data-sb-input-id="element-rotate"] input:not([data-shiny-no-bind-input])',{visible:true});
  await page.click('[data-sb-input-id="element-rotate"] input:not([data-shiny-no-bind-input])',{clickCount:3});
  await page.keyboard.type('30');await page.keyboard.press('Tab');
  await page.waitForFunction(()=>document.querySelector('#gcChart text.label[rowID="1"]').__drawn__.rotate===30);
  await page.$eval('#gcChart',n=>{n.style.transform='scale(0.8)';n.style.transformOrigin='top left'});
  await drag(label,16,8);
  // A queued settings redraw may finish before the drag's saved position arrives.
  await page.waitForFunction(()=>Math.abs(document.querySelector('#gcChart text.label[rowID="1"]').__drawn__.moveX-20)<1);
  assert.ok(Math.abs((await drawn(label)).moveX-20)<1);
  assert.ok(Math.abs((await drawn(label)).moveY-10)<1);
  await page.$eval('#gcChart',n=>{n.style.transform='';n.style.transformOrigin=''});
  await clearPosition();
  await page.waitForFunction(()=>document.querySelector('#gcChart text.label[rowID="1"]').__drawn__.moveX===0);
  assert.equal((await drawn(label)).rotate,30);
  const selectors=[title,'#gcChart [data-component="title"] [data-part="subtitle"]',
   '#gcChart :is([data-cluster="ophA"] [data-component="clusterTitle"], [data-cluster="ophA"][data-component="clusterTitle"]) [data-part="title"]',
   '#gcChart [data-cluster="ophA"] text[data-component="clusterFooter"][data-part="subtitle"]',
   '#gcChart [data-cluster="ophA"] text[data-component="clusterLabel"][data-part="title"]',
   '#gcChart [data-cluster="ophA"] text[data-component="clusterLabel"][data-part="subtitle"]'];
  for(const selector of selectors){
   await page.waitForSelector(selector);
   if(selector.includes('[data-component="title"] [data-part="subtitle"]')) {
    await drag(selector,10,5,true);
    assert.equal(Number(await page.$eval('[data-sb-input-id="element-moveX"] input',n=>n.value)),12);
   }
   await drag(selector,12,8);
   try {
    await page.waitForFunction(selector=>document.querySelector(selector).__drawn__.moveX>11,{timeout:10000},selector);
   } catch(error) {
    console.log(await page.evaluate(selector=>({drawn:document.querySelector(selector).__drawn__,
      select:Shiny.shinyapp.$inputValues.gcChart_select, drag:Shiny.shinyapp.$inputValues.gcChart_drag,
      down:window.lastDragDown,
      text:document.querySelector(selector).textContent}),selector));
    await page.screenshot({path:'/tmp/geneviewer-text-drag-failure.png'});throw error;
   }
  }
  await page.setViewport({width:1500,height:1200});
  await page.waitForFunction(()=>document.querySelector('#gcChart [data-component="title"] [data-part="title"]').__drawn__.moveX>11);
  // The chart's undo, redo and reset; the reset is one step for all moved
  // elements, reported once, and can be undone.
  const titleMoveX=()=>page.$eval('#gcChart [data-component="title"] [data-part="title"]',n=>n.__drawn__.moveX);
  await page.hover('#gcChart');
  await page.waitForSelector('#gcChart .geneviewer-history:not([hidden])',{visible:true});
  assert.equal(await page.$eval('#gcChart .geneviewer-redo',n=>n.disabled),true);
  await page.click('#gcChart .geneviewer-undo');
  await page.waitForFunction(()=>Shiny.shinyapp.$inputValues.gcChart_drag?.action==='undo');
  await page.waitForSelector('#gcChart .geneviewer-redo:not([disabled])');
  await page.keyboard.down('Meta');await page.keyboard.down('Shift');await page.keyboard.press('z');
  await page.keyboard.up('Shift');await page.keyboard.up('Meta');
  await page.waitForFunction(()=>Shiny.shinyapp.$inputValues.gcChart_drag?.action==='redo');
  await page.waitForFunction(()=>document.querySelector('#gcChart [data-component="title"] [data-part="title"]').__drawn__.moveX>11);
  await page.hover('#gcChart .geneviewer-reset');
  await page.click('#gcChart .geneviewer-reset');
  await page.waitForFunction(()=>Shiny.shinyapp.$inputValues.gcChart_moves?.action==='reset');
  await page.waitForFunction(()=>!/moveX|moveY/.test(document.querySelector('#chartCode').textContent));
  await page.waitForFunction(()=>document.querySelector('#gcChart [data-component="title"] [data-part="title"]').__drawn__.moveX===0);
  await page.waitForFunction(()=>document.querySelector('#gcChart .geneviewer-reset').disabled);
  await page.hover('#gcChart .geneviewer-undo');
  await page.click('#gcChart .geneviewer-undo');
  await page.waitForFunction(()=>document.querySelector('#gcChart [data-component="title"] [data-part="title"]').__drawn__.moveX>11);
  await page.waitForFunction(()=>/moveX/.test(document.querySelector('#chartCode').textContent));
  await page.hover('#gcChart .geneviewer-redo');
  await page.click('#gcChart .geneviewer-redo');
  await page.waitForFunction(()=>document.querySelector('#gcChart [data-component="title"] [data-part="title"]').__drawn__.moveX===0);
  assert.equal(await titleMoveX(),0);
  assert.deepEqual(errors,[]);
  console.log('text drag browser test passed: labels, chart/cluster text, cancellation, persistence, position-only reset, reset positions and undo');
 }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exit(1)});
