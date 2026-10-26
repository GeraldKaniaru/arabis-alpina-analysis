/* Archive rendering keeps numeric research files unchanged. */
function filterFiles(files, state) {
  const query = state.query.trim().toLowerCase();
  return files.filter(f => {
    const historical = f.path.startsWith('data/migration_records/');
    return (state.category === 'all' || f.category === state.category) &&
      (!state.type || f.extension === state.type) &&
      (state.history === 'all' || (state.history === 'historical' ? historical : !historical)) &&
      (!query || f.path.toLowerCase().includes(query));
  });
}
function formatSize(n) { return n >= 1e9 ? (n / 1e9).toFixed(2) + ' GB' : n >= 1e6 ? (n / 1e6).toFixed(1) + ' MB' : n >= 1000 ? (n / 1000).toFixed(1) + ' KB' : n + ' B'; }
function escapeHtml(s) { return String(s).replace(/[&<>"']/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c])); }
if (typeof module !== 'undefined' && module.exports) module.exports = {filterFiles, formatSize, escapeHtml};
if (typeof document !== 'undefined') {
 const $ = id => document.getElementById(id);
 const state = {category:'all', query:'', type:'', history:'current', page:0};
 const pageSize = 30;
 const repository = 'https://github.com/GeraldKaniaru/arabis-alpina-analysis';
 let files = [];
 const stages = [
 ['01','Study area','Map the study area, source plots and environmental context.','01_study_area.R','Context'],
 ['02','Vegetation zones','Assemble photographs across the elevational vegetation zones.','02_vegetation_zones.R','Field context'],
 ['03','Target species','Create the species photograph plate.','03_target_species_photo_plate.R','Field context'],
 ['04','Sampling design','Illustrate the transect, blocks, plots and sampled rosettes.','04_sampling_design_schematic.R','Design'],
 ['05a','Data distributions','Inspect response distributions and transformations.','05a_data_distribution.R','Diagnostics'],
 ['05b','Model residuals','Evaluate fitted-model residuals after scripts 06–10.','05b_model_residuals.R','Diagnostics'],
 ['05c','Spearman correlations','Explore environmental and trait associations.','05c_spearman_correlations.R','Exploration'],
 ['06','Flowering phenology','Model flowering onset, final date and duration.','06_flowering_phenology.R','H1–H3'],
 ['07','Maternal seed allocation','Analyse individual seed mass, seed number and allocation.','07_maternal_seed_allocation.R','H4–H6'],
 ['08','Germination & elevation','Fit germination models and distributional sensitivities.','08_germination_elevation.R','H7'],
 ['09','Seed mass & germination','Analyse seed-mass associations with germination responses.','09_maternal_seed_mass_germination_responses.R','H8'],
 ['10','Mid-century projections','Project trait changes and bootstrap uncertainty.','10_projected_trait_changes_mid_century.R','Projections']
 ];
 $('workflow-grid').innerHTML = stages.map(s => `<a class="workflow-card" href="${repository}/blob/main/scripts/${s[3]}" target="_blank" rel="noopener"><div class="card-top"><strong>${s[0]}</strong><span>${s[4]} ↗</span></div><h3>${s[1]}</h3><p>${s[2]}</p><span class="card-file">${s[3]}</span></a>`).join('');
 const bundles = [['data','Data','Raw and processed data, climate layers, spatial data and historical records'],['outputs','Outputs','Figures, fitted models, diagnostics, tables and bootstrap results'],['images','Source photographs','Species and vegetation-zone photographs used in the figure scripts']];
 $('download-list').innerHTML = bundles.map((b,i) => `<a href="${repository}/releases/download/analysis-2026-10-26/analysis-${b[0]}.zip"><span>0${i+1}</span><div><h3>${b[1]}</h3><p>${b[2]}</p><small id="${b[0]}-size">ZIP archive</small></div><b>↓</b></a>`).join('') + `<a href="${repository}/tree/main/scripts" target="_blank" rel="noopener"><span>04</span><div><h3>R code & project</h3><p>Browse all 12 scripts and the project instructions</p><small>GitHub repository</small></div><b>↗</b></a>`;
 function render() {
  const matches = filterFiles(files,state);
  const pages = Math.max(1,Math.ceil(matches.length/pageSize));
  state.page = Math.min(state.page,pages-1);
  const start = state.page*pageSize;
  $('result-info').textContent = `${matches.length} matching files · ${formatSize(matches.reduce((n,f) => n+f.size,0))} uncompressed`;
  $('file-rows').innerHTML = matches.slice(start,start+pageSize).map(f => {
   const index = files.indexOf(f);
   const historical = f.path.startsWith('data/migration_records/');
   const previewable = f.inRepository && f.size < 800000 && ['r','csv','txt','md','json','py'].includes(f.extension);
   return `<tr><td><strong>${escapeHtml(f.name)}</strong><span class="file-path">${escapeHtml(f.path)}</span>${historical?'<br><span class="badge">Historical record</span>':''}</td><td>${escapeHtml(f.extension.toUpperCase() || 'PROJECT')}</td><td>${formatSize(f.size)}</td><td><div class="file-actions">${previewable?`<button data-preview="${index}">Preview</button>`:''}${f.inRepository?`<a href="${escapeHtml(f.view)}" target="_blank" rel="noopener">View ↗</a>`:''}<a href="${escapeHtml(f.download)}">${f.inRepository?'Download ↓':'Get bundle ↓'}</a></div></td></tr>`;
  }).join('') || '<tr><td colspan="4">No files match these filters. Try a different search or include historical records.</td></tr>';
  $('page-info').textContent=`Page ${state.page+1} of ${pages}`;
  $('previous').disabled=state.page===0;
  $('next').disabled=state.page>=pages-1;
 }
 document.querySelectorAll('[data-category]').forEach(button => button.addEventListener('click',() => {
  state.category=button.dataset.category;state.page=0;
  document.querySelectorAll('[data-category]').forEach(b => {b.classList.toggle('active',b===button);b.setAttribute('aria-pressed',String(b===button));});render();
 }));
 for(const id of ['search','type','history']) $(id).addEventListener(id==='search'?'input':'change',()=>{state[id==='search'?'query':id]=$(id).value;state.page=0;render();});
 $('previous').addEventListener('click',()=>{state.page--;render();});
 $('next').addEventListener('click',()=>{state.page++;render();});
 document.querySelectorAll('[data-query]').forEach(a=>a.addEventListener('click',()=>{state.query=a.dataset.query;state.category='all';state.type='';state.history='current';state.page=0;$('search').value=state.query;$('type').value='';$('history').value='current';document.querySelectorAll('[data-category]').forEach(b=>{const active=b.dataset.category==='all';b.classList.toggle('active',active);b.setAttribute('aria-pressed',String(active));});render();}));
 let previewController;
 $('file-rows').addEventListener('click',async event=>{
  const button=event.target.closest('[data-preview]');if(!button)return;
  if(previewController)previewController.abort();previewController=new AbortController();
  const f=files[Number(button.dataset.preview)];
  $('preview-title').textContent=f.name;$('preview-meta').textContent=f.path+' · '+formatSize(f.size);$('preview-text').textContent='Loading…';$('preview-github').href=f.view;$('preview').showModal();
  try {const response=await fetch(f.download,{signal:previewController.signal});if(!response.ok)throw Error('HTTP '+response.status);const text=await response.text();$('preview-text').textContent=text.length>100000?text.slice(0,100000)+'\n\n[Preview truncated. Open the full file on GitHub.]':text;}
  catch(error){if(error.name!=='AbortError')$('preview-text').textContent='Preview is unavailable. Use the GitHub link below to open this file.';}
 });
 $('close-preview').addEventListener('click',()=>{$('preview').close();if(previewController)previewController.abort();});
 fetch('catalog.json').then(r=>{if(!r.ok)throw Error(r.status);return r.json();}).then(catalog=>{
  files=catalog.files;$('file-count').textContent=files.length;
  const types=[...new Set(files.map(f=>f.extension).filter(Boolean))].sort();
  for(const type of types){const option=document.createElement('option');option.value=type;option.textContent=type.toUpperCase();$('type').append(option);}
  for(const bundle of bundles){const group=files.filter(f=>f.category===bundle[0]);$(`${bundle[0]}-size`).textContent=`${group.length} files · ${formatSize(group.reduce((n,f)=>n+f.size,0))} uncompressed`;}
  render();
 }).catch(()=>{$('result-info').textContent='The file catalogue could not be loaded. Please reload, or browse the GitHub repository and release downloads.';});
}

// Match the compact academic navigation to the current section.
if (typeof document !== 'undefined') {
  function updateNavigation() {
    const hash = window.location.hash || '#home';
    document.querySelectorAll('nav a').forEach(link => {
      if (link.getAttribute('href') === hash) link.setAttribute('aria-current', 'location');
      else link.removeAttribute('aria-current');
    });
  }
  window.addEventListener('hashchange', updateNavigation);
  updateNavigation();
}
