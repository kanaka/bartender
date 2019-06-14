let thumb_display = 'none'
let tlink_display = 'inline'
let ws = null
let curTestSlug = null


function update_thumbs() {
  for (let x of document.getElementsByClassName('thumb')) {
    x.style.display = thumb_display
  }
  for (let x of document.getElementsByClassName('tlink')) {
    x.style.display = tlink_display
  }
}

function toggle_thumbs() {
  let toggleb = document.getElementById('toggle')
  if (toggleb.value === 'Show Thumbnails') {
    history.pushState(null, null, '#thumbs')
    toggleb.value = 'Hide Thumbnails'
    thumb_display = 'inline'
    tlink_display = 'none'
  } else {
    history.pushState(null, null, '#')
    toggleb.value = 'Show Thumbnails'
    thumb_display = 'none'
    tlink_display = 'inline'
  }
  update_thumbs()
}

function parseTestSlug(path) {
    return path.match(/\/([\d]+-[\d]+-[\d]+)/)[1]
}

function reportHandler(msg) {
  if (curTestSlug === null) {
    curTestSlug = parseTestSlug(location.pathname)
    console.log(curTestSlug)
  }
  const {msgType,testSlug,data} = msg
  if (curTestSlug !== testSlug) {
      console.error(`msg test ${testSlug} does not match page ${curTestSlug}, ignoring`)
      return
  }
  switch (msgType) {
  case 'row':
    let results = document.getElementById('results')
    let tr = document.createElement('tr')
    tr.innerHTML = data.replace(/^<tr>(.*)<\/tr>$/, '$1')
    results.appendChild(tr)
    update_thumbs()
    break
  case 'summary':
    let summary = document.getElementById('summary')
    summary.innerHTML = data
    break
  case 'newDir':
    console.log('ignoring msg type:', msgType)
    break
  default:
    console.error('unknown msg type:', msgType)
  }
}

function logHandler(msg) {
  const {msgType,testSlug,iteration,data} = msg
  let logs = document.getElementById('logs')
  let tr = document.createElement('tr')
  let tds = `<td>${msgType}</td><td><a href="/gen/${testSlug}/">${testSlug}</a></td>`
  switch (msgType) {
  case 'row':
    // iteration is already included in received row data
    tds += data
        .replace(/^<tr>(.*)<\/tr>$/, '$1')
        .replace(/((?:href="|src="))/g, `$1/gen/${testSlug}/`)
    break
  case 'summary':
    // There will be at least 9 additional cols (with 2 browsers)
    tds += `<td>${iteration}</td><td colspan="9">${data}</td>`
    break
  case 'newDir':
    tds += `<td>&nbsp;</td><td colspan="9">&nbsp;</td>`
    break
  default:
    console.error('unknown msg type:', msgType)
    return
  }
  tr.innerHTML = tds
  logs.appendChild(tr)
  update_thumbs()
}

function dirHandler(msg) {
  const {msgType,data} = msg
  let dirs = document.getElementById('dirs')
  switch (msgType) {
  case 'newDir':
    dirs.innerHTML = ''
    for (let testSlug of data) {
        let tr = document.createElement('tr')
        tr.innerHTML = `<td><a href="/gen/${testSlug}/">${testSlug}</a></td>`
        dirs.appendChild(tr)
    }
    break
  default:
    console.log('ignoring msg type:', msgType)
    break
  }
}

function connect(uri, handler) {
  ws = new WebSocket(uri)
  ws.onopen = function() {
      console.log("WebSocket connection opened")
  }
  ws.onclose = function() {
      console.log("WebSocket connection closed")
  }
  ws.onmessage = function (event) {
    const msg = JSON.parse(event.data)
    const {msgType,testSlug,iteration,data} = msg
    console.log(`msg ${msgType}, slug ${testSlug}, iteration ${iteration}: ${data.toString().slice(0,60)}...`)
    if (!msgType) {
      console.log('msg without msgType, ignoring')
      return
    }
    handler(msg)
  }
}

if (location.hash.indexOf('thumbs') > -1) {
  toggle_thumbs()
}



