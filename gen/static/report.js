let thumb_display = 'none'
let tlink_display = 'inline'
let ws = null

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

function connect(uri) {
  let results = document.getElementById('results')
  let summary = document.getElementById('summary')
  ws = new WebSocket(uri)
  ws.onmessage = function (msg) {
    const match = msg.data.match(/^([^:]*):(.*)/)
    if (!match) {
      console.log('msg without type, ignoring')
      return
    }
    const [_,msgType,data] = match
    console.log(`msg '${msgType}': ${data.slice(0,40)}...`)
    switch (msgType) {
    case 'row':
      let tr = document.createElement('tr')
      tr.innerHTML = data.replace(/^<tr>(.*)<\/tr>$/, '$1')
      results.appendChild(tr)
      update_thumbs()
      break
    case 'summary':
      summary.innerHTML = data
      break
    default:
      console.log('unknown msg type:', msgType)
    }
  }
}

if (location.hash.indexOf('thumbs') > -1) {
  toggle_thumbs()
}

