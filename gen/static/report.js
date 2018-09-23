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
  ws = new WebSocket(uri)
  ws.onmessage = function (msg) {
    const match = msg.data.match(/^([^:]*):(.*)/)
    if (!match) {
      console.log('msg without type, ignoring')
      return
    }
    const [_,msgType,data] = match
    switch (msgType) {
    case 'row':
      console.log('msg row:', data)
      let tr = document.createElement('tr')
      tr.innerHTML = data.replace(/^<tr>(.*)<\/tr>$/, '$1')
      document.getElementById('results').appendChild(tr)
      update_thumbs()
      break
    default:
      console.log('unknown msg type:', msgType)
    }
  }
}

if (location.hash.indexOf('thumbs') > -1) {
  toggle_thumbs()
}

