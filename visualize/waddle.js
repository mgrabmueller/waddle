/* Waddle WAD visualizer */

var width = 640;
var height = 400;

var canvas = document.getElementById("main-canvas");
canvas.width = width;
canvas.height = height;

var ctx = canvas.getContext('2d');

var columns = 320;
var rows = 200;

var screenCanvas = document.getElementById("screen-canvas");
screenCanvas.width = columns*2;
screenCanvas.height = rows*2;

var sctx = screenCanvas.getContext('2d');

function clearScreen() {
    sctx.clearText(0,0,screenCanvas.width, screenCanvas.height);
}

function putPixel(x, y, color) {
    sctx.fillStyle = color;
    sctx.fillRect(x*2, y*2, 2, 2);
}

function clear() {
    ctx.clearRect(0,0,width,height);
}

var userScale = 1;
var userXOffset = 0;
var userYOffset = 0;
var moveMode = false;

var drawLineDefs = true;
var drawSegs = false;
var drawNodes = false;
var drawGrid = false;
var drawThings = false;

var redraw = true;

var viewMode = 'level';

var currentLevel = '';
var currentLevelFrame = 0;
var currentSprite = '';
var currentSpriteIndex = 0;
var animSprites = [];

var currentTexture = '';

var currentFlat = '';

var currentPatch = '';

var player = null;

function searchNode(nodes, i, x, y) {
    var node = nodes[i];

    if (x >= node.lbblx && y >= node.lbbly && x <= node.lbbux && y <= node.lbbuy) {
        if (node.leftNodeOrSSector < 0x8000) {
            return searchNode(nodes, node.leftNodeOrSSector, x, y);
        } else {
            return node.leftNodeOrSSector - 0x8000;
        }
    } else if (x >= node.rbblx && y >= node.rbbly && x <= node.rbbux && y <= node.rbbuy) {
        if (node.rightNodeOrSSector < 0x8000) {
            return searchNode(nodes, node.rightNodeOrSSector, x, y);
        } else {
            return node.rightNodeOrSSector - 0x8000;
        }
    } else {
        return null;
    }
}

function dotprod(x0, y0, x1, y1) {
    return x0 * x1 + y0 * y1;
}
function drawNode(nodes, i, depth) {
    if (depth > 100) {
        return;
    }
    var node = nodes[i];

    if (!player) {
        return;
    }

    var x = player.x;
    var y = player.y;
    var angle = player.angle;

    // Calculate normal on splitting line.
    var normal_x = node.dy;
    var normal_y = -node.dx;
    var nlen = Math.sqrt(normal_x * normal_x + normal_y * normal_y);
    normal_x /= nlen;
    normal_y /= nlen;

    ctx.save();
    ctx.lineWidth = ctx.lineWidth * 2;
    
    // Draw line along the splitting line.
    ctx.strokeStyle = 'rgba(0,255,0,1)';
    ctx.beginPath();
    ctx.moveTo(node.x, node.y);
    ctx.lineTo(node.x + node.dx, node.y + node.dy);
    // ctx.moveTo(node.x - node.dx*3, node.y - node.dx*3);
    // ctx.lineTo(node.x + node.dx*4, node.y + node.dy*4);
    ctx.stroke();

    // Draw split line normal.
    ctx.strokeStyle = 'rgba(0,200,100,1)';
    ctx.beginPath();
    ctx.moveTo(node.x + node.dx/2, node.y + node.dy/2);
    ctx.lineTo(node.x + node.dx/2 + normal_x*100, node.y + node.dy/2 + normal_y*100);
    ctx.stroke();

    ctx.restore();

    var dist_x = node.x - x;
    var dist_y = node.y - y;

    var dot = dotprod(dist_x, dist_y, normal_x, normal_y);

    // Depending on the dot product between the splitting line normal
    // and the difference between a point on the spliting line and the
    // player pos, choose the left or right subspace of the BSP.
    //
    if (dot >= 0) {
        ctx.strokeStyle = 'rgba(255,0,0,0.25)';
        ctx.strokeRect(node.lbblx, node.lbbly, node.lbbux - node.lbblx, node.lbbuy - node.lbbly);
        if (node.leftNodeOrSSector < 0x8000) {
            drawNode(nodes, node.leftNodeOrSSector, depth + 1);
        } else {
            console.log('player is in ssector', node.leftNodeOrSSector - 0x8000);
        }
    } else if (dot < 0) {
        ctx.strokeStyle = 'rgba(0,0,255,0.25)';
        ctx.strokeRect(node.rbblx, node.rbbly, node.rbbux - node.rbblx, node.rbbuy - node.rbbly);
        if (node.rightNodeOrSSector < 0x8000) {
            drawNode(nodes, node.rightNodeOrSSector, depth + 1);
        } else {
            console.log('player is in ssector', node.rightNodeOrSSector - 0x8000);
        }
    } else {
        return;
    }
}

function drawLevel(s) {
    if (levels[s] === undefined) {
	return;
    }
    clear();
    var level = levels[s];

    var lines = level.linedefs;
    var vertices = level.vertices;
    var ssectors = level.ssectors;
    var segs = level.segs;
    var minx =  1000000;
    var maxx = -1000000;
    var miny =  1000000;
    var maxy = -1000000;
    for (var i = 0; i < lines.length; i++) {
        var l = lines[i];
        minx = Math.min(minx, Math.min(vertices[l.start].x, vertices[l.end].x));
        maxx = Math.max(maxx, Math.max(vertices[l.start].x, vertices[l.end].x));
        miny = Math.min(miny, Math.min(vertices[l.start].y, vertices[l.end].y));
        maxy = Math.max(maxy, Math.max(vertices[l.start].y, vertices[l.end].y));
    }
    
    var mapWidth = maxx-minx;
    var mapHeight = maxy-miny;

    var scale = (Math.max(mapWidth/width, mapHeight/height))*1.1*userScale;

    ctx.save();
    ctx.translate(userXOffset, userYOffset);
    ctx.scale(1/scale, -1/scale);
    ctx.translate(-minx+((width*scale-mapWidth)/2), -maxy-((height*scale-mapHeight)/2));

    ctx.lineWidth = scale;

    if (drawGrid) {
        var minxDown = Math.floor(minx / 128)*128;
        var maxxUp = Math.ceil(maxx / 128)*128;
        var minyDown = Math.floor(miny/128)*128;
        var maxyUp = Math.ceil(maxy/128)*128
        for (var x = minxDown; x <= maxxUp; x += 128) {
	    ctx.strokeStyle = x == 0 ? 'red' : 'rgba(100,100,100,0.5)';
	    ctx.beginPath();
	    ctx.moveTo(x, minyDown);
	    ctx.lineTo(x, maxyUp);
	    ctx.stroke();
        }
        for (var y = minyDown; y <= maxyUp; y += 128) {
	    ctx.strokeStyle = y == 0 ? 'red' : 'rgba(100,100,100,0.5)';
	    ctx.beginPath();
	    ctx.moveTo(minxDown, y);
	    ctx.lineTo(maxxUp, y);
	    ctx.stroke();
        }
    }

    if (drawLineDefs) {
        ctx.strokeStyle = 'black';

        for (var i = 0; i < lines.length; i++) {
            var l = lines[i];
            ctx.beginPath();
            ctx.moveTo(vertices[l.start].x, vertices[l.start].y);
            ctx.lineTo(vertices[l.end].x, vertices[l.end].y);
            ctx.stroke();
        }
    }

    if (drawSegs) {
        var segColors = ['red', 'blue', 'green', 'gray'];
        var segs = level.segs;
        
        for (var i = 0; i < segs.length; i++) {
            var seg = segs[i];
            ctx.strokeStyle = segColors[i % segColors.length];
            ctx.beginPath();
            ctx.moveTo(vertices[seg.start].x, vertices[seg.start].y);
            ctx.lineTo(vertices[seg.end].x, vertices[seg.end].y);
            ctx.stroke();
        }

        if (false) {
            var ssectors = level.ssectors;
            for (var i = 0; i < ssectors.length; i++) {
                var ssector = ssectors[i];
                ctx.fillStyle = 'rgba(' + (i % 255) + ',' + (i % 255) + ',' + (i % 255) + ',0.5)';
                ctx.beginPath();
                ctx.moveTo(vertices[segs[ssector.segStart].start].x, vertices[segs[ssector.segStart].start].y);
                for (var j = 1; j < ssector.segCount; i++) {
                    ctx.lineTo(vertices[segs[j+ssector.segStart].start].x, vertices[segs[j+ssector.segStart].start].y);
                }
                ctx.fill();
            }
        }
    }

    if (drawNodes) {
        var nodes = level.nodes;

        drawNode(nodes, nodes.length - 1, 0)
    }

    if (drawThings) {
        var things = level.things;
        for (var i = 0; i < things.length; i++) {
            var t = things[i];
            switch (t.type) {
            case 'Player1StartPos':
            case 'Player2StartPos':
            case 'Player3StartPos':
            case 'Player4StartPos':
                ctx.strokeStyle = 'green';
	        ctx.fillStyle = 'green';
	        break;
            case 'DeathMatchStartPos':
	        ctx.strokeStyle = 'rgb(0,200,0)';
	        ctx.fillStyle = 'rgb(0,200,0)';
	        break;
            case 'Imp':
	        ctx.strokeStyle = 'darkgreen';
	        ctx.fillStyle = 'darkgreen';
	        break;
            case 'TeleportLanding':
	        ctx.strokeStyle = 'red';
	        ctx.fillStyle = 'red';
	        break;
            case 'HealthPotion':
	        ctx.strokeStyle = 'blue';
	        ctx.fillStyle = 'blue';
	        break;
            case 'SpiritArmor':
	        ctx.strokeStyle = 'rgb(100,100,100)';
	        ctx.fillStyle = 'rgb(100,100,100)';
	        break;
            case 'AmmoClip':
            case 'BoxOfAmmo':
	        ctx.strokeStyle = 'rgb(50,50,50)';
	        ctx.fillStyle = 'rgb(50,50,50)';
	        break;
            default:
	        ctx.strokeStyle = 'black';
	        ctx.fillStyle = 'black';
	        break;
            }
            ctx.beginPath();
            ctx.moveTo(t.x, t.y);
            var ang = t.angle/180*Math.PI;
            ctx.lineTo(t.x + Math.cos(ang)*25, t.y + Math.sin(ang)*25);
            ctx.stroke();
            ctx.beginPath();
            ctx.arc(t.x, t.y, 15, 0, 2*Math.PI);
            ctx.fill();
        }
    }
    if (player) {
        ctx.strokeStyle = 'black';
        ctx.fillStyle = 'black';
        ctx.beginPath();
        ctx.moveTo(player.x, player.y);
        ctx.lineTo(player.x + Math.cos(player.angle)*30, player.y + Math.sin(player.angle)*30);
        ctx.stroke();
        ctx.beginPath();
        ctx.arc(player.x, player.y, 20, 0, 2*Math.PI);
        ctx.fill();
    }
    ctx.restore();
}

function drawSprite(s) {
    if (sprites[s] === undefined) {
	return;
    }
    clear();

    var scale = 2;

    var sprite = animSprites.length > 0 ? animSprites[Math.round(currentSpriteIndex / 6) % animSprites.length] : sprites[s];

    if (animSprites.length > 0) {
        redraw = true;
    }
    
    var cx = (width / 2) | 0;
    var cy = (height * 2 / 3) | 0;
    ctx.strokeStyle = 'rgba(255,0,0,0.5)';
    ctx.beginPath();
    ctx.moveTo(0,cy);
    ctx.lineTo(width,cy);
    ctx.moveTo(cx,0);
    ctx.lineTo(cx,height);
    ctx.stroke();
    for (var x = 0; x < sprite.width; x++) {
	var y = 0;
	var col = sprite.columns[x];
	for (var p = 0; p < col.length; p++) {
	    var post = col[p];
	    var tx = cx - sprite.leftOffset*scale + x*scale;
	    for (var i = 0; i < post.pixels.length; i++) {
		var ty = cy - sprite.topOffset*scale + (post.top + i)*scale;
		var idx = post.pixels[i];
		var rgb = palettes[0][idx];
		ctx.fillStyle = 'rgb(' + rgb[0] + ',' + rgb[1] + ',' + rgb[2] + ')';
		ctx.fillRect(tx, ty, scale, scale);
	    }
	}
    }
}

function drawPatch(s) {
    if (patches[s] === undefined) {
	return;
    }
    clear();
    var scale = 2;
    var patch = patches[s];
    var cx = (width / 2) | 0;
    var cy = (height * 2 / 3) | 0;
    ctx.strokeStyle = 'rgba(255,0,0,0.5)';
    ctx.beginPath();
    ctx.moveTo(0,cy);
    ctx.lineTo(width,cy);
    ctx.moveTo(cx,0);
    ctx.lineTo(cx,height);
    ctx.stroke();
    for (var x = 0; x < patch.width; x++) {
	var y = 0;
	var col = patch.columns[x];
	for (var p = 0; p < col.length; p++) {
	    var post = col[p];
	    var tx = cx - patch.leftOffset*scale + x*scale;
	    for (var i = 0; i < post.pixels.length; i++) {
		var ty = cy - patch.topOffset*scale + (post.top + i)*scale;
		var idx = post.pixels[i];
		var rgb = palettes[0][idx];
		ctx.fillStyle = 'rgb(' + rgb[0] + ',' + rgb[1] + ',' + rgb[2] + ')';
		ctx.fillRect(tx, ty, scale, scale);
	    }
	}
    }
}

function drawFlat(s) {
    if (flats[s] === undefined) {
	return;
    }
    clear();
    var flat = flats[s];
    var scale = 4;
    for (var i = 0; i < flat.data.length; i++) {
	var tx = (i % 64 | 0) * scale;
	var ty = (i / 64 | 0) * scale;
	var idx = flat.data[i];
	var rgb = palettes[0][idx];
	ctx.fillStyle = 'rgb(' + rgb[0] + ',' + rgb[1] + ',' + rgb[2] + ')';
	ctx.fillRect(tx, ty, scale, scale);
    }
}

function drawTexture(s) {
    if (textures[s] === undefined) {
	return;
    }
    clear();
    var texture = textures[s];
    var scale = 2;
    var minx = 10;
    var maxx = minx + texture.width;
    var miny = 10;
    var maxy = miny + texture.height;
    for (var patchIdx = 0; patchIdx < texture.patches.length; patchIdx++) {
	var patchDesc = texture.patches[patchIdx];
	var patchName = pnames[patchDesc.pname];
	var patch = patches[patchName];

	var cx = patchDesc.xoffset;
	var cy = patchDesc.yoffset;

	for (var x = 0; x < patch.width; x++) {
	    var y = 0;
	    var col = patch.columns[x];
	    for (var p = 0; p < col.length; p++) {
		var post = col[p];
//		var tx = cx - patch.leftOffset + x;
		var tx = cx + x;
		for (var i = 0; i < post.pixels.length; i++) {
//		    var ty = cy - patch.topOffset + (post.top + i);
		    var ty = cy + (post.top + i);
		    var idx = post.pixels[i];
		    var rgb = palettes[0][idx];
		    if (tx >= 0 && tx <= texture.width &&
			ty >= 0 && ty <= texture.height) {
			ctx.fillStyle = 'rgb(' + rgb[0] + ',' + rgb[1] + ',' + rgb[2] + ')';
		    } else {
			ctx.fillStyle = 'rgba(' + rgb[0] + ',' + rgb[1] + ',' + rgb[2] + ',0.1)';
		    }
		    ctx.fillRect((minx+tx)*scale, (miny+ty)*scale, scale, scale);
		}
	    }
	}
    }
    ctx.strokeStyle = 'black';
    ctx.strokeRect(minx*scale,miny*scale,texture.width*scale,texture.height*scale);
}

function frame() {
    requestAnimationFrame(frame);
    currentSpriteIndex += 1;
    if (!redraw) {
        return;
    }
    redraw = false;
    switch (viewMode) {
    case 'level':
        drawLevel(currentLevel);
        break;
    case 'texture':
        drawTexture(currentTexture);
        break;
    case 'patch':
        drawPatch(currentPatch);
        break;
    case 'flat':
        drawFlat(currentFlat);
        break;
    case 'sprite':
        drawSprite(currentSprite);
        break;
    }

    for (var d = 0; d < 1000; d++) {
        putPixel(Math.random() * columns | 0, Math.random() * rows | 0, 'red');
    }
}

function keyUpHandler(e) {
    switch (e.which) {
    case 83: // 's'
	userScale *= 1.2;
	if (userScale > 10) {
	    userScale = 10;
	}
	redraw = true;
	break;
    case 87: // 'w'
	userScale *= 0.8;
	if (userScale < 0.25) {
	    userScale = 0.25;
	}
	redraw = true;
	break;
    case 77: // 'm'
        moveMode = !moveMode;
        break;
    case 38: // up
        if (moveMode) {
            if (player) player.y += 30;
        } else {
	    userYOffset += 10;
        }
	redraw = true;
	break;
    case 40: // down
        if (moveMode) {
            if (player) player.y -= 30;
        } else {
	    userYOffset -= 10;
        }
	redraw = true;
	break;
    case 37: // left
        if (moveMode) {
            if (player) player.x -= 30;
        } else {
	    userXOffset += 10;
        }
	redraw = true;
	break;
    case 39: // right
        if (moveMode) {
            if (player) player.x += 30;
        } else {
	    userXOffset -= 10;
        }
	redraw = true;
	break;
    default:
	console.log('key up', e.which);
	break;
    }
}

function chooseLevel(s) {
    viewMode = 'level';
    currentLevel = s;
    redraw = true;
    player = null;
    if (currentLevel in levels) {
        var things = levels[currentLevel].things;
        var nodes = levels[currentLevel].nodes;
        for (var i = 0; i < things.length; i++) {
            if (things[i].type == "Player1StartPos") {
                player = {x: things[i].x,
                          y: things[i].y,
                          angle: things[i].angle*Math.PI/180
                         };
                var ssector = searchNode(nodes, nodes.length - 1, player.x, player.y);
                player.ssector = ssector;
            }
        }
    }
}

var levelSelect = document.getElementById('levels');
for (var s in levels) {
    var o = document.createElement('option');
    if (currentLevel == '') {
        chooseLevel(s);
        o.selected = true;
    }
    o.text = s;
    levelSelect.add(o);
}
levelSelect.addEventListener('change', function(e) {
    chooseLevel(this.options[this.selectedIndex].text);
});

select = document.getElementById('sprites');
for (var s in sprites) {
    var o = document.createElement('option');
    o.text = s;
    select.add(o);
}
select.addEventListener('change', function(e) {
    viewMode = 'sprite';
    currentSprite = this.options[this.selectedIndex].text;
    var animation = currentSprite.substr(0, 4);
    animSprites = [];
    for (var k in sprites) {
        if (k.substr(0,4) == animation) {
            animSprites.push(sprites[k]);
        }
    }
    redraw = true;
});

select = document.getElementById('patches');
for (var s in patches) {
    var o = document.createElement('option');
    o.text = s;
    select.add(o);
}
select.addEventListener('change', function(e) {
    viewMode = 'patch';
    currentPatch = this.options[this.selectedIndex].text;
    redraw = true;
});

select = document.getElementById('flats');
for (var s in flats) {
    var o = document.createElement('option');
    o.text = s;
    select.add(o);
}
select.addEventListener('change', function(e) {
    viewMode = 'flat';
    currentFlat = this.options[this.selectedIndex].text;
    redraw = true;
});

select = document.getElementById('textures');
for (var s in textures) {
    var o = document.createElement('option');
    o.text = s;
    select.add(o);
}
select.addEventListener('change', function(e) {
    viewMode = 'texture';
    currentTexture = this.options[this.selectedIndex].text;
    redraw = true;
});

var gridCheckbox = document.getElementById('drawGrid');
gridCheckbox.addEventListener('change', function(e) {
    drawGrid = !drawGrid;
    redraw = true;
});

var lineDefsCheckbox = document.getElementById('drawLineDefs');
lineDefsCheckbox.addEventListener('change', function(e) {
    drawLineDefs = !drawLineDefs;
    redraw = true;
});

var segsCheckbox = document.getElementById('drawSegs');
segsCheckbox.addEventListener('change', function(e) {
    drawSegs = !drawSegs;
    redraw = true;
});

var nodesCheckbox = document.getElementById('drawNodes');
nodesCheckbox.addEventListener('change', function(e) {
    drawNodes = !drawNodes;
    currentLevelFrame = 0;
    redraw = true;
});

var thingsCheckbox = document.getElementById('drawThings');
thingsCheckbox.addEventListener('change', function(e) {
    drawThings = !drawThings;
    redraw = true;
});

function resizeHandler(e) {
    width = window.innerWidth - 700;
    height = window.innerHeight - 80;
    canvas.width = width;
    canvas.height = height;
    redraw = true;
}

window.addEventListener('resize', resizeHandler);

document.addEventListener('keyup', keyUpHandler);

resizeHandler();

requestAnimationFrame(frame);
