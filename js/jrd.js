function onAllImagesLoaded(proc) {
	Promise.all(
		Array.
			from(document.images).
			filter(img => !img.complete).
			map(img => new Promise(resolve => { img.onload = img.onerror = resolve; }))
	).then(() => {
		console.log('onAllImagesLoaded');
		proc();
	});
}

function setupArrowKeys() {
	addEventListener('keydown' , function(ev) {
		switch(ev.keyCode) {
		case 37: $('.cycle-slideshow').cycle('prev'); break;
		case 39: $('.cycle-slideshow').cycle('next'); break;
		}
	});
}

function setStatus(aString) {
	const statusElement = document.getElementsByClassName('status')[0];
	if(statusElement) {
		statusElement.innerHTML = aString;
	}
}
