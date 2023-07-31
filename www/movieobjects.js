// on add function for hall 1
function onAddFunction1(evt) {
    var mlen = parseInt(evt.item.dataset.mlen);
    var adind = parseInt(evt.to.id.split('hall1period')[1]);
    var rtind = parseInt(adind + 1);
    var cleanind = parseInt(adind + mlen + 1);
    var checking = 0;


    for (var i = rtind; i < rtind + mlen + 1; i++) {
        entryCell = document.getElementById('hall1period' + i);
        if (i > 48) { break; }
        else if (entryCell.children.length != 0) { checking = 1; } //checks if there is overlapping timing
    }

    if (checking == 1) {  //if overlap, show warning pop up 
        alert('cannot place here. timing clash');
        this.el.removeChild(evt.item);
    } else {
        evt.to.style.backgroundColor = '#edd361'; //change colour to yellow for scheduled div
        evt.item.classList.add('scheduled'); //add scheduled class to div, for query when "run"
        evt.item.setAttribute('data-period', adind); //add period as attribute to read during "run"
        if (cleanind < 49) {
            var cleanframe = document.getElementById('1time' + cleanind);
            var cleancell = document.getElementById('hall1period' + cleanind);
            cleanframe.style.backgroundColor = '#a061ed'; //change colour to purple for cleaning
            cleancell.style.display = 'none'; //remove the green cell
        }
        for (var i = rtind; i < rtind + mlen; i++) {
            var entryCell = document.getElementById('hall1period' + i);
            var showframes = document.getElementById('1time' + i);
            var entryframe = document.getElementById('1time' + adind);
            entryframe.style.backgroundColor = '#d93df5'; //change colour to pink for ads
            if (i < 49) {
                entryCell.style.display = 'none';
                showframes.style.backgroundColor = '#7e88f2'; //change colour to blue for runtime
            }
        }
    }
}

// on move function for hall 1
function onMoveFunction1(evt) {
    evt.from.style.backgroundColor = 'rgba(102, 255, 153, 0.5)';

    var adind = parseInt(evt.from.id.split('hall1period')[1]);
    var mlen = parseInt(evt.dragged.dataset.mlen);
    var rtind = parseInt(adind + 1);
    var cleanind = parseInt(adind + 1 + mlen);
    var cleanframe = document.getElementById('1time' + cleanind);
    var cleancell = document.getElementById('hall1period' + cleanind);

    if (cleanind < 49) {
        cleanframe.style.backgroundColor = 'white';
        cleancell.style.display = 'flex';
    }
    for (var i = rtind; i < rtind + mlen; i++) {
        var entryCell = document.getElementById('hall1period' + i);
        var showframes = document.getElementById('1time' + i);
        var entryframe = document.getElementById('1time' + adind);
        entryframe.style.backgroundColor = 'white';
        if (i < 49) {
            entryCell.style.display = 'flex';
            showframes.style.backgroundColor = 'white';
        }
    }
}

// on add function for hall 2
function onAddFunction2(evt) {
    var mlen = parseInt(evt.item.dataset.mlen)
    var adind = parseInt(evt.to.id.split('hall2period')[1]);
    var rtind = parseInt(adind + 1);
    var cleanind = parseInt(adind + mlen + 1);
    var checking = 0;


    for (var i = rtind; i < rtind + mlen + 1; i++) {
        entryCell = document.getElementById('hall2period' + i);
        if (i > 48) { break }
        else if (entryCell.children.length != 0) { checking = 1 }

    };

    if (checking == 1) { alert('cannot place here. timing clash'); this.el.removeChild(evt.item) }
    else {
        evt.to.style.backgroundColor = '#edd361';
        evt.item.classList.add('scheduled');
        evt.item.setAttribute('data-period', adind);
        if (cleanind < 49) {
            var cleanframe = document.getElementById('2time' + cleanind);
            var cleancell = document.getElementById('hall2period' + cleanind);
            cleanframe.style.backgroundColor = '#a061ed'; cleancell.style.display = 'none'
        };
        for (var i = rtind; i < rtind + mlen; i++) {
            var entryCell = document.getElementById('hall2period' + i);
            var showframes = document.getElementById('2time' + i);
            var entryframe = document.getElementById('2time' + adind);
            entryframe.style.backgroundColor = '#d93df5'
            if (i < 49) { entryCell.style.display = 'none'; showframes.style.backgroundColor = '#7e88f2'; };
        }
    }
}

// on move function for hall 2
function onMoveFunction2(evt) {
    evt.from.style.backgroundColor = 'rgba(102, 255, 153, 0.5)'
    var adind = parseInt(evt.from.id.split('hall2period')[1]);
    var mlen = parseInt(evt.dragged.dataset.mlen);
    var rtind = parseInt(adind + 1);
    var cleanind = parseInt(adind + 1 + mlen);
    var cleanframe = document.getElementById('2time' + cleanind);
    var cleancell = document.getElementById('hall2period' + cleanind);
    //console.log(mlen)//
    //console.log(cleanind)//
    if (cleanind < 49) { cleanframe.style.backgroundColor = 'white'; cleancell.style.display = 'flex' };
    for (var i = rtind; i < rtind + mlen; i++) {
        var entryCell = document.getElementById('hall2period' + i);
        var showframes = document.getElementById('2time' + i);
        var entryframe = document.getElementById('2time' + adind);
        entryframe.style.backgroundColor = 'white';
        if (i < 49) { entryCell.style.display = 'flex'; showframes.style.backgroundColor = 'white' };
    }
}


// on add function for hall 3
function onAddFunction3(evt) {
    var mlen = parseInt(evt.item.dataset.mlen)
    var adind = parseInt(evt.to.id.split('hall3period')[1]);
    var rtind = parseInt(adind + 1);
    var cleanind = parseInt(adind + mlen + 1);
    var checking = 0;


    for (var i = rtind; i < rtind + mlen + 1; i++) {
        entryCell = document.getElementById('hall3period' + i);
        if (i > 48) { break }
        else if (entryCell.children.length != 0) { checking = 1 }

    };

    if (checking == 1) { alert('cannot place here. timing clash'); this.el.removeChild(evt.item) }
    else {
        evt.to.style.backgroundColor = '#edd361';
        evt.item.classList.add('scheduled');
        evt.item.setAttribute('data-period', adind);
        if (cleanind < 49) {
            var cleanframe = document.getElementById('3time' + cleanind);
            var cleancell = document.getElementById('hall3period' + cleanind);
            cleanframe.style.backgroundColor = '#a061ed'; cleancell.style.display = 'none'
        };
        for (var i = rtind; i < rtind + mlen; i++) {
            var entryCell = document.getElementById('hall3period' + i);
            var showframes = document.getElementById('3time' + i);
            var entryframe = document.getElementById('3time' + adind);
            entryframe.style.backgroundColor = '#d93df5'
            if (i < 49) { entryCell.style.display = 'none'; showframes.style.backgroundColor = '#7e88f2'; };
        }
    }
}

// on move function for hall 3
function onMoveFunction3(evt) {
    evt.from.style.backgroundColor = 'rgba(102, 255, 153, 0.5)'
    var adind = parseInt(evt.from.id.split('hall3period')[1]);
    var mlen = parseInt(evt.dragged.dataset.mlen);
    var rtind = parseInt(adind + 1);
    var cleanind = parseInt(adind + 1 + mlen);
    var cleanframe = document.getElementById('3time' + cleanind);
    var cleancell = document.getElementById('hall3period' + cleanind);
    //console.log(mlen)//
    //console.log(cleanind)//
    if (cleanind < 49) { cleanframe.style.backgroundColor = 'white'; cleancell.style.display = 'flex' };
    for (var i = rtind; i < rtind + mlen; i++) {
        var entryCell = document.getElementById('hall3period' + i);
        var showframes = document.getElementById('3time' + i);
        var entryframe = document.getElementById('3time' + adind);
        entryframe.style.backgroundColor = 'white';
        if (i < 49) { entryCell.style.display = 'flex'; showframes.style.backgroundColor = 'white' };
    }
}

// on add function for hall 4
function onAddFunction4(evt) {
    var mlen = parseInt(evt.item.dataset.mlen)
    var adind = parseInt(evt.to.id.split('hall4period')[1]);
    var rtind = parseInt(adind + 1);
    var cleanind = parseInt(adind + mlen + 1);
    var checking = 0;


    for (var i = rtind; i < rtind + mlen + 1; i++) {
        entryCell = document.getElementById('hall4period' + i);
        if (i > 48) { break }
        else if (entryCell.children.length != 0) { checking = 1 }

    };

    if (checking == 1) { alert('cannot place here. timing clash'); this.el.removeChild(evt.item) }
    else {
        evt.to.style.backgroundColor = '#edd361';
        evt.item.classList.add('scheduled');
        evt.item.setAttribute('data-period', adind);
        if (cleanind < 49) {
            var cleanframe = document.getElementById('4time' + cleanind);
            var cleancell = document.getElementById('hall4period' + cleanind);
            cleanframe.style.backgroundColor = '#a061ed'; cleancell.style.display = 'none'
        };
        for (var i = rtind; i < rtind + mlen; i++) {
            var entryCell = document.getElementById('hall4period' + i);
            var showframes = document.getElementById('4time' + i);
            var entryframe = document.getElementById('4time' + adind);
            entryframe.style.backgroundColor = '#d93df5'
            if (i < 49) { entryCell.style.display = 'none'; showframes.style.backgroundColor = '#7e88f2'; };
        }
    }
}

// on move function for hall 4
function onMoveFunction4(evt) {
    evt.from.style.backgroundColor = 'rgba(102, 255, 153, 0.5)'
    var adind = parseInt(evt.from.id.split('hall4period')[1]);
    var mlen = parseInt(evt.dragged.dataset.mlen);
    var rtind = parseInt(adind + 1);
    var cleanind = parseInt(adind + 1 + mlen);
    var cleanframe = document.getElementById('4time' + cleanind);
    var cleancell = document.getElementById('hall4period' + cleanind);
    //console.log(mlen)//
    //console.log(cleanind)//
    if (cleanind < 49) { cleanframe.style.backgroundColor = 'white'; cleancell.style.display = 'flex' };
    for (var i = rtind; i < rtind + mlen; i++) {
        var entryCell = document.getElementById('hall4period' + i);
        var showframes = document.getElementById('4time' + i);
        var entryframe = document.getElementById('4time' + adind);
        entryframe.style.backgroundColor = 'white';
        if (i < 49) { entryCell.style.display = 'flex'; showframes.style.backgroundColor = 'white' };
    }
}

// for querying scheduled class when "run" button is clicked
function getScheduledData() {

    // Query elements with class "scheduled"
    var scheduledElements = document.getElementsByClassName("scheduled");

    // Loop through each element and get custom attributes
    var dataArr = [];
    for (var i = 0; i < scheduledElements.length; i++) {
        var element = scheduledElements[i];
        var dataName = element.getAttribute("data-name");
        var dataPeriod = element.getAttribute("data-period");

        // Push the data into the array
        dataArr.push({ name: dataName, period: dataPeriod });
    }

    return dataArr;
}

