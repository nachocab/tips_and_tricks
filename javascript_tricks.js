// coffeescript compile
cake watch

// coffeescript hash object
math =
  root:   Math.sqrt
  square: square
  cube:   (x) -> x * square x

// pop and shift modify arr, clone arr instead by using slice(0)
arr2 = arr.slice(0).pop()
arr2 = arr.slice(0).shift()

// create an object with the frequencies of each element in an array
t={}
_.chain(arr)
    .groupBy (function (p) { return p; })
    .each (function (e, i) {
        t[i] = _.size (e);
    }); // use .reverse() if you want descending

// underscore.js flatten(array, [shallow])
_.flatten([1, [2], [3, [[4]]]]) // [1, 2, 3, 4]
_.flatten([1, [2], [3, [[4]]]], true) // only first level [1, 2, 3, [[4]]]

// underscore.js without(array, [*values])
_.without([1, 2, 1, 0, 3, 1, 4], 0, 1); //[2, 3, 4]

// underscore.js union(*arrays)
_.union([1, 2, 3], [101, 2, 1, 10], [2, 1]); //[1, 2, 3, 101, 10]

// underscore.js intersection(*arrays)
_.intersection([1, 2, 3], [101, 2, 1, 10], [2, 1]) // [1, 2]

// underscore.js difference(array, *others)
_.difference([1, 2, 3, 4, 5], [5, 2, 10]); // others can be multiple arrays [1, 3, 4]

// underscore.js unique or uniq(array, [isSorted], [iterator])
_.uniq([1, 2, 1, 3, 1, 4]); //[1, 2, 3, 4]
_.uniq([1, 2, 2, 3, 4, 4], true); // faster algorithm
_.uniq([1, 2, 2, 3, 4, 4], function(){}); //  If you want to compute unique items based on a transformation, pass an iterator function.

// underscore.js zip(*arrays)
_.zip(['moe', 'larry', 'curly'], [30, 40, 50], [true, false, false]); // [["moe", 30, true], ["larry", 40, false], ["curly", 50, false]]

// underscore.js extend(destination, *sources) combine into an object
_.extend({name : 'moe'}, {age : 50}); // {name : 'moe', age : 50}

// underscore.js pick(object, *keys)
_.pick({gene_name : 'STAT1', lassa_day_0: 3.2, lassa_day_3: 5.3, cluster:3}, 'gene_name', 'cluster'); // {gene_name : 'STAT1', cluster : 3}

// underscore.js defaults(object, *defaults)
var iceCream = {flavor : "chocolate"};
_.defaults(iceCream, {flavor : "vanilla", sprinkles : "lots"}); //{flavor : "chocolate", sprinkles : "lots"}

// underscore.js has(object, key)
_.has({a: 1, b: 2, c: 3}, "b"); //true

// underscore.js all(list, iterator, [context]) Alias: every
_.all([true, 1, null, 'yes'], _.identity); // false

// underscore.js any(list, [iterator], [context]) Alias: some
_.any([null, 0, 'yes', false]); // true

// underscore.js include(list, value) Alias: contains, nicer than indexOf
_.include([1, 2, 3], 3); //true

// underscore.js invoke(list, methodName, [*arguments])
// Calls the method named by methodName on each value in the list. Any extra arguments passed to invoke will be forwarded on to the method invocation.
_.invoke([[5, 1, 7], [3, 2, 1]], 'sort'); // [[1, 5, 7], [1, 2, 3]]

// underscore.js pluck(list, propertyName)
var stooges = [{name : 'moe', age : 40}, {name : 'larry', age : 50}, {name : 'curly', age : 60}];
_.pluck(stooges, 'name'); // ["moe", "larry", "curly"] convenient version of "map"

// underscore.js sortBy(list, iterator, [context])
_.sortBy([1, 2, 3, 4, 5, 6], function(num){ return Math.sin(num); }); // [5, 4, 6, 3, 1, 2]

// underscore.js groupBy(list, iterator)
_.groupBy([1.3, 2.1, 2.4], function(num){ return Math.floor(num); }); // {1: [1.3], 2: [2.1, 2.4]}
_.groupBy(['lassa_day_3', 'lassa_day_6','ebola_day_3'], function(experiment){ return experiment.split("_")[0]; }); // {ebola: ['ebola_day_3'], lassa: ['lassa_day_3','ebola_day_6']}
_.groupBy(['one', 'two', 'three'], 'length');// iterator can also be a string {3: ["one", "two"], 5: ["three"]}

// underscore.js first
_([1,2,3]).first(2) //[1,2]
_.first([1,2,3],2) //[1,2]

// underscore.js chain
var stooges = [{name : 'curly', age : 25}, {name : 'moe', age : 21}, {name : 'larry', age : 23}];
var youngest = _.chain(stooges)
  .sortBy(function(stooge){ return stooge.age; }) //[{name : 'moe', age : 21}, {name : 'larry', age : 23}, {name : 'curly', age : 25}];
  .map(function(stooge){ return stooge.name + ' is ' + stooge.age; }) // ["moe is 21", "larry is 23", "curly is 25"]
  .first() // "moe is 21"
  .value(); // return unwrapped objects: "moe is 21"

// coffeescript ternary
if item == replaceItem then replaceWith else item // item == replaceItem ? replaceWith : item

// coffeescript there is no elsif
if student.excellentWork
    "A+"
else if student.okayStuff
    "B"
else
    "C"

// coffeescript loops
my_func(item) for item in array              // for(i=0, len=array.length;i < len; i++) { item=array[i]; my_func(item) }
my_func(i+1,item) for item, i in array // for (i=j=0, len=array.length; j < len; i=++j) { item=array[i]; my_func(i+1, item); }
my_func(item) for item in array when item isnt "gene_1" // for (k=0,len=array.length ; k<len ; k++){item = array[k];
//                                                                                                   if (item !== 'gene_1') {
//                                                                                                     my_func(item);
//                                                                                                   }
//                                                                                                 }
// coffeescript map / filter / select
objects.map (object)->
    object.num
objects.filter (object)->
    object.num > 2
objects.select (object)->
    object.num > 2


// comprehensions - use it to collect

countdown = (num for num in [10..7]) // [10,9,8,7]
countdown = (num for num in [10..1] when num > 6) // [10,9,8,7]
even_numbers = (x for x in [0..10] by 2) // [0,2,4,6,8,10]
for num in [10..7]
    "the number is #{num}"

yearsOld = joe: 10, ida: 9, tim: 11 // yearsOld = {joe:10, ida:9, tim:11}
ages = for child, age of yearsOld // "of" iterates over the properties of an object
    "#{child} is #{age}"
ages = (age for child, age of yearsOld) // collect the keys. you need the parenthesis
ages = (child.age for child in yearsOld) // do the same with "in"
ages = ([child.age,child.firstName] for child in yearsOld) // collect a subset of properties

ages = (age for own child, age of yearsOld) // use own to ensure you're not inheriting properties

buy()  while supply > demand // while (supply > demand) { buy() }
sell()  until supply > demand // while (!(supply > demand)) { sell() }
num = 99
cancion = while num -= 1
  "#{num} bottles of beer in the wall..."

until // while not
loop // while true

// array.reverse
[1,2,3].reverse() // [3,2,1]

// array.shift
[1,2,3].shift() // 1 array is now [2,3]

// array.sort - Sort the elements of the array.
// array.splice - Add or remove elements from the array.
// array.unshift - Add one or more elements to the front of the array.

// array.concat - Join the array with other array(s) or value(s).
// array.join - Join all elements of the array into a string.
// array.slice - Extract a section of the array.
// array.indexOf - Find the first occurrence of a value within the array.
// array.lastIndexOf - Find the last occurrence of a value within the array.

// array.filter - Create a new array with only the elements for which a predicate is true. (check out _.reject)
// array.forEach - Call a function for each element in the array.
// array.every - See if every element in the array satisfies a predicate.
// array.map - Create a new array with the result of a function of every element in the array.
// array.some - See if at least one element in the array satisfies a predicate.
// array.reduce - Apply a function to reduce the array to a single value (from left-to-right).
// array.reduceRight - Apply a function to reduce the array to a single value (from right-to-left)

// sorting comparators
d3.ascending(4,1) //1
d3.ascending(1,1) //0
d3.ascending(1,4) //-1
d3.descending(1,4) //1

//d3.min d3.max d3.extent d3.sum
d3.min([1,2,3,-10]) // -10
d3.min([{name:"a",val:3},{name:"b",val:-10}], function(d){ return d.val}) // -10

// d3.first d3.last
d3.first([{name:"a",val:3},{name:"b",val:-10}], function(d){ return d.val}) // {name:"b",val:-10}

// d3.map(object)
a = d3.map({name:"STAT1", logFC:3.25, "p value":1e-8, cluster:1, type:"TF"})
a.keys() // ["name", "logFC", "p value", "cluster", "type"] same as d3.keys({name:"STAT1", logFC:3.25, p_value:1e-8, cluster:1, type:"TF"})
a.values() // ["STAT1", 3.25, 1e-8, 1, "TF"]
a.has("p value") // true
a.get("p value")
a.set("p value",.5)
a.remove("p value")
a.forEach(function(k, v) { // returns undefined, iteration order is arbitrary
    // USEFUL?
});

// d3.line.defined
line.defined((d) => d.geneExpression != null) # specific to d3.line, useful to determine which elements are not defined

// d3.split(array, function)
d3.split([1,3,null,5,undefined,8,9]) // [[1,3],[5],[8,9]]
d3.split([1,3,5,8,9], function(d){ return d == 5}) // [[1,3],[8,9]]

// d3.nest
genes = [{name:"STAT1", logFC:3.25, p_value:1e-8, cluster:1, type:"TF"},
         {name:"IL10", logFC:4.25, p_value:1e-8, cluster:1, type:"cytokine"},
         {name:"STAT2", logFC:2.25, p_value:1e-8, cluster:2, type:"TF"},
         {name:"STAT3", logFC:2.25, p_value:1e-8, cluster:2, type:"TF"},
         {name:"IL2", logFC:1.25, p_value:1e-8, cluster:2, type:"cytokine"},
         {name:"IL3", logFC:3.25, p_value:1e-8, cluster:3, type:"cytokine"},
        ]

nested_genes = d3.nest() // you get the original objects accessible by the keys that you specify
    .key(function(d) { return d.cluster; })
    .key(function(d) { return d.type; })
    .sortKeys(d3.ascending) // sort by cluster and type ascending (cytokine before TF)

nested_genes.entries(genes)
// [...,{key:"2", values: [
//      {key:"TF", values: [
//          {name:"STAT2", logFC:2.25, p_value:1e-8, cluster:2, type:"TF"},
//          {name:"STAT3", logFC:2.25, p_value:1e-8, cluster:2, type:"TF"},
//      ]}
//]}]

nested_genes.rollup(function(d){ // aggregate
    return {
        mean_expression: d3.mean(d, function(g) {return +g.logFC})
    }
}).entries(genes)
// [...,{key:"2", values: [
//      {key:"TF", values: [
//          mean_expression: 2.25
//      ]}
//]}]


// d3.zip(arrays, ...) DON'T QUITE GET THIS
d3.zip([[1,3],[3,5,6]])

// d3.transpose(matrix) DON'T QUITE GET THIS

// d3.merge(arrays)
d3.merge([[1,3],[3,5]]) // [1,3,3,5]

// d3.permute(array,indexes)
arr = ["a", "b", "c"]
d3.permute(arr, [1, 2, 0]) //["b", "c", "a"]
d3.permute(arr, [2, 1]) //["c", "b"]
d3.permute(arr, [2, 1, 2]) //["c", "b", "c"]

// d3.range(start,stop,step)
d3.range(4)       // 0,1,2,3
d3.range(1,4)     // 1,2,3 stop is not included
d3.range(1,8,2)   // 1,3,5,7 ascending
d3.range(8,1,-2)   // 8,6,4,2 descending
d3.range(8,1,2)   // []
d3.range(1,8,-2)   // []

// append a single element
circles = svg.selectAll("circle")
    .data([32, 57, 112, 293])
  .enter().append("circle")
circles.enter().append("circle") // append a single circle

// update the graph on a dropdown menu change
d3.select("#order").on("change", function() { // <select id="order"><option value="name">by Name</option></select>
    order(this.value);
});

function order(value) {
    x.domain(orders[value]); // change the ordering of the matrix

    var t = svg.transition().duration(1000);

    t.selectAll(".row")
        .delay(function(d, i) { return x(i) * 4; })
        .attr("transform", function(d, i) { return "translate(0," + x(i) + ")"; }) //change the position of all rows to the new x scale
      .selectAll(".cell")
        .delay(function(d) { return x(d.x) * 4; })
        .attr("x", function(d) { return x(d.x); }); // change the position of the cells within each row to the new x scale

    t.selectAll(".column")
        .delay(function(d, i) { return x(i) * 4; })
        .attr("transform", function(d, i) { return "translate(" + x(i) + ")rotate(-90)"; });
  }

// classed
matrix.on("mouseover", mouseover)
function mouseover(target) {
    d3.selectAll(".row text").classed("active", function(d, i) { return i == target.y; }); // add the active class to the row text whose target.column was mouseovered
}

// sort
d3.range(5).map(Math.random).sort(function(a, b) { return d3.ascending(a,b);}) // same as { return a-b;}
d3.range(5).map(Math.random).sort(function(a, b) { return d3.ascending(b,a);}) // same as { return b-a;}
d3.range(n).sort(function(a, b) { return nodes[b].count - nodes[a].count; })

order_by_name = d3.range(n).sort(function(a, b) { return d3.ascending(nodes[a].name, nodes[b].name); })
x = d3.scale.ordinal().rangeBands([0, width])
x.domain(order_by_name) // sets the default ordering

// forEach - build a matrix of objects
matrix = [];
arr =  [{name: "paco", age: 32}, {name: "luis", age:33}];
n = arr.length;
arr.forEach(function(object, i) {
    object.index = i;
    object.count = 0;

    matrix[i] = d3.range(n).map(function(j) { return {x: j, y: i, z: 0}; });
}); // matrix is [{x: 0, y: 0, z: 0},{x: 0, y: 1, z: 0}
//                {x: 1, y: 0, z: 0},{x: 1, y: 1, z: 0}]
// arr is [{name: "paco", age: 32, index:0, count:0}, {name: "luis", age:33, index:1, count:0}];

// color the first column/row differently
td = d3.selectAll("tr").selectAll("td")
td.style("color", function(d, i) { return i ? null : "red"; }); // index 0 is falsy, so it's red
td.style("color", function(d,i,j) { return j ? null : "red"}) // color the first cell red

// convert a number to a char
.text(function(d, i) { return String.fromCharCode(65 + i); }); // A B C D E F ...

// d3.scale
d3.scale.linear().domain([0, 1]).range([h, 0])
d3.scale.linear().domain([0, 4]).clamp(true)

d3.scale.ordinal().rangeBands([0, width])
d3.scale.ordinal().domain(d3.range(n)).rangeBands([0, w], .2) //.2 is padding in the range [0,1], .5 means the band is equal to the padding width
d3.scale.ordinal().domain(d3.range(m)).rangeBands([0, y0.rangeBand()])

d3.scale.category10()
d3.scale.category10().domain(d3.range(10))
d3.scale.category20c()

// transform translate - when translating, don't set the x or y attributes, add them to the translation: "translate(20" + (x(i) + 50) +")"
.attr("transform", "translate(20)") // move 20 px to the right
.attr("transform", "translate(-20)") // move 20 px to the left
.attr("transform", "translate(0,10)") // move 10 px down
.attr("transform", "translate(0,-10)") // move 10 px up
.attr("transform", "translate(20,10)") // move 20 px to the right and 10 px down
.attr("transform", "translate(" + margin.left + "," + margin.top + ")")
.attr("transform", "translate(" + x.rangeExtent()[1] + ") rotate(-90)")
.attr("transform", function(d, i) { return "translate(" + y1(i) + ",0)"; })
.attr("transform", function(d, i) { return "translate(0," + x(i) + ")"; })

// d3.select
d3.select("p")
d3.select(e.target)
d3.selectAll("table tr").select("td") // flat selection, select preserves the existing grouping of the previous selection, parent is still body

// d3.selectAll
d3.selectAll("p")
d3.selectAll(e.target)
d3.selectAll(".a, .b") // selects the union of both classes
d3.selectAll("#outer .inner") // same as d3.select("#outer").selectAll(".inner")
d3.selectAll("td") // flat array of td
d3.selectAll("tr").selectAll("td") // nested selection, selectAll creates a new grouping and returns the grouped array of td by tr AND sets the parent to tr
d3.selectAll("table tr").data(arr).enter().append("td") // ERROR: the parent is body, you can't add a td to body

// try catch exception
try
  {
  //Run some code here
  }
catch(err)
  {
  //Handle errors here
  }

// selection.data (when data is assigned to an element, it's stored in __data__, and then DATA NEEDS AN ARRAY OF AN ARRAY)
selection.data() // returns the update selection, the DOM elems that were successfully bound to the data elems.
selection.data(arr) // joins arr with the current selection, by-index (default, to change it, provide a key function)
selection.data(function(d,i) { return i; }) // same as selection.data(arr), except when updating
selection.data(Object) // DON'T USE THIS. the selection already has a data object loaded, which is an array of objects/arrays, Object goes into each element
selection.data(function(d) { return d; }) // USE THIS. d is an element of a previously bound data
selection.data(function(d,i) { return my_array[i]; }) // this is useful when the selection already has data that you don't care about. The length of my_array must match the size of the selection.
selection.data(d3.layout.pie()) // the selection already has a data object loaded, pie() returns the angles for each arc, it's just a function
selection.data(arr, function(d,i) { return d.property; }) // if selection has __data__ (previously bound data), it will do a join with arr

// selection.datum (it clears any previously bound data (it doesn't compute a join), unlike selection.data)
selection.datum() //returns the bound datum for the first non-null element in the selection. This is generally useful only if you know the selection contains exactly one element.
selection.datum(constant) //same for all elems
selection.datum(function(d,i) {return d})
// log 10
function log10(val) {
  return Math.log(val) / Math.log(10);
}

// exponent
Math.pow(2,3) // 8

// create new D3 app
subl models/volcano_model.coffee
subl views/volcano_view.coffee
subl apps/volcano_app.haml apps/volcano_app.coffee

// VAINILLA ELEMENTS svg:
var svg = d3.select("body")
    .append("svg:svg")
    .attr("width", 1000)
    .attr("height", 1000);


var rect = svg.append("rect")
    .attr("x",100)
    .attr("y",50)
    .attr("width", 100)
    .attr("height", 100)
    .style("fill","lightgrey");

var line = svg.append("line")
    // .attr("x1",0)
    .attr("x2",50)
    // .attr("y1",0)
    .attr("y2",50)
    // .style("stroke-width","2px")
    .style("stroke","black");

var text = svg.append("text")
    .attr("x", 20)
    .attr("y", 30)
    .attr("text-anchor", "middle")
    .attr("dx", ".5em")
    // .attr("dy",rect.attr("height")/2)
    // .style("stroke","black")
    .text("hola paco");

// I can't get this to work
var d3_line = d3.svg.line()
    .x(function(d) { return x(d.x); })
    .y(function(d) { return y(d.y); });
var path = svg.selectAll("path")
    .data([{x:1,y:2},{x:10,y:11},{x:20,y:22}])
  .enter().append("path")
    .attr("d", function(d){ return d3_line(d); })

// do this inside the d3.csv(...) loop
var div = d3.select("body").append("div")
        .style("position", "absolute")
        .style("left", 100 + "px")
        .style("width", 100 + "px")
        .style("height", 100 + "px")
        .style("background", "lightgrey")
        // .style("padding", "5px")
        // .text("hola");

// select elements of an array
a = [1,2,3,4]
a.slice(0) // [1,2,3,4]
a.slice(1) // [2,3,4]
a.slice(2,3) // [3]

// debug
debugger

// d3 hover
.on("mouseover", ()-> d3.select(this).style("fill", "#d62728"))
.on("mouseout", ()-> d3.select(this).style("fill", "black"))

// convert an array of strings into integers (or use parseInt)
a = ['1','20','3']
for(var i=0; i<a.length;i++) a[i] = +a[i];
for(var i=0; i<a.length;i++) a[i] = parseInt(a[i], 10); //decimal radix/base

// check for NaNs
isNaN(myVar)

// exponential scientific notation format
num.toExponential(digits) // 3.53e+3

// d3.csv(path,callback) - issues a GET request for the path, asynchronous method, when the data is loaded the callback is called
d3.csv('path/file.csv', function(parsedRows) {
    parsedRows.column_one // file.csv must have a header with these column names
    parsedRows.column_two
})

// d3.csv.parse(csv_contents) - requires a header
contents="col1,col2\n1,2\n3,4"
d3.csv.parse(contents) // [{col1:"1",col2:"2"},{col1:"3",col2:"4"}]

// d3.csv.parseRows(contents, accessor) - transform lines into arrays of comma-separated elems - doesn't require a header
contents="col1,col2\n1,2,3,4"
d3.csv.parseRows(contents) //[["col1","col2"],["1","2","3","4"]]
d3.csv.parseRows(contents, function(row,i) {return i == 0 ? null : row}) //[["1","2","3","4"]] null deletes the row
d3.csv.parseRows(contents, function(row,i) {row[0] = +row[0]; return i == 0 ? null : row}}) //[[1,"2","3","4"]]

// d3.csv.format(rows)

// d3.text(url, mime, callback) d3.json d3.xml d3.html - doesn't require a header
d3.text('genes.csv', 'text/csv', function(text) { // text is a string
    var genes = d3.csv.parseRows(text); // genes is an array (rows) of arrays (cols)
})

d3.csv('genes.csv', function(genes) { // genes is an object. genes.csv must have column names
    genes.gene_names
    genes.cluster
    genes.my_new_column = [3,3,2,34,4]
})

// map
[-2,3,-13].map(Math.abs) // [2, 3, 13]
m=2;n=3;d3.range(m).map(function() { return d3.range(n).map(Math.random); }); // [[.03,.009,.047],[.5,.02,.1]]


// Add column lines [DELETE]
column.append("line")
    .attr("x1", -height);
heatmap.append("line") // last column
    .attr("transform", "translate(" + x.rangeExtent()[1] + ")rotate(-90)")
    .attr("x2", -height);

// Add row lines [DELETE]
    row.append("line")
        .attr("x2", width);
    heatmap.append("line") // last row
        .attr("transform", "translate(0," + y.rangeExtent()[1] + ")")
        .attr("x2", width);

// add a function to d3
d3.selection.prototype.moveToFront = function() {
    return this.each(function() {
      this.parentNode.appendChild(this);
    });
};
d3.select("p").moveToFront()

// draw a number of rectangles with colors
var data = d3.range(-6,7); //-6..6
var rect_size = 25; //px
var x = d3.scale.ordinal().domain(d3.range(data.length)).rangeBands([0, rect_size*data.length]);
var color = d3.scale.linear().domain([-1.5, 0, 1.5]).range(["#278DD6","#ffffff","#d62728"]).clamp(false);
var my_rect = d3.select("body").append("svg")
    .attr("width", 800)
    .attr("height", 200)
    .selectAll(".rect")
    .data(data)
  .enter().append("rect")
    .attr("x", function(d,i) { return x(i); })
    .attr("y",0)
    .attr("width", rect_size)
    .attr("height", rect_size)
    .style("fill", function(d) { return color(d); });

// get file basename
path.replace(/^.*\/|\.[^.]*$/g, '') //remove path and extension
path.replace(/^.*\//g, '') // remove path
path.replace(/\.[^.]*$/g, '') // remove extension

// jQuery ajax
$.get(file, function(data) {
  console.warn(data)
});

// synchronous ajax
$.ajax({
  url: "file.txt",
  context: document.body,
  async: false,
  success: function(data){
    console.warn(data)
  }
})

// svg text align use attr text-anchor values: start | middle | end | inherit

// gsub, use String.replace
"poco".replace(/o/ig,"a")
var re = new RegExp(first_non_alpha,"g");
"poco".replace(re,"a")

// access parentNode
this.parentNode
some_selection.node().parentNode

// format a number with 2 decimals
d3.round(x,2)

// get the computed size of an SVG element
var bbox = node.getBBox()
bbox.width; bbox.height;

// get the computed size of an HTML element
node.offsetHeight
node.offsetWidth

// capitalize string
String.prototype.capitalize = function(){
    return this.replace( /(^|\s)([a-z])/g , function(m,p1,p2){ return p1+p2.toUpperCase();
    } );
};

// d3.nest
d3.nest()
.key(function(d) { return d.year })
.entries([{year:1099, deaths:3},{year:1098, deaths:3},{year:1099, deaths:2}])

clusters = d3.nest()
    .key((d) -> d.cluster)
    .entries(@model.get "genes")


// rename using hash
({PaCo:"paco",Sanchez:"Sánchez"})["Sanchez"]

//external libraries
      %script{:src => "http://code.jquery.com/jquery-1.7.1.min.js"}
      %script{:src => "http://documentcloud.github.com/underscore/underscore.js"}
      %script{:src => "http://documentcloud.github.com/backbone/backbone.js"}
      %script{:src => "http://mbostock.github.com/d3/d3.js"}
      %script{:src => "http://twitter.github.com/bootstrap/assets/js/bootstrap-tooltip.js"}
      %script{:src => "http://twitter.github.com/bootstrap/assets/js/bootstrap-popover.js"}
    <script src='http://code.jquery.com/jquery-1.7.1.min.js'></script>
    <script src='http://documentcloud.github.com/underscore/underscore.js'></script>
    <script src='http://documentcloud.github.com/backbone/backbone.js'></script>
    <script src='http://mbostock.github.com/d3/d3.js'></script>
    <script src='http://twitter.github.com/bootstrap/assets/js/bootstrap-tooltip.js'></script>
    <script src='http://twitter.github.com/bootstrap/assets/js/bootstrap-popover.js'></script>
// internal libraries
    %script{:src => "../../js/jquery.js"}
    %script{:src => "../../js/underscore.js"}
    %script{:src => "../../js/backbone.js"}
    %script{:src => "../../js/d3.js"}
    <script src='../js/jquery.js'></script>
    <script src='../js/underscore.js'></script>
    <script src='../js/backbone.js'></script>
    <script src='../js/d3.js'></script>
    <script src='../js/bootstrap-tooltip.js'></script>
    <script src='../js/bootstrap-popover.js'></script>

// Using class variables
columnNames = getColumnNames(parsed_genes)

Gene = Backbone.Model.extend
    initialize: (attributes)->
        @formatColumns()
        @id = @cid

    formatColumns: ->
        @attributes[columnName] = +@attributes[columnName] for columnName in @constructor.columnNames # column names (conditions) are numeric
,
    columnNames: columnNames  # class variable

Genes = Backbone.Collection.extend
    model: Gene

genes = new Genes(parsed_genes)

// index array (include)
a = [1,2,4]
a.indexOf(4) // 2
a.indexOf(334) // -1

// append array push
a = [1,2,4]
a.push(5) // [ 1, 2, 4, 5 ]

// coffeescript regex with interpolation
///#{interpolatedVariable}///g
/literalString/g

// check if substring matches string
long_str.match ///#{myVar}///g

// absolute value
Math.abs(-1)

// convert number to string
a += ''
a.toString()

//clone an array
clone = originalArray.slice(0)

// quick map, for console
.map(function(x){return x.x})

// get the keys of an object
Object.keys(my_obj)
d3.keys(my_obj)
d3.values(my_obj)
d3.entries(my_obj) //[{key:xx,value:yy},{key:xx,value:yy},{key:xx,value:yy},...]

// count appearance frequency of an element in an array
arr.filter((elem) =>
    !elem.match(/cluster/)
).length

// get the first element of a selection
d3.select("p")

// get the position of an object with jquery
$("#heatmap .row span:first").offset()
