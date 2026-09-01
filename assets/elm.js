(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.a8.I === region.bo.I)
	{
		return 'on line ' + region.a8.I;
	}
	return 'on lines ' + region.a8.I + ' through ' + region.bo.I;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**_UNUSED/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.cu,
		impl.cY,
		impl.cS,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS
//
// For some reason, tabs can appear in href protocols and it still works.
// So '\tjava\tSCRIPT:alert("!!!")' and 'javascript:alert("!!!")' are the same
// in practice. That is why _VirtualDom_RE_js and _VirtualDom_RE_js_html look
// so freaky.
//
// Pulling the regular expressions out to the top level gives a slight speed
// boost in small benchmarks (4-10%) but hoisting values to reduce allocation
// can be unpredictable in large programs where JIT may have a harder time with
// functions are not fully self-contained. The benefit is more that the js and
// js_html ones are so weird that I prefer to see them near each other.


var _VirtualDom_RE_script = /^script$/i;
var _VirtualDom_RE_on_formAction = /^(on|formAction$)/i;
var _VirtualDom_RE_js = /^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/i;
var _VirtualDom_RE_js_html = /^\s*(j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:|d\s*a\s*t\s*a\s*:\s*t\s*e\s*x\s*t\s*\/\s*h\s*t\s*m\s*l\s*(,|;))/i;


function _VirtualDom_noScript(tag)
{
	return _VirtualDom_RE_script.test(tag) ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return _VirtualDom_RE_on_formAction.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return _VirtualDom_RE_js.test(value)
		? /**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return _VirtualDom_RE_js_html.test(value)
		? /**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlJson(value)
{
	return (typeof _Json_unwrap(value) === 'string' && _VirtualDom_RE_js_html.test(_Json_unwrap(value)))
		? _Json_wrap(
			/**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		) : value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		X: func(record.X),
		a9: record.a9,
		a5: record.a5
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.X;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.a9;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.a5) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.cu,
		impl.cY,
		impl.cS,
		function(sendToApp, initialModel) {
			var view = impl.cZ;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.cu,
		impl.cY,
		impl.cS,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.a6 && impl.a6(sendToApp)
			var view = impl.cZ;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.cb);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.cV) && (_VirtualDom_doc.title = title = doc.cV);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.cI;
	var onUrlRequest = impl.cJ;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		a6: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.bO === next.bO
							&& curr.bx === next.bx
							&& curr.bL.a === next.bL.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		cu: function(flags)
		{
			return A3(impl.cu, flags, _Browser_getUrl(), key);
		},
		cZ: impl.cZ,
		cY: impl.cY,
		cS: impl.cS
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { cs: 'hidden', ce: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { cs: 'mozHidden', ce: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { cs: 'msHidden', ce: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { cs: 'webkitHidden', ce: 'webkitvisibilitychange' }
		: { cs: 'hidden', ce: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		bT: _Browser_getScene(),
		b0: {
			b4: _Browser_window.pageXOffset,
			b5: _Browser_window.pageYOffset,
			b2: _Browser_doc.documentElement.clientWidth,
			bv: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		b2: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		bv: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			bT: {
				b2: node.scrollWidth,
				bv: node.scrollHeight
			},
			b0: {
				b4: node.scrollLeft,
				b5: node.scrollTop,
				b2: node.clientWidth,
				bv: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			bT: _Browser_getScene(),
			b0: {
				b4: x,
				b5: y,
				b2: _Browser_doc.documentElement.clientWidth,
				bv: _Browser_doc.documentElement.clientHeight
			},
			cl: {
				b4: x + rect.left,
				b5: y + rect.top,
				b2: rect.width,
				bv: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $author$project$Listing$Ignore = {$: 8};
var $author$project$Listing$Model = F6(
	function (cols, rows, at, flags, hint, narrow) {
		return {aH: at, aK: cols, P: flags, bw: hint, az: narrow, C: rows};
	});
var $elm$core$Maybe$Nothing = {$: 1};
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\u000A    ',
		A2($elm$core$String$split, '\u000A', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\u000A\u000A(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\u0027' + (f + '\u0027]'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\u000A\u000A',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\u000A\u000A';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\u000A\u000A    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\u000A\u000A' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.k) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.n),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.n);
		} else {
			var treeLen = builder.k * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.o) : builder.o;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.k);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.n) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.n);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{o: nodeList, k: (len / $elm$core$Array$branchFactor) | 0, n: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {bq: fragment, bx: host, bJ: path, bL: port_, bO: protocol, bP: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 1) {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$element = _Browser_element;
var $author$project$Listing$Column = F4(
	function (key, header, kind, badges) {
		return {bg: badges, bu: header, bA: key, j: kind};
	});
var $author$project$Listing$Badge = F2(
	function (value, colour) {
		return {cg: colour, b$: value};
	});
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Listing$badgeD = A3(
	$elm$json$Json$Decode$map2,
	$author$project$Listing$Badge,
	A2($elm$json$Json$Decode$field, 'value', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'color', $elm$json$Json$Decode$string));
var $elm$json$Json$Decode$list = _Json_decodeList;
var $elm$json$Json$Decode$map4 = _Json_map4;
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $author$project$Listing$columnD = A5(
	$elm$json$Json$Decode$map4,
	$author$project$Listing$Column,
	A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'header', $elm$json$Json$Decode$string),
	$elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2($elm$json$Json$Decode$field, 'type', $elm$json$Json$Decode$string),
				$elm$json$Json$Decode$succeed('text')
			])),
	$elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$json$Json$Decode$field,
				'badges',
				$elm$json$Json$Decode$list($author$project$Listing$badgeD)),
				$elm$json$Json$Decode$succeed(_List_Nil)
			])));
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $author$project$Listing$flagsD = A3(
	$elm$json$Json$Decode$map2,
	$elm$core$Tuple$pair,
	A2(
		$elm$json$Json$Decode$field,
		'cols',
		$elm$json$Json$Decode$list($author$project$Listing$columnD)),
	A2($elm$json$Json$Decode$field, 'hint', $elm$json$Json$Decode$string));
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $author$project$Listing$listIn = _Platform_incomingPort('listIn', $elm$json$Json$Decode$value);
var $author$project$Listing$ClearFlags = {$: 5};
var $author$project$Listing$Flag = function (a) {
	return {$: 3, a: a};
};
var $author$project$Listing$Narrow = function (a) {
	return {$: 6, a: a};
};
var $author$project$Listing$Select = function (a) {
	return {$: 1, a: a};
};
var $author$project$Listing$SetRows = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$Listing$Step = function (a) {
	return {$: 2, a: a};
};
var $author$project$Listing$Unflag = function (a) {
	return {$: 4, a: a};
};
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$json$Json$Decode$null = _Json_decodeNull;
var $elm$json$Json$Decode$nullable = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				$elm$json$Json$Decode$null($elm$core$Maybe$Nothing),
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder)
			]));
};
var $author$project$Listing$Row = F3(
	function (id, cells, colour) {
		return {ac: cells, cg: colour, c: id};
	});
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $elm$core$String$fromFloat = _String_fromNumber;
var $author$project$Listing$cellD = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			$elm$json$Json$Decode$string,
			A2($elm$json$Json$Decode$map, $elm$core$String$fromInt, $elm$json$Json$Decode$int),
			A2($elm$json$Json$Decode$map, $elm$core$String$fromFloat, $elm$json$Json$Decode$float),
			$elm$json$Json$Decode$null('')
		]));
var $elm$json$Json$Decode$keyValuePairs = _Json_decodeKeyValuePairs;
var $elm$json$Json$Decode$map3 = _Json_map3;
var $elm$json$Json$Decode$maybe = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder),
				$elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing)
			]));
};
var $author$project$Listing$rowD = A4(
	$elm$json$Json$Decode$map3,
	$author$project$Listing$Row,
	A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'cells',
		$elm$json$Json$Decode$keyValuePairs($author$project$Listing$cellD)),
	$elm$json$Json$Decode$maybe(
		A2($elm$json$Json$Decode$field, 'colour', $elm$json$Json$Decode$string)));
var $author$project$Listing$msgD = A2(
	$elm$json$Json$Decode$andThen,
	function (kind) {
		switch (kind) {
			case 'setRows':
				return A3(
					$elm$json$Json$Decode$map2,
					$author$project$Listing$SetRows,
					A2(
						$elm$json$Json$Decode$field,
						'rows',
						$elm$json$Json$Decode$list($author$project$Listing$rowD)),
					A2(
						$elm$json$Json$Decode$field,
						'at',
						$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string)));
			case 'select':
				return A2(
					$elm$json$Json$Decode$map,
					$author$project$Listing$Select,
					A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string));
			case 'step':
				return A2(
					$elm$json$Json$Decode$map,
					$author$project$Listing$Step,
					A2($elm$json$Json$Decode$field, 'by', $elm$json$Json$Decode$int));
			case 'flag':
				return A2(
					$elm$json$Json$Decode$map,
					$author$project$Listing$Flag,
					A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string));
			case 'unflag':
				return A2(
					$elm$json$Json$Decode$map,
					$author$project$Listing$Unflag,
					A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string));
			case 'clearFlags':
				return $elm$json$Json$Decode$succeed($author$project$Listing$ClearFlags);
			case 'narrow':
				return A2(
					$elm$json$Json$Decode$map,
					$author$project$Listing$Narrow,
					A2(
						$elm$json$Json$Decode$field,
						'text',
						$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string)));
			default:
				return $elm$json$Json$Decode$succeed($author$project$Listing$Ignore);
		}
	},
	A2($elm$json$Json$Decode$field, 'kind', $elm$json$Json$Decode$string));
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (!_v0.$) {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Listing$cellOf = F2(
	function (r, key) {
		return A2(
			$elm$core$Maybe$withDefault,
			'',
			$elm$core$List$head(
				A2(
					$elm$core$List$filterMap,
					function (_v0) {
						var k = _v0.a;
						var v = _v0.b;
						return _Utils_eq(k, key) ? $elm$core$Maybe$Just(v) : $elm$core$Maybe$Nothing;
					},
					r.ac)));
	});
var $elm$core$String$toLower = _String_toLower;
var $author$project$Listing$holds = F3(
	function (want, m, r) {
		return A2(
			$elm$core$String$contains,
			$elm$core$String$toLower(want),
			$elm$core$String$toLower(
				A2(
					$elm$core$String$join,
					'\u001F',
					A2(
						$elm$core$List$map,
						function (c) {
							return A2($author$project$Listing$cellOf, r, c.bA);
						},
						m.aK))));
	});
var $author$project$Listing$shown = function (m) {
	var _v0 = m.az;
	if (_v0.$ === 1) {
		return m.C;
	} else {
		var want = _v0.a;
		return A2(
			$elm$core$List$filter,
			A2($author$project$Listing$holds, want, m),
			m.C);
	}
};
var $author$project$Listing$clamp = function (m) {
	return _Utils_update(
		m,
		{
			aH: A2(
				$elm$core$Basics$max,
				0,
				A2(
					$elm$core$Basics$min,
					$elm$core$List$length(
						$author$project$Listing$shown(m)) - 1,
					m.aH))
		});
};
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $author$project$Scan$nth = F2(
	function (i, xs) {
		return $elm$core$List$head(
			A2($elm$core$List$drop, i, xs));
	});
var $author$project$Listing$idAt = function (m) {
	return A2(
		$elm$core$Maybe$withDefault,
		'',
		A2(
			$elm$core$Maybe$map,
			function ($) {
				return $.c;
			},
			A2(
				$author$project$Scan$nth,
				m.aH,
				$author$project$Listing$shown(m))));
};
var $author$project$Listing$listClicked = _Platform_outgoingPort('listClicked', $elm$core$Basics$identity);
var $author$project$Listing$listState = _Platform_outgoingPort('listState', $elm$core$Basics$identity);
var $elm$core$Basics$neq = _Utils_notEqual;
var $author$project$Scan$indexWhere = F2(
	function (pred, xs) {
		return $elm$core$List$head(
			A2(
				$elm$core$List$filterMap,
				function (_v0) {
					var i = _v0.a;
					var x = _v0.b;
					return pred(x) ? $elm$core$Maybe$Just(i) : $elm$core$Maybe$Nothing;
				},
				A2($elm$core$List$indexedMap, $elm$core$Tuple$pair, xs)));
	});
var $author$project$Listing$placeIn = F2(
	function (m, id) {
		return A2(
			$author$project$Scan$indexWhere,
			function (r) {
				return _Utils_eq(r.c, id);
			},
			$author$project$Listing$shown(m));
	});
var $author$project$Listing$placeOf = F2(
	function (m, id) {
		return A2(
			$elm$core$Maybe$withDefault,
			m.aH,
			A2($author$project$Listing$placeIn, m, id));
	});
var $elm$json$Json$Encode$int = _Json_wrap;
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(0),
				entries));
	});
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(0),
			pairs));
};
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Listing$stateJSON = function (m) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'at',
				$elm$json$Json$Encode$int(m.aH)),
				_Utils_Tuple2(
				'id',
				$elm$json$Json$Encode$string(
					$author$project$Listing$idAt(m))),
				_Utils_Tuple2(
				'ids',
				A2(
					$elm$json$Json$Encode$list,
					$elm$json$Json$Encode$string,
					A2(
						$elm$core$List$map,
						function ($) {
							return $.c;
						},
						$author$project$Listing$shown(m)))),
				_Utils_Tuple2(
				'flags',
				A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, m.P)),
				_Utils_Tuple2(
				'narrow',
				A2(
					$elm$core$Maybe$withDefault,
					$elm$json$Json$Encode$null,
					A2($elm$core$Maybe$map, $elm$json$Json$Encode$string, m.az))),
				_Utils_Tuple2(
				'all',
				$elm$json$Json$Encode$int(
					$elm$core$List$length(m.C)))
			]));
};
var $author$project$Listing$told = function (m) {
	return _Utils_Tuple2(
		m,
		$author$project$Listing$listState(
			$author$project$Listing$stateJSON(m)));
};
var $author$project$Listing$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 8:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 0:
				var rows = msg.a;
				var landing = msg.b;
				var filled = _Utils_update(
					model,
					{C: rows});
				return $author$project$Listing$told(
					$author$project$Listing$clamp(
						function () {
							if (!landing.$) {
								var id = landing.a;
								return _Utils_update(
									filled,
									{
										aH: A2($author$project$Listing$placeOf, filled, id)
									});
							} else {
								return filled;
							}
						}()));
			case 1:
				var id = msg.a;
				return $author$project$Listing$told(
					$author$project$Listing$clamp(
						_Utils_update(
							model,
							{
								aH: A2($author$project$Listing$placeOf, model, id)
							})));
			case 2:
				var by = msg.a;
				return $author$project$Listing$told(
					$author$project$Listing$clamp(
						_Utils_update(
							model,
							{aH: model.aH + by})));
			case 3:
				var id = msg.a;
				return $author$project$Listing$told(
					_Utils_update(
						model,
						{
							P: _Utils_ap(
								A2(
									$elm$core$List$filter,
									$elm$core$Basics$neq(id),
									model.P),
								_List_fromArray(
									[id]))
						}));
			case 4:
				var id = msg.a;
				return $author$project$Listing$told(
					_Utils_update(
						model,
						{
							P: A2(
								$elm$core$List$filter,
								$elm$core$Basics$neq(id),
								model.P)
						}));
			case 5:
				return $author$project$Listing$told(
					_Utils_update(
						model,
						{P: _List_Nil}));
			case 6:
				var want = msg.a;
				var next = _Utils_update(
					model,
					{az: want});
				var held = $author$project$Listing$idAt(model);
				return $author$project$Listing$told(
					$author$project$Listing$clamp(
						_Utils_update(
							next,
							{
								aH: A2(
									$elm$core$Maybe$withDefault,
									0,
									A2($author$project$Listing$placeIn, next, held))
							})));
			default:
				var id = msg.a;
				var moved = $author$project$Listing$clamp(
					_Utils_update(
						model,
						{
							aH: A2($author$project$Listing$placeOf, model, id)
						}));
				return _Utils_Tuple2(
					moved,
					$elm$core$Platform$Cmd$batch(
						_List_fromArray(
							[
								$author$project$Listing$listState(
								$author$project$Listing$stateJSON(moved)),
								$author$project$Listing$listClicked(
								$elm$json$Json$Encode$string(id))
							])));
		}
	});
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 1, a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$html$Html$Attributes$placeholder = $elm$html$Html$Attributes$stringProperty('placeholder');
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$Listing$bar = function (m) {
	var _v0 = m.az;
	if (_v0.$ === 1) {
		return _List_Nil;
	} else {
		var want = _v0.a;
		return _List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('tv-chips')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('tv-filter-wrap')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$input,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('tv-filter'),
										$elm$html$Html$Attributes$type_('search'),
										A2($elm$html$Html$Attributes$attribute, 'spellcheck', 'false'),
										$elm$html$Html$Attributes$placeholder('narrow'),
										$elm$html$Html$Attributes$value(want),
										$elm$html$Html$Events$onInput(
										A2($elm$core$Basics$composeL, $author$project$Listing$Narrow, $elm$core$Maybe$Just))
									]),
								_List_Nil)
							]))
					]))
			]);
	}
};
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $elm$html$Html$th = _VirtualDom_node('th');
var $author$project$Listing$head = function (c) {
	return A2(
		$elm$html$Html$th,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$attribute, 'data-key', c.bA)
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$span,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('tv-hd')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('tv-hn')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(c.bu)
							])),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('tv-arrow')
							]),
						_List_Nil)
					]))
			]));
};
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $elm$html$Html$table = _VirtualDom_node('table');
var $elm$html$Html$tbody = _VirtualDom_node('tbody');
var $elm$html$Html$thead = _VirtualDom_node('thead');
var $elm$html$Html$tr = _VirtualDom_node('tr');
var $author$project$Listing$Clicked = function (a) {
	return {$: 7, a: a};
};
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $elm$core$Basics$modBy = _Basics_modBy;
var $author$project$Listing$rowClass = F3(
	function (m, i, r) {
		return A2(
			$elm$core$String$join,
			' ',
			A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						(A2($elm$core$Basics$modBy, 2, i) === 1) ? $elm$core$Maybe$Just('tv-alt') : $elm$core$Maybe$Nothing,
						A2($elm$core$List$member, r.c, m.P) ? $elm$core$Maybe$Just('tv-flagged') : $elm$core$Maybe$Nothing,
						_Utils_eq(i, m.aH) ? $elm$core$Maybe$Just('tv-sel') : $elm$core$Maybe$Nothing
					])));
	});
var $elm$html$Html$td = _VirtualDom_node('td');
var $author$project$Listing$viewCell = F2(
	function (r, c) {
		var val = A2($author$project$Listing$cellOf, r, c.bA);
		var hue = $elm$core$List$head(
			A2(
				$elm$core$List$filter,
				function (b) {
					return _Utils_eq(b.b$, val);
				},
				c.bg));
		var _v0 = _Utils_Tuple2(c.j, hue);
		if ((_v0.a === 'badge') && (!_v0.b.$)) {
			var b = _v0.b.a;
			return A2(
				$elm$html$Html$td,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('tv-pill'),
								A2($elm$html$Html$Attributes$attribute, 'style', '--tv-badge:' + b.cg)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(val)
							]))
					]));
		} else {
			return A2(
				$elm$html$Html$td,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$Attributes$style,
						'color',
						A2($elm$core$Maybe$withDefault, '', r.cg))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(val)
					]));
		}
	});
var $author$project$Listing$viewRow = F3(
	function (m, i, r) {
		return A2(
			$elm$html$Html$tr,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class(
					A3($author$project$Listing$rowClass, m, i, r)),
					A2($elm$html$Html$Attributes$attribute, 'data-id', r.c),
					$elm$html$Html$Events$onClick(
					$author$project$Listing$Clicked(r.c))
				]),
			A2(
				$elm$core$List$map,
				$author$project$Listing$viewCell(r),
				m.aK));
	});
var $author$project$Listing$view = function (m) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('tv-root tv-pal')
			]),
		_Utils_ap(
			$author$project$Listing$bar(m),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('tv-scroll')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$table,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('tv-table')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$thead,
									_List_Nil,
									_List_fromArray(
										[
											A2(
											$elm$html$Html$tr,
											_List_Nil,
											A2($elm$core$List$map, $author$project$Listing$head, m.aK))
										])),
									A2(
									$elm$html$Html$tbody,
									_List_Nil,
									A2(
										$elm$core$List$indexedMap,
										$author$project$Listing$viewRow(m),
										$author$project$Listing$shown(m)))
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('tv-empty'),
									A2(
									$elm$html$Html$Attributes$style,
									'display',
									$elm$core$List$isEmpty(
										$author$project$Listing$shown(m)) ? '' : 'none')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('no rows')
								]))
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('tv-hint')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(m.bw)
						]))
				])));
};
var $elm$core$Result$withDefault = F2(
	function (def, result) {
		if (!result.$) {
			var a = result.a;
			return a;
		} else {
			return def;
		}
	});
var $author$project$Listing$main = $elm$browser$Browser$element(
	{
		cu: function (raw) {
			var _v0 = A2(
				$elm$core$Result$withDefault,
				_Utils_Tuple2(_List_Nil, ''),
				A2($elm$json$Json$Decode$decodeValue, $author$project$Listing$flagsD, raw));
			var cols = _v0.a;
			var hint = _v0.b;
			return _Utils_Tuple2(
				A6($author$project$Listing$Model, cols, _List_Nil, 0, _List_Nil, hint, $elm$core$Maybe$Nothing),
				$elm$core$Platform$Cmd$none);
		},
		cS: function (_v1) {
			return $author$project$Listing$listIn(
				function (v) {
					return A2(
						$elm$core$Result$withDefault,
						$author$project$Listing$Ignore,
						A2($elm$json$Json$Decode$decodeValue, $author$project$Listing$msgD, v));
				});
		},
		cY: $author$project$Listing$update,
		cZ: $author$project$Listing$view
	});
var $author$project$Doc$Ignore = {$: 28};
var $author$project$Doc$docIn = _Platform_incomingPort('docIn', $elm$json$Json$Decode$value);
var $elm$core$Set$Set_elm_builtin = $elm$core$Basics$identity;
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Set$empty = $elm$core$Dict$empty;
var $author$project$Doc$empty = {au: $elm$core$Array$empty, aH: 0, q: $elm$core$Maybe$Nothing, U: false, aw: $elm$core$Maybe$Nothing, P: _List_Nil, y: $elm$core$Set$empty, Q: $elm$core$Maybe$Nothing, V: 1, aO: _List_Nil, aP: _List_Nil, aB: $elm$core$Array$empty, A: _List_Nil, B: $elm$core$Maybe$Nothing, an: _List_Nil, aT: _List_Nil, cM: _List_Nil, C: _List_Nil, Z: 0, f: $elm$core$Set$empty, aC: $elm$core$Maybe$Nothing, aY: $elm$core$Maybe$Nothing};
var $author$project$Doc$AddProp = F2(
	function (a, b) {
		return {$: 25, a: a, b: b};
	});
var $author$project$Doc$Broader = {$: 6};
var $author$project$Doc$Clear = {$: 1};
var $author$project$Doc$ClearFlags = {$: 10};
var $author$project$Doc$Climb = {$: 7};
var $author$project$Doc$Delete = function (a) {
	return {$: 11, a: a};
};
var $author$project$Doc$Draft = F2(
	function (a, b) {
		return {$: 14, a: a, b: b};
	});
var $author$project$Doc$DraftPair = {$: 17};
var $author$project$Doc$DraftPlan = function (a) {
	return {$: 19, a: a};
};
var $author$project$Doc$Edit = F2(
	function (a, b) {
		return {$: 12, a: a, b: b};
	});
var $author$project$Doc$EditCell = F3(
	function (a, b, c) {
		return {$: 13, a: a, b: b, c: c};
	});
var $author$project$Doc$Fill = function (a) {
	return {$: 0, a: a};
};
var $author$project$Doc$Finer = {$: 5};
var $author$project$Doc$Flag = function (a) {
	return {$: 8, a: a};
};
var $author$project$Doc$Fold = function (a) {
	return {$: 22, a: a};
};
var $author$project$Doc$HideDone = {$: 23};
var $author$project$Doc$Insert = F3(
	function (a, b, c) {
		return {$: 15, a: a, b: b, c: c};
	});
var $author$project$Doc$Select = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $author$project$Doc$SelectCell = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $author$project$Doc$SetCells = function (a) {
	return {$: 27, a: a};
};
var $author$project$Doc$SetMeta = F2(
	function (a, b) {
		return {$: 26, a: a, b: b};
	});
var $author$project$Doc$Shift = function (a) {
	return {$: 24, a: a};
};
var $author$project$Doc$Step = function (a) {
	return {$: 4, a: a};
};
var $author$project$Doc$Tab = {$: 21};
var $author$project$Doc$Undraft = function (a) {
	return {$: 16, a: a};
};
var $author$project$Doc$UndraftPair = function (a) {
	return {$: 18, a: a};
};
var $author$project$Doc$UndraftPlan = function (a) {
	return {$: 20, a: a};
};
var $author$project$Doc$Unflag = function (a) {
	return {$: 9, a: a};
};
var $author$project$Doc$caretD = $elm$json$Json$Decode$maybe(
	A2($elm$json$Json$Decode$field, 'at', $elm$json$Json$Decode$int));
var $author$project$Body$Cell = F3(
	function (key, val, colour) {
		return {cg: colour, bA: key, bc: val};
	});
var $author$project$Doc$cellD = A4(
	$elm$json$Json$Decode$map3,
	$author$project$Body$Cell,
	A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'val', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'colour', $elm$json$Json$Decode$string));
var $elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, list);
			var jsArray = _v0.a;
			var remainingItems = _v0.b;
			if (_Utils_cmp(
				$elm$core$Elm$JsArray$length(jsArray),
				$elm$core$Array$branchFactor) < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					true,
					{o: nodeList, k: nodeListSize, n: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					$elm$core$List$cons,
					$elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var $elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return $elm$core$Array$empty;
	} else {
		return A3($elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var $author$project$Body$Kid = F4(
	function (index, level, line, cells) {
		return {ac: cells, ax: index, V: level, I: line};
	});
var $author$project$Doc$kidD = A5(
	$elm$json$Json$Decode$map4,
	$author$project$Body$Kid,
	A2($elm$json$Json$Decode$field, 'index', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'level', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'line', $elm$json$Json$Decode$int),
	A2(
		$elm$json$Json$Decode$field,
		'cells',
		$elm$json$Json$Decode$list($author$project$Doc$cellD)));
var $author$project$Doc$Link = F3(
	function (from, to, desc) {
		return {bm: desc, d: from, g: to};
	});
var $author$project$Doc$linkD = A4(
	$elm$json$Json$Decode$map3,
	$author$project$Doc$Link,
	A2($elm$json$Json$Decode$field, 'from', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'to', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'desc', $elm$json$Json$Decode$string));
var $elm$json$Json$Decode$map8 = _Json_map8;
var $author$project$Doc$offsetsOf = function (lines) {
	return $elm$core$Array$fromList(
		$elm$core$List$reverse(
			A3(
				$elm$core$List$foldl,
				F2(
					function (ln, acc) {
						if (acc.b) {
							var prev = acc.a;
							return A2(
								$elm$core$List$cons,
								(prev + $elm$core$String$length(ln)) + 1,
								acc);
						} else {
							return acc;
						}
					}),
				_List_fromArray(
					[0]),
				lines)));
};
var $elm$json$Json$Decode$index = _Json_decodeIndex;
var $author$project$Doc$pairD = A3(
	$elm$json$Json$Decode$map2,
	$elm$core$Tuple$pair,
	A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$string));
var $author$project$Body$Child = 2;
var $author$project$Scan$Element = 0;
var $author$project$Body$Head = 0;
var $author$project$Body$Para = 1;
var $author$project$Body$Row = function (id) {
	return function (kind) {
		return function (grain) {
			return function (name) {
				return function (owner) {
					return function (from) {
						return function (to) {
							return function (text) {
								return function (was) {
									return function (cells) {
										return function (index) {
											return function (level) {
												return function (alone) {
													return {aa: alone, ac: cells, d: from, x: grain, c: id, ax: index, j: kind, V: level, am: name, u: owner, O: text, g: to, S: was};
												};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $author$project$Body$blank = $author$project$Body$Row('')(1)(0)($elm$core$Maybe$Nothing)($elm$core$Maybe$Nothing)(0)(0)('')('')(_List_Nil)(0)(1)(false);
var $author$project$Scan$Block = 3;
var $author$project$Scan$Composite = 1;
var $author$project$Scan$Drawer = 4;
var $author$project$Scan$Stop = F5(
	function (from, to, grain, name, up) {
		return {d: from, x: grain, am: name, g: to, cX: up};
	});
var $author$project$Scan$Table = 2;
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$core$Array$bitMask = 4294967295 >>> (32 - $elm$core$Array$shiftStep);
var $elm$core$Basics$ge = _Utils_ge;
var $elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var $elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = $elm$core$Array$bitMask & (index >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (!_v0.$) {
				var subTree = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _v0.a;
				return A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, values);
			}
		}
	});
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var $elm$core$Array$get = F2(
	function (index, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? $elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? $elm$core$Maybe$Just(
			A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, tail)) : $elm$core$Maybe$Just(
			A3($elm$core$Array$getHelp, startShift, index, tree)));
	});
var $author$project$Scan$at = F2(
	function (i, xs) {
		return A2(
			$elm$core$Maybe$withDefault,
			'',
			A2($elm$core$Array$get, i, xs));
	});
var $author$project$Scan$closes = function (kind) {
	switch (kind) {
		case 3:
			return true;
		case 4:
			return true;
		case 0:
			return false;
		case 1:
			return false;
		default:
			return false;
	}
};
var $author$project$Scan$Run = F2(
	function (to, items) {
		return {aN: items, g: to};
	});
var $elm$core$String$trimLeft = _String_trimLeft;
var $elm$core$String$words = _String_words;
var $author$project$Scan$blockName = function (line) {
	var low = $elm$core$String$toLower(
		$elm$core$String$trimLeft(line));
	if (A2($elm$core$String$startsWith, '#+begin_', low)) {
		var _v0 = $elm$core$String$words(
			A2($elm$core$String$dropLeft, 8, low));
		if (_v0.b) {
			var w = _v0.a;
			return $elm$core$String$isEmpty(w) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(w);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$String$trim = _String_trim;
var $author$project$Scan$endsBlock = F2(
	function (name, line) {
		return _Utils_eq(
			$elm$core$String$toLower(
				$elm$core$String$trim(line)),
			'#+end_' + name);
	});
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $author$project$Scan$blockRun = F4(
	function (lines, i, end, name) {
		var go = function (j) {
			go:
			while (true) {
				if (_Utils_cmp(j, end) > -1) {
					return -1;
				} else {
					if (A2(
						$author$project$Scan$endsBlock,
						name,
						A2($author$project$Scan$at, j, lines))) {
						return j + 1;
					} else {
						var $temp$j = j + 1;
						j = $temp$j;
						continue go;
					}
				}
			}
		};
		return go(i + 1);
	});
var $elm$core$String$toUpper = _String_toUpper;
var $author$project$Scan$drawerEnds = function (line) {
	return $elm$core$String$toUpper(
		$elm$core$String$trim(line)) === ':END:';
};
var $author$project$Scan$drawerRun = F3(
	function (lines, i, end) {
		var go = function (j) {
			go:
			while (true) {
				if (_Utils_cmp(j, end) > -1) {
					return -1;
				} else {
					if ($author$project$Scan$drawerEnds(
						A2($author$project$Scan$at, j, lines))) {
						return j + 1;
					} else {
						var $temp$j = j + 1;
						j = $temp$j;
						continue go;
					}
				}
			}
		};
		return go(i + 1);
	});
var $author$project$Scan$isBlank = function (line) {
	return $elm$core$String$trim(line) === '';
};
var $author$project$Scan$Item = 1;
var $author$project$Scan$Plain = 0;
var $author$project$Scan$drawerChar = function (c) {
	return $elm$core$Char$isAlphaNum(c) || ((c === '-') || (c === '_'));
};
var $elm$core$String$endsWith = _String_endsWith;
var $author$project$Scan$drawerName = function (line) {
	var body = $elm$core$String$trim(line);
	var inner = A3(
		$elm$core$String$slice,
		1,
		$elm$core$String$length(body) - 1,
		body);
	return (A2($elm$core$String$startsWith, ':', body) && (A2($elm$core$String$endsWith, ':', body) && (($elm$core$String$length(body) > 2) && (A2($elm$core$String$all, $author$project$Scan$drawerChar, inner) && ($elm$core$String$toUpper(inner) !== 'END'))))) ? $elm$core$Maybe$Just(
		$elm$core$String$toUpper(inner)) : $elm$core$Maybe$Nothing;
};
var $author$project$Scan$isTable = function (line) {
	return A2(
		$elm$core$String$startsWith,
		'|',
		$elm$core$String$trimLeft(line));
};
var $author$project$Scan$Opener = F2(
	function (indent, bullet) {
		return {aI: bullet, by: indent};
	});
var $elm$core$String$fromList = _String_fromList;
var $author$project$Scan$takeWhileList = F2(
	function (f, xs) {
		if (!xs.b) {
			return _List_Nil;
		} else {
			var y = xs.a;
			var rest = xs.b;
			return f(y) ? A2(
				$elm$core$List$cons,
				y,
				A2($author$project$Scan$takeWhileList, f, rest)) : _List_Nil;
		}
	});
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $author$project$Scan$gapAfter = function (after) {
	var run = $elm$core$String$fromList(
		A2(
			$author$project$Scan$takeWhileList,
			function (c) {
				return (c === ' ') || (c === '\t');
			},
			$elm$core$String$toList(after)));
	return $elm$core$String$isEmpty(run) ? ' ' : run;
};
var $author$project$Scan$numberedAt = function (rest) {
	var digits = $elm$core$String$fromList(
		A2(
			$author$project$Scan$takeWhileList,
			$elm$core$Char$isDigit,
			$elm$core$String$toList(rest)));
	var after = A2(
		$elm$core$String$dropLeft,
		$elm$core$String$length(digits),
		rest);
	return $elm$core$String$isEmpty(digits) ? $elm$core$Maybe$Nothing : ((A2($elm$core$String$startsWith, '. ', after) || (A2($elm$core$String$startsWith, ') ', after) || ((after === '.') || (after === ')')))) ? $elm$core$Maybe$Just(digits) : $elm$core$Maybe$Nothing);
};
var $author$project$Scan$listOpener = function (line) {
	var spaces = $elm$core$String$length(line) - $elm$core$String$length(
		$elm$core$String$trimLeft(line));
	var rest = A2($elm$core$String$dropLeft, spaces, line);
	var tokenAt = (A2($elm$core$String$startsWith, '- ', rest) || (rest === '-')) ? $elm$core$Maybe$Just('-') : ((A2($elm$core$String$startsWith, '+ ', rest) || (rest === '+')) ? $elm$core$Maybe$Just('+') : ((A2($elm$core$String$startsWith, '* ', rest) || (rest === '*')) ? $elm$core$Maybe$Just('*') : A2(
		$elm$core$Maybe$map,
		function (d) {
			return _Utils_ap(
				d,
				A3(
					$elm$core$String$slice,
					$elm$core$String$length(d),
					$elm$core$String$length(d) + 1,
					rest));
		},
		$author$project$Scan$numberedAt(rest))));
	var opened = function (token) {
		return A2(
			$author$project$Scan$Opener,
			spaces,
			_Utils_ap(
				token,
				$author$project$Scan$gapAfter(
					A2(
						$elm$core$String$dropLeft,
						$elm$core$String$length(token),
						rest))));
	};
	if (tokenAt.$ === 1) {
		return $elm$core$Maybe$Nothing;
	} else {
		if (tokenAt.a === '*') {
			return (!spaces) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
				opened('*'));
		} else {
			var token = tokenAt.a;
			return $elm$core$Maybe$Just(
				opened(token));
		}
	}
};
var $author$project$Scan$kindAt = F2(
	function (lines, i) {
		var line = A2($author$project$Scan$at, i, lines);
		return (!_Utils_eq(
			$author$project$Scan$drawerName(line),
			$elm$core$Maybe$Nothing)) ? 4 : ((!_Utils_eq(
			$author$project$Scan$blockName(line),
			$elm$core$Maybe$Nothing)) ? 3 : ($author$project$Scan$isTable(line) ? 2 : ((!_Utils_eq(
			$author$project$Scan$listOpener(line),
			$elm$core$Maybe$Nothing)) ? 1 : 0)));
	});
var $elm$core$Basics$not = _Basics_not;
var $author$project$Scan$proseEnd = F3(
	function (lines, end, j) {
		proseEnd:
		while (true) {
			if ((_Utils_cmp(j, end) > -1) || ($author$project$Scan$isBlank(
				A2($author$project$Scan$at, j, lines)) || (!(!A2($author$project$Scan$kindAt, lines, j))))) {
				return j;
			} else {
				var $temp$lines = lines,
					$temp$end = end,
					$temp$j = j + 1;
				lines = $temp$lines;
				end = $temp$end;
				j = $temp$j;
				continue proseEnd;
			}
		}
	});
var $author$project$Scan$rides = function (line) {
	return (!_Utils_eq(
		$author$project$Scan$listOpener(line),
		$elm$core$Maybe$Nothing)) || (A2($elm$core$String$startsWith, ' ', line) || A2($elm$core$String$startsWith, '\u0009', line));
};
var $author$project$Scan$tableEnd = F3(
	function (lines, end, j) {
		tableEnd:
		while (true) {
			if ((_Utils_cmp(j, end) < 0) && $author$project$Scan$isTable(
				A2($author$project$Scan$at, j, lines))) {
				var $temp$lines = lines,
					$temp$end = end,
					$temp$j = j + 1;
				lines = $temp$lines;
				end = $temp$end;
				j = $temp$j;
				continue tableEnd;
			} else {
				return j;
			}
		}
	});
var $author$project$Scan$closedRun = F3(
	function (lines, end, j) {
		var kind = A2($author$project$Scan$kindAt, lines, j);
		var shut = $author$project$Scan$closes(kind) ? A4($author$project$Scan$extentOf, lines, end, j, kind) : (-1);
		return _Utils_eq(shut, -1) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(shut);
	});
var $author$project$Scan$extentOf = F4(
	function (lines, end, i, kind) {
		switch (kind) {
			case 0:
				return A3($author$project$Scan$proseEnd, lines, end, i + 1);
			case 1:
				return A3($author$project$Scan$listRun, lines, i, end).g;
			case 2:
				return A3($author$project$Scan$tableEnd, lines, end, i);
			case 3:
				return A4(
					$author$project$Scan$blockRun,
					lines,
					i,
					end,
					A2(
						$elm$core$Maybe$withDefault,
						'',
						$author$project$Scan$blockName(
							A2($author$project$Scan$at, i, lines))));
			default:
				return A3($author$project$Scan$drawerRun, lines, i, end);
		}
	});
var $author$project$Scan$listRun = F3(
	function (lines, i, end) {
		var close = F3(
			function (from, last, items) {
				return _Utils_eq(from, -1) ? items : _Utils_ap(
					items,
					_List_fromArray(
						[
							_Utils_Tuple2(from, last)
						]));
			});
		var blanksFrom = function (j) {
			blanksFrom:
			while (true) {
				if ((_Utils_cmp(j, end) < 0) && $author$project$Scan$isBlank(
					A2($author$project$Scan$at, j, lines))) {
					var $temp$j = j + 1;
					j = $temp$j;
					continue blanksFrom;
				} else {
					return j;
				}
			}
		};
		var base = function () {
			var _v2 = $author$project$Scan$listOpener(
				A2($author$project$Scan$at, i, lines));
			if (!_v2.$) {
				var o = _v2.a;
				return o.by;
			} else {
				return 0;
			}
		}();
		var go = F4(
			function (j, from, last, items) {
				go:
				while (true) {
					if (_Utils_cmp(j, end) > -1) {
						return A2(
							$author$project$Scan$Run,
							last,
							A3(close, from, last, items));
					} else {
						if ($author$project$Scan$isBlank(
							A2($author$project$Scan$at, j, lines))) {
							var k = blanksFrom(j);
							if (((k - j) > 1) || ((_Utils_cmp(k, end) > -1) || (!$author$project$Scan$rides(
								A2($author$project$Scan$at, k, lines))))) {
								return A2(
									$author$project$Scan$Run,
									last,
									A3(close, from, last, items));
							} else {
								var $temp$j = k,
									$temp$from = from,
									$temp$last = last,
									$temp$items = items;
								j = $temp$j;
								from = $temp$from;
								last = $temp$last;
								items = $temp$items;
								continue go;
							}
						} else {
							var _v0 = $author$project$Scan$listOpener(
								A2($author$project$Scan$at, j, lines));
							if (!_v0.$) {
								var o = _v0.a;
								if (_Utils_cmp(o.by, base) < 1) {
									var $temp$j = j + 1,
										$temp$from = j,
										$temp$last = j + 1,
										$temp$items = A3(close, from, last, items);
									j = $temp$j;
									from = $temp$from;
									last = $temp$last;
									items = $temp$items;
									continue go;
								} else {
									var $temp$j = j + 1,
										$temp$from = from,
										$temp$last = j + 1,
										$temp$items = items;
									j = $temp$j;
									from = $temp$from;
									last = $temp$last;
									items = $temp$items;
									continue go;
								}
							} else {
								if ($author$project$Scan$rides(
									A2($author$project$Scan$at, j, lines))) {
									var _v1 = A3($author$project$Scan$closedRun, lines, end, j);
									if (!_v1.$) {
										var shut = _v1.a;
										var $temp$j = shut,
											$temp$from = from,
											$temp$last = shut,
											$temp$items = items;
										j = $temp$j;
										from = $temp$from;
										last = $temp$last;
										items = $temp$items;
										continue go;
									} else {
										var $temp$j = j + 1,
											$temp$from = from,
											$temp$last = j + 1,
											$temp$items = items;
										j = $temp$j;
										from = $temp$from;
										last = $temp$last;
										items = $temp$items;
										continue go;
									}
								} else {
									return A2(
										$author$project$Scan$Run,
										last,
										A3(close, from, last, items));
								}
							}
						}
					}
				}
			});
		return A4(go, i, -1, i, _List_Nil);
	});
var $author$project$Scan$drawerLeaves = F3(
	function (lines, a, b) {
		var go = F2(
			function (j, out) {
				go:
				while (true) {
					if (_Utils_cmp(j, b) > -1) {
						return out;
					} else {
						if ($author$project$Scan$isBlank(
							A2($author$project$Scan$at, j, lines))) {
							var $temp$j = j + 1,
								$temp$out = out;
							j = $temp$j;
							out = $temp$out;
							continue go;
						} else {
							var stop = A2(
								$elm$core$Maybe$withDefault,
								j + 1,
								A3($author$project$Scan$closedRun, lines, b, j));
							var $temp$j = stop,
								$temp$out = A2(
								$elm$core$List$cons,
								_Utils_Tuple2(j, stop),
								out);
							j = $temp$j;
							out = $temp$out;
							continue go;
						}
					}
				}
			});
		return $elm$core$List$reverse(
			A2(go, a, _List_Nil));
	});
var $author$project$Scan$runsIn = F3(
	function (lines, a, b) {
		var go = F3(
			function (i, from, out) {
				go:
				while (true) {
					if (_Utils_cmp(i, b) > 0) {
						return out;
					} else {
						if (_Utils_eq(i, b) || $author$project$Scan$isBlank(
							A2($author$project$Scan$at, i, lines))) {
							if (_Utils_eq(from, -1)) {
								var $temp$i = i + 1,
									$temp$from = -1,
									$temp$out = out;
								i = $temp$i;
								from = $temp$from;
								out = $temp$out;
								continue go;
							} else {
								var $temp$i = i + 1,
									$temp$from = -1,
									$temp$out = _Utils_ap(
									out,
									_List_fromArray(
										[
											_Utils_Tuple2(from, i)
										]));
								i = $temp$i;
								from = $temp$from;
								out = $temp$out;
								continue go;
							}
						} else {
							if (_Utils_eq(from, -1)) {
								var $temp$i = i + 1,
									$temp$from = i,
									$temp$out = out;
								i = $temp$i;
								from = $temp$from;
								out = $temp$out;
								continue go;
							} else {
								var $temp$i = i + 1,
									$temp$from = from,
									$temp$out = out;
								i = $temp$i;
								from = $temp$from;
								out = $temp$out;
								continue go;
							}
						}
					}
				}
			});
		return A3(go, a, -1, _List_Nil);
	});
var $author$project$Scan$leavesOf = F4(
	function (lines, kind, i, shut) {
		switch (kind) {
			case 4:
				return A3($author$project$Scan$drawerLeaves, lines, i + 1, shut - 1);
			case 3:
				return A3($author$project$Scan$runsIn, lines, i + 1, shut - 1);
			case 2:
				return A2(
					$elm$core$List$map,
					function (n) {
						return _Utils_Tuple2(n, n + 1);
					},
					A2($elm$core$List$range, i, shut - 1));
			case 0:
				return _List_Nil;
			default:
				return _List_Nil;
		}
	});
var $elm$core$Array$length = function (_v0) {
	var len = _v0.a;
	return len;
};
var $author$project$Scan$Leaf = 2;
var $author$project$Scan$Region = F3(
	function (kind, from, to) {
		return {d: from, j: kind, g: to};
	});
var $author$project$Scan$regionsIn = F3(
	function (lines, from, end) {
		var items = function (run) {
			return A3(
				$elm$core$List$map2,
				F2(
					function (_v1, b) {
						var a = _v1.a;
						return A3($author$project$Scan$Region, 1, a, b);
					}),
				run.aN,
				_Utils_ap(
					A2(
						$elm$core$List$drop,
						1,
						A2($elm$core$List$map, $elm$core$Tuple$first, run.aN)),
					_List_fromArray(
						[run.g])));
		};
		var go = F2(
			function (i, out) {
				if (_Utils_cmp(i, end) > -1) {
					return out;
				} else {
					if ($author$project$Scan$isBlank(
						A2($author$project$Scan$at, i, lines))) {
						return A2(go, i + 1, out);
					} else {
						var _v0 = A2($author$project$Scan$kindAt, lines, i);
						switch (_v0) {
							case 0:
								return A2(prose, i, out);
							case 1:
								var run = A3($author$project$Scan$listRun, lines, i, end);
								return A2(
									go,
									A2($elm$core$Basics$max, i + 1, run.g),
									_Utils_ap(
										out,
										items(run)));
							case 2:
								return A3(held, 2, i, out);
							case 3:
								return A3(held, 3, i, out);
							default:
								return A3(held, 4, i, out);
						}
					}
				}
			});
		var held = F3(
			function (kind, i, out) {
				var to = A4($author$project$Scan$extentOf, lines, end, i, kind);
				return _Utils_eq(to, -1) ? A2(prose, i, out) : A2(
					go,
					to,
					_Utils_ap(
						out,
						_List_fromArray(
							[
								A3($author$project$Scan$Region, kind, i, to)
							])));
			});
		var prose = F2(
			function (i, out) {
				var j = A4($author$project$Scan$extentOf, lines, end, i, 0);
				return A2(
					go,
					j,
					_Utils_ap(
						out,
						_List_fromArray(
							[
								A3($author$project$Scan$Region, 0, i, j)
							])));
			});
		return A2(go, from, _List_Nil);
	});
var $author$project$Scan$snug = F3(
	function (lines, from, to) {
		snug:
		while (true) {
			if ((_Utils_cmp(to, from) > 0) && $author$project$Scan$isBlank(
				A2($author$project$Scan$at, to - 1, lines))) {
				var $temp$lines = lines,
					$temp$from = from,
					$temp$to = to - 1;
				lines = $temp$lines;
				from = $temp$from;
				to = $temp$to;
				continue snug;
			} else {
				return to;
			}
		}
	});
var $author$project$Scan$pushItem = F5(
	function (lines, from, to, up, out) {
		var here = $elm$core$List$length(out);
		var deeper = F2(
			function (reg, got) {
				return (reg.j === 1) ? A5(
					$author$project$Scan$pushItem,
					lines,
					reg.d,
					A3($author$project$Scan$snug, lines, reg.d, reg.g),
					$elm$core$Maybe$Just(here),
					got) : got;
			});
		return A3(
			$elm$core$List$foldl,
			deeper,
			_Utils_ap(
				out,
				_List_fromArray(
					[
						A5($author$project$Scan$Stop, from, to, 2, $elm$core$Maybe$Nothing, up)
					])),
			A3($author$project$Scan$regionsIn, lines, from + 1, to));
	});
var $author$project$Scan$stopName = F3(
	function (lines, kind, i) {
		switch (kind) {
			case 4:
				return $elm$core$String$toLower(
					A2(
						$elm$core$Maybe$withDefault,
						'drawer',
						$author$project$Scan$drawerName(
							A2($author$project$Scan$at, i, lines))));
			case 3:
				return A2(
					$elm$core$Maybe$withDefault,
					'',
					$author$project$Scan$blockName(
						A2($author$project$Scan$at, i, lines)));
			case 2:
				return 'table';
			case 0:
				return '';
			default:
				return '';
		}
	});
var $author$project$Scan$whole = F5(
	function (a, b, name, leaves, out) {
		var here = $elm$core$List$length(out);
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, got) {
					var p = _v0.a;
					var q = _v0.b;
					return _Utils_ap(
						got,
						_List_fromArray(
							[
								A5(
								$author$project$Scan$Stop,
								p,
								q,
								2,
								$elm$core$Maybe$Nothing,
								$elm$core$Maybe$Just(here))
							]));
				}),
			_Utils_ap(
				out,
				_List_fromArray(
					[
						A5(
						$author$project$Scan$Stop,
						a,
						b,
						1,
						$elm$core$Maybe$Just(name),
						$elm$core$Maybe$Nothing)
					])),
			leaves);
	});
var $author$project$Scan$blocksInRange = F3(
	function (lines, start, own) {
		var end = A2(
			$elm$core$Basics$max,
			0,
			A2(
				$elm$core$Basics$min,
				own,
				$elm$core$Array$length(lines)));
		var plain = F2(
			function (i, out) {
				var j = A3($author$project$Scan$proseEnd, lines, end, i + 1);
				return A2(
					go,
					j,
					_Utils_ap(
						out,
						_List_fromArray(
							[
								A5($author$project$Scan$Stop, i, j, 0, $elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing)
							])));
			});
		var go = F2(
			function (i, out) {
				if (_Utils_cmp(i, end) > -1) {
					return out;
				} else {
					if ($author$project$Scan$isBlank(
						A2($author$project$Scan$at, i, lines))) {
						return A2(go, i + 1, out);
					} else {
						var _v0 = A2($author$project$Scan$kindAt, lines, i);
						switch (_v0) {
							case 4:
								return A3(held, 4, i, out);
							case 0:
								return A2(plain, i, out);
							case 3:
								return A3(held, 3, i, out);
							case 2:
								return A3(held, 2, i, out);
							default:
								var run = A3($author$project$Scan$listRun, lines, i, end);
								var opened = _Utils_ap(
									out,
									_List_fromArray(
										[
											A5(
											$author$project$Scan$Stop,
											i,
											run.g,
											1,
											$elm$core$Maybe$Just('list'),
											$elm$core$Maybe$Nothing)
										]));
								var here = $elm$core$List$length(out);
								return A2(
									go,
									A2($elm$core$Basics$max, i + 1, run.g),
									A3(
										$elm$core$List$foldl,
										F2(
											function (_v1, got) {
												var a = _v1.a;
												var b = _v1.b;
												return A5(
													$author$project$Scan$pushItem,
													lines,
													a,
													b,
													$elm$core$Maybe$Just(here),
													got);
											}),
										opened,
										run.aN));
						}
					}
				}
			});
		var held = F3(
			function (kind, i, out) {
				var shut = A4($author$project$Scan$extentOf, lines, end, i, kind);
				return ($author$project$Scan$closes(kind) && (_Utils_cmp(shut, i + 1) < 1)) ? A2(plain, i, out) : A2(
					go,
					shut,
					A5(
						$author$project$Scan$whole,
						i,
						shut,
						A3($author$project$Scan$stopName, lines, kind, i),
						A4($author$project$Scan$leavesOf, lines, kind, i, shut),
						out));
			});
		return A2(
			go,
			A2($elm$core$Basics$max, 1, start),
			_List_Nil);
	});
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $author$project$Scan$cut = F3(
	function (lines, a, b) {
		return A2(
			$elm$core$String$join,
			'\u000A',
			A2(
				$elm$core$List$take,
				b - a,
				A2($elm$core$List$drop, a, lines)));
	});
var $author$project$Body$ownEnd = F2(
	function (rows, r) {
		if (r.x === 1) {
			return r.g;
		} else {
			var _v0 = A2(
				$elm$core$List$filter,
				function (k) {
					return _Utils_eq(
						k.u,
						$elm$core$Maybe$Just(r.c));
				},
				rows);
			if (_v0.b) {
				var kid = _v0.a;
				return kid.d;
			} else {
				return r.g;
			}
		}
	});
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $author$project$Body$tailId = 'T';
var $author$project$Body$rowsFrom = F4(
	function (lines, own, headCells, kids) {
		var tail = _Utils_update(
			$author$project$Body$blank,
			{
				aa: true,
				d: $elm$core$List$length(lines),
				c: $author$project$Body$tailId,
				g: $elm$core$List$length(lines)
			});
		var head = _Utils_update(
			$author$project$Body$blank,
			{ac: headCells, x: 0, c: 'H', j: 0});
		var arr = $elm$core$Array$fromList(lines);
		var rowsIn = F5(
			function (prefix, owner, lvl, a, b) {
				var idAt = function (k) {
					return prefix + ('B' + $elm$core$String$fromInt(k));
				};
				var blocks = A3($author$project$Scan$blocksInRange, arr, a, b);
				return A2(
					$elm$core$List$indexedMap,
					F2(
						function (i, b_) {
							var held = A3($author$project$Scan$cut, lines, b_.d, b_.g);
							return _Utils_update(
								$author$project$Body$blank,
								{
									d: b_.d,
									x: b_.x,
									c: idAt(i),
									j: 1,
									V: lvl,
									am: b_.am,
									u: function () {
										var _v3 = b_.cX;
										if (!_v3.$) {
											var k = _v3.a;
											return $elm$core$Maybe$Just(
												idAt(k));
										} else {
											return owner;
										}
									}(),
									O: held,
									g: b_.g,
									S: held
								});
						}),
					blocks);
			});
		var body = A5(rowsIn, '', $elm$core$Maybe$Nothing, 1, 0, own);
		var owned = function (r) {
			var stop = A2($author$project$Body$ownEnd, body, r);
			return _Utils_eq(stop, r.g) ? r : _Utils_update(
				r,
				{
					O: A3($author$project$Scan$cut, lines, r.d, stop),
					S: A3($author$project$Scan$cut, lines, r.d, stop)
				});
		};
		var descend = F3(
			function (seen, ks, out) {
				descend:
				while (true) {
					if (!ks.b) {
						return out;
					} else {
						var k = ks.a;
						var rest = ks.b;
						var stop = function () {
							if (rest.b) {
								var next = rest.a;
								return next.I;
							} else {
								return $elm$core$List$length(lines);
							}
						}();
						var cid = 'C' + $elm$core$String$fromInt(k.ax);
						var above = A2(
							$elm$core$List$filter,
							function (_v1) {
								var l = _v1.a;
								return _Utils_cmp(l, k.V) < 0;
							},
							seen);
						var row = _Utils_update(
							$author$project$Body$blank,
							{
								ac: k.ac,
								d: k.I,
								x: 0,
								c: cid,
								ax: k.ax,
								j: 2,
								V: k.V,
								u: A2(
									$elm$core$Maybe$map,
									$elm$core$Tuple$second,
									$elm$core$List$head(above)),
								g: stop
							});
						var $temp$seen = A2(
							$elm$core$List$cons,
							_Utils_Tuple2(k.V, cid),
							above),
							$temp$ks = rest,
							$temp$out = _Utils_ap(
							out,
							A2(
								$elm$core$List$cons,
								row,
								A5(
									rowsIn,
									cid + ':',
									$elm$core$Maybe$Just(cid),
									k.V,
									k.I + 1,
									stop)));
						seen = $temp$seen;
						ks = $temp$ks;
						out = $temp$out;
						continue descend;
					}
				}
			});
		return _Utils_ap(
			A2(
				$elm$core$List$cons,
				head,
				A2($elm$core$List$map, owned, body)),
			_Utils_ap(
				A3(descend, _List_Nil, kids, _List_Nil),
				_List_fromArray(
					[tail])));
	});
var $author$project$Body$Meta = 3;
var $author$project$Body$planEntries = F3(
	function (slots, plan, summoned) {
		var valueOf = function (key) {
			return A2(
				$elm$core$Maybe$withDefault,
				'',
				A2(
					$elm$core$Maybe$map,
					$elm$core$Tuple$second,
					$elm$core$List$head(
						A2(
							$elm$core$List$filter,
							function (_v3) {
								var k = _v3.a;
								return _Utils_eq(k, key);
							},
							plan))));
		};
		var drawn = _Utils_ap(
			A2(
				$elm$core$List$map,
				function (key) {
					return _Utils_Tuple2(
						key,
						valueOf(key));
				},
				slots),
			A2(
				$elm$core$List$filter,
				function (_v2) {
					var k = _v2.a;
					return !A2($elm$core$List$member, k, slots);
				},
				plan));
		if (!summoned.$) {
			var key = summoned.a;
			return A2(
				$elm$core$List$any,
				function (_v1) {
					var k = _v1.a;
					return _Utils_eq(k, key);
				},
				drawn) ? drawn : _Utils_ap(
				drawn,
				_List_fromArray(
					[
						_Utils_Tuple2(key, '')
					]));
		} else {
			return drawn;
		}
	});
var $author$project$Doc$entriesOf = function (m) {
	return A3($author$project$Body$planEntries, m.aT, m.A, m.aw);
};
var $author$project$Body$drawerId = 'PR';
var $author$project$Body$draftPairId = $author$project$Body$drawerId + 'D';
var $author$project$Body$planId = 'PLN';
var $author$project$Body$planningText = function (plan) {
	return A2(
		$elm$core$String$join,
		' ',
		A2(
			$elm$core$List$map,
			function (_v0) {
				var k = _v0.a;
				var v = _v0.b;
				return k + (': ' + v);
			},
			plan));
};
var $author$project$Body$propId = function (n) {
	return _Utils_ap(
		$author$project$Body$drawerId,
		$elm$core$String$fromInt(n));
};
var $author$project$Body$propertyText = function (_v0) {
	var key = _v0.a;
	var value = _v0.b;
	return ':' + (key + (': ' + value));
};
var $author$project$Body$metaRows = function (_v0) {
	var drafting = _v0.ck;
	var props = _v0.cM;
	var entries = _v0.cm;
	var planning = $elm$core$List$isEmpty(entries) ? _List_Nil : _List_fromArray(
		[
			_Utils_update(
			$author$project$Body$blank,
			{
				x: 0,
				c: $author$project$Body$planId,
				j: 3,
				O: $author$project$Body$planningText(entries),
				S: $author$project$Body$planningText(entries)
			})
		]);
	var pair = F2(
		function (i, _v1) {
			var key = _v1.a;
			var value = _v1.b;
			return _Utils_update(
				$author$project$Body$blank,
				{
					x: 2,
					c: $author$project$Body$propId(i),
					j: 3,
					u: $elm$core$Maybe$Just($author$project$Body$drawerId),
					O: $author$project$Body$propertyText(
						_Utils_Tuple2(key, value)),
					S: $author$project$Body$propertyText(
						_Utils_Tuple2(key, value))
				});
		});
	var drawer = _Utils_update(
		$author$project$Body$blank,
		{
			x: 1,
			c: $author$project$Body$drawerId,
			j: 3,
			am: $elm$core$Maybe$Just('properties')
		});
	var drafts = drafting ? _List_fromArray(
		[
			_Utils_update(
			$author$project$Body$blank,
			{
				x: 2,
				c: $author$project$Body$draftPairId,
				j: 3,
				u: $elm$core$Maybe$Just($author$project$Body$drawerId)
			})
		]) : _List_Nil;
	return _Utils_ap(
		planning,
		A2(
			$elm$core$List$cons,
			drawer,
			_Utils_ap(
				A2($elm$core$List$indexedMap, pair, props),
				drafts)));
};
var $author$project$Doc$remeta = function (m) {
	return _Utils_update(
		m,
		{
			C: function () {
				var _v0 = A2(
					$elm$core$List$filter,
					function (r) {
						return r.j !== 3;
					},
					m.C);
				if (_v0.b) {
					var head = _v0.a;
					var rest = _v0.b;
					return A2(
						$elm$core$List$cons,
						head,
						_Utils_ap(
							$author$project$Body$metaRows(
								{
									ck: m.U,
									cm: $author$project$Doc$entriesOf(m),
									cM: m.cM
								}),
							rest));
				} else {
					return _List_Nil;
				}
			}()
		});
};
var $author$project$Doc$lineOf = F2(
	function (m, r) {
		return A2(
			$elm$core$Maybe$withDefault,
			'',
			A2($elm$core$Array$get, r.d, m.au));
	});
var $author$project$Doc$drawer = F2(
	function (m, r) {
		return (r.x === 1) && ((r.j === 3) || (!_Utils_eq(
			$author$project$Scan$drawerName(
				A2($author$project$Doc$lineOf, m, r)),
			$elm$core$Maybe$Nothing)));
	});
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1) {
				case 0:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0;
		return A3($elm$core$Dict$insert, key, 0, dict);
	});
var $elm$core$Set$fromList = function (list) {
	return A3($elm$core$List$foldl, $elm$core$Set$insert, $elm$core$Set$empty, list);
};
var $author$project$Doc$idsWhere = F2(
	function (p, m) {
		return $elm$core$Set$fromList(
			A2(
				$elm$core$List$filterMap,
				function (r) {
					return p(r) ? $elm$core$Maybe$Just(r.c) : $elm$core$Maybe$Nothing;
				},
				m.C));
	});
var $author$project$Doc$seedShut = function (m) {
	return A2(
		$author$project$Doc$idsWhere,
		$author$project$Doc$drawer(m),
		m);
};
var $author$project$Doc$seedMeta = function (m) {
	var seeded = $author$project$Doc$remeta(m);
	return _Utils_update(
		seeded,
		{
			f: $author$project$Doc$seedShut(seeded)
		});
};
var $author$project$Doc$fillD = A2(
	$elm$json$Json$Decode$andThen,
	function (m) {
		return A5(
			$elm$json$Json$Decode$map4,
			F4(
				function (props, plan, keys, slots) {
					return $author$project$Doc$seedMeta(
						_Utils_update(
							m,
							{A: plan, an: keys, aT: slots, cM: props}));
				}),
			A2(
				$elm$json$Json$Decode$field,
				'props',
				$elm$json$Json$Decode$list($author$project$Doc$pairD)),
			A2(
				$elm$json$Json$Decode$field,
				'plan',
				$elm$json$Json$Decode$list($author$project$Doc$pairD)),
			A2(
				$elm$json$Json$Decode$field,
				'planKeys',
				$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
			A2(
				$elm$json$Json$Decode$field,
				'planSlots',
				$elm$json$Json$Decode$list($elm$json$Json$Decode$string)));
	},
	A2(
		$elm$json$Json$Decode$andThen,
		function (m) {
			return A2(
				$elm$json$Json$Decode$map,
				function (l) {
					return _Utils_update(
						m,
						{Q: l});
				},
				$elm$json$Json$Decode$maybe(
					A2($elm$json$Json$Decode$field, 'landing', $elm$json$Json$Decode$int)));
		},
		A2(
			$elm$json$Json$Decode$andThen,
			function (m) {
				return A2(
					$elm$json$Json$Decode$map,
					function (t) {
						return _Utils_update(
							m,
							{aY: t});
					},
					A2(
						$elm$json$Json$Decode$field,
						'titleAt',
						$elm$json$Json$Decode$nullable($elm$json$Json$Decode$int)));
			},
			A9(
				$elm$json$Json$Decode$map8,
				F8(
					function (lines, own, headCells, kids, links, spanAt, shift, level) {
						var seeded = _Utils_update(
							$author$project$Doc$empty,
							{
								au: $elm$core$Array$fromList(lines),
								V: level,
								aO: lines,
								aP: links,
								aB: $author$project$Doc$offsetsOf(lines),
								Z: shift,
								aC: spanAt
							});
						return _Utils_update(
							seeded,
							{
								C: A4($author$project$Body$rowsFrom, lines, own, headCells, kids)
							});
					}),
				A2(
					$elm$json$Json$Decode$field,
					'lines',
					$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
				A2($elm$json$Json$Decode$field, 'own', $elm$json$Json$Decode$int),
				A2(
					$elm$json$Json$Decode$field,
					'cells',
					$elm$json$Json$Decode$list($author$project$Doc$cellD)),
				A2(
					$elm$json$Json$Decode$field,
					'kids',
					$elm$json$Json$Decode$list($author$project$Doc$kidD)),
				A2(
					$elm$json$Json$Decode$field,
					'links',
					$elm$json$Json$Decode$list($author$project$Doc$linkD)),
				A2(
					$elm$json$Json$Decode$field,
					'spanAt',
					$elm$json$Json$Decode$nullable($elm$json$Json$Decode$int)),
				A2($elm$json$Json$Decode$field, 'shift', $elm$json$Json$Decode$int),
				A2($elm$json$Json$Decode$field, 'level', $elm$json$Json$Decode$int)))));
var $author$project$Doc$msgD = A2(
	$elm$json$Json$Decode$andThen,
	function (kind) {
		switch (kind) {
			case 'fill':
				return A2($elm$json$Json$Decode$map, $author$project$Doc$Fill, $author$project$Doc$fillD);
			case 'clear':
				return $elm$json$Json$Decode$succeed($author$project$Doc$Clear);
			case 'select':
				return A3(
					$elm$json$Json$Decode$map2,
					$author$project$Doc$Select,
					A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string),
					$elm$json$Json$Decode$maybe(
						A2($elm$json$Json$Decode$field, 'plan', $elm$json$Json$Decode$int)));
			case 'selectcell':
				return A3(
					$elm$json$Json$Decode$map2,
					$author$project$Doc$SelectCell,
					A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string),
					$elm$json$Json$Decode$maybe(
						A2($elm$json$Json$Decode$field, 'col', $elm$json$Json$Decode$int)));
			case 'step':
				return A2(
					$elm$json$Json$Decode$map,
					$author$project$Doc$Step,
					A2($elm$json$Json$Decode$field, 'by', $elm$json$Json$Decode$int));
			case 'finer':
				return $elm$json$Json$Decode$succeed($author$project$Doc$Finer);
			case 'broader':
				return $elm$json$Json$Decode$succeed($author$project$Doc$Broader);
			case 'climb':
				return $elm$json$Json$Decode$succeed($author$project$Doc$Climb);
			case 'flag':
				return A2(
					$elm$json$Json$Decode$map,
					$author$project$Doc$Flag,
					A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string));
			case 'unflag':
				return A2(
					$elm$json$Json$Decode$map,
					$author$project$Doc$Unflag,
					A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string));
			case 'clearFlags':
				return $elm$json$Json$Decode$succeed($author$project$Doc$ClearFlags);
			case 'delete':
				return A2(
					$elm$json$Json$Decode$map,
					$author$project$Doc$Delete,
					A2(
						$elm$json$Json$Decode$field,
						'ids',
						$elm$json$Json$Decode$list($elm$json$Json$Decode$string)));
			case 'edit':
				return A3(
					$elm$json$Json$Decode$map2,
					$author$project$Doc$Edit,
					A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string),
					A2($elm$json$Json$Decode$field, 'text', $elm$json$Json$Decode$string));
			case 'editcell':
				return A4(
					$elm$json$Json$Decode$map3,
					$author$project$Doc$EditCell,
					A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string),
					A2($elm$json$Json$Decode$field, 'col', $elm$json$Json$Decode$int),
					A2($elm$json$Json$Decode$field, 'text', $elm$json$Json$Decode$string));
			case 'insert':
				return A4(
					$elm$json$Json$Decode$map3,
					$author$project$Doc$Insert,
					A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string),
					$author$project$Doc$caretD,
					A2($elm$json$Json$Decode$field, 'text', $elm$json$Json$Decode$string));
			case 'draft':
				return A3(
					$elm$json$Json$Decode$map2,
					$author$project$Doc$Draft,
					A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string),
					$author$project$Doc$caretD);
			case 'undraft':
				return A2(
					$elm$json$Json$Decode$map,
					$author$project$Doc$Undraft,
					A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string));
			case 'draftpair':
				return $elm$json$Json$Decode$succeed($author$project$Doc$DraftPair);
			case 'undraftpair':
				return A2(
					$elm$json$Json$Decode$map,
					$author$project$Doc$UndraftPair,
					A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string));
			case 'draftplan':
				return A2(
					$elm$json$Json$Decode$map,
					$author$project$Doc$DraftPlan,
					A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string));
			case 'undraftplan':
				return A2(
					$elm$json$Json$Decode$map,
					$author$project$Doc$UndraftPlan,
					A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string));
			case 'tab':
				return $elm$json$Json$Decode$succeed($author$project$Doc$Tab);
			case 'fold':
				return A2(
					$elm$json$Json$Decode$map,
					$author$project$Doc$Fold,
					A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string));
			case 'hidedone':
				return $elm$json$Json$Decode$succeed($author$project$Doc$HideDone);
			case 'shift':
				return A2(
					$elm$json$Json$Decode$map,
					$author$project$Doc$Shift,
					A2($elm$json$Json$Decode$field, 'by', $elm$json$Json$Decode$int));
			case 'addprop':
				return A3(
					$elm$json$Json$Decode$map2,
					$author$project$Doc$AddProp,
					A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string),
					A2($elm$json$Json$Decode$field, 'value', $elm$json$Json$Decode$string));
			case 'meta':
				return A3(
					$elm$json$Json$Decode$map2,
					$author$project$Doc$SetMeta,
					A2(
						$elm$json$Json$Decode$field,
						'props',
						$elm$json$Json$Decode$list($author$project$Doc$pairD)),
					A2(
						$elm$json$Json$Decode$field,
						'plan',
						$elm$json$Json$Decode$list($author$project$Doc$pairD)));
			case 'cells':
				return A2(
					$elm$json$Json$Decode$map,
					$author$project$Doc$SetCells,
					A2(
						$elm$json$Json$Decode$field,
						'cells',
						$elm$json$Json$Decode$list($author$project$Doc$cellD)));
			default:
				return $elm$json$Json$Decode$succeed($author$project$Doc$Ignore);
		}
	},
	A2($elm$json$Json$Decode$field, 'kind', $elm$json$Json$Decode$string));
var $author$project$Scan$blankAt = F2(
	function (i, xs) {
		return $author$project$Scan$isBlank(
			A2(
				$elm$core$Maybe$withDefault,
				'',
				A2($author$project$Scan$nth, i, xs)));
	});
var $author$project$Body$apart = F3(
	function (lines, line, written) {
		return _Utils_ap(
			((line > 1) && (!A2($author$project$Scan$blankAt, line - 1, lines))) ? _List_fromArray(
				['']) : _List_Nil,
			_Utils_ap(
				written,
				A2($author$project$Scan$blankAt, line, lines) ? _List_Nil : _List_fromArray(
					[''])));
	});
var $author$project$Scan$closerWord = F2(
	function (line, name) {
		var raw = $elm$core$String$trimLeft(line);
		var begin = A3($elm$core$String$slice, 2, 7, raw);
		return '#+' + ((_Utils_eq(
			begin,
			$elm$core$String$toUpper(begin)) ? 'END' : 'end') + ('_' + A3(
			$elm$core$String$slice,
			8,
			8 + $elm$core$String$length(name),
			raw)));
	});
var $author$project$Scan$indentOf = function (line) {
	return A2(
		$elm$core$String$left,
		$elm$core$String$length(line) - $elm$core$String$length(
			$elm$core$String$trimLeft(line)),
		line);
};
var $author$project$Scan$verbatim = function (name) {
	return A2(
		$elm$core$List$member,
		name,
		_List_fromArray(
			['comment', 'example', 'export', 'src', 'verse']));
};
var $author$project$Scan$closerOpen = function (line) {
	var _v0 = $author$project$Scan$blockName(line);
	if (!_v0.$) {
		var name = _v0.a;
		return $elm$core$Maybe$Just(
			{
				aJ: _Utils_ap(
					$author$project$Scan$indentOf(line),
					A2($author$project$Scan$closerWord, line, name)),
				aS: $author$project$Scan$verbatim(name),
				f: $author$project$Scan$endsBlock(name)
			});
	} else {
		var _v1 = $author$project$Scan$drawerName(line);
		if (!_v1.$) {
			return $elm$core$Maybe$Just(
				{
					aJ: $author$project$Scan$indentOf(line) + ':END:',
					aS: false,
					f: $author$project$Scan$drawerEnds
				});
		} else {
			return $elm$core$Maybe$Nothing;
		}
	}
};
var $author$project$Scan$closerPush = F2(
	function (line, stack) {
		var _v0 = $author$project$Scan$closerOpen(line);
		if (!_v0.$) {
			var open = _v0.a;
			return _Utils_Tuple2(
				A2($elm$core$List$cons, open, stack),
				true);
		} else {
			return _Utils_Tuple2(stack, false);
		}
	});
var $author$project$Scan$closerStep = F2(
	function (line, _v0) {
		var stack = _v0.a;
		if (stack.b) {
			var top = stack.a;
			var below = stack.b;
			return top.f(line) ? _Utils_Tuple2(below, false) : (top.aS ? _Utils_Tuple2(stack, false) : A2($author$project$Scan$closerPush, line, stack));
		} else {
			return A2($author$project$Scan$closerPush, line, stack);
		}
	});
var $author$project$Scan$closers = function (lines) {
	var _v0 = A3(
		$elm$core$List$foldl,
		$author$project$Scan$closerStep,
		_Utils_Tuple2(_List_Nil, false),
		lines);
	var stack = _v0.a;
	var empty = _v0.b;
	return _Utils_ap(
		empty ? _List_fromArray(
			['']) : _List_Nil,
		A2(
			$elm$core$List$map,
			function ($) {
				return $.aJ;
			},
			stack));
};
var $author$project$Body$indentIn = function (text) {
	return $elm$core$String$length(
		$author$project$Scan$indentOf(
			A2(
				$elm$core$Maybe$withDefault,
				'',
				$elm$core$List$head(
					A2($elm$core$String$split, '\u000A', text)))));
};
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $author$project$Body$nudge = F2(
	function (n, line) {
		return (n > 0) ? _Utils_ap(
			A2($elm$core$String$repeat, n, ' '),
			line) : ((n < 0) ? A2(
			$elm$core$String$dropLeft,
			A2(
				$elm$core$Basics$min,
				-n,
				$elm$core$String$length(
					$author$project$Scan$indentOf(line))),
			line) : line);
	});
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (!maybeValue.$) {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Body$rowById = F2(
	function (m, id) {
		return $elm$core$List$head(
			A2(
				$elm$core$List$filter,
				function (r) {
					return _Utils_eq(r.c, id);
				},
				m.C));
	});
var $author$project$Body$ownersOf = F2(
	function (m, id) {
		var _v0 = A2(
			$elm$core$Maybe$andThen,
			function ($) {
				return $.u;
			},
			A2($author$project$Body$rowById, m, id));
		if (_v0.$ === 1) {
			return _List_Nil;
		} else {
			var up = _v0.a;
			return A2(
				$elm$core$List$cons,
				up,
				A2($author$project$Body$ownersOf, m, up));
		}
	});
var $author$project$Body$bodyText = F2(
	function (m, gone) {
		var splice = F2(
			function (r, out) {
				if (A2($elm$core$List$member, r.c, gone)) {
					var spare = ((_Utils_cmp(
						r.g,
						$elm$core$List$length(out) - 1) < 0) && A2($author$project$Scan$blankAt, r.g, out)) ? 1 : 0;
					return _Utils_ap(
						A2($elm$core$List$take, r.d, out),
						A2($elm$core$List$drop, r.g + spare, out));
				} else {
					if (!_Utils_eq(r.O, r.S)) {
						var typed = A2($elm$core$String$split, '\u000A', r.O);
						var written = _Utils_ap(
							typed,
							$author$project$Scan$closers(typed));
						var shift = $author$project$Body$indentIn(r.O) - $author$project$Body$indentIn(r.S);
						var cut = A2($author$project$Body$ownEnd, m.C, r);
						var under = A2(
							$elm$core$List$map,
							$author$project$Body$nudge(shift),
							A2(
								$elm$core$List$take,
								r.g - cut,
								A2($elm$core$List$drop, cut, out)));
						return _Utils_ap(
							A2($elm$core$List$take, r.d, out),
							_Utils_ap(
								r.aa ? A3($author$project$Body$apart, out, r.d, written) : written,
								_Utils_ap(
									under,
									A2($elm$core$List$drop, r.g, out))));
					} else {
						return out;
					}
				}
			});
		var moved = function (r) {
			return (r.j === 1) && (A2($elm$core$List$member, r.c, gone) || (!_Utils_eq(r.O, r.S)));
		};
		var spoken = A2(
			$elm$core$List$map,
			function ($) {
				return $.c;
			},
			A2($elm$core$List$filter, moved, m.C));
		var silenced = function (r) {
			return (!$elm$core$List$isEmpty(spoken)) && A2(
				$elm$core$List$any,
				function (o) {
					return A2($elm$core$List$member, o, spoken);
				},
				A2($author$project$Body$ownersOf, m, r.c));
		};
		var paras = $elm$core$List$reverse(
			A2(
				$elm$core$List$filter,
				function (r) {
					return (r.j === 1) && (!silenced(r));
				},
				m.C));
		return A2(
			$elm$core$String$join,
			'\u000A',
			A3($elm$core$List$foldl, splice, m.aO, paras));
	});
var $author$project$Doc$docState = _Platform_outgoingPort('docState', $elm$core$Basics$identity);
var $author$project$Doc$docTook = _Platform_outgoingPort('docTook', $elm$core$Basics$identity);
var $author$project$Doc$pairsJSON = $elm$json$Json$Encode$list(
	function (_v0) {
		var k = _v0.a;
		var v = _v0.b;
		return A2(
			$elm$json$Json$Encode$list,
			$elm$json$Json$Encode$string,
			_List_fromArray(
				[k, v]));
	});
var $author$project$Doc$headerJSON = function (m) {
	return _List_fromArray(
		[
			_Utils_Tuple2(
			'properties',
			$author$project$Doc$pairsJSON(m.cM)),
			_Utils_Tuple2(
			'planning',
			$author$project$Doc$pairsJSON(m.A))
		]);
};
var $author$project$Doc$heading = function (r) {
	return (!r.j) || (r.j === 2);
};
var $author$project$Doc$idAtRow = F2(
	function (m, i) {
		return A2(
			$elm$core$Maybe$withDefault,
			'',
			A2(
				$elm$core$Maybe$map,
				function ($) {
					return $.c;
				},
				A2($author$project$Scan$nth, i, m.C)));
	});
var $author$project$Doc$landAt = F2(
	function (i, m) {
		return _Utils_update(
			m,
			{aH: i, q: $elm$core$Maybe$Nothing, B: $elm$core$Maybe$Nothing});
	});
var $author$project$Body$placeOf = F2(
	function (m, id) {
		return A2(
			$elm$core$Maybe$withDefault,
			m.aH,
			A2(
				$author$project$Scan$indexWhere,
				function (r) {
					return _Utils_eq(r.c, id);
				},
				m.C));
	});
var $author$project$Doc$landOn = F2(
	function (id, m) {
		return A2(
			$author$project$Doc$landAt,
			A2(
				$author$project$Body$placeOf,
				m,
				(!_Utils_eq(
					A2($author$project$Body$rowById, m, id),
					$elm$core$Maybe$Nothing)) ? id : $author$project$Body$drawerId),
			m);
	});
var $author$project$Doc$landingAfter = F3(
	function (model, taken, cut) {
		var _v0 = $elm$core$List$head(taken);
		if (_v0.$ === 1) {
			return model.Q;
		} else {
			var first = _v0.a;
			var kin = A2(
				$elm$core$List$filter,
				function (r) {
					return (r.j === 1) && (_Utils_eq(r.u, first.u) && (!A2(
						$elm$core$List$member,
						r.c,
						A2(
							$elm$core$List$map,
							function ($) {
								return $.c;
							},
							taken))));
				},
				model.C);
			var _v1 = A2(
				$elm$core$List$filter,
				function (r) {
					return _Utils_cmp(r.d, first.d) > 0;
				},
				kin);
			if (_v1.b) {
				var next = _v1.a;
				return $elm$core$Maybe$Just(next.d - cut);
			} else {
				var _v2 = $elm$core$List$reverse(
					A2(
						$elm$core$List$filter,
						function (r) {
							return _Utils_cmp(r.d, first.d) < 0;
						},
						kin));
				if (_v2.b) {
					var prev = _v2.a;
					return $elm$core$Maybe$Just(prev.d);
				} else {
					return A2(
						$elm$core$Maybe$map,
						function ($) {
							return $.d;
						},
						A2(
							$elm$core$Maybe$andThen,
							$author$project$Body$rowById(model),
							first.u));
				}
			}
		}
	});
var $author$project$Doc$boxLen = function (rest) {
	return A2(
		$elm$core$List$member,
		A2($elm$core$String$left, 3, rest),
		_List_fromArray(
			['[ ]', '[X]', '[x]', '[-]'])) ? (3 + $elm$core$String$length(
		$author$project$Scan$indentOf(
			A2($elm$core$String$dropLeft, 3, rest)))) : 0;
};
var $author$project$Doc$openedLen = A2(
	$elm$core$Basics$composeL,
	$elm$core$Maybe$withDefault(0),
	$elm$core$Maybe$map(
		function (o) {
			return o.by + $elm$core$String$length(o.aI);
		}));
var $author$project$Doc$markerOf = F2(
	function (op, line) {
		var _v0 = $author$project$Doc$openedLen(op);
		if (!_v0) {
			return 0;
		} else {
			var k = _v0;
			return k + $author$project$Doc$boxLen(
				A2($elm$core$String$dropLeft, k, line));
		}
	});
var $author$project$Doc$openerAt = F2(
	function (m, r) {
		return ((r.x !== 2) || (r.j !== 1)) ? $elm$core$Maybe$Nothing : $author$project$Scan$listOpener(
			A2($author$project$Doc$lineOf, m, r));
	});
var $author$project$Doc$boxState = F2(
	function (m, r) {
		var op = A2($author$project$Doc$openerAt, m, r);
		var opened = $author$project$Doc$openedLen(op);
		var line = A2($author$project$Doc$lineOf, m, r);
		var k = A2($author$project$Doc$markerOf, op, line);
		var box = A3($elm$core$String$slice, opened, k, line);
		return ((_Utils_cmp(k, opened) < 1) || $elm$core$String$isEmpty(
			$elm$core$String$trim(box))) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
			A2($elm$core$String$contains, 'X', box) || A2($elm$core$String$contains, 'x', box));
	});
var $elm$core$Dict$isEmpty = function (dict) {
	if (dict.$ === -2) {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Set$isEmpty = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$isEmpty(dict);
};
var $author$project$Doc$isListRoot = F2(
	function (m, id) {
		var _v0 = A2($author$project$Body$rowById, m, id);
		if (!_v0.$) {
			var r = _v0.a;
			return (r.x === 1) && _Utils_eq(
				r.am,
				$elm$core$Maybe$Just('list'));
		} else {
			return false;
		}
	});
var $author$project$Doc$listRootOf = F2(
	function (m, id) {
		return $elm$core$List$head(
			A2(
				$elm$core$List$filter,
				$author$project$Doc$isListRoot(m),
				A2($author$project$Body$ownersOf, m, id)));
	});
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (!_v0.$) {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0;
		return A2($elm$core$Dict$member, key, dict);
	});
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $author$project$Doc$checkKids = F2(
	function (m, id) {
		return A2(
			$elm$core$List$filter,
			function (r) {
				return _Utils_eq(
					r.u,
					$elm$core$Maybe$Just(id)) && (!_Utils_eq(
					A2($author$project$Doc$boxState, m, r),
					$elm$core$Maybe$Nothing));
			},
			m.C);
	});
var $author$project$Doc$subtreeDone = F2(
	function (m, r) {
		var _v0 = A2($author$project$Doc$checkKids, m, r.c);
		if (!_v0.b) {
			return _Utils_eq(
				A2($author$project$Doc$boxState, m, r),
				$elm$core$Maybe$Just(true));
		} else {
			var kids = _v0;
			return A2(
				$elm$core$List$all,
				$author$project$Doc$subtreeDone(m),
				kids);
		}
	});
var $author$project$Doc$hiddenDone = function (m) {
	return $elm$core$Set$isEmpty(m.y) ? $elm$core$Set$empty : $elm$core$Set$fromList(
		A2(
			$elm$core$List$filterMap,
			function (r) {
				if (A2($author$project$Doc$isListRoot, m, r.c)) {
					return (A2($elm$core$Set$member, r.c, m.y) && A2($author$project$Doc$subtreeDone, m, r)) ? $elm$core$Maybe$Just(r.c) : $elm$core$Maybe$Nothing;
				} else {
					var _v0 = _Utils_Tuple2(
						A2($author$project$Doc$boxState, m, r),
						A2($author$project$Doc$listRootOf, m, r.c));
					if ((!_v0.a.$) && (!_v0.b.$)) {
						var root = _v0.b.a;
						return (A2($elm$core$Set$member, root, m.y) && A2($author$project$Doc$subtreeDone, m, r)) ? $elm$core$Maybe$Just(r.c) : $elm$core$Maybe$Nothing;
					} else {
						return $elm$core$Maybe$Nothing;
					}
				}
			},
			m.C));
};
var $author$project$Doc$hiddenIn = function (m) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (r, acc) {
				var _v0 = r.u;
				if (!_v0.$) {
					var o = _v0.a;
					return (A2($elm$core$Set$member, o, m.f) || A2($elm$core$Set$member, o, acc)) ? A2($elm$core$Set$insert, r.c, acc) : acc;
				} else {
					return acc;
				}
			}),
		$elm$core$Set$empty,
		m.C);
};
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === -2) {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$core$Set$union = F2(
	function (_v0, _v1) {
		var dict1 = _v0;
		var dict2 = _v1;
		return A2($elm$core$Dict$union, dict1, dict2);
	});
var $author$project$Doc$snapVisible = function (m) {
	var n = $elm$core$List$length(m.C);
	var hidden = A2(
		$elm$core$Set$union,
		$author$project$Doc$hiddenDone(m),
		$author$project$Doc$hiddenIn(m));
	var visibleAt = function (i) {
		var _v0 = A2($author$project$Scan$nth, i, m.C);
		if (!_v0.$) {
			var r = _v0.a;
			return !A2($elm$core$Set$member, r.c, hidden);
		} else {
			return false;
		}
	};
	var seek = F2(
		function (by, i) {
			seek:
			while (true) {
				if ((i < 0) || (_Utils_cmp(i, n) > -1)) {
					return -1;
				} else {
					if (visibleAt(i)) {
						return i;
					} else {
						var $temp$by = by,
							$temp$i = i + by;
						by = $temp$by;
						i = $temp$i;
						continue seek;
					}
				}
			}
		});
	var landing = (A2(seek, 1, m.aH) >= 0) ? A2(seek, 1, m.aH) : A2(seek, -1, m.aH);
	return (A2(
		$elm$core$Set$member,
		A2($author$project$Doc$idAtRow, m, m.aH),
		hidden) && (landing >= 0)) ? _Utils_update(
		m,
		{aH: landing}) : m;
};
var $elm$core$Basics$clamp = F3(
	function (low, high, number) {
		return (_Utils_cmp(number, low) < 0) ? low : ((_Utils_cmp(number, high) > 0) ? high : number);
	});
var $author$project$Body$tableCells = function (line) {
	var cells = A2(
		$elm$core$List$drop,
		1,
		A2(
			$elm$core$String$split,
			'|',
			$elm$core$String$trim(line)));
	var n = $elm$core$List$length(cells);
	return _Utils_eq(
		A2($author$project$Scan$nth, n - 1, cells),
		$elm$core$Maybe$Just('')) ? A2($elm$core$List$take, n - 1, cells) : cells;
};
var $author$project$Body$caretIn = function (marker) {
	var open = $elm$core$String$length(
		$author$project$Scan$indentOf(marker)) + 1;
	var firstCell = open + $elm$core$String$length(
		A2(
			$elm$core$Maybe$withDefault,
			'',
			$elm$core$List$head(
				$author$project$Body$tableCells(marker))));
	return $author$project$Scan$isTable(marker) ? A3($elm$core$Basics$clamp, open, firstCell, open + 1) : $elm$core$String$length(marker);
};
var $author$project$Body$draftId = 'D';
var $author$project$Doc$planPick = function (m) {
	return m.B;
};
var $author$project$Doc$planKeyAt = function (m) {
	return A2(
		$elm$core$Maybe$andThen,
		function (i) {
			return A2(
				$elm$core$Maybe$map,
				$elm$core$Tuple$first,
				A2(
					$author$project$Scan$nth,
					i,
					$author$project$Doc$entriesOf(m)));
		},
		$author$project$Doc$planPick(m));
};
var $author$project$Body$rowAt = function (m) {
	return A2($author$project$Scan$nth, m.aH, m.C);
};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $author$project$Doc$cellJSON = function (c) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'key',
				$elm$json$Json$Encode$string(c.bA)),
				_Utils_Tuple2(
				'val',
				$elm$json$Json$Encode$string(c.bc)),
				_Utils_Tuple2(
				'colour',
				$elm$json$Json$Encode$string(c.cg))
			]));
};
var $author$project$Doc$charOf = F2(
	function (m, line) {
		return A2(
			$elm$core$Maybe$withDefault,
			0,
			A2($elm$core$Array$get, line, m.aB));
	});
var $author$project$Doc$elementSpan = F2(
	function (m, r) {
		var _v0 = m.aC;
		if (_v0.$ === 1) {
			return $elm$core$Maybe$Nothing;
		} else {
			var base = _v0.a;
			var _v1 = r.j;
			switch (_v1) {
				case 2:
					return $elm$core$Maybe$Nothing;
				case 3:
					return $elm$core$Maybe$Nothing;
				case 1:
					return _Utils_eq(r.c, $author$project$Body$tailId) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
						_Utils_Tuple2(
							(base + m.Z) + A2($author$project$Doc$charOf, m, r.d),
							(base + m.Z) + A2($author$project$Doc$charOf, m, r.g)));
				default:
					return $elm$core$Maybe$Just(
						_Utils_Tuple2(
							base,
							base + A2($author$project$Doc$charOf, m, 1)));
			}
		}
	});
var $author$project$Doc$foldable = F2(
	function (m, r) {
		return (r.j === 2) || A2($author$project$Doc$drawer, m, r);
	});
var $author$project$Body$kindWord = function (k) {
	switch (k) {
		case 0:
			return 'head';
		case 1:
			return 'para';
		case 2:
			return 'child';
		default:
			return 'meta';
	}
};
var $author$project$Doc$reachSpan = F2(
	function (m, r) {
		var _v0 = _Utils_Tuple2(m.aC, r.j);
		_v0$2:
		while (true) {
			if (!_v0.a.$) {
				switch (_v0.b) {
					case 0:
						var base = _v0.a.a;
						var _v1 = _v0.b;
						return $elm$core$Maybe$Just(
							_Utils_Tuple2(
								base,
								(base + m.Z) + A2(
									$author$project$Doc$charOf,
									m,
									$elm$core$List$length(m.aO))));
					case 2:
						var base = _v0.a.a;
						var _v2 = _v0.b;
						return $elm$core$Maybe$Just(
							_Utils_Tuple2(
								(base + m.Z) + A2($author$project$Doc$charOf, m, r.d),
								(base + m.Z) + A2($author$project$Doc$charOf, m, r.g)));
					default:
						break _v0$2;
				}
			} else {
				break _v0$2;
			}
		}
		return $elm$core$Maybe$Nothing;
	});
var $author$project$Doc$rowJSON = F2(
	function (m, r) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'id',
					$elm$json$Json$Encode$string(r.c)),
					_Utils_Tuple2(
					'kind',
					$elm$json$Json$Encode$string(
						$author$project$Body$kindWord(r.j))),
					_Utils_Tuple2(
					'fold',
					$elm$json$Json$Encode$bool(
						A2($author$project$Doc$foldable, m, r))),
					_Utils_Tuple2(
					'entries',
					$elm$json$Json$Encode$bool(
						_Utils_eq(r.c, $author$project$Body$planId))),
					_Utils_Tuple2(
					'grain',
					$elm$json$Json$Encode$string(
						function () {
							var _v0 = r.x;
							switch (_v0) {
								case 2:
									return 'leaf';
								case 1:
									return 'composite';
								default:
									return 'element';
							}
						}())),
					_Utils_Tuple2(
					'name',
					A2(
						$elm$core$Maybe$withDefault,
						$elm$json$Json$Encode$null,
						A2($elm$core$Maybe$map, $elm$json$Json$Encode$string, r.am))),
					_Utils_Tuple2(
					'owner',
					A2(
						$elm$core$Maybe$withDefault,
						$elm$json$Json$Encode$null,
						A2($elm$core$Maybe$map, $elm$json$Json$Encode$string, r.u))),
					_Utils_Tuple2(
					'from',
					$elm$json$Json$Encode$int(r.d)),
					_Utils_Tuple2(
					'to',
					$elm$json$Json$Encode$int(r.g)),
					_Utils_Tuple2(
					'text',
					$elm$json$Json$Encode$string(r.O)),
					_Utils_Tuple2(
					'index',
					$elm$json$Json$Encode$int(r.ax)),
					_Utils_Tuple2(
					'level',
					$elm$json$Json$Encode$int(r.V)),
					_Utils_Tuple2(
					'cells',
					A2($elm$json$Json$Encode$list, $author$project$Doc$cellJSON, r.ac)),
					_Utils_Tuple2(
					'span',
					function () {
						var _v1 = A2($author$project$Doc$elementSpan, m, r);
						if (!_v1.$) {
							var _v2 = _v1.a;
							var a = _v2.a;
							var b = _v2.b;
							return A2(
								$elm$json$Json$Encode$list,
								$elm$json$Json$Encode$int,
								_List_fromArray(
									[a, b]));
						} else {
							return $elm$json$Json$Encode$null;
						}
					}()),
					_Utils_Tuple2(
					'reach',
					function () {
						var _v3 = A2($author$project$Doc$reachSpan, m, r);
						if (!_v3.$) {
							var _v4 = _v3.a;
							var a = _v4.a;
							var b = _v4.b;
							return A2(
								$elm$json$Json$Encode$list,
								$elm$json$Json$Encode$int,
								_List_fromArray(
									[a, b]));
						} else {
							return $elm$json$Json$Encode$null;
						}
					}())
				]));
	});
var $author$project$Doc$stateJSON = function (m) {
	return $elm$json$Json$Encode$object(
		_Utils_ap(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'rows',
					A2(
						$elm$json$Json$Encode$list,
						$author$project$Doc$rowJSON(m),
						m.C)),
					_Utils_Tuple2(
					'at',
					$elm$json$Json$Encode$int(m.aH)),
					_Utils_Tuple2(
					'id',
					$elm$json$Json$Encode$string(
						A2(
							$elm$core$Maybe$withDefault,
							'',
							A2(
								$elm$core$Maybe$map,
								function ($) {
									return $.c;
								},
								$author$project$Body$rowAt(m))))),
					_Utils_Tuple2(
					'planKey',
					A2(
						$elm$core$Maybe$withDefault,
						$elm$json$Json$Encode$null,
						A2(
							$elm$core$Maybe$map,
							$elm$json$Json$Encode$string,
							$author$project$Doc$planKeyAt(m)))),
					_Utils_Tuple2(
					'col',
					A2(
						$elm$core$Maybe$withDefault,
						$elm$json$Json$Encode$null,
						A2($elm$core$Maybe$map, $elm$json$Json$Encode$int, m.q))),
					_Utils_Tuple2(
					'flags',
					A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, m.P)),
					_Utils_Tuple2(
					'lines',
					$elm$json$Json$Encode$int(
						$elm$core$List$length(m.aO))),
					_Utils_Tuple2(
					'caret',
					$elm$json$Json$Encode$int(
						$author$project$Body$caretIn(
							A2(
								$elm$core$Maybe$withDefault,
								'',
								A2(
									$elm$core$Maybe$map,
									function ($) {
										return $.O;
									},
									A2($author$project$Body$rowById, m, $author$project$Body$draftId)))))),
					_Utils_Tuple2(
					'body',
					$elm$json$Json$Encode$string(
						A2($author$project$Body$bodyText, m, _List_Nil)))
				]),
			$author$project$Doc$headerJSON(m)));
};
var $author$project$Doc$applyDelete = F2(
	function (ids, model) {
		var named = A2(
			$elm$core$List$filter,
			function (r) {
				return A2($elm$core$List$member, r.c, ids);
			},
			model.C);
		var refused = $elm$core$List$length(
			A2($elm$core$List$filter, $author$project$Doc$heading, named));
		var taken = A2(
			$elm$core$List$filter,
			function (r) {
				return r.j === 1;
			},
			named);
		var written = A2(
			$author$project$Body$bodyText,
			model,
			A2(
				$elm$core$List$map,
				function ($) {
					return $.c;
				},
				taken));
		var metaN = $elm$core$List$length(
			A2(
				$elm$core$List$filter,
				function (r) {
					return r.j === 3;
				},
				named));
		var keptProps = A2($elm$core$List$member, $author$project$Body$drawerId, ids) ? _List_Nil : A2(
			$elm$core$List$map,
			$elm$core$Tuple$second,
			A2(
				$elm$core$List$filter,
				function (_v0) {
					var j = _v0.a;
					return !A2(
						$elm$core$List$member,
						$author$project$Body$propId(j),
						ids);
				},
				A2($elm$core$List$indexedMap, $elm$core$Tuple$pair, model.cM)));
		var keptPlan = A2($elm$core$List$member, $author$project$Body$planId, ids) ? _List_Nil : model.A;
		var model_ = $author$project$Doc$remeta(
			_Utils_update(
				model,
				{A: keptPlan, cM: keptProps}));
		var cut = $elm$core$List$length(model.aO) - $elm$core$List$length(
			A2($elm$core$String$split, '\u000A', written));
		var landsOn = A3($author$project$Doc$landingAfter, model, taken, cut);
		var done = A2(
			$author$project$Doc$landOn,
			A2($author$project$Doc$idAtRow, model, model.aH),
			_Utils_update(
				model_,
				{Q: landsOn}));
		var snapped = $author$project$Doc$snapVisible(done);
		return _Utils_Tuple2(
			snapped,
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[
						$author$project$Doc$docState(
						$author$project$Doc$stateJSON(snapped)),
						$author$project$Doc$docTook(
						$elm$json$Json$Encode$object(
							_Utils_ap(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'taken',
										A2(
											$elm$json$Json$Encode$list,
											$elm$json$Json$Encode$string,
											A2(
												$elm$core$List$map,
												function ($) {
													return $.c;
												},
												taken))),
										_Utils_Tuple2(
										'refused',
										$elm$json$Json$Encode$int(refused)),
										_Utils_Tuple2(
										'meta',
										$elm$json$Json$Encode$int(metaN)),
										_Utils_Tuple2(
										'body',
										$elm$json$Json$Encode$string(written))
									]),
								$author$project$Doc$headerJSON(snapped))))
					])));
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === -1) && (dict.d.$ === -1)) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.e.d.$ === -1) && (!dict.e.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.d.d.$ === -1) && (!dict.d.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === -1) && (!left.a)) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === -1) && (right.a === 1)) {
					if (right.d.$ === -1) {
						if (right.d.a === 1) {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === -1) && (dict.d.$ === -1)) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor === 1) {
			if ((lLeft.$ === -1) && (!lLeft.a)) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === -1) {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === -1) && (left.a === 1)) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === -1) && (!lLeft.a)) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === -1) {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === -1) {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === -1) {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$diff = F2(
	function (t1, t2) {
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (k, v, t) {
					return A2($elm$core$Dict$remove, k, t);
				}),
			t1,
			t2);
	});
var $elm$core$Set$diff = F2(
	function (_v0, _v1) {
		var dict1 = _v0;
		var dict2 = _v1;
		return A2($elm$core$Dict$diff, dict1, dict2);
	});
var $author$project$Body$placeAtLine = F2(
	function (m, line) {
		var stops = A2(
			$elm$core$List$filter,
			function (_v7) {
				var r = _v7.b;
				return r.j === 1;
			},
			A2($elm$core$List$indexedMap, $elm$core$Tuple$pair, m.C));
		var last = $elm$core$List$head(
			$elm$core$List$reverse(stops));
		var covering = A3(
			$elm$core$List$foldl,
			F2(
				function (_v6, held) {
					var i = _v6.a;
					var r = _v6.b;
					return ((_Utils_cmp(r.d, line) < 1) && (_Utils_cmp(line, r.g) < 0)) ? $elm$core$Maybe$Just(i) : held;
				}),
			$elm$core$Maybe$Nothing,
			stops);
		var after = $elm$core$List$head(
			A2(
				$elm$core$List$filter,
				function (_v5) {
					var r = _v5.b;
					return _Utils_cmp(r.d, line) > -1;
				},
				stops));
		var _v0 = _Utils_Tuple2(covering, after);
		if (!_v0.a.$) {
			var i = _v0.a.a;
			return i;
		} else {
			if (!_v0.b.$) {
				var _v1 = _v0.a;
				var _v2 = _v0.b.a;
				var i = _v2.a;
				return i;
			} else {
				var _v3 = _v0.a;
				var _v4 = _v0.b;
				return A2(
					$elm$core$Maybe$withDefault,
					m.aH,
					A2($elm$core$Maybe$map, $elm$core$Tuple$first, last));
			}
		}
	});
var $author$project$Body$placeOfLine = F2(
	function (m, line) {
		return A2(
			$elm$core$Maybe$withDefault,
			m.aH,
			A2(
				$author$project$Scan$indexWhere,
				function (r) {
					return (r.j === 1) && _Utils_eq(r.d, line);
				},
				m.C));
	});
var $author$project$Doc$fillLanding = F2(
	function (fresh, model) {
		var _v0 = _Utils_Tuple2(fresh.Q, model.Q);
		if (!_v0.a.$) {
			var line = _v0.a.a;
			return A2($author$project$Body$placeAtLine, fresh, line);
		} else {
			if (!_v0.b.$) {
				var _v1 = _v0.a;
				var line = _v0.b.a;
				return A2($author$project$Body$placeOfLine, fresh, line);
			} else {
				var _v2 = _v0.a;
				var _v3 = _v0.b;
				var _v4 = A2(
					$elm$core$Maybe$map,
					function ($) {
						return $.c;
					},
					$author$project$Body$rowAt(model));
				if (!_v4.$) {
					var id = _v4.a;
					return A2($author$project$Body$placeOf, fresh, id);
				} else {
					return 0;
				}
			}
		}
	});
var $author$project$Doc$foldables = function (m) {
	return A2(
		$author$project$Doc$idsWhere,
		$author$project$Doc$foldable(m),
		m);
};
var $elm$core$Dict$filter = F2(
	function (isGood, dict) {
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (k, v, d) {
					return A2(isGood, k, v) ? A3($elm$core$Dict$insert, k, v, d) : d;
				}),
			$elm$core$Dict$empty,
			dict);
	});
var $elm$core$Dict$intersect = F2(
	function (t1, t2) {
		return A2(
			$elm$core$Dict$filter,
			F2(
				function (k, _v0) {
					return A2($elm$core$Dict$member, k, t2);
				}),
			t1);
	});
var $elm$core$Set$intersect = F2(
	function (_v0, _v1) {
		var dict1 = _v0;
		var dict2 = _v1;
		return A2($elm$core$Dict$intersect, dict1, dict2);
	});
var $author$project$Doc$listRoots = function (m) {
	return A2(
		$elm$core$List$filterMap,
		function (r) {
			return A2($author$project$Doc$isListRoot, m, r.c) ? $elm$core$Maybe$Just(r.c) : $elm$core$Maybe$Nothing;
		},
		m.C);
};
var $elm$core$Set$remove = F2(
	function (key, _v0) {
		var dict = _v0;
		return A2($elm$core$Dict$remove, key, dict);
	});
var $author$project$Doc$reveal = function (m) {
	return _Utils_update(
		m,
		{
			f: A3(
				$elm$core$List$foldl,
				$elm$core$Set$remove,
				m.f,
				A2(
					$elm$core$List$filter,
					function (id) {
						return _Utils_eq(
							A2(
								$elm$core$Maybe$map,
								$author$project$Doc$foldable(m),
								A2($author$project$Body$rowById, m, id)),
							$elm$core$Maybe$Just(true));
					},
					A2(
						$author$project$Body$ownersOf,
						m,
						A2($author$project$Doc$idAtRow, m, m.aH))))
		});
};
var $author$project$Body$isRule = function (line) {
	return A2(
		$elm$core$String$startsWith,
		'|-',
		$elm$core$String$trimLeft(line));
};
var $author$project$Doc$tableKids = F2(
	function (m, comp) {
		return A2(
			$elm$core$List$filter,
			function (k) {
				return _Utils_eq(
					k.u,
					$elm$core$Maybe$Just(comp.c));
			},
			m.C);
	});
var $author$project$Doc$tableHasHeader = F2(
	function (m, comp) {
		var _v0 = A2($author$project$Doc$tableKids, m, comp);
		if (_v0.b && _v0.b.b) {
			var _v1 = _v0.b;
			var second = _v1.a;
			return $author$project$Body$isRule(second.O);
		} else {
			return false;
		}
	});
var $author$project$Doc$tableBodyRows = F2(
	function (m, comp) {
		var data = A2(
			$elm$core$List$filter,
			A2(
				$elm$core$Basics$composeL,
				A2($elm$core$Basics$composeL, $elm$core$Basics$not, $author$project$Body$isRule),
				function ($) {
					return $.O;
				}),
			A2($author$project$Doc$tableKids, m, comp));
		return A2($author$project$Doc$tableHasHeader, m, comp) ? A2($elm$core$List$drop, 1, data) : data;
	});
var $author$project$Doc$tableCompOf = F2(
	function (m, r) {
		var _v0 = A2(
			$elm$core$Maybe$andThen,
			$author$project$Body$rowById(m),
			r.u);
		if (!_v0.$) {
			var comp = _v0.a;
			return (_Utils_eq(
				comp.am,
				$elm$core$Maybe$Just('table')) && A2(
				$elm$core$List$any,
				function (k) {
					return _Utils_eq(k.c, r.c);
				},
				A2($author$project$Doc$tableBodyRows, m, comp))) ? $elm$core$Maybe$Just(comp) : $elm$core$Maybe$Nothing;
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Doc$tableCompAt = function (m) {
	return A2(
		$elm$core$Maybe$andThen,
		$author$project$Doc$tableCompOf(m),
		$author$project$Body$rowAt(m));
};
var $author$project$Doc$settled = function (m) {
	var keptPlan = _Utils_eq(
		A2($author$project$Doc$idAtRow, m, m.aH),
		$author$project$Body$planId) ? m.B : $elm$core$Maybe$Nothing;
	var keptCol = (!_Utils_eq(
		$author$project$Doc$tableCompAt(m),
		$elm$core$Maybe$Nothing)) ? m.q : $elm$core$Maybe$Nothing;
	return $author$project$Doc$snapVisible(
		_Utils_update(
			m,
			{q: keptCol, B: keptPlan}));
};
var $author$project$Doc$told = function (model) {
	var m = $author$project$Doc$settled(model);
	return _Utils_Tuple2(
		m,
		$author$project$Doc$docState(
			$author$project$Doc$stateJSON(m)));
};
var $author$project$Doc$applyFill = F2(
	function (fresh, model) {
		return $author$project$Doc$told(
			$author$project$Doc$reveal(
				$author$project$Doc$snapVisible(
					_Utils_update(
						fresh,
						{
							aH: A2($author$project$Doc$fillLanding, fresh, model),
							y: A2(
								$elm$core$Set$intersect,
								model.y,
								$elm$core$Set$fromList(
									$author$project$Doc$listRoots(fresh))),
							Q: $elm$core$Maybe$Nothing,
							f: A2(
								$elm$core$Set$union,
								A2(
									$elm$core$Set$intersect,
									model.f,
									$author$project$Doc$foldables(fresh)),
								A2(
									$elm$core$Set$diff,
									fresh.f,
									$author$project$Doc$foldables(model)))
						}))));
	});
var $author$project$Doc$backTo = F2(
	function (id, m) {
		var fresh = $author$project$Doc$remeta(m);
		return $author$project$Doc$told(
			_Utils_update(
				fresh,
				{
					aH: A2($author$project$Body$placeOf, fresh, id)
				}));
	});
var $author$project$Doc$grainWord = F4(
	function (grain, name, i, n) {
		return grain + (' (' + (name + (' ' + ($elm$core$String$fromInt(i) + ('/' + ($elm$core$String$fromInt(n) + ')'))))));
	});
var $author$project$Doc$planWord = F3(
	function (grain, i, entries) {
		return A4(
			$author$project$Doc$grainWord,
			grain,
			A2(
				$elm$core$Maybe$withDefault,
				'',
				A2(
					$elm$core$Maybe$map,
					$elm$core$Tuple$first,
					A2($author$project$Scan$nth, i, entries))),
			i + 1,
			$elm$core$List$length(entries));
	});
var $author$project$Doc$planBroader = F2(
	function (i, m) {
		var entries = $author$project$Doc$entriesOf(m);
		return (i <= 0) ? _Utils_Tuple2(
			_Utils_update(
				m,
				{B: $elm$core$Maybe$Nothing}),
			'grain-broader (the planning line)') : _Utils_Tuple2(
			_Utils_update(
				m,
				{
					B: $elm$core$Maybe$Just(i - 1)
				}),
			A3($author$project$Doc$planWord, 'grain-broader', i - 1, entries));
	});
var $author$project$Doc$broader = function (m) {
	var _v0 = _Utils_Tuple2(
		$author$project$Body$rowAt(m),
		$author$project$Doc$planPick(m));
	if (_v0.a.$ === 1) {
		var _v1 = _v0.a;
		return _Utils_Tuple2(m, '');
	} else {
		if (!_v0.b.$) {
			var i = _v0.b.a;
			return A2($author$project$Doc$planBroader, i, m);
		} else {
			var r = _v0.a.a;
			var _v2 = _v0.b;
			var _v3 = A2(
				$elm$core$Maybe$map,
				$author$project$Body$placeOf(m),
				r.u);
			if (!_v3.$) {
				var i = _v3.a;
				var up = A2(
					$elm$core$Maybe$withDefault,
					$author$project$Body$blank,
					A2($author$project$Scan$nth, i, m.C));
				var word = function () {
					var _v4 = up.am;
					if (!_v4.$) {
						var w = _v4.a;
						return w;
					} else {
						return (up.x === 2) ? 'item' : $author$project$Body$kindWord(up.j);
					}
				}();
				return _Utils_Tuple2(
					_Utils_update(
						m,
						{aH: i}),
					'grain-broader (' + (word + ')'));
			} else {
				return (!r.j) ? _Utils_Tuple2(m, 'grain-broader (the whole entry)') : _Utils_Tuple2(
					_Utils_update(
						m,
						{
							aH: A2($author$project$Body$placeOf, m, 'H')
						}),
					'grain-broader (the headline)');
			}
		}
	}
};
var $author$project$Doc$cargoJSON = F2(
	function (said, m) {
		var fields = A2(
			$elm$core$List$cons,
			_Utils_Tuple2(
				'body',
				$elm$json$Json$Encode$string(
					A2($author$project$Body$bodyText, m, _List_Nil))),
			$author$project$Doc$headerJSON(m));
		return $elm$json$Json$Encode$object(
			function () {
				if (!said.$) {
					var what = said.a;
					return A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							'said',
							$elm$json$Json$Encode$string(what)),
						fields);
				} else {
					return fields;
				}
			}());
	});
var $author$project$Doc$docBody = _Platform_outgoingPort('docBody', $elm$core$Basics$identity);
var $author$project$Doc$composedWith = F2(
	function (said, model) {
		var m = $author$project$Doc$settled(model);
		return _Utils_Tuple2(
			m,
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[
						$author$project$Doc$docState(
						$author$project$Doc$stateJSON(m)),
						$author$project$Doc$docBody(
						A2($author$project$Doc$cargoJSON, said, m))
					])));
	});
var $author$project$Doc$composed = $author$project$Doc$composedWith($elm$core$Maybe$Nothing);
var $author$project$Doc$docSaid = _Platform_outgoingPort('docSaid', $elm$core$Basics$identity);
var $author$project$Body$riding = F2(
	function (n, text) {
		return (!n) ? text : A2(
			$elm$core$String$join,
			'\u000A' + A2($elm$core$String$repeat, n, ' '),
			A2($elm$core$String$split, '\u000A', text));
	});
var $author$project$Body$draftRow = F2(
	function (j, text) {
		return _Utils_update(
			$author$project$Body$blank,
			{
				aa: j.aa,
				d: j.I,
				x: function () {
					var _v0 = j.u;
					if (!_v0.$) {
						return 2;
					} else {
						return 0;
					}
				}(),
				c: $author$project$Body$draftId,
				j: 1,
				u: j.u,
				O: A2(
					$author$project$Body$riding,
					$elm$core$String$length(j.aR),
					text),
				g: j.I,
				S: j.aR
			});
	});
var $author$project$Body$Join = F6(
	function (under, line, marker, owner, alone, word) {
		return {aa: alone, I: line, aR: marker, u: owner, bb: under, b3: word};
	});
var $author$project$Body$caretLine = F2(
	function (r, off) {
		return r.d + A3(
			$elm$core$Basics$clamp,
			0,
			A2($elm$core$Basics$max, 0, (r.g - r.d) - 1),
			off);
	});
var $author$project$Body$holding = F3(
	function (m, r, line) {
		return A3(
			$elm$core$List$foldl,
			F2(
				function (s, best) {
					return (_Utils_cmp(s.d, best.d) > -1) ? s : best;
				}),
			r,
			A2(
				$elm$core$List$filter,
				function (s) {
					return (s.j === 1) && ((!_Utils_eq(s.c, $author$project$Body$draftId)) && ((_Utils_cmp(s.d, line) < 1) && ((_Utils_cmp(line, s.g) < 0) && A2(
						$elm$core$List$member,
						r.c,
						A2($author$project$Body$ownersOf, m, s.c)))));
				},
				m.C));
	});
var $author$project$Body$anchored = F5(
	function (m, r, line, marker, word) {
		var host = A3($author$project$Body$holding, m, r, line - 1);
		return A6(
			$author$project$Body$Join,
			r.c,
			line,
			marker,
			(_Utils_cmp(line, host.g) > -1) ? host.u : $elm$core$Maybe$Just(host.c),
			false,
			word);
	});
var $author$project$Scan$closerAt = F2(
	function (reg, line) {
		return $author$project$Scan$closes(reg.j) && _Utils_eq(line, reg.g - 1);
	});
var $author$project$Body$boxAfter = function (after) {
	return A2(
		$elm$core$List$member,
		A2($elm$core$String$left, 3, after),
		_List_fromArray(
			['[ ]', '[X]', '[x]', '[-]'])) ? '[ ] ' : '';
};
var $author$project$Scan$numberAt = function (line) {
	var spaces = $elm$core$String$length(line) - $elm$core$String$length(
		$elm$core$String$trimLeft(line));
	return A2(
		$elm$core$Maybe$andThen,
		$elm$core$String$toInt,
		$author$project$Scan$numberedAt(
			A2($elm$core$String$dropLeft, spaces, line)));
};
var $author$project$Body$nextBullet = F2(
	function (line, o) {
		var digits = $elm$core$String$fromList(
			A2(
				$author$project$Scan$takeWhileList,
				$elm$core$Char$isDigit,
				$elm$core$String$toList(o.aI)));
		var _v0 = _Utils_Tuple2(
			$elm$core$String$isEmpty(digits),
			$author$project$Scan$numberAt(line));
		if ((!_v0.a) && (!_v0.b.$)) {
			var n = _v0.b.a;
			return _Utils_ap(
				$elm$core$String$fromInt(n + 1),
				A2(
					$elm$core$String$dropLeft,
					$elm$core$String$length(digits),
					o.aI));
		} else {
			return o.aI;
		}
	});
var $author$project$Body$itemMarker = F2(
	function (lines, from) {
		var line = A2($author$project$Scan$at, from, lines);
		var _v0 = $author$project$Scan$listOpener(line);
		if (_v0.$ === 1) {
			return $author$project$Scan$indentOf(line);
		} else {
			var o = _v0.a;
			return _Utils_ap(
				A2($elm$core$String$left, o.by, line),
				_Utils_ap(
					A2($author$project$Body$nextBullet, line, o),
					$author$project$Body$boxAfter(
						A2(
							$elm$core$String$dropLeft,
							o.by + $elm$core$String$length(o.aI),
							line))));
		}
	});
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $elm$core$Elm$JsArray$appendN = _JsArray_appendN;
var $elm$core$Elm$JsArray$slice = _JsArray_slice;
var $elm$core$Array$appendHelpBuilder = F2(
	function (tail, builder) {
		var tailLen = $elm$core$Elm$JsArray$length(tail);
		var notAppended = ($elm$core$Array$branchFactor - $elm$core$Elm$JsArray$length(builder.n)) - tailLen;
		var appended = A3($elm$core$Elm$JsArray$appendN, $elm$core$Array$branchFactor, builder.n, tail);
		return (notAppended < 0) ? {
			o: A2(
				$elm$core$List$cons,
				$elm$core$Array$Leaf(appended),
				builder.o),
			k: builder.k + 1,
			n: A3($elm$core$Elm$JsArray$slice, notAppended, tailLen, tail)
		} : ((!notAppended) ? {
			o: A2(
				$elm$core$List$cons,
				$elm$core$Array$Leaf(appended),
				builder.o),
			k: builder.k + 1,
			n: $elm$core$Elm$JsArray$empty
		} : {o: builder.o, k: builder.k, n: appended});
	});
var $elm$core$Array$sliceLeft = F2(
	function (from, array) {
		var len = array.a;
		var tree = array.c;
		var tail = array.d;
		if (!from) {
			return array;
		} else {
			if (_Utils_cmp(
				from,
				$elm$core$Array$tailIndex(len)) > -1) {
				return A4(
					$elm$core$Array$Array_elm_builtin,
					len - from,
					$elm$core$Array$shiftStep,
					$elm$core$Elm$JsArray$empty,
					A3(
						$elm$core$Elm$JsArray$slice,
						from - $elm$core$Array$tailIndex(len),
						$elm$core$Elm$JsArray$length(tail),
						tail));
			} else {
				var skipNodes = (from / $elm$core$Array$branchFactor) | 0;
				var helper = F2(
					function (node, acc) {
						if (!node.$) {
							var subTree = node.a;
							return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
						} else {
							var leaf = node.a;
							return A2($elm$core$List$cons, leaf, acc);
						}
					});
				var leafNodes = A3(
					$elm$core$Elm$JsArray$foldr,
					helper,
					_List_fromArray(
						[tail]),
					tree);
				var nodesToInsert = A2($elm$core$List$drop, skipNodes, leafNodes);
				if (!nodesToInsert.b) {
					return $elm$core$Array$empty;
				} else {
					var head = nodesToInsert.a;
					var rest = nodesToInsert.b;
					var firstSlice = from - (skipNodes * $elm$core$Array$branchFactor);
					var initialBuilder = {
						o: _List_Nil,
						k: 0,
						n: A3(
							$elm$core$Elm$JsArray$slice,
							firstSlice,
							$elm$core$Elm$JsArray$length(head),
							head)
					};
					return A2(
						$elm$core$Array$builderToArray,
						true,
						A3($elm$core$List$foldl, $elm$core$Array$appendHelpBuilder, initialBuilder, rest));
				}
			}
		}
	});
var $elm$core$Array$fetchNewTail = F4(
	function (shift, end, treeEnd, tree) {
		fetchNewTail:
		while (true) {
			var pos = $elm$core$Array$bitMask & (treeEnd >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (!_v0.$) {
				var sub = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$end = end,
					$temp$treeEnd = treeEnd,
					$temp$tree = sub;
				shift = $temp$shift;
				end = $temp$end;
				treeEnd = $temp$treeEnd;
				tree = $temp$tree;
				continue fetchNewTail;
			} else {
				var values = _v0.a;
				return A3($elm$core$Elm$JsArray$slice, 0, $elm$core$Array$bitMask & end, values);
			}
		}
	});
var $elm$core$Array$hoistTree = F3(
	function (oldShift, newShift, tree) {
		hoistTree:
		while (true) {
			if ((_Utils_cmp(oldShift, newShift) < 1) || (!$elm$core$Elm$JsArray$length(tree))) {
				return tree;
			} else {
				var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, 0, tree);
				if (!_v0.$) {
					var sub = _v0.a;
					var $temp$oldShift = oldShift - $elm$core$Array$shiftStep,
						$temp$newShift = newShift,
						$temp$tree = sub;
					oldShift = $temp$oldShift;
					newShift = $temp$newShift;
					tree = $temp$tree;
					continue hoistTree;
				} else {
					return tree;
				}
			}
		}
	});
var $elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var $elm$core$Array$sliceTree = F3(
	function (shift, endIdx, tree) {
		var lastPos = $elm$core$Array$bitMask & (endIdx >>> shift);
		var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, lastPos, tree);
		if (!_v0.$) {
			var sub = _v0.a;
			var newSub = A3($elm$core$Array$sliceTree, shift - $elm$core$Array$shiftStep, endIdx, sub);
			return (!$elm$core$Elm$JsArray$length(newSub)) ? A3($elm$core$Elm$JsArray$slice, 0, lastPos, tree) : A3(
				$elm$core$Elm$JsArray$unsafeSet,
				lastPos,
				$elm$core$Array$SubTree(newSub),
				A3($elm$core$Elm$JsArray$slice, 0, lastPos + 1, tree));
		} else {
			return A3($elm$core$Elm$JsArray$slice, 0, lastPos, tree);
		}
	});
var $elm$core$Array$sliceRight = F2(
	function (end, array) {
		var len = array.a;
		var startShift = array.b;
		var tree = array.c;
		var tail = array.d;
		if (_Utils_eq(end, len)) {
			return array;
		} else {
			if (_Utils_cmp(
				end,
				$elm$core$Array$tailIndex(len)) > -1) {
				return A4(
					$elm$core$Array$Array_elm_builtin,
					end,
					startShift,
					tree,
					A3($elm$core$Elm$JsArray$slice, 0, $elm$core$Array$bitMask & end, tail));
			} else {
				var endIdx = $elm$core$Array$tailIndex(end);
				var depth = $elm$core$Basics$floor(
					A2(
						$elm$core$Basics$logBase,
						$elm$core$Array$branchFactor,
						A2($elm$core$Basics$max, 1, endIdx - 1)));
				var newShift = A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep);
				return A4(
					$elm$core$Array$Array_elm_builtin,
					end,
					newShift,
					A3(
						$elm$core$Array$hoistTree,
						startShift,
						newShift,
						A3($elm$core$Array$sliceTree, startShift, endIdx, tree)),
					A4($elm$core$Array$fetchNewTail, startShift, end, endIdx, tree));
			}
		}
	});
var $elm$core$Array$translateIndex = F2(
	function (index, _v0) {
		var len = _v0.a;
		var posIndex = (index < 0) ? (len + index) : index;
		return (posIndex < 0) ? 0 : ((_Utils_cmp(posIndex, len) > 0) ? len : posIndex);
	});
var $elm$core$Array$slice = F3(
	function (from, to, array) {
		var correctTo = A2($elm$core$Array$translateIndex, to, array);
		var correctFrom = A2($elm$core$Array$translateIndex, from, array);
		return (_Utils_cmp(correctFrom, correctTo) > 0) ? $elm$core$Array$empty : A2(
			$elm$core$Array$sliceLeft,
			correctFrom,
			A2($elm$core$Array$sliceRight, correctTo, array));
	});
var $author$project$Body$widest = F2(
	function (_new, acc) {
		var _v0 = _Utils_Tuple2(_new, acc);
		if (!_v0.a.b) {
			var rest = _v0.b;
			return rest;
		} else {
			if (!_v0.b.b) {
				var rest = _v0.a;
				return rest;
			} else {
				var _v1 = _v0.a;
				var a = _v1.a;
				var more = _v1.b;
				var _v2 = _v0.b;
				var b = _v2.a;
				var rest = _v2.b;
				return A2(
					$elm$core$List$cons,
					A2($elm$core$Basics$max, a, b),
					A2($author$project$Body$widest, more, rest));
			}
		}
	});
var $author$project$Body$tableRow = F3(
	function (lines, from, to) {
		return '|' + $elm$core$String$concat(
			A2(
				$elm$core$List$map,
				function (w) {
					return A2($elm$core$String$repeat, w, ' ') + '|';
				},
				A3(
					$elm$core$List$foldl,
					$author$project$Body$widest,
					_List_Nil,
					A2(
						$elm$core$List$map,
						A2(
							$elm$core$Basics$composeL,
							$elm$core$List$map($elm$core$String$length),
							$author$project$Body$tableCells),
						A2(
							$elm$core$List$filter,
							A2($elm$core$Basics$composeL, $elm$core$Basics$not, $author$project$Body$isRule),
							$elm$core$Array$toList(
								A3($elm$core$Array$slice, from, to, lines)))))));
	});
var $author$project$Body$markerFor = F2(
	function (lines, reg) {
		var indent = $author$project$Scan$indentOf(
			A2($author$project$Scan$at, reg.d, lines));
		var _v0 = reg.j;
		switch (_v0) {
			case 0:
				return '';
			case 1:
				return A2($author$project$Body$itemMarker, lines, reg.d);
			case 2:
				return _Utils_ap(
					indent,
					A3($author$project$Body$tableRow, lines, reg.d, reg.g));
			case 3:
				return indent;
			default:
				return indent;
		}
	});
var $author$project$Body$outermost = F2(
	function (m, r) {
		var _v0 = $elm$core$List$reverse(
			A2(
				$elm$core$List$filter,
				function (o) {
					return !_Utils_eq(
						A2(
							$elm$core$Maybe$map,
							function ($) {
								return $.j;
							},
							A2($author$project$Body$rowById, m, o)),
						$elm$core$Maybe$Just(2));
				},
				A2($author$project$Body$ownersOf, m, r.c)));
		if (_v0.b) {
			var top = _v0.a;
			return A2(
				$elm$core$Maybe$withDefault,
				r,
				A2($author$project$Body$rowById, m, top));
		} else {
			return r;
		}
	});
var $author$project$Body$pastWord = function (top) {
	var _v0 = top.am;
	if (!_v0.$) {
		var name = _v0.a;
		return 'after the ' + name;
	} else {
		return (top.x === 1) ? 'after the block' : 'after this paragraph';
	}
};
var $author$project$Scan$greater = F2(
	function (lines, reg) {
		var _v0 = reg.j;
		switch (_v0) {
			case 1:
				return true;
			case 4:
				return true;
			case 3:
				return !$author$project$Scan$verbatim(
					A2(
						$elm$core$Maybe$withDefault,
						'',
						$author$project$Scan$blockName(
							A2($author$project$Scan$at, reg.d, lines))));
			case 2:
				return false;
			default:
				return false;
		}
	});
var $author$project$Scan$interiorEnd = function (reg) {
	return $author$project$Scan$closes(reg.j) ? (reg.g - 1) : reg.g;
};
var $author$project$Scan$regionAt = F4(
	function (lines, from, end, line) {
		var _v0 = A2(
			$elm$core$List$filter,
			function (r) {
				return (_Utils_cmp(r.d, line) < 1) && (_Utils_cmp(line, r.g) < 0);
			},
			A3($author$project$Scan$regionsIn, lines, from, end));
		if (_v0.b) {
			var reg = _v0.a;
			return A2($author$project$Scan$greater, lines, reg) ? A3($author$project$Scan$within, lines, reg, line) : reg;
		} else {
			return A3($author$project$Scan$Region, 0, line, line + 1);
		}
	});
var $author$project$Scan$within = F3(
	function (lines, reg, line) {
		var nested = A4(
			$author$project$Scan$regionAt,
			lines,
			reg.d + 1,
			$author$project$Scan$interiorEnd(reg),
			line);
		return ((!nested.j) || A2($author$project$Scan$closerAt, nested, line)) ? reg : nested;
	});
var $author$project$Body$regionWord = function (kind) {
	switch (kind) {
		case 1:
			return 'an item at this level';
		case 2:
			return 'a row in this table';
		case 3:
			return 'a line in this block';
		case 4:
			return 'a line in this drawer';
		default:
			return 'a line here';
	}
};
var $author$project$Body$inside = F3(
	function (m, r, line) {
		var top = A2($author$project$Body$outermost, m, r);
		var lines = $elm$core$Array$fromList(m.aO);
		var reg = A4($author$project$Scan$regionAt, lines, top.d, top.g, line);
		return ((!reg.j) || A2($author$project$Scan$closerAt, reg, line)) ? A6(
			$author$project$Body$Join,
			top.c,
			reg.g,
			'',
			$elm$core$Maybe$Nothing,
			true,
			$author$project$Body$pastWord(top)) : A5(
			$author$project$Body$anchored,
			m,
			r,
			line + 1,
			A2($author$project$Body$markerFor, lines, reg),
			$author$project$Body$regionWord(reg.j));
	});
var $author$project$Body$sibling = F2(
	function (m, r) {
		var top = A2($author$project$Body$outermost, m, r);
		return (_Utils_eq(
			top.am,
			$elm$core$Maybe$Just('list')) && (r.x === 2)) ? A6(
			$author$project$Body$Join,
			r.c,
			r.g,
			A2(
				$author$project$Body$itemMarker,
				$elm$core$Array$fromList(m.aO),
				r.d),
			r.u,
			false,
			$author$project$Body$regionWord(1)) : A6(
			$author$project$Body$Join,
			top.c,
			top.g,
			'',
			$elm$core$Maybe$Nothing,
			true,
			$author$project$Body$pastWord(top));
	});
var $author$project$Body$joinAt = F3(
	function (m, id, caret) {
		var _v0 = A2($author$project$Body$rowById, m, id);
		if (_v0.$ === 1) {
			return $elm$core$Maybe$Nothing;
		} else {
			var r = _v0.a;
			var _v1 = r.j;
			switch (_v1) {
				case 2:
					return $elm$core$Maybe$Nothing;
				case 3:
					return $elm$core$Maybe$Nothing;
				case 0:
					return $elm$core$Maybe$Just(
						A6($author$project$Body$Join, r.c, 1, '', $elm$core$Maybe$Nothing, true, 'at the top'));
				default:
					return $elm$core$Maybe$Just(
						function () {
							if (!caret.$) {
								var off = caret.a;
								return A3(
									$author$project$Body$inside,
									m,
									r,
									A2($author$project$Body$caretLine, r, off));
							} else {
								return A2($author$project$Body$sibling, m, r);
							}
						}());
			}
		}
	});
var $author$project$Body$joined = F3(
	function (m, under, row) {
		var kept = A2(
			$elm$core$List$filter,
			function (r) {
				return !_Utils_eq(r.c, $author$project$Body$draftId);
			},
			m.C);
		var owned = function (r) {
			return (_Utils_cmp(r.d, row.d) < 0) && A2(
				$elm$core$List$member,
				under,
				A2(
					$author$project$Body$ownersOf,
					{C: kept},
					r.c));
		};
		var place = F2(
			function (out, rows) {
				place:
				while (true) {
					if (!rows.b) {
						return out;
					} else {
						var r = rows.a;
						var rest = rows.b;
						if (_Utils_eq(r.c, under)) {
							var kin = A2($author$project$Scan$takeWhileList, owned, rest);
							return _Utils_ap(
								out,
								_Utils_ap(
									A2($elm$core$List$cons, r, kin),
									A2(
										$elm$core$List$cons,
										row,
										A2(
											$elm$core$List$drop,
											$elm$core$List$length(kin),
											rest))));
						} else {
							var $temp$out = _Utils_ap(
								out,
								_List_fromArray(
									[r])),
								$temp$rows = rest;
							out = $temp$out;
							rows = $temp$rows;
							continue place;
						}
					}
				}
			});
		return A2(place, _List_Nil, kept);
	});
var $author$project$Body$drafted = F3(
	function (m, id, caret) {
		return A2(
			$elm$core$Maybe$map,
			function (j) {
				return A3(
					$author$project$Body$joined,
					m,
					j.bb,
					A2($author$project$Body$draftRow, j, j.aR));
			},
			A3($author$project$Body$joinAt, m, id, caret));
	});
var $author$project$Doc$keep = F2(
	function (id, m) {
		return $author$project$Doc$composed(
			A2($author$project$Doc$landOn, id, m));
	});
var $author$project$Body$planningKey = F2(
	function (keywords, key) {
		var up = $elm$core$String$toUpper(key);
		return A2($elm$core$List$member, up, keywords) ? $elm$core$Maybe$Just(up) : $elm$core$Maybe$Nothing;
	});
var $author$project$Body$propIndex = function (id) {
	return $elm$core$String$toInt(
		A2(
			$elm$core$String$dropLeft,
			$elm$core$String$length($author$project$Body$drawerId),
			id));
};
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $elm$core$List$sortBy = _List_sortBy;
var $author$project$Body$readPlanning = F2(
	function (keywords, line) {
		var slice = F2(
			function (_v2, rest) {
				var i = _v2.a;
				var k = _v2.b;
				var stop = A2(
					$elm$core$Maybe$withDefault,
					$elm$core$String$length(line),
					A2(
						$elm$core$Maybe$map,
						$elm$core$Tuple$first,
						$elm$core$List$head(rest)));
				return _Utils_Tuple2(
					k,
					$elm$core$String$trim(
						A3(
							$elm$core$String$slice,
							(i + $elm$core$String$length(k)) + 1,
							stop,
							line)));
			});
		var marks = A2(
			$elm$core$List$sortBy,
			$elm$core$Tuple$first,
			A2(
				$elm$core$List$concatMap,
				function (k) {
					return A2(
						$elm$core$List$map,
						function (i) {
							return _Utils_Tuple2(i, k);
						},
						A2($elm$core$String$indexes, k + ':', line));
				},
				keywords));
		var go = function (seen) {
			if (!seen.b) {
				return _List_Nil;
			} else {
				var mark = seen.a;
				var rest = seen.b;
				return A2(
					$elm$core$List$cons,
					A2(slice, mark, rest),
					go(rest));
			}
		};
		return A2(
			$elm$core$List$filter,
			function (_v1) {
				var v = _v1.b;
				return v !== '';
			},
			go(marks));
	});
var $author$project$Body$readProperty = function (line) {
	var t = $elm$core$String$trim(line);
	if (A2($elm$core$String$startsWith, ':', t)) {
		var _v0 = A2(
			$elm$core$String$indexes,
			':',
			A2($elm$core$String$dropLeft, 1, t));
		if (_v0.b) {
			var close = _v0.a;
			var key = A3($elm$core$String$slice, 1, close + 1, t);
			return ((key === '') || A2($elm$core$String$contains, ' ', key)) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
				_Utils_Tuple2(
					key,
					$elm$core$String$trim(
						A2($elm$core$String$dropLeft, close + 2, t))));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Body$routedWord = F2(
	function (landing, _v0) {
		var key = _v0.a;
		var value = _v0.b;
		return (value === '') ? (key + ' cleared, and the drawer pair with it') : ($author$project$Body$planningText(
			_List_fromArray(
				[
					_Utils_Tuple2(key, value)
				])) + (' — ' + landing));
	});
var $author$project$Body$setPlanning = F2(
	function (_v0, plan) {
		var key = _v0.a;
		var value = _v0.b;
		return (value === '') ? A2(
			$elm$core$List$filter,
			function (_v1) {
				var k = _v1.a;
				return !_Utils_eq(k, key);
			},
			plan) : (A2(
			$elm$core$List$any,
			function (_v2) {
				var k = _v2.a;
				return _Utils_eq(k, key);
			},
			plan) ? A2(
			$elm$core$List$map,
			function (p) {
				return _Utils_eq(p.a, key) ? _Utils_Tuple2(key, value) : p;
			},
			plan) : _Utils_ap(
			plan,
			_List_fromArray(
				[
					_Utils_Tuple2(key, value)
				])));
	});
var $author$project$Doc$spoke = function (_v0) {
	var model = _v0.a;
	var said = _v0.b;
	var m = $author$project$Doc$settled(model);
	return _Utils_Tuple2(
		m,
		$elm$core$Platform$Cmd$batch(
			_List_fromArray(
				[
					$author$project$Doc$docState(
					$author$project$Doc$stateJSON(m)),
					$author$project$Doc$docSaid(
					$elm$json$Json$Encode$string(said))
				])));
};
var $author$project$Doc$editMeta = F3(
	function (m, id, written) {
		if (_Utils_eq(id, $author$project$Body$planId)) {
			return A2(
				$author$project$Doc$keep,
				id,
				$author$project$Doc$remeta(
					_Utils_update(
						m,
						{
							A: A2($author$project$Body$readPlanning, m.an, written)
						})));
		} else {
			var _v0 = _Utils_Tuple2(
				$author$project$Body$readProperty(written),
				$author$project$Body$propIndex(id));
			if ((!_v0.a.$) && (!_v0.b.$)) {
				var _v1 = _v0.a.a;
				var key = _v1.a;
				var value = _v1.b;
				var i = _v0.b.a;
				var pairsWith = function (now) {
					return _Utils_ap(
						A2($elm$core$List$take, i, m.cM),
						_Utils_ap(
							now,
							A2($elm$core$List$drop, i + 1, m.cM)));
				};
				var _v2 = A2($author$project$Body$planningKey, m.an, key);
				if (!_v2.$) {
					var word = _v2.a;
					return A2(
						$author$project$Doc$composedWith,
						$elm$core$Maybe$Just(
							A2(
								$author$project$Body$routedWord,
								'moved to the planning line',
								_Utils_Tuple2(word, value))),
						A2(
							$author$project$Doc$landOn,
							$author$project$Body$planId,
							$author$project$Doc$remeta(
								_Utils_update(
									m,
									{
										A: A2(
											$author$project$Body$setPlanning,
											_Utils_Tuple2(word, value),
											m.A),
										cM: pairsWith(_List_Nil)
									}))));
				} else {
					return A2(
						$author$project$Doc$keep,
						id,
						$author$project$Doc$remeta(
							_Utils_update(
								m,
								{
									cM: pairsWith(
										_List_fromArray(
											[
												_Utils_Tuple2(key, value)
											]))
								})));
				}
			} else {
				return $author$project$Doc$spoke(
					_Utils_Tuple2(m, 'not a `:KEY: value\u0027 line — left as it was'));
			}
		}
	});
var $author$project$Body$kidsOf = F2(
	function (m, id) {
		return $elm$core$List$length(
			A2(
				$elm$core$List$filter,
				function (r) {
					return _Utils_eq(
						r.u,
						$elm$core$Maybe$Just(id));
				},
				m.C));
	});
var $author$project$Doc$planFiner = function (m) {
	var want = 1 + A2($elm$core$Maybe$withDefault, -1, m.B);
	var entries = $author$project$Doc$entriesOf(m);
	var _v0 = A2($author$project$Scan$nth, want, entries);
	if (_v0.$ === 1) {
		return _Utils_Tuple2(m, 'grain-finer (at the finest)');
	} else {
		return _Utils_Tuple2(
			_Utils_update(
				m,
				{
					B: $elm$core$Maybe$Just(want)
				}),
			A3($author$project$Doc$planWord, 'grain-finer', want, entries));
	}
};
var $author$project$Doc$tableEnter = F2(
	function (m, comp) {
		var _v0 = $elm$core$List$head(
			A2($author$project$Doc$tableBodyRows, m, comp));
		if (!_v0.$) {
			var first = _v0.a;
			return _Utils_Tuple2(
				_Utils_update(
					m,
					{
						aH: A2($author$project$Body$placeOf, m, first.c),
						q: $elm$core$Maybe$Nothing
					}),
				'grain-finer (a row)');
		} else {
			return _Utils_Tuple2(m, 'grain-finer (an empty table)');
		}
	});
var $elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$max, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Doc$tableColCount = F2(
	function (m, comp) {
		return A2(
			$elm$core$Maybe$withDefault,
			1,
			$elm$core$List$maximum(
				A2(
					$elm$core$List$map,
					A2(
						$elm$core$Basics$composeL,
						A2($elm$core$Basics$composeL, $elm$core$List$length, $author$project$Body$tableCells),
						function ($) {
							return $.O;
						}),
					A2(
						$elm$core$List$filter,
						A2(
							$elm$core$Basics$composeL,
							A2($elm$core$Basics$composeL, $elm$core$Basics$not, $author$project$Body$isRule),
							function ($) {
								return $.O;
							}),
						A2($author$project$Doc$tableKids, m, comp)))));
	});
var $author$project$Doc$tableFiner = F2(
	function (m, comp) {
		var ncols = A2($author$project$Doc$tableColCount, m, comp);
		var next = function () {
			var _v0 = m.q;
			if (_v0.$ === 1) {
				return 0;
			} else {
				var i = _v0.a;
				return A2($elm$core$Basics$min, i + 1, ncols - 1);
			}
		}();
		return _Utils_Tuple2(
			_Utils_update(
				m,
				{
					q: $elm$core$Maybe$Just(next)
				}),
			A4($author$project$Doc$grainWord, 'grain-finer', 'cell', next + 1, ncols));
	});
var $author$project$Doc$finer = function (m) {
	var _v0 = $author$project$Body$rowAt(m);
	if (_v0.$ === 1) {
		return _Utils_Tuple2(m, '');
	} else {
		var r = _v0.a;
		if (_Utils_eq(r.c, $author$project$Body$planId)) {
			return $author$project$Doc$planFiner(m);
		} else {
			if (_Utils_eq(
				r.am,
				$elm$core$Maybe$Just('table')) && (r.x === 1)) {
				return A2($author$project$Doc$tableEnter, m, r);
			} else {
				var _v1 = A2($author$project$Doc$tableCompOf, m, r);
				if (!_v1.$) {
					var comp = _v1.a;
					return A2($author$project$Doc$tableFiner, m, comp);
				} else {
					var kids = A2($author$project$Body$kidsOf, m, r.c);
					var entered = (!r.j) ? ($elm$core$List$length(m.C) > 1) : (kids > 0);
					return $author$project$Doc$heading(r) ? (entered ? _Utils_Tuple2(
						_Utils_update(
							m,
							{aH: m.aH + 1}),
						'grain-finer (the body)') : _Utils_Tuple2(m, 'grain-finer (an empty entry)')) : ((kids > 0) ? _Utils_Tuple2(
						_Utils_update(
							m,
							{aH: m.aH + 1}),
						A4(
							$author$project$Doc$grainWord,
							'grain-finer',
							A2($elm$core$Maybe$withDefault, 'item', r.am),
							1,
							kids)) : ((r.x === 2) ? _Utils_Tuple2(m, 'grain-finer (at the finest)') : _Utils_Tuple2(m, 'grain-finer (nothing finer here)')));
				}
			}
		}
	}
};
var $author$project$Doc$foldAt = F2(
	function (r, m) {
		var opened = A2($elm$core$Set$member, r.c, m.f);
		return _Utils_Tuple2(
			_Utils_update(
				m,
				{
					aH: A2($author$project$Body$placeOf, m, r.c),
					f: opened ? A2($elm$core$Set$remove, r.c, m.f) : A2($elm$core$Set$insert, r.c, m.f)
				}),
			'org-cycle (' + (((r.j === 2) ? 'subtree' : A2($elm$core$Maybe$withDefault, 'drawer', r.am)) + (opened ? ' open)' : ' folded)')));
	});
var $author$project$Doc$foldTarget = function (m) {
	var here = A2($author$project$Doc$idAtRow, m, m.aH);
	var ups = A2(
		$elm$core$List$filter,
		function (o) {
			return !_Utils_eq(
				A2(
					$elm$core$Maybe$map,
					function ($) {
						return $.j;
					},
					A2($author$project$Body$rowById, m, o)),
				$elm$core$Maybe$Just(2));
		},
		A2($author$project$Body$ownersOf, m, here));
	return $elm$core$List$head(
		A2(
			$elm$core$List$filter,
			$author$project$Doc$foldable(m),
			A2(
				$elm$core$List$filterMap,
				$author$project$Body$rowById(m),
				A2($elm$core$List$cons, here, ups))));
};
var $author$project$Body$insertion = F4(
	function (m, id, caret, text) {
		return A2(
			$elm$core$Maybe$map,
			function (j) {
				return A3(
					$author$project$Body$joined,
					m,
					j.bb,
					A2($author$project$Body$draftRow, j, text));
			},
			A3($author$project$Body$joinAt, m, id, caret));
	});
var $author$project$Body$joinLine = F3(
	function (m, id, caret) {
		return A2(
			$elm$core$Maybe$map,
			function (j) {
				return (j.aa && ((j.I > 1) && (!A2($author$project$Scan$blankAt, j.I - 1, m.aO)))) ? (j.I + 1) : j.I;
			},
			A3($author$project$Body$joinAt, m, id, caret));
	});
var $author$project$Body$joinWord = F3(
	function (m, id, caret) {
		return A2(
			$elm$core$Maybe$map,
			function ($) {
				return $.b3;
			},
			A3($author$project$Body$joinAt, m, id, caret));
	});
var $author$project$Doc$starsAt = function (line) {
	var _v0 = $elm$core$String$uncons(line);
	if ((!_v0.$) && ('*' === _v0.a.a)) {
		var _v1 = _v0.a;
		var rest = _v1.b;
		return 1 + $author$project$Doc$starsAt(rest);
	} else {
		return 0;
	}
};
var $author$project$Doc$headlineAt = function (line) {
	var n = $author$project$Doc$starsAt(line);
	return (n > 0) && A2(
		$elm$core$String$startsWith,
		' ',
		A2($elm$core$String$dropLeft, n, line));
};
var $author$project$Doc$narrowed = F2(
	function (m, text) {
		var deepen = function (line) {
			var n = $author$project$Doc$starsAt(line);
			return ((n > 0) && ((_Utils_cmp(n, m.V) < 1) && $author$project$Doc$headlineAt(line))) ? _Utils_ap(
				A2($elm$core$String$repeat, m.V + 1, '*'),
				A2($elm$core$String$dropLeft, n, line)) : line;
		};
		return A2(
			$elm$core$String$join,
			'\u000A',
			A2(
				$elm$core$List$map,
				deepen,
				A2($elm$core$String$split, '\u000A', text)));
	});
var $author$project$Doc$nextVisible = F2(
	function (by, m) {
		var total = $elm$core$List$length(m.C);
		var gone = A2(
			$elm$core$Set$union,
			$author$project$Doc$hiddenDone(m),
			$author$project$Doc$hiddenIn(m));
		var scan = function (i) {
			scan:
			while (true) {
				if ((i < 0) || (_Utils_cmp(i, total) > -1)) {
					return m.aH;
				} else {
					var _v0 = A2($author$project$Scan$nth, i, m.C);
					if (!_v0.$) {
						var r = _v0.a;
						if (A2($elm$core$Set$member, r.c, gone)) {
							var $temp$i = i + by;
							i = $temp$i;
							continue scan;
						} else {
							return i;
						}
					} else {
						return m.aH;
					}
				}
			}
		};
		return _Utils_update(
			m,
			{
				aH: scan(m.aH + by)
			});
	});
var $elm$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				var $temp$result = A2($elm$core$List$cons, value, result),
					$temp$n = n - 1,
					$temp$value = value;
				result = $temp$result;
				n = $temp$n;
				value = $temp$value;
				continue repeatHelp;
			}
		}
	});
var $elm$core$List$repeat = F2(
	function (n, value) {
		return A3($elm$core$List$repeatHelp, _List_Nil, n, value);
	});
var $elm$core$String$replace = F3(
	function (before, after, string) {
		return A2(
			$elm$core$String$join,
			after,
			A2($elm$core$String$split, before, string));
	});
var $author$project$Doc$replaceCell = F3(
	function (col, value, line) {
		var indent = A2(
			$elm$core$String$left,
			$elm$core$String$length(line) - $elm$core$String$length(
				$elm$core$String$trimLeft(line)),
			line);
		var flat = A3(
			$elm$core$String$replace,
			'\u000A',
			' ',
			$elm$core$String$trim(value));
		var cells = $author$project$Body$tableCells(line);
		var n = A2(
			$elm$core$Basics$max,
			col + 1,
			$elm$core$List$length(cells));
		var padded = _Utils_ap(
			cells,
			A2(
				$elm$core$List$repeat,
				n - $elm$core$List$length(cells),
				''));
		var replaced = A2(
			$elm$core$List$indexedMap,
			F2(
				function (i, c) {
					return _Utils_eq(i, col) ? flat : c;
				}),
			padded);
		return indent + ('| ' + (A2($elm$core$String$join, ' | ', replaced) + ' |'));
	});
var $author$project$Doc$extentOf = F2(
	function (m, r) {
		return _Utils_Tuple2(
			r.d,
			function () {
				var _v0 = A2(
					$elm$core$List$filter,
					function (k) {
						return (k.j === 2) && ((_Utils_cmp(k.d, r.d) > 0) && (_Utils_cmp(k.V, r.V) < 1));
					},
					m.C);
				if (_v0.b) {
					var next = _v0.a;
					return next.d;
				} else {
					return $elm$core$List$length(m.aO);
				}
			}());
	});
var $author$project$Doc$restarred = F2(
	function (by, _v0) {
		var from = _v0.a;
		var to = _v0.b;
		return $elm$core$List$indexedMap(
			F2(
				function (i, line) {
					if ((_Utils_cmp(i, from) < 0) || ((_Utils_cmp(i, to) > -1) || (!$author$project$Doc$headlineAt(line)))) {
						return line;
					} else {
						var n = $author$project$Doc$starsAt(line);
						return _Utils_ap(
							A2(
								$elm$core$String$repeat,
								A2($elm$core$Basics$max, 1, n + by),
								'*'),
							A2($elm$core$String$dropLeft, n, line));
					}
				}));
	});
var $author$project$Doc$shifted = F2(
	function (by, m) {
		var word = (by < 0) ? 'org-promote-subtree' : 'org-demote-subtree';
		var no = function (why) {
			return $author$project$Doc$spoke(
				_Utils_Tuple2(m, word + (' (' + (why + ')'))));
		};
		var _v0 = $author$project$Body$rowAt(m);
		if (_v0.$ === 1) {
			return no('a headline alone');
		} else {
			var r = _v0.a;
			if (!r.j) {
				return no('the entry\u0027s own level is the table\u0027s');
			} else {
				if (r.j !== 2) {
					return no('a headline alone');
				} else {
					if ((by < 0) && (_Utils_cmp(r.V, m.V + 1) < 1)) {
						return no('nothing shallower than a child of the entry');
					} else {
						var _v1 = A2($author$project$Doc$extentOf, m, r);
						var from = _v1.a;
						var to = _v1.b;
						var deepen = function (k) {
							return ((_Utils_cmp(k.d, from) > -1) && ((_Utils_cmp(k.d, to) < 0) && ((k.j === 2) || (k.j === 1)))) ? _Utils_update(
								k,
								{V: k.V + by}) : k;
						};
						var fresh = A3(
							$author$project$Doc$restarred,
							by,
							_Utils_Tuple2(from, to),
							m.aO);
						return A2(
							$author$project$Doc$composedWith,
							$elm$core$Maybe$Just(
								word + (' (level ' + ($elm$core$String$fromInt(r.V + by) + ')'))),
							_Utils_update(
								m,
								{
									au: $elm$core$Array$fromList(fresh),
									aO: fresh,
									aB: $author$project$Doc$offsetsOf(fresh),
									C: A2($elm$core$List$map, deepen, m.C)
								}));
					}
				}
			}
		}
	});
var $author$project$Doc$shelved = F2(
	function (m, r) {
		var _v0 = r.u;
		if (_v0.$ === 1) {
			return true;
		} else {
			var up = _v0.a;
			return A2(
				$elm$core$Maybe$withDefault,
				false,
				A2(
					$elm$core$Maybe$map,
					$author$project$Doc$heading,
					A2($author$project$Body$rowById, m, up)));
		}
	});
var $author$project$Doc$step = F2(
	function (by, m) {
		var _v0 = $author$project$Body$rowAt(m);
		if (_v0.$ === 1) {
			return m;
		} else {
			var cur = _v0.a;
			var n = $elm$core$List$length(m.C);
			var gone = $author$project$Doc$hiddenDone(m);
			var cohort = function () {
				if (cur.j === 2) {
					var hidden = $author$project$Doc$hiddenIn(m);
					return function (r) {
						return ($author$project$Doc$heading(r) || _Utils_eq(r.c, $author$project$Body$tailId)) && (!A2($elm$core$Set$member, r.c, hidden));
					};
				} else {
					if ((by < 0) && A2($author$project$Doc$shelved, m, cur)) {
						var hidden = $author$project$Doc$hiddenIn(m);
						return function (r) {
							return (_Utils_eq(r.u, cur.u) || $author$project$Doc$heading(r)) && (!A2($elm$core$Set$member, r.c, hidden));
						};
					} else {
						return function (r) {
							return _Utils_eq(r.u, cur.u);
						};
					}
				}
			}();
			var fits = function (r) {
				return cohort(r) && (!A2($elm$core$Set$member, r.c, gone));
			};
			var scan = function (i) {
				scan:
				while (true) {
					if ((i < 0) || (_Utils_cmp(i, n) > -1)) {
						return $elm$core$Maybe$Nothing;
					} else {
						if (A2(
							$elm$core$Maybe$withDefault,
							false,
							A2(
								$elm$core$Maybe$map,
								fits,
								A2($author$project$Scan$nth, i, m.C)))) {
							return $elm$core$Maybe$Just(i);
						} else {
							var $temp$i = i + by;
							i = $temp$i;
							continue scan;
						}
					}
				}
			};
			var _v1 = scan(m.aH + by);
			if (_v1.$ === 1) {
				return m;
			} else {
				var i = _v1.a;
				return _Utils_update(
					m,
					{aH: i});
			}
		}
	});
var $author$project$Doc$tableBroader = F2(
	function (m, comp) {
		var _v0 = m.q;
		if (!_v0.$) {
			var i = _v0.a;
			return (i > 0) ? _Utils_Tuple2(
				_Utils_update(
					m,
					{
						q: $elm$core$Maybe$Just(i - 1)
					}),
				A4(
					$author$project$Doc$grainWord,
					'grain-broader',
					'cell',
					i,
					A2($author$project$Doc$tableColCount, m, comp))) : _Utils_Tuple2(
				_Utils_update(
					m,
					{q: $elm$core$Maybe$Nothing}),
				'grain-broader (the row)');
		} else {
			return _Utils_Tuple2(
				_Utils_update(
					m,
					{
						aH: A2($author$project$Body$placeOf, m, comp.c),
						q: $elm$core$Maybe$Nothing
					}),
				'grain-broader (the table)');
		}
	});
var $author$project$Doc$indexOfId = F2(
	function (id, rows) {
		var go = F2(
			function (i, rs) {
				go:
				while (true) {
					if (rs.b) {
						var x = rs.a;
						var xs = rs.b;
						if (_Utils_eq(x.c, id)) {
							return i;
						} else {
							var $temp$i = i + 1,
								$temp$rs = xs;
							i = $temp$i;
							rs = $temp$rs;
							continue go;
						}
					} else {
						return 0;
					}
				}
			});
		return A2(go, 0, rows);
	});
var $author$project$Doc$tableStep = F3(
	function (by, m, comp) {
		var rows = A2($author$project$Doc$tableBodyRows, m, comp);
		var idx = A2(
			$author$project$Doc$indexOfId,
			A2($author$project$Doc$idAtRow, m, m.aH),
			rows);
		var _v0 = A2(
			$author$project$Scan$nth,
			A3(
				$elm$core$Basics$clamp,
				0,
				$elm$core$List$length(rows) - 1,
				idx + by),
			rows);
		if (!_v0.$) {
			var target = _v0.a;
			return _Utils_update(
				m,
				{
					aH: A2($author$project$Body$placeOf, m, target.c)
				});
		} else {
			return m;
		}
	});
var $author$project$Body$undrafted = function (m) {
	return A2(
		$elm$core$List$filter,
		function (r) {
			return !_Utils_eq(r.c, $author$project$Body$draftId);
		},
		m.C);
};
var $author$project$Doc$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 28:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 1:
				return $author$project$Doc$told($author$project$Doc$empty);
			case 0:
				var fresh = msg.a;
				return A2($author$project$Doc$applyFill, fresh, model);
			case 2:
				var id = msg.a;
				var plan = msg.b;
				var base = A2(
					$author$project$Doc$landAt,
					A2($author$project$Body$placeOf, model, id),
					model);
				return $author$project$Doc$told(
					$author$project$Doc$reveal(
						function () {
							if (plan.$ === 1) {
								return base;
							} else {
								var i = plan.a;
								return _Utils_update(
									base,
									{
										B: $elm$core$Maybe$Just(i)
									});
							}
						}()));
			case 3:
				var id = msg.a;
				var col = msg.b;
				var landed = A2(
					$author$project$Doc$landAt,
					A2($author$project$Body$placeOf, model, id),
					model);
				return $author$project$Doc$told(
					$author$project$Doc$reveal(
						_Utils_update(
							landed,
							{q: col})));
			case 4:
				var by = msg.a;
				var _v2 = $author$project$Doc$tableCompAt(model);
				if (!_v2.$) {
					var comp = _v2.a;
					return $author$project$Doc$spoke(
						_Utils_Tuple2(
							A3($author$project$Doc$tableStep, by, model, comp),
							(by > 0) ? 'next-row' : 'previous-row'));
				} else {
					return $author$project$Doc$spoke(
						_Utils_Tuple2(
							A2($author$project$Doc$step, by, model),
							(by > 0) ? 'next-row' : 'previous-row'));
				}
			case 5:
				var opened = function () {
					var _v4 = $author$project$Body$rowAt(model);
					if (!_v4.$) {
						var r = _v4.a;
						return A2($author$project$Doc$foldable, model, r) ? _Utils_update(
							model,
							{
								f: A2($elm$core$Set$remove, r.c, model.f)
							}) : model;
					} else {
						return model;
					}
				}();
				var _v3 = $author$project$Doc$finer(opened);
				var fined = _v3.a;
				var word = _v3.b;
				return (!_Utils_eq(
					$author$project$Doc$tableCompAt(model),
					$elm$core$Maybe$Nothing)) ? $author$project$Doc$spoke(
					_Utils_Tuple2(fined, word)) : ((_Utils_eq(fined.aH, model.aH) && (_Utils_eq(fined.B, model.B) && (_Utils_eq(fined.q, model.q) && _Utils_eq(fined.f, model.f)))) ? $author$project$Doc$spoke(
					_Utils_Tuple2(
						A2($author$project$Doc$nextVisible, 1, model),
						'grain-finer')) : $author$project$Doc$spoke(
					_Utils_Tuple2(fined, word)));
			case 6:
				var _v5 = $author$project$Doc$tableCompAt(model);
				if (!_v5.$) {
					var comp = _v5.a;
					return $author$project$Doc$spoke(
						A2($author$project$Doc$tableBroader, model, comp));
				} else {
					return $author$project$Doc$spoke(
						_Utils_Tuple2(
							A2($author$project$Doc$nextVisible, -1, model),
							'grain-broader'));
				}
			case 7:
				return $author$project$Doc$spoke(
					$author$project$Doc$broader(model));
			case 8:
				var id = msg.a;
				return $author$project$Doc$told(
					_Utils_update(
						model,
						{
							P: _Utils_ap(
								A2(
									$elm$core$List$filter,
									$elm$core$Basics$neq(id),
									model.P),
								_List_fromArray(
									[id]))
						}));
			case 9:
				var id = msg.a;
				return $author$project$Doc$told(
					_Utils_update(
						model,
						{
							P: A2(
								$elm$core$List$filter,
								$elm$core$Basics$neq(id),
								model.P)
						}));
			case 10:
				return $author$project$Doc$told(
					_Utils_update(
						model,
						{P: _List_Nil}));
			case 23:
				var scoped = function () {
					var _v8 = $author$project$Body$rowAt(model);
					if (!_v8.$) {
						var r = _v8.a;
						return A2($author$project$Doc$isListRoot, model, r.c) ? $elm$core$Maybe$Just(r.c) : A2($author$project$Doc$listRootOf, model, r.c);
					} else {
						return $elm$core$Maybe$Nothing;
					}
				}();
				var next = function () {
					if (!scoped.$) {
						var root = scoped.a;
						return A2($elm$core$Set$member, root, model.y) ? A2($elm$core$Set$remove, root, model.y) : A2($elm$core$Set$insert, root, model.y);
					} else {
						return $elm$core$Set$isEmpty(model.y) ? $elm$core$Set$fromList(
							$author$project$Doc$listRoots(model)) : $elm$core$Set$empty;
					}
				}();
				var word = function () {
					if (!scoped.$) {
						var root = scoped.a;
						return A2($elm$core$Set$member, root, next) ? 'hide-done (this list)' : 'hide-done (this list off)';
					} else {
						return $elm$core$Set$isEmpty(next) ? 'hide-done (all lists off)' : 'hide-done (all lists)';
					}
				}();
				return $author$project$Doc$spoke(
					_Utils_Tuple2(
						$author$project$Doc$reveal(
							$author$project$Doc$snapVisible(
								_Utils_update(
									model,
									{y: next}))),
						word));
			case 21:
				var _v9 = $author$project$Doc$foldTarget(model);
				if (_v9.$ === 1) {
					return $author$project$Doc$spoke(
						_Utils_Tuple2(model, 'nothing folds here'));
				} else {
					var r = _v9.a;
					return $author$project$Doc$spoke(
						A2($author$project$Doc$foldAt, r, model));
				}
			case 22:
				var id = msg.a;
				var _v10 = A2($author$project$Body$rowById, model, id);
				if (_v10.$ === 1) {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				} else {
					var r = _v10.a;
					return $author$project$Doc$spoke(
						A2($author$project$Doc$foldAt, r, model));
				}
			case 24:
				var by = msg.a;
				return A2($author$project$Doc$shifted, by, model);
			case 26:
				var props = msg.a;
				var plan = msg.b;
				return $author$project$Doc$told(
					$author$project$Doc$remeta(
						_Utils_update(
							model,
							{A: plan, cM: props})));
			case 27:
				var cells = msg.a;
				return $author$project$Doc$told(
					_Utils_update(
						model,
						{
							C: A2(
								$elm$core$List$map,
								function (r) {
									return (!r.j) ? _Utils_update(
										r,
										{ac: cells}) : r;
								},
								model.C)
						}));
			case 25:
				var key = msg.a;
				var value = msg.b;
				var _v11 = A2($author$project$Body$planningKey, model.an, key);
				if (!_v11.$) {
					var word = _v11.a;
					return A2(
						$author$project$Doc$composedWith,
						$elm$core$Maybe$Just(
							A2(
								$author$project$Body$routedWord,
								'the planning line',
								_Utils_Tuple2(word, value))),
						A2(
							$author$project$Doc$landOn,
							$author$project$Body$planId,
							$author$project$Doc$remeta(
								_Utils_update(
									model,
									{
										U: false,
										A: A2(
											$author$project$Body$setPlanning,
											_Utils_Tuple2(word, value),
											model.A)
									}))));
				} else {
					if ((key === '') || ((value === '') || (A2($elm$core$String$contains, ' ', key) || A2($elm$core$String$contains, ':', key)))) {
						return $author$project$Doc$spoke(
							_Utils_Tuple2(
								A2(
									$author$project$Doc$landOn,
									$author$project$Body$drawerId,
									$author$project$Doc$remeta(
										_Utils_update(
											model,
											{U: false}))),
								'a property needs a key and a value'));
					} else {
						var fresh = $author$project$Doc$remeta(
							_Utils_update(
								model,
								{
									U: false,
									cM: _Utils_ap(
										model.cM,
										_List_fromArray(
											[
												_Utils_Tuple2(key, value)
											]))
								}));
						return A2(
							$author$project$Doc$composedWith,
							$elm$core$Maybe$Just(
								$author$project$Body$propertyText(
									_Utils_Tuple2(key, value))),
							_Utils_update(
								fresh,
								{
									aH: A2(
										$author$project$Body$placeOf,
										fresh,
										$author$project$Body$propId(
											$elm$core$List$length(model.cM))),
									f: A2($elm$core$Set$remove, $author$project$Body$drawerId, fresh.f)
								}));
					}
				}
			case 11:
				var ids = msg.a;
				return A2($author$project$Doc$applyDelete, ids, model);
			case 12:
				var id = msg.a;
				var written = msg.b;
				if (_Utils_eq(
					A2(
						$elm$core$Maybe$map,
						function ($) {
							return $.j;
						},
						A2($author$project$Body$rowById, model, id)),
					$elm$core$Maybe$Just(3))) {
					return A3($author$project$Doc$editMeta, model, id, written);
				} else {
					var write = function (r) {
						return _Utils_eq(r.c, id) ? _Utils_update(
							r,
							{
								O: A2($author$project$Doc$narrowed, model, written)
							}) : r;
					};
					return $author$project$Doc$composed(
						_Utils_update(
							model,
							{
								C: A2($elm$core$List$map, write, model.C)
							}));
				}
			case 13:
				var id = msg.a;
				var col = msg.b;
				var written = msg.c;
				var write = function (r) {
					return _Utils_eq(r.c, id) ? _Utils_update(
						r,
						{
							O: A3($author$project$Doc$replaceCell, col, written, r.O)
						}) : r;
				};
				return $author$project$Doc$composed(
					_Utils_update(
						model,
						{
							C: A2($elm$core$List$map, write, model.C)
						}));
			case 14:
				var id = msg.a;
				var caret = msg.b;
				var _v12 = _Utils_Tuple2(
					A3($author$project$Body$drafted, model, id, caret),
					A3($author$project$Body$joinWord, model, id, caret));
				if ((!_v12.a.$) && (!_v12.b.$)) {
					var rows = _v12.a.a;
					var word = _v12.b.a;
					return $author$project$Doc$spoke(
						_Utils_Tuple2(
							_Utils_update(
								model,
								{
									aH: A2(
										$author$project$Body$placeOf,
										_Utils_update(
											model,
											{C: rows}),
										$author$project$Body$draftId),
									C: rows
								}),
							word));
				} else {
					return _Utils_Tuple2(
						model,
						$author$project$Doc$docSaid(
							$elm$json$Json$Encode$string('nothing here takes a paragraph')));
				}
			case 15:
				var id = msg.a;
				var caret = msg.b;
				var written = msg.c;
				var _v13 = _Utils_Tuple2(
					A4(
						$author$project$Body$insertion,
						model,
						id,
						caret,
						A2($author$project$Doc$narrowed, model, written)),
					A3($author$project$Body$joinLine, model, id, caret));
				if (!_v13.a.$) {
					var rows = _v13.a.a;
					var line = _v13.b;
					return $author$project$Doc$composed(
						_Utils_update(
							model,
							{Q: line, C: rows}));
				} else {
					var _v14 = _v13.a;
					return _Utils_Tuple2(
						model,
						$author$project$Doc$docSaid(
							$elm$json$Json$Encode$string('nothing here takes a paragraph')));
				}
			case 16:
				var id = msg.a;
				var rows = $author$project$Body$undrafted(model);
				return $author$project$Doc$told(
					_Utils_update(
						model,
						{
							aH: A2(
								$author$project$Body$placeOf,
								_Utils_update(
									model,
									{C: rows}),
								id),
							C: rows
						}));
			case 17:
				var fresh = $author$project$Doc$remeta(
					_Utils_update(
						model,
						{U: true}));
				return $author$project$Doc$told(
					_Utils_update(
						fresh,
						{
							aH: A2($author$project$Body$placeOf, fresh, $author$project$Body$draftPairId),
							f: A2($elm$core$Set$remove, $author$project$Body$drawerId, fresh.f)
						}));
			case 18:
				var id = msg.a;
				return A2(
					$author$project$Doc$backTo,
					id,
					_Utils_update(
						model,
						{U: false}));
			case 19:
				var key = msg.a;
				var fresh = $author$project$Doc$remeta(
					_Utils_update(
						model,
						{
							aw: $elm$core$Maybe$Just(key)
						}));
				return $author$project$Doc$told(
					A2(
						$author$project$Doc$landAt,
						A2($author$project$Body$placeOf, fresh, $author$project$Body$planId),
						fresh));
			default:
				var id = msg.a;
				return A2(
					$author$project$Doc$backTo,
					id,
					_Utils_update(
						model,
						{aw: $elm$core$Maybe$Nothing}));
		}
	});
var $author$project$Doc$foldSign = F2(
	function (m, r) {
		return A2(
			$elm$html$Html$span,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('fold')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(
					A2($elm$core$Set$member, r.c, m.f) ? '+' : '\u2212')
				]));
	});
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$node = $elm$virtual_dom$VirtualDom$node;
var $elm$virtual_dom$VirtualDom$property = F2(
	function (key, value) {
		return A2(
			_VirtualDom_property,
			_VirtualDom_noInnerHtmlOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlJson(value));
	});
var $elm$html$Html$Attributes$property = $elm$virtual_dom$VirtualDom$property;
var $author$project$Doc$tableHeadRow = F2(
	function (m, comp) {
		return A2($author$project$Doc$tableHasHeader, m, comp) ? $elm$core$List$head(
			A2($author$project$Doc$tableKids, m, comp)) : $elm$core$Maybe$Nothing;
	});
var $author$project$Doc$tableView = F2(
	function (m, r) {
		var ncols = A2($author$project$Doc$tableColCount, m, r);
		var rowVal = function (k) {
			var cs = $author$project$Body$tableCells(k.O);
			var cell = function (i) {
				return _Utils_Tuple2(
					'c' + $elm$core$String$fromInt(i),
					$elm$json$Json$Encode$string(
						$elm$core$String$trim(
							A2(
								$elm$core$Maybe$withDefault,
								'',
								A2($author$project$Scan$nth, i, cs)))));
			};
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'id',
						$elm$json$Json$Encode$string(k.c)),
						_Utils_Tuple2(
						'cells',
						$elm$json$Json$Encode$object(
							A2(
								$elm$core$List$map,
								cell,
								A2($elm$core$List$range, 0, ncols - 1))))
					]));
		};
		var headCells = A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			A2(
				$elm$core$Maybe$map,
				A2(
					$elm$core$Basics$composeL,
					$author$project$Body$tableCells,
					function ($) {
						return $.O;
					}),
				A2($author$project$Doc$tableHeadRow, m, r)));
		var colHeader = function (i) {
			var _v0 = $elm$core$String$trim(
				A2(
					$elm$core$Maybe$withDefault,
					'',
					A2($author$project$Scan$nth, i, headCells)));
			if (_v0 === '') {
				return ' ';
			} else {
				var h = _v0;
				return h;
			}
		};
		var column = function (i) {
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'key',
						$elm$json$Json$Encode$string(
							'c' + $elm$core$String$fromInt(i))),
						_Utils_Tuple2(
						'header',
						$elm$json$Json$Encode$string(
							colHeader(i))),
						_Utils_Tuple2(
						'type',
						$elm$json$Json$Encode$string('text')),
						_Utils_Tuple2(
						'sortable',
						$elm$json$Json$Encode$bool(true))
					]));
		};
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'columns',
					A2(
						$elm$json$Json$Encode$list,
						column,
						A2($elm$core$List$range, 0, ncols - 1))),
					_Utils_Tuple2(
					'rows',
					A2(
						$elm$json$Json$Encode$list,
						rowVal,
						A2($author$project$Doc$tableBodyRows, m, r))),
					_Utils_Tuple2(
					'hasHeader',
					$elm$json$Json$Encode$bool(
						A2($author$project$Doc$tableHasHeader, m, r)))
				]));
	});
var $author$project$Doc$glanceTable = F2(
	function (m, r) {
		return A3(
			$elm$html$Html$node,
			'glance-table',
			_List_fromArray(
				[
					A2(
					$elm$html$Html$Attributes$property,
					'view',
					A2($author$project$Doc$tableView, m, r))
				]),
			_List_Nil);
	});
var $author$project$Doc$headOf = function (m) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (r, _v0) {
				var stack = _v0.a;
				var acc = _v0.b;
				var _v1 = r.j;
				switch (_v1) {
					case 0:
						return _Utils_Tuple2(
							_List_fromArray(
								[
									_Utils_Tuple2(m.V, 'H')
								]),
							acc);
					case 2:
						var kept = A2(
							$elm$core$List$filter,
							function (_v2) {
								var l = _v2.a;
								return _Utils_cmp(l, r.V) < 0;
							},
							stack);
						var up = A2(
							$elm$core$Maybe$withDefault,
							'H',
							A2(
								$elm$core$Maybe$map,
								$elm$core$Tuple$second,
								$elm$core$List$head(kept)));
						return _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(r.V, r.c),
								kept),
							A3($elm$core$Dict$insert, r.c, up, acc));
					default:
						return _Utils_Tuple2(
							stack,
							A3(
								$elm$core$Dict$insert,
								r.c,
								A2(
									$elm$core$Maybe$withDefault,
									'H',
									A2(
										$elm$core$Maybe$map,
										$elm$core$Tuple$second,
										$elm$core$List$head(stack))),
								acc));
				}
			}),
		_Utils_Tuple2(
			_List_fromArray(
				[
					_Utils_Tuple2(m.V, 'H')
				]),
			$elm$core$Dict$empty),
		m.C).b;
};
var $author$project$Doc$inList = function (m) {
	return !_Utils_eq(
		A2(
			$elm$core$Maybe$andThen,
			function ($) {
				return $.u;
			},
			$author$project$Body$rowAt(m)),
		$elm$core$Maybe$Nothing);
};
var $author$project$Doc$stars = F2(
	function (m, level) {
		return A2(
			$elm$core$String$repeat,
			A2($elm$core$Basics$max, 0, 2 * (level - m.V)),
			' ') + '* ';
	});
var $author$project$Doc$rail = F2(
	function (m, level) {
		return '--rail:calc(var(--g-doc-pad) + ' + ($elm$core$String$fromFloat(
			$elm$core$String$length(
				A2($author$project$Doc$stars, m, level)) - 1.5) + 'ch)');
	});
var $author$project$Doc$inset = F2(
	function (m, r) {
		return (_Utils_cmp(r.V, m.V) > 0) ? _List_fromArray(
			[
				A2(
				$elm$html$Html$Attributes$attribute,
				'style',
				'--g-doc-indent:' + ($elm$core$String$fromInt(
					$elm$core$String$length(
						A2($author$project$Doc$stars, m, r.V))) + (';' + A2($author$project$Doc$rail, m, r.V))))
			]) : _List_Nil;
	});
var $author$project$Doc$litOf = function (m) {
	return {
		aL: $author$project$Doc$hiddenDone(m),
		a4: $elm$core$Set$fromList(
			A2(
				$elm$core$List$filterMap,
				function ($) {
					return $.u;
				},
				m.C)),
		a7: A2(
			$elm$core$Maybe$andThen,
			function ($) {
				return $.u;
			},
			$author$project$Body$rowAt(m)),
		a_: A2(
			$author$project$Body$ownersOf,
			m,
			A2($author$project$Doc$idAtRow, m, m.aH))
	};
};
var $author$project$Doc$compactedRun = F3(
	function (m, done, r) {
		return A2($author$project$Doc$isListRoot, m, r.c) && (A2($elm$core$Set$member, r.c, m.y) && ((!A2($elm$core$Set$member, r.c, done)) && A2(
			$elm$core$List$any,
			function (h) {
				return _Utils_eq(
					A2($author$project$Doc$listRootOf, m, h),
					$elm$core$Maybe$Just(r.c));
			},
			$elm$core$Set$toList(done))));
	});
var $author$project$Doc$indexOfIn = F2(
	function (id, ups) {
		return A2(
			$author$project$Scan$indexWhere,
			$elm$core$Basics$eq(id),
			ups);
	});
var $author$project$Doc$markOf = F4(
	function (lit, m, i, r) {
		return _Utils_eq(i, m.aH) ? '' : (A2($elm$core$List$member, r.c, lit.a_) ? (' up up-' + $elm$core$String$fromInt(
			A2(
				$elm$core$Basics$min,
				2,
				A2(
					$elm$core$Maybe$withDefault,
					0,
					A2($author$project$Doc$indexOfIn, r.c, lit.a_))))) : (((!_Utils_eq(r.u, $elm$core$Maybe$Nothing)) && _Utils_eq(r.u, lit.a7)) ? ' sib' : ''));
	});
var $author$project$Doc$rowClass = F5(
	function (lit, m, i, r, top) {
		return _Utils_ap(
			(_Utils_eq(r.c, $author$project$Body$draftId) || _Utils_eq(r.c, $author$project$Body$draftPairId)) ? 'de d-draft d-' : 'de d-',
			_Utils_ap(
				function () {
					var _v0 = r.x;
					switch (_v0) {
						case 2:
							return (r.j === 3) ? 'meta' : 'item';
						case 1:
							return 'comp d-' + A2($elm$core$Maybe$withDefault, '', r.am);
						default:
							return $author$project$Body$kindWord(r.j);
					}
				}(),
				_Utils_ap(
					_Utils_eq(i, m.aH) ? ' dat' : '',
					_Utils_ap(
						A2($elm$core$List$member, r.c, m.P) ? ' dfl' : '',
						_Utils_ap(
							top ? ' lvl-top' : '',
							_Utils_ap(
								_Utils_eq(r.c, $author$project$Body$tailId) ? ' d-tail' : '',
								_Utils_ap(
									A2($elm$core$Set$member, r.c, lit.aL) ? ' d-hidden' : '',
									_Utils_ap(
										A3($author$project$Doc$compactedRun, m, lit.aL, r) ? ' d-compacted' : '',
										_Utils_ap(
											A2($author$project$Doc$drawer, m, r) ? (A2($elm$core$Set$member, r.c, lit.a4) ? ' d-drawer' : ' d-drawer bare') : '',
											A4($author$project$Doc$markOf, lit, m, i, r))))))))));
	});
var $author$project$Doc$rowEl = F7(
	function (lit, m, i, r, top, extra, inner) {
		return A2(
			$elm$html$Html$div,
			A2(
				$elm$core$List$cons,
				$elm$html$Html$Attributes$class(
					A5($author$project$Doc$rowClass, lit, m, i, r, top)),
				A2(
					$elm$core$List$cons,
					A2($elm$html$Html$Attributes$attribute, 'data-id', r.c),
					extra)),
			inner);
	});
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $author$project$Doc$spineRanks = F2(
	function (m, heads) {
		var start = A2(
			$elm$core$Maybe$andThen,
			function (r) {
				return $author$project$Doc$heading(r) ? $elm$core$Maybe$Just(r.c) : A2($elm$core$Dict$get, r.c, heads);
			},
			$author$project$Body$rowAt(m));
		var chain = F2(
			function (id, acc) {
				chain:
				while (true) {
					if (id === 'H') {
						return _Utils_ap(
							acc,
							_List_fromArray(
								['H']));
					} else {
						var $temp$id = A2(
							$elm$core$Maybe$withDefault,
							'H',
							A2($elm$core$Dict$get, id, heads)),
							$temp$acc = _Utils_ap(
							acc,
							_List_fromArray(
								[id]));
						id = $temp$id;
						acc = $temp$acc;
						continue chain;
					}
				}
			});
		if (start.$ === 1) {
			return $elm$core$Dict$empty;
		} else {
			var s = start.a;
			return $elm$core$Dict$fromList(
				A2(
					$elm$core$List$indexedMap,
					F2(
						function (k, h) {
							return _Utils_Tuple2(h, k);
						}),
					A2(chain, s, _List_Nil)));
		}
	});
var $author$project$Doc$token = function (word) {
	return _List_fromArray(
		[
			A2(
			$elm$html$Html$span,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('dpunc dlead')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(':')
				])),
			$elm$html$Html$text(word),
			A2(
			$elm$html$Html$span,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('dpunc')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(':')
				]))
		]);
};
var $author$project$Doc$drawText = F3(
	function (m, body, base) {
		var n = $elm$core$String$length(body);
		var inside = function (l) {
			return (_Utils_cmp(l.d, base) > -1) && (_Utils_cmp(l.g, base + n) < 1);
		};
		var go = F3(
			function (links, seen, out) {
				go:
				while (true) {
					if (!links.b) {
						return (!seen) ? _List_fromArray(
							[
								$elm$html$Html$text(body)
							]) : ((_Utils_cmp(seen, n) < 0) ? _Utils_ap(
							out,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$span,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('dt')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(
											A2($elm$core$String$dropLeft, seen, body))
										]))
								])) : out);
					} else {
						var l = links.a;
						var rest = links.b;
						var b = l.g - base;
						var a = l.d - base;
						if (_Utils_cmp(a, seen) < 0) {
							var $temp$links = rest,
								$temp$seen = seen,
								$temp$out = out;
							links = $temp$links;
							seen = $temp$seen;
							out = $temp$out;
							continue go;
						} else {
							var $temp$links = rest,
								$temp$seen = b,
								$temp$out = _Utils_ap(
								out,
								_Utils_ap(
									(_Utils_cmp(a, seen) > 0) ? _List_fromArray(
										[
											A2(
											$elm$html$Html$span,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$class('dt')
												]),
											_List_fromArray(
												[
													$elm$html$Html$text(
													A3($elm$core$String$slice, seen, a, body))
												]))
										]) : _List_Nil,
									_List_fromArray(
										[
											A2(
											$elm$html$Html$span,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$class('dl')
												]),
											_List_fromArray(
												[
													$elm$html$Html$text(l.bm)
												]))
										])));
							links = $temp$links;
							seen = $temp$seen;
							out = $temp$out;
							continue go;
						}
					}
				}
			});
		return A3(
			go,
			A2($elm$core$List$filter, inside, m.aP),
			0,
			_List_Nil);
	});
var $author$project$Body$shown = function (r) {
	return A2(
		$elm$core$List$filter,
		function (c) {
			return c.bc !== '';
		},
		r.ac);
};
var $author$project$Doc$drawnCells = function (r) {
	return (!r.j) ? A2(
		$elm$core$List$filter,
		function (c) {
			return (c.bc !== '') || (c.bA === 'title');
		},
		r.ac) : $author$project$Body$shown(r);
};
var $author$project$Doc$viewCells = F2(
	function (m, r) {
		return A2(
			$elm$core$List$cons,
			A2(
				$elm$html$Html$span,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('ds')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(
						A2(
							$author$project$Doc$stars,
							m,
							(r.j === 2) ? r.V : m.V))
					])),
			A2(
				$elm$core$List$indexedMap,
				F2(
					function (j, c) {
						return A2(
							$elm$html$Html$span,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('dc dc-' + c.bA),
									A2($elm$html$Html$Attributes$style, 'color', c.cg)
								]),
							function () {
								var _v0 = _Utils_Tuple3(c.bA, r.j, m.aY);
								_v0$2:
								while (true) {
									if (_v0.a === 'title') {
										switch (_v0.b) {
											case 0:
												if (!_v0.c.$) {
													var _v1 = _v0.b;
													var t = _v0.c.a;
													return A3($author$project$Doc$drawText, m, c.bc, t);
												} else {
													break _v0$2;
												}
											case 2:
												var _v2 = _v0.b;
												return A2(
													$elm$core$List$cons,
													$elm$html$Html$text(c.bc),
													A2($elm$core$Set$member, r.c, m.f) ? _List_fromArray(
														[
															A2(
															$elm$html$Html$span,
															_List_fromArray(
																[
																	$elm$html$Html$Attributes$class('dg')
																]),
															_List_fromArray(
																[
																	$elm$html$Html$text(' …')
																]))
														]) : _List_Nil);
											default:
												break _v0$2;
										}
									} else {
										break _v0$2;
									}
								}
								return _List_fromArray(
									[
										$elm$html$Html$text(c.bc)
									]);
							}());
					}),
				$author$project$Doc$drawnCells(r)));
	});
var $author$project$Doc$rung = function (depth) {
	return A2(
		$elm$html$Html$Attributes$attribute,
		'style',
		'--rail:calc(' + ($elm$core$String$fromInt(2 * depth) + 'ch - 1.5ch)'));
};
var $author$project$Doc$BoxEmpty = 0;
var $author$project$Doc$boxChar = F2(
	function (m, r) {
		var op = A2($author$project$Doc$openerAt, m, r);
		var opened = $author$project$Doc$openedLen(op);
		var line = A2($author$project$Doc$lineOf, m, r);
		var k = A2($author$project$Doc$markerOf, op, line);
		var box = A3($elm$core$String$slice, opened, k, line);
		return ((_Utils_cmp(k, opened) < 1) || $elm$core$String$isEmpty(
			$elm$core$String$trim(box))) ? $elm$core$Maybe$Nothing : ((A2($elm$core$String$contains, 'X', box) || A2($elm$core$String$contains, 'x', box)) ? $elm$core$Maybe$Just('X') : (A2($elm$core$String$contains, '-', box) ? $elm$core$Maybe$Just('-') : $elm$core$Maybe$Just(' ')));
	});
var $author$project$Doc$BoxFull = 2;
var $author$project$Doc$BoxPart = 1;
var $author$project$Doc$leafFace = function (c) {
	switch (c) {
		case 'X':
			return 2;
		case '-':
			return 1;
		default:
			return 0;
	}
};
var $author$project$Doc$rollUp = function (faces) {
	return A2(
		$elm$core$List$all,
		$elm$core$Basics$eq(2),
		faces) ? 2 : (A2(
		$elm$core$List$all,
		$elm$core$Basics$eq(0),
		faces) ? 0 : 1);
};
var $author$project$Doc$boxFace = F2(
	function (m, r) {
		var _v0 = A2($author$project$Doc$boxChar, m, r);
		if (_v0.$ === 1) {
			return $elm$core$Maybe$Nothing;
		} else {
			var c = _v0.a;
			var _v1 = A2($author$project$Doc$checkKids, m, r.c);
			if (!_v1.b) {
				return $elm$core$Maybe$Just(
					$author$project$Doc$leafFace(c));
			} else {
				var kids = _v1;
				return $elm$core$Maybe$Just(
					$author$project$Doc$rollUp(
						A2(
							$elm$core$List$filterMap,
							$author$project$Doc$boxFace(m),
							kids)));
			}
		}
	});
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $author$project$Doc$setBox = F2(
	function (c, box) {
		var _v0 = A2($elm$core$String$indexes, '[', box);
		if (_v0.b) {
			var i = _v0.a;
			return _Utils_ap(
				A2($elm$core$String$left, i + 1, box),
				_Utils_ap(
					$elm$core$String$fromChar(c),
					A2($elm$core$String$dropLeft, i + 2, box)));
		} else {
			return box;
		}
	});
var $author$project$Doc$boxSpan = F3(
	function (m, r, box) {
		var _v0 = A2($author$project$Doc$checkKids, m, r.c);
		if (!_v0.b) {
			return A2(
				$elm$html$Html$span,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class(
						'dbx' + ((A2($elm$core$String$contains, 'X', box) || A2($elm$core$String$contains, 'x', box)) ? ' on' : ''))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(box)
					]));
		} else {
			var _v1 = function () {
				var _v2 = A2(
					$elm$core$Maybe$withDefault,
					0,
					A2($author$project$Doc$boxFace, m, r));
				switch (_v2) {
					case 2:
						return _Utils_Tuple2('X', ' on');
					case 1:
						return _Utils_Tuple2('-', ' part');
					default:
						return _Utils_Tuple2(' ', '');
				}
			}();
			var ch = _v1.a;
			var cls = _v1.b;
			return A2(
				$elm$html$Html$span,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('dbx derived' + cls)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(
						A2($author$project$Doc$setBox, ch, box))
					]));
		}
	});
var $elm$core$String$dropRight = F2(
	function (n, string) {
		return (n < 1) ? string : A3($elm$core$String$slice, 0, -n, string);
	});
var $author$project$Doc$cookieKind = function (inside) {
	var digits = function (str) {
		return A2(
			$elm$core$String$all,
			function (c) {
				return (c >= '0') && (c <= '9');
			},
			str);
	};
	if (A2($elm$core$String$endsWith, '%', inside) && digits(
		A2($elm$core$String$dropRight, 1, inside))) {
		return $elm$core$Maybe$Just(true);
	} else {
		var _v0 = A2($elm$core$String$split, '/', inside);
		if ((_v0.b && _v0.b.b) && (!_v0.b.b.b)) {
			var a = _v0.a;
			var _v1 = _v0.b;
			var b = _v1.a;
			return (digits(a) && digits(b)) ? $elm$core$Maybe$Just(false) : $elm$core$Maybe$Nothing;
		} else {
			return $elm$core$Maybe$Nothing;
		}
	}
};
var $author$project$Doc$indexFrom = F3(
	function (c, s, from) {
		return $elm$core$List$head(
			A2(
				$elm$core$List$filter,
				function (i) {
					return _Utils_cmp(i, from) > -1;
				},
				A2(
					$elm$core$String$indexes,
					$elm$core$String$fromChar(c),
					s)));
	});
var $author$project$Doc$findCookie = F2(
	function (s, from) {
		findCookie:
		while (true) {
			var _v0 = A3($author$project$Doc$indexFrom, '[', s, from);
			if (_v0.$ === 1) {
				return $elm$core$Maybe$Nothing;
			} else {
				var lb = _v0.a;
				var _v1 = A3($author$project$Doc$indexFrom, ']', s, lb + 1);
				if (_v1.$ === 1) {
					return $elm$core$Maybe$Nothing;
				} else {
					var rb = _v1.a;
					var _v2 = $author$project$Doc$cookieKind(
						A3($elm$core$String$slice, lb + 1, rb, s));
					if (!_v2.$) {
						var percent = _v2.a;
						return $elm$core$Maybe$Just(
							_Utils_Tuple3(lb, rb + 1, percent));
					} else {
						var $temp$s = s,
							$temp$from = lb + 1;
						s = $temp$s;
						from = $temp$from;
						continue findCookie;
					}
				}
			}
		}
	});
var $author$project$Doc$cookieIn = function (s) {
	return A2($author$project$Doc$findCookie, s, 0);
};
var $author$project$Doc$cookieSpan = F3(
	function (m, r, percent) {
		var kids = A2($author$project$Doc$checkKids, m, r.c);
		var total = $elm$core$List$length(kids);
		var done = $elm$core$List$length(
			A2(
				$elm$core$List$filter,
				function (k) {
					return _Utils_eq(
						A2($author$project$Doc$boxFace, m, k),
						$elm$core$Maybe$Just(2));
				},
				kids));
		var shown = percent ? ('[' + ($elm$core$String$fromInt(
			(!total) ? 0 : (((100 * done) / total) | 0)) + '%]')) : ('[' + ($elm$core$String$fromInt(done) + ('/' + ($elm$core$String$fromInt(total) + ']'))));
		return A2(
			$elm$html$Html$span,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class(
					'dcookie' + (((total > 0) && _Utils_eq(done, total)) ? ' done' : ''))
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(shown)
				]));
	});
var $author$project$Doc$drawWithCookie = F4(
	function (m, r, rest, base) {
		var _v0 = $author$project$Doc$cookieIn(rest);
		if (!_v0.$) {
			var _v1 = _v0.a;
			var from = _v1.a;
			var to = _v1.b;
			var percent = _v1.c;
			return _Utils_ap(
				A3(
					$author$project$Doc$drawText,
					m,
					A2($elm$core$String$left, from, rest),
					base),
				_Utils_ap(
					_List_fromArray(
						[
							A3($author$project$Doc$cookieSpan, m, r, percent)
						]),
					A4(
						$author$project$Doc$drawWithCookie,
						m,
						r,
						A2($elm$core$String$dropLeft, to, rest),
						base + to)));
		} else {
			return A3($author$project$Doc$drawText, m, rest, base);
		}
	});
var $author$project$Doc$keyOf = function (r) {
	return ((r.j === 3) && (r.x === 2)) ? A2(
		$elm$core$Maybe$map,
		$elm$core$Tuple$first,
		$author$project$Body$readProperty(r.O)) : $elm$core$Maybe$Nothing;
};
var $author$project$Doc$stepsAside = function (tok) {
	return A2(
		$elm$core$List$member,
		tok,
		_List_fromArray(
			['-', '+', '*']));
};
var $elm$core$String$trimRight = _String_trimRight;
var $author$project$Doc$markParts = F2(
	function (op, head) {
		if (!op.$) {
			var o = op.a;
			var tok = A3(
				$elm$core$String$slice,
				o.by,
				o.by + $elm$core$String$length(
					$elm$core$String$trimRight(o.aI)),
				head);
			return $author$project$Doc$stepsAside(tok) ? _List_fromArray(
				[
					$elm$html$Html$text(
					A2($elm$core$String$left, o.by, head)),
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('dbul')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(tok)
						])),
					$elm$html$Html$text(
					A2(
						$elm$core$String$dropLeft,
						o.by + $elm$core$String$length(tok),
						head))
				]) : _List_fromArray(
				[
					$elm$html$Html$text(head)
				]);
		} else {
			return _List_fromArray(
				[
					$elm$html$Html$text(head)
				]);
		}
	});
var $author$project$Doc$unsetSlot = _List_fromArray(
	[
		A2(
		$elm$html$Html$span,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('dpunset')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text('—')
			]))
	]);
var $author$project$Doc$viewPlanning = function (m) {
	var picked = $author$project$Doc$planPick(m);
	return $elm$core$List$concat(
		A2(
			$elm$core$List$indexedMap,
			F2(
				function (i, _v0) {
					var key = _v0.a;
					var value = _v0.b;
					return _List_fromArray(
						[
							$elm$html$Html$text(
							(!i) ? '' : ' '),
							A2(
							$elm$html$Html$span,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('dk')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(key),
									A2(
									$elm$html$Html$span,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('dpunc')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(':')
										]))
								])),
							$elm$html$Html$text(' '),
							A2(
							$elm$html$Html$span,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class(
									_Utils_eq(
										picked,
										$elm$core$Maybe$Just(i)) ? 'dpv dat' : 'dpv'),
									A2($elm$html$Html$Attributes$attribute, 'data-key', key)
								]),
							(value === '') ? $author$project$Doc$unsetSlot : _List_fromArray(
								[
									$elm$html$Html$text(value)
								]))
						]);
				}),
			$author$project$Doc$entriesOf(m)));
};
var $author$project$Doc$viewPara = F2(
	function (m, r) {
		var op = A2($author$project$Doc$openerAt, m, r);
		var opened = $author$project$Doc$openedLen(op);
		var k = A2(
			$author$project$Doc$markerOf,
			op,
			A2($author$project$Doc$lineOf, m, r));
		var rest = A2($elm$core$String$dropLeft, k, r.O);
		var box = A3($elm$core$String$slice, opened, k, r.O);
		var mark = (k <= 0) ? _List_Nil : A2(
			$elm$core$List$cons,
			A2(
				$elm$html$Html$span,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('dm')
					]),
				A2(
					$author$project$Doc$markParts,
					op,
					A2($elm$core$String$left, opened, r.O))),
			$elm$core$String$isEmpty(box) ? _List_Nil : _List_fromArray(
				[
					A3($author$project$Doc$boxSpan, m, r, box)
				]));
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('dp')
				]),
			_Utils_ap(
				mark,
				function () {
					var _v0 = _Utils_Tuple2(
						A2($author$project$Doc$elementSpan, m, r),
						$author$project$Doc$keyOf(r));
					if (!_v0.a.$) {
						var _v1 = _v0.a.a;
						var a = _v1.a;
						return A4($author$project$Doc$drawWithCookie, m, r, rest, a + k);
					} else {
						if (!_v0.b.$) {
							var _v2 = _v0.a;
							var key = _v0.b.a;
							return _List_fromArray(
								[
									A2(
									$elm$html$Html$span,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('dk')
										]),
									$author$project$Doc$token(key)),
									$elm$html$Html$text(
									A2(
										$elm$core$String$dropLeft,
										$elm$core$String$length(key) + 2,
										r.O))
								]);
						} else {
							var _v3 = _v0.a;
							var _v4 = _v0.b;
							return _Utils_eq(r.c, $author$project$Body$planId) ? $author$project$Doc$viewPlanning(m) : _List_fromArray(
								[
									$elm$html$Html$text(rest)
								]);
						}
					}
				}()));
	});
var $author$project$Doc$viewKids = F6(
	function (lit, m, parent, from, at0, depth) {
		var tail = F2(
			function (mark, out) {
				return (_Utils_cmp(mark, parent.g) < 0) ? _Utils_ap(
					out,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('dg')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(
									A3($author$project$Scan$cut, m.aO, mark, parent.g))
								]))
						])) : out;
			});
		var rowN = function (j) {
			return A2($author$project$Scan$nth, j, m.C);
		};
		var n = $elm$core$List$length(m.C);
		var go = F3(
			function (j, mark, out) {
				go:
				while (true) {
					var _v0 = rowN(j);
					if (!_v0.$) {
						var kid = _v0.a;
						if ((kid.j === 1) && _Utils_eq(
							kid.u,
							$elm$core$Maybe$Just(parent.c))) {
							var under = function () {
								var _v3 = rowN(j + 1);
								if (!_v3.$) {
									var next = _v3.a;
									return _Utils_eq(
										next.u,
										$elm$core$Maybe$Just(kid.c));
								} else {
									return false;
								}
							}();
							var gap = (_Utils_cmp(kid.d, mark) > 0) ? _List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('dg')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(
											A3($author$project$Scan$cut, m.aO, mark, kid.d))
										]))
								]) : _List_Nil;
							var _v1 = function () {
								if (under) {
									var headAt = A2(
										$elm$core$Maybe$withDefault,
										kid.d,
										A2(
											$elm$core$Maybe$map,
											function ($) {
												return $.d;
											},
											rowN(j + 1)));
									var own = (_Utils_cmp(headAt, kid.d) > 0) ? _List_fromArray(
										[
											A2(
											$author$project$Doc$viewPara,
											m,
											_Utils_update(
												kid,
												{
													O: A3($author$project$Scan$cut, m.aO, kid.d, headAt),
													g: headAt
												}))
										]) : _List_Nil;
									var _v2 = A6($author$project$Doc$viewKids, lit, m, kid, j + 1, headAt, depth + 1);
									var deeper = _v2.a;
									var jj = _v2.b;
									return _Utils_Tuple2(
										_Utils_ap(own, deeper),
										jj);
								} else {
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($author$project$Doc$viewPara, m, kid)
											]),
										j + 1);
								}
							}();
							var inner = _v1.a;
							var jNext = _v1.b;
							var $temp$j = jNext,
								$temp$mark = kid.g,
								$temp$out = _Utils_ap(
								out,
								_Utils_ap(
									gap,
									_List_fromArray(
										[
											A7(
											$author$project$Doc$rowEl,
											lit,
											m,
											j,
											kid,
											false,
											_List_fromArray(
												[
													$author$project$Doc$rung(depth)
												]),
											inner)
										])));
							j = $temp$j;
							mark = $temp$mark;
							out = $temp$out;
							continue go;
						} else {
							return _Utils_Tuple2(
								A2(tail, mark, out),
								j);
						}
					} else {
						return _Utils_Tuple2(
							A2(tail, mark, out),
							j);
					}
				}
			});
		return A3(go, from, at0, _List_Nil);
	});
var $author$project$Doc$viewMeta = F4(
	function (lit, m, parent, from) {
		var walk = F2(
			function (j, got) {
				walk:
				while (true) {
					var _v0 = A2($author$project$Scan$nth, j, m.C);
					if (!_v0.$) {
						var kid = _v0.a;
						if ((kid.j === 3) && _Utils_eq(
							kid.u,
							$elm$core$Maybe$Just(parent.c))) {
							var $temp$j = j + 1,
								$temp$got = _Utils_ap(
								got,
								_List_fromArray(
									[
										_Utils_Tuple2(j, kid)
									]));
							j = $temp$j;
							got = $temp$got;
							continue walk;
						} else {
							return _Utils_Tuple2(got, j);
						}
					} else {
						return _Utils_Tuple2(got, j);
					}
				}
			});
		var leaf = function (_v2) {
			var j = _v2.a;
			var kid = _v2.b;
			return A7(
				$author$project$Doc$rowEl,
				lit,
				m,
				j,
				kid,
				false,
				_List_Nil,
				_List_fromArray(
					[
						A2($author$project$Doc$viewPara, m, kid)
					]));
		};
		var _v1 = A2(walk, from, _List_Nil);
		var kids = _v1.a;
		var next = _v1.b;
		return _Utils_Tuple2(
			A2(
				$elm$core$List$cons,
				A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('dg')
						]),
					$author$project$Doc$token('PROPERTIES')),
				_Utils_ap(
					A2($elm$core$List$map, leaf, kids),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('dg')
								]),
							$author$project$Doc$token('END'))
						]))),
			next);
	});
var $author$project$Body$cellOf = F2(
	function (key, r) {
		return A2(
			$elm$core$Maybe$withDefault,
			'',
			A2(
				$elm$core$Maybe$map,
				function ($) {
					return $.bc;
				},
				$elm$core$List$head(
					A2(
						$elm$core$List$filter,
						function (c) {
							return _Utils_eq(c.bA, key);
						},
						r.ac))));
	});
var $elm$core$String$indices = _String_indexes;
var $author$project$Doc$delink = function (s) {
	var _v0 = A2($elm$core$String$indices, '[[', s);
	if (!_v0.b) {
		return s;
	} else {
		var i = _v0.a;
		var _v1 = A2(
			$elm$core$String$indices,
			']]',
			A2($elm$core$String$dropLeft, i + 2, s));
		if (!_v1.b) {
			return s;
		} else {
			var j = _v1.a;
			var close = (i + 2) + j;
			var inner = A3($elm$core$String$slice, i + 2, close, s);
			var shown = function () {
				var _v2 = A2($elm$core$String$indices, '][', inner);
				if (_v2.b) {
					var k = _v2.a;
					return A2($elm$core$String$dropLeft, k + 2, inner);
				} else {
					return inner;
				}
			}();
			return _Utils_ap(
				A2($elm$core$String$left, i, s),
				_Utils_ap(
					shown,
					$author$project$Doc$delink(
						A2($elm$core$String$dropLeft, close + 2, s))));
		}
	}
};
var $author$project$Doc$markerLen = F2(
	function (m, r) {
		return A2(
			$author$project$Doc$markerOf,
			A2($author$project$Doc$openerAt, m, r),
			A2($author$project$Doc$lineOf, m, r));
	});
var $author$project$Doc$crumb = F2(
	function (m, r) {
		var clip = function (s) {
			return ($elm$core$String$length(s) > 24) ? (A2($elm$core$String$left, 23, s) + '…') : s;
		};
		if (r.j === 2) {
			var t = $author$project$Doc$delink(
				A2($author$project$Body$cellOf, 'title', r));
			return clip(
				$elm$core$String$isEmpty(t) ? 'child' : t);
		} else {
			if (r.x === 1) {
				return A2($author$project$Doc$drawer, m, r) ? (':' + ($elm$core$String$toUpper(
					A2($elm$core$Maybe$withDefault, 'drawer', r.am)) + ':')) : A2($elm$core$Maybe$withDefault, 'item', r.am);
			} else {
				return clip(
					function () {
						var _v0 = _Utils_Tuple2(
							r.j,
							$author$project$Body$readProperty(r.O));
						if ((_v0.a === 3) && (!_v0.b.$)) {
							var _v1 = _v0.a;
							var _v2 = _v0.b.a;
							var key = _v2.a;
							return ':' + (key + ':');
						} else {
							return $author$project$Doc$delink(
								$elm$core$String$trim(
									A2(
										$elm$core$String$dropLeft,
										A2($author$project$Doc$markerLen, m, r),
										r.O)));
						}
					}());
			}
		}
	});
var $author$project$Doc$viewPath = function (m) {
	var here = A2($author$project$Doc$idAtRow, m, m.aH);
	var named = A2(
		$elm$core$List$map,
		$author$project$Doc$crumb(m),
		A2(
			$elm$core$List$filter,
			function (r) {
				return (!(!r.x)) || (r.j === 2);
			},
			A2(
				$elm$core$List$filterMap,
				$author$project$Body$rowById(m),
				$elm$core$List$reverse(
					A2(
						$elm$core$List$cons,
						here,
						A2($author$project$Body$ownersOf, m, here))))));
	var words = _Utils_eq(
		A2(
			$elm$core$Maybe$map,
			function ($) {
				return $.j;
			},
			$author$project$Body$rowAt(m)),
		$elm$core$Maybe$Just(0)) ? _List_fromArray(
		['headline']) : ($elm$core$List$isEmpty(named) ? _List_fromArray(
		[
			'headline',
			_Utils_eq(
			A2(
				$elm$core$Maybe$map,
				function ($) {
					return $.j;
				},
				$author$project$Body$rowAt(m)),
			$elm$core$Maybe$Just(3)) ? 'planning' : 'paragraph'
		]) : A2($elm$core$List$cons, 'headline', named));
	var n = $elm$core$List$length(words);
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('dpath')
			]),
		$elm$core$List$concat(
			A2(
				$elm$core$List$indexedMap,
				F2(
					function (i, w) {
						return _Utils_ap(
							(i > 0) ? _List_fromArray(
								[
									A2(
									$elm$html$Html$span,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('dsep')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('›')
										]))
								]) : _List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$span,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class(
											'dcr cr-' + $elm$core$String$fromInt(
												A2($elm$core$Basics$min, 3, (n - 1) - i)))
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(w)
										]))
								]));
					}),
				words)));
};
var $author$project$Doc$view = function (m) {
	var ranks = A2(
		$author$project$Doc$spineRanks,
		m,
		$author$project$Doc$headOf(m));
	var opener = function (r) {
		return (r.j === 3) ? $author$project$Doc$token('PROPERTIES') : _List_fromArray(
			[
				$elm$html$Html$text(
				A2(
					$elm$core$Maybe$withDefault,
					'',
					A2($author$project$Scan$nth, r.d, m.aO)))
			]);
	};
	var n = $elm$core$List$length(m.C);
	var lit = $author$project$Doc$litOf(m);
	var hidden = $author$project$Doc$hiddenIn(m);
	var blkOf = F3(
		function (id, level, inner) {
			return $elm$core$List$isEmpty(inner) ? _List_Nil : _List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							'blk' + function () {
								var _v4 = A2($elm$core$Dict$get, id, ranks);
								if (!_v4.$) {
									var k = _v4.a;
									return ' sp-' + $elm$core$String$fromInt(
										A2($elm$core$Basics$min, 3, k));
								} else {
									return '';
								}
							}()),
							A2(
							$elm$html$Html$Attributes$attribute,
							'style',
							A2($author$project$Doc$rail, m, level))
						]),
					inner)
				]);
		});
	var go = F3(
		function (i, level, out) {
			go:
			while (true) {
				if (_Utils_cmp(i, n) > -1) {
					return _Utils_Tuple2(out, i);
				} else {
					var r = A2(
						$elm$core$Maybe$withDefault,
						$author$project$Body$blank,
						A2($author$project$Scan$nth, i, m.C));
					if ((r.j === 2) && (_Utils_cmp(r.V, level) < 1)) {
						return _Utils_Tuple2(out, i);
					} else {
						if (A2($elm$core$Set$member, r.c, hidden)) {
							var $temp$i = i + 1,
								$temp$level = level,
								$temp$out = out;
							i = $temp$i;
							level = $temp$level;
							out = $temp$out;
							continue go;
						} else {
							if (r.j === 2) {
								var headline = A7(
									$author$project$Doc$rowEl,
									lit,
									m,
									i,
									r,
									true,
									_List_Nil,
									A2($author$project$Doc$viewCells, m, r));
								var _v0 = A3(go, i + 1, r.V, _List_Nil);
								var inner = _v0.a;
								var j = _v0.b;
								var $temp$i = j,
									$temp$level = level,
									$temp$out = _Utils_ap(
									out,
									A2(
										$elm$core$List$cons,
										headline,
										A3(blkOf, r.c, r.V, inner)));
								i = $temp$i;
								level = $temp$level;
								out = $temp$out;
								continue go;
							} else {
								if (r.x === 1) {
									var _v1 = (r.j === 3) ? A4($author$project$Doc$viewMeta, lit, m, r, i + 1) : A6($author$project$Doc$viewKids, lit, m, r, i + 1, r.d, 0);
									var inner = _v1.a;
									var j = _v1.b;
									var shown = A2($elm$core$Set$member, r.c, m.f) ? _List_fromArray(
										[
											A2(
											$elm$html$Html$div,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$class('dg')
												]),
											_Utils_ap(
												opener(r),
												_List_fromArray(
													[
														$elm$html$Html$text(' …')
													])))
										]) : (_Utils_eq(
										r.am,
										$elm$core$Maybe$Just('table')) ? _List_fromArray(
										[
											A2($author$project$Doc$glanceTable, m, r)
										]) : inner);
									var signed = A2($author$project$Doc$drawer, m, r) ? A2(
										$elm$core$List$cons,
										A2($author$project$Doc$foldSign, m, r),
										shown) : shown;
									var $temp$i = j,
										$temp$level = level,
										$temp$out = _Utils_ap(
										out,
										_List_fromArray(
											[
												A7(
												$author$project$Doc$rowEl,
												lit,
												m,
												i,
												r,
												true,
												A2($author$project$Doc$inset, m, r),
												signed)
											]));
									i = $temp$i;
									level = $temp$level;
									out = $temp$out;
									continue go;
								} else {
									var $temp$i = i + 1,
										$temp$level = level,
										$temp$out = _Utils_ap(
										out,
										_List_fromArray(
											[
												A7(
												$author$project$Doc$rowEl,
												lit,
												m,
												i,
												r,
												true,
												A2($author$project$Doc$inset, m, r),
												_List_fromArray(
													[
														A2($author$project$Doc$viewPara, m, r)
													]))
											]));
									i = $temp$i;
									level = $temp$level;
									out = $temp$out;
									continue go;
								}
							}
						}
					}
				}
			}
		});
	var body = function () {
		var _v2 = m.C;
		if (_v2.b) {
			var head = _v2.a;
			var headline = A7(
				$author$project$Doc$rowEl,
				lit,
				m,
				0,
				head,
				true,
				_List_Nil,
				A2($author$project$Doc$viewCells, m, head));
			var _v3 = A3(go, 1, m.V, _List_Nil);
			var inner = _v3.a;
			return A2(
				$elm$core$List$cons,
				headline,
				A3(blkOf, 'H', m.V, inner));
		} else {
			return _List_Nil;
		}
	}();
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class(
				$author$project$Doc$inList(m) ? 'focus' : '')
			]),
		A2(
			$elm$core$List$cons,
			$author$project$Doc$viewPath(m),
			body));
};
var $author$project$Doc$main = $elm$browser$Browser$element(
	{
		cu: function (_v0) {
			return _Utils_Tuple2($author$project$Doc$empty, $elm$core$Platform$Cmd$none);
		},
		cS: function (_v1) {
			return $author$project$Doc$docIn(
				function (v) {
					return A2(
						$elm$core$Result$withDefault,
						$author$project$Doc$Ignore,
						A2($elm$json$Json$Decode$decodeValue, $author$project$Doc$msgD, v));
				});
		},
		cY: $author$project$Doc$update,
		cZ: $author$project$Doc$view
	});
_Platform_export({'Doc':{'init':$author$project$Doc$main(
	$elm$json$Json$Decode$succeed(0))(0)},'Listing':{'init':$author$project$Listing$main($elm$json$Json$Decode$value)(0)}});}(this));