"use strict";

// programmed by watayan <watayan@watayan.net>
// edit run.js, and transpile with Babel to make run1.js
const typeOfValue=
{
	typeInt:1,
	typeFloat:2,
	typeString:3,
	typeBoolean:4,
	typeArray:5
};

var graphColor = [
	'#c00000','#00c000','#0000c0','#007070','#700070','#707000'
];

const nameOfType=['','整数','実数','文字列','真偽','配列'];

var code = null;		// コードを積む（関数・手続き単位で）
var varTables = [];		// 変数テーブルを積む
var myFuncs = {};		// プログラム中で定義される関数・手続き
var run_flag = false, step_flag = false, editable_flag = true;
var flowchart = null;
var textarea = null;
var context = null;
var current_line = -1;
var wait_time = 0;
var flowchart_display = false;
var converting = false;
var dirty = null;
var timeouts = [];
var selected_quiz = -1, selected_quiz_case = -1, selected_quiz_input = 0, selected_quiz_output = 0;
var output_str = '';
var test_limit_time = 0;
var fontsize = 16;
var editor = null;

/** parsedCodeクラス */
class parsedCode
{
	/**
	 * @constructor
	 * @param {Array<Statement>} statementlist 
	 */
	 constructor(statementlist){this.stack = [{statementlist:statementlist, index: 0}]}
}

/** parsedMainRoutineクラス
 * @extends parsedCode
 */
class parsedMainRoutine extends parsedCode
{
	/**
	 * @constructor
	 * @param {Array<Statement>} statementlist 
	 */
	constructor(statementlist){super(statementlist);}
}

/** parsedFunctionクラス
 * @extends parsedCode
 */
class parsedFunction extends parsedCode
{
	/**
	 * @constructor
	 * @param {Array<Statement>} statementlist 
	 */
	constructor(statementlist){
		super(statementlist);
//		this.caller = null;
	}
}

/** parsedStepクラス
 * @extends parsedCode
 */
class parsedStep extends parsedCode
{
	/**
	 * @constructor
	 * @param {Array<Statement>} statementlist 
	 */
	constructor(statementlist){super(statementlist);}
}

/**
 * 変数テーブルのクラス
 */
class varTable
{
	/**
	 * @constructor
	 */
	constructor()
	{
		this.vars = {};
	}
	/**
	 * 
	 * @param {Array<string>} oldvars 
	 * @retuns {Array} 持っている変数名をnamesに追加してソートしたもの
	 */
	varnames(oldvars)
	{
		var names = oldvars;
		for(var name in this.vars)
			if(names.indexOf(name) < 0) names.push(name);
		return names.sort();
	}
	/**
	* 変数名が変数テーブルにあるか(関数 findVarTableからしか呼んではならない)
	* @param {string} varname 
	* @returns {varTable} 自分がvarnameを持てば自分自身を返す
	*/
	findVarTable(varname)
	{
			if(this.vars[varname]) return this;
			else return null;
	}
	
}

/**
 * varnameを持つ変数テーブルを返す
 * @param {string} varname 
 * @returns {varTable} varnameを持つvarTable
 */
function findVarTable(varname)
{
	var t = varTables[0].findVarTable(varname);
	if(t) return t;
	var n = varTables.length - 1;
	if(n > 0) return varTables[n].findVarTable(varname);
	return null;
}

/**
 * コードをフローチャートに反映させる
 */
function codeChange()
{
	if(converting || !flowchart_display) return;
	var code = document.getElementById("sourceTextarea").value + "\n";
	try{
		myFuncs = {};
		var parse = dncl.parse(code);
		var flag = false; // 関数・手続き定義がないか調べる
		for(var i = 0; i < parse.length; i++)
			if(parse[i] instanceof DefineFunction || parse[i] instanceof DefineStep) flag = true;
		if(flag)
		{
			textarea.value = "関数定義や手続き定義のあるプログラムのフローチャートはまだ実装していません。\n";
			return;
		}
		converting = true;
		flowchart.code2flowchart(parse);
		converting = false;
	}
	catch(e)
	{
		highlightLine(-1);
		textareaClear();
		if(e.line) textareaAppend(e.line + "行目");
		textareaAppend('構文エラーです\n' + e.message);;
		converting = false;
	}
}

/************************************************************************************ユーティリティ関数 */

/**
 * 有限な値であるか
 * @param {number|string} v 
 * @returns {boolean} vが有限な値であるか
 */
function isFinite(v)
{
	return Number.isFinite(v);
}

/**
 * 整数で表せる値であるか
 * @param {number|string} v
 * @returns {boolean} vが整数で表せる値であるか 
 */
function isSafeInteger(v)
{
	return Number.isSafeInteger(v);
}

/**
 * 整数であるか
 * @param {number|string} v 
 * @returns {boolean} vが整数であるか
 */
function isInteger(v)
{
	return Number.isInteger(v);
}

/**
 * クラス名を返す
 * @param {Object} obj
 * @return {string} クラス名 
 */
function constructor_name(obj)
{
	// var result = /^(class|function)\s+([\w\d]+)/.exec(obj.constructor.toString());
	// return result ? result[2] : null;
	return obj.constructor.name;
}

/**
 * 全角英数を半角にする
 * @param {string} s 
 * @returns {string}
 * @throws {RuntimeError}
 */
function toHalf(s, loc)
{
	s = s.toString();
	if(setting.zenkaku_mode == 1 && /[Ａ-Ｚａ-ｚ０-９．−]/.exec(s))
		throw new RuntimeError(loc.first_line, "数値や変数名を全角文字で入力してはいけません");
	return s.replace(/[Ａ-Ｚａ-ｚ０-９．−]/g, function(s) {
		return String.fromCharCode(s.charCodeAt(0) - 0xFEE0);}
	);
}

/**
 * プログラムコードに改変が加えられたことを示すフラグの操作
 * @param {boolean} b 
 */
function makeDirty(b)
{
	if(b !== dirty)
	{
		dirty = b;
		document.getElementById("dirty").style.visibility = dirty ? "visible" : "hidden";
	}
}

/**
 * 結果表示画面にテキストを追加する
 * @param {string} v 
 */
function textareaAppend(v)
{
	textarea.value += v;
	textarea.scrollTop = textarea.scrollHeight;
}

/**
 * 結果表示画面をクリアする
 */
function textareaClear()
{
	textarea.value = '';
}

/**
 * プログラムにおける位置を表す
 */
class Location
{
	/**
	 * @constructor
	 * @param {Token} first_token 
	 * @param {Token} last_token 
	 */
	constructor(first_token, last_token)
	{
		this._first_line = first_token.first_line;
		this._last_line = last_token.last_line;
	}
	get first_line(){return this._first_line;}
	get last_line() {return this._last_line;}
}

/**
 * 実行時エラー
 */
class RuntimeError
{
	/**
	 * @constructor
	 * @param {number} line 
	 * @param {string} message 
	 */
	constructor(line, message)
	{
		this._line = line;
		this._message = message;
		setRunflag(false);
	}
	get line() {return this._line;}
	get message() {return this._message;}
}

/**
 * 値クラスの親クラス
 */
 class Value
 {
	 /**
	  * @constructor
	  * @param {number|string|boolean} v 
	  * @param {Location} loc 
	  */
	 constructor(v, loc)
	 {
		 this._value = v;
		 this.rtnv = null;
		 this._loc = loc;
	 }
	 clone()
	 {
		 throw new RuntimeError(this.first_line, constructor_name(this) + "はcloneが作られていません");
	 }
	 /**
	  * @returns 生のJavaScriptにおける値
	  */
	 get value() {return this._value;}
	 get loc() {return this._loc;}
	 get first_line() {return this._loc.first_line;}
	 /**
	  * @returns {Value} 値
	  */
	 getValue()
	 {
		 return this;
	 }
	 /**
	  * @returns {string} PyPENの文法で表した文字列
	  */
	 getCode()
	 {
		 return '' + this._value;
	 }
	 /**
	  * @returns {string} Pythonの文法で表した文字列
	  */
	 run()
	 {
		 code[0].stack[0].index++;
	 }
 }
 
/**
 * 型の決まってない値
 * @extends Value
 */
class NullValue extends Value
{
	/**
	 * @constructor
	 * @param {Location} loc 
	 */
	constructor(loc)
	{
		super(0, loc);
	}
	clone()
	{
		return new NullValue(this.loc);
	}
}

/**
 * vtにあるvn[args]にnewvalをセットする
 * @param {VarTable} vt 
 * @param {String} vn 
 * @param {Array<Value>} args 
 * @param {Value} newval 
 * @param {Location} loc 
 */
 function setVariableByArgs(vt,vn, args, newval, loc)
 {
	 if(args)
	 {
		 var v = vt.vars[vn];
		 for(var i = 0; i < args.length - 1; i++)
		 {
			 var arg = args[i];
			 if(arg.getValue() instanceof IntValue)
			 {
				 if(v.getValue() instanceof ArrayValue || v.getValue() instanceof StringValue)
				 {
					 var idx = arg.getValue().value;
					 var l = v.getValue().length;
					 if(idx < 0) idx += l;
					 if(idx >= 0 && idx < l) v = v.getValue().value[idx];
					 else throw new RuntimeError(loc.first_line, "配列の範囲を超えて代入しようとしました");
				 }
				 else if(v.getValue() instanceof StringValue)
					 throw new RuntimeError(loc.first_line, "部分文字列の部分文字列への代入はできません");
				 else throw new RuntimeError(loc.first_line, "整数の添字は配列か文字列でないと使えません");
			 }
			 else if(arg.getValue() instanceof StringValue)
			 {
				 var key0 = arg.getValue().value;
				 if(v.getValue() instanceof DictionaryValue)
				 {
					 for(var key in v.getValue().value)
					 {
						 if(key == key0)
						 {
							 v = v.getValue().value[key0];
							 key0 = null;
							 break;
						 }
					 }
					 if(key0) throw new RuntimeError(loc.first_line, "辞書にキー"+arg.getValue().value+"がありません");
				 } 
				 else throw new RuntimeError(loc.first_line, "文字列の添字は辞書でないと使えません");
			 }
			 else if(arg.getValue() instanceof SliceValue)
				 throw new RuntimeError(loc.first_line, "スライスの使い方が正しくありません");
			 else throw new RuntimeError(loc.first_line, "添字が正しくありません");
		 }
		 //代入
		 var arg = args[args.length - 1];
		 if(arg.getValue() instanceof IntValue)
		 {
			 var idx = arg.getValue().value;
			 var l = v.getValue().value.length;
			 if(idx < 0) idx += l;
			 if(idx < 0 || idx >= l) throw new RuntimeError(loc.first_line, "配列の範囲を超えて代入しようとしました");
			 if(v.getValue() instanceof ArrayValue) v.getValue().value[idx] = newval.clone();
			 else if(v.getValue() instanceof StringValue)
			 {
				 if(!(newval.getValue() instanceof StringValue)) throw new RuntimeError(loc.first_line, "文字列の途中に文字列でないものを挿入しようとしました");
				 var str = v.getValue().value;
				 v.getValue()._value = str.substr(0, idx) + newval.getValue()._value + str.substr(idx + 1);
			 }
			 else throw new RuntimeError(loc.first_line, "整数の添字は配列か文字列にしか使えません");
		 }
		 else if(arg.getValue() instanceof StringValue)
		 {
			 if(v.getValue() instanceof DictionaryValue) v.getValue().value[arg.getValue().value] = newval.clone();
			 else throw new RuntimeError(loc.first_line, "文字列の添字は辞書にしか使えません");
		 }
		 else if(arg.getValue() instanceof SliceValue)
		 {
			 var idx1 = arg.getValue1().getValue().value;
			 var idx2 = arg.getValue2().getValue().value;
			 if(v.getValue() instanceof ArrayValue)
			 {
				 if(!(newval.getValue() instanceof ArrayValue)) throw new RuntimeError(loc.first_line, "配列に配列でないものを挿入しようとしました");
				 var l = v.getValue().length;
				 if(!idx1) idx1 = 0;
				 if(!idx2) idx2 = l;
				 if(idx1 < 0) idx1 += l;
				 if(idx2 < 0) idx2 += l;
				 if(idx1 >= 0 && idx2 >= 0 && idx1 < l && idx2 < l)
				 {
					 var a = [];
					 for(var i = 0; i < idx1; i++) a.push(v.getValue().value[i].clone());
					 for(var i = 0; i < newval.getValue().length; i++) a.push(newval.getValue().value[i].clone());
					 for(var i = idx2; i <  l; i++) a.push(v.getValue().value[i].clone());
					 v.getValue()._value = a;
				 }
				 else throw new RuntimeError(loc.first_line, "配列の範囲外に挿入しようとしました");
			 }
			 else if(v.getValue() instanceof StringValue)
			 {
				 if(!(newval.getValue() instanceof StringValue)) throw new RuntimeError(loc.first_line, "文字列の途中に文字列でないものを挿入しようとしました");
				 var l = v.getValue().length;
				 if(!idx1) idx1 = 0;
				 if(!idx2) idx2 = l;
				 if(idx1 < 0) idx1 += l;
				 if(idx2 < 0) idx2 += l;
				 if(idx1 >= 0 && idx2 >= 0 && idx1 < l && idx2 < l) 
				 {
					 var str = v.getValue().value.substr(0, idx1) + newval.getValue().value + v.getValue().value.substr(idx2);
					 v.getValue()._value = str;
				 }
			 }
			 else throw new RuntimeError("スライスの添字は配列か文字列でないと使えません");
		 }
		 else throw new RuntimeError(loc.first_line, "添字が正しくありません");
 
	 }
	 else
	 {
		 vt.vars[vn] = newval.clone();
		 return;
	 }
 }
 
 /**
  * v[args]の値を取得する
  * @param {Variable} v 
  * @param {Array<Value>} args 
  * @param {Location} loc 
  * @returns Value
  */
 function getValueByArgs(v, args, loc)
 {
	 if(args)
	 {
		 for(var i = 0; i < args.length; i++)
		 {
			 var arg = args[i];
			 if(arg.getValue() instanceof IntValue)
			 {
				 if(v.getValue() instanceof ArrayValue)	// 配列のidx番目
				 {
					 var idx = arg.getValue().value;
					 var l = v.length;
					 if(idx < 0) idx += l;
					 if(idx >= 0 && idx < l) v = v.getValue().value[idx];
					 else throw new RuntimeError(loc.first_line, "配列の範囲を超えてアクセスしました");
				 }
				 else if(v.getValue() instanceof StringValue)	// 文字列のidx文字目
				 {
					 var idx = arg.getValue().value;
					 var l = v.getValue().length;
					 if(idx < 0) idx += l;
					 if(idx >= 0 && idx < l) v = new StringValue(v.getValue().value[idx], loc);
					 else throw new RuntimeError(loc.first_line, "文字列の範囲を超えてアクセスしました");
				 }
				 else throw new RuntimeError(loc.first_line, "整数の添字は配列か文字列でないと使えません");
			 }
			 else if(arg.getValue() instanceof StringValue)
			 {
				 if(v.getValue() instanceof DictionaryValue) v = v.getValue().value[arg.getValue().value];
				 else throw new RuntimeError(loc.first_line, "文字列の添字は辞書でないと使えません");
			 }
			 else if(arg.getValue() instanceof SliceValue)
			 {
				 var idx1 = arg.getValue1().getValue().value;
				 var idx2 = arg.getValue2().getValue().value;
				 if(v.getValue() instanceof ArrayValue)	// 配列のスライス
				 {
					 var l = v.getValue().length;
					 if(!idx1) idx1 = 0;
					 if(!idx2) idx2 = l;
					 if(idx1 < 0) idx1 += l;
					 if(idx2 < 0) idx2 += l;
					 if(idx1 >= 0 && idx2 >= 0 && idx1 <= l && idx2 <= l)
					 {
						 var a = [];
						 for(var j = idx1; j < idx2; j++) a.push(v.getValue().value[j].clone());
						 v = new ArrayValue(a, loc);
					 }
					 else throw new RuntimeError(loc.first_line, "配列の範囲を超えて読み出そうとしました");
				 }
				 else if(v.getValue() instanceof StringValue)	// 文字列のスライス
				 {
					 var l = v.getValue().length;
					 if(!idx1) idx1 = 0;
					 if(!idx2) idx2 = l;
					 if(idx1 < 0) idx1 += l;
					 if(idx2 < 0) idx2 += l;
					 if(idx1 >= 0 && idx2 >= 0 && idx1 <= l && idx2 <= l) v = new StringValue(v.getValue().value.substr(idx1, idx2 - idx1), loc);
					 else throw new RuntimeError(loc.first_line, "文字列の範囲を超えて読み出そうとしました");
				 }
				 else throw new RuntimeError(loc.first_line, "スライスの添字は配列か文字列でないと使えません");
			 }
			 else throw new RuntimeError(loc.first_line, "添字が正しくありません");
		 }
	 }
	 return v.clone();
 }
 

/**
 * 値の配列
 */
class ArrayValue extends Value
{
	/**
	 * @constructor
	 * @param {Array<Value>} v 
	 * @param {Location} loc 
	 */
	constructor(v, loc)
	{
		super(v, loc);
		this.rtnv = null;
		this.state = 0;
	}
	clone()
	{
		var a = [];
		for(var i = 0; i < this.value.length; i++) a.push(this.value[i].clone());
		var rtnv = new ArrayValue(a, this.loc);
		rtnv.rtnv = rtnv;
		return rtnv;
	}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index: 0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			var a = [];
			for(var i = 0; i < this.value.length; i++) 
			{
				a.push(this.value[i].getValue());
			}
			this.rtnv = new ArrayValue(a, this.loc);
			this.rtnv.rtnv = this.rtnv;
			this.state = 0;
		}
	}
	getCode()
	{
		var ag = [];
		for(var i = 0; i < this.value.length; i++) ag.push(this.value[i].getCode());
		return '[' + ag.join(',') + ']';
	}
	get length() {return this.value.length;}
	getValue()
	{
		return this.rtnv;
	}
	append(a)
	{
		for(var i of a) this._value.push(i);
	}
}

/**
 * 辞書
 */
 class DictionaryValue extends Value
 {
	 /**
	  * @constructor
	  * @param {Array} v 
	  * @param {Location} loc 
	  */
	 constructor(v, loc)
	 {
		 super({}, loc);
		 for(var i = 0; i < v.length; i++)
		 {
			 if(v[i] instanceof SliceValue && v[i].getValue1().getValue() instanceof StringValue && !(v[i].getValue2().getValue() instanceof NullValue))
				 this.value[v[i].getValue1().getValue().value] = v[i].getValue2();
			 else throw new RuntimeError(loc.first_line, "辞書の初期化が間違っています");
		 }
		 this.state = 0;
	 }
	 clone()
	 {
		 var rtnv = new DictionaryValue({}, this.loc);
		 for(var key of Object.keys(this.value))
		 {
			 if(this.value[key])
			 {
				 rtnv.value[key] = this.value[key].getValue().clone();
			 }
			 else throw new RuntimeError(this.first_line, keys[i]+"が定義されていません");
		 }
		 rtnv.rtnv = rtnv;
		 return rtnv;
	 }
	 getCode()
	 {
		 var ag = [];
		 var keys = Object.keys(this.value);
		 keys.sort();
		 for(var i = 0; i < keys.length; i++) 
			 ag.push(keys[i] + ':' + this.value[keys[i]].getCode());
		 return '{' + ag.join(',') + '}';
	 }
	 getValue()
	 {
		 return this.rtnv;
	 }
	 run()
	 {
		 if(this.state == 0)
		 {
			 var a = [];
			 for(let key of Object.keys(this.value))
			 {
				 // a.push(key);
				 a.push(this.value[key]);
			 }
			 code[0].stack.unshift({statementlist: a, index: 0});
			 this.state = 1;
		 }
		 else
		 {
			 code[0].stack[0].index++;
			 var a = [];
			 for(let key of Object.keys(this.value))
			 {
				 a.push(new SliceValue(new StringValue(key, this.loc), this.value[key], this.loc));
			 }
			 this.rtnv = new DictionaryValue(a, this.loc);
			 this.rtnv.rtnv = this.rtnv;
			 this.state = 0;
		 }
	 }
 }

class IntValue extends Value
{
	constructor(v, loc)
	{
		 super(v, loc);
		 if(!isSafeInteger(v)) throw new RuntimeError(this.first_line, "整数で表せない値です");
		 this.rtnv = this.value;
		}
	clone()
	{
		return new IntValue(this.value, this.loc);
	}
	getValue()
	{
		return this;
	}
}
class FloatValue extends Value
{
	constructor(v, loc)
	{
		super(v, loc);
		if(!isFinite(v)) throw new RuntimeError(this.first_line, "オーバーフローしました");
		this.rtnv = this.value;
	}
	getCode()
	{
		let str = this.value.toString();
		if(str.match(/[Ee]/) != undefined)  return str;
		else if(isSafeInteger(this.value)) return this.value + '.0';
		else return this.value;
	}
	clone()
	{
		return new FloatValue(this.value, this.loc);
	}
	getValue()
	{
		return this;
	}
}
class StringValue extends Value 
{
	constructor(v, loc)
	{
		super(v, loc);
		this.rtnv = this.value;
	}
	get length(){return this.value.length;}
	getCode()
	{
		if(this.value.match(/[「」]/)) return '"' + this.value + '"';
		else return '「' + this.value + '」';
	}
	clone()
	{
		return new StringValue(this.value, this.loc);
	}
	getValue()
	{
		return this;
	}
}
class BooleanValue extends Value 
{
	constructor(v, loc)
	{
		super(v, loc);
		this.rtnv = this.value;
	}
	getCode()
	{
		return this.value ? 'true' : 'false';
	}
	clone()
	{
		return new BooleanValue(this.value, this.loc);
	}
	getValue()
	{
		return this;
	}
}

class UNDEFINED extends Value
{
	constructor(v, loc)
	{
		super(v, loc);
	}
	clone()
	{
		return new UNDEFINED(this.value, this.loc);
	}
	get varname()
	{
		return this.value;
	}
	getValue()
	{
		throw new RuntimeError(this.first_line, "未完成のプログラムです");
	}
	getCode()
	{
		return this.value;
	}
}

class Pow extends Value
{
	constructor(x, y, loc)
	{
		super([x,y], loc);
		this.rtnv = null;
		this.state = 0;
	}
	clone()
	{
		var rtnv = new Pow(this.value[0], this.value[1], this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index:0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			let v1 = this.value[0].getValue(), v2 = this.value[1].getValue();
			if(v1 instanceof IntValue && v2 instanceof IntValue && v2.value >= 0) // 整数の自然数乗
			{
				if(v1.value == 0 && v2.value <= 0) throw new RuntimeError(this.first_line, "0は正の数乗しかできません");
				let v = Math.pow(v1.value, v2.value);
				if(isSafeInteger(v)) this.rtnv = new IntValue(v, this.loc);
				else throw new RuntimeError(this.first_line, "整数で表せる範囲を越えました");
			}
			else if((v1 instanceof IntValue || v1 instanceof FloatValue) && (v2 instanceof IntValue || v2 instanceof FloatValue))
			{
				if(v1.value < 0 && !Number.isSafeInteger(v2.value)) throw new RuntimeError(this.first_line, "負の数の非整数乗はできません");
				if(v1.value == 0 && v2.value <= 0) throw new RuntimeError(this.first_line, "0は正の数乗しかできません");
				let v = Math.pow(v1.value, v2.value);
				if(isFinite(v)) this.rtnv = new FloatValue(v, this.loc);
				else throw new RuntimeError(this.first_line, "オーバーフローしました");
			} else throw new RuntimeError('数値でないもののべき乗はできません');
			this.state = 0;
		}
	}
	getCode()
	{
		let v1 = this.value[0], v2 = this.value[1];
		let c1 = constructor_name(v1), c2 = constructor_name(v2);
		let brace1 = false, brace2 = false;
		if(c1 == "Minus" || c1 == "Add" || c1 == "Sub" || c1 == "Mul" || c1 == "Div" || c1 == "DivInt" || c1 == "Mod") brace1 = true;
		if(c2 == "Minus" || c2 == "Add" || c2 == "Sub" || c2 == "Mul" || c2 == "Div" || c2 == "DivInt" || c2 == "Mod") brace2 = true;
		return (brace1 ? '(' : '') + v1.getCode() + (brace1 ? ')' : '')
			+ '**'
			+ (brace2 ? '(' : '') + v2.getCode() + (brace2 ? ')' : '')
	}
	getValue()
	{
		return this.rtnv;
	}
}

class Add extends Value
{
	constructor(x, y, loc)
	{
		super([x,y], loc);
		this.rtnv = null;
		this.state = 0;
	}
	clone()
	{
		var rtnv = new Add(this.value[0], this.value[1], this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index: 0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			let v1 = this.value[0].getValue(), v2 = this.value[1].getValue();
			if(v1 instanceof ArrayValue && v2 instanceof ArrayValue)
			{
				let v = []
				for(let i = 0; i < v1.length; i++) v.push(v1.value[i])
				for(let i = 0; i < v2.length; i++) v.push(v2.value[i])
				this.rtnv = new ArrayValue(v, this.loc);
			}
			else if(v1 instanceof ArrayValue || v2 instanceof ArrayValue) throw new RuntimeError(this.first_line, "配列の足し算はできません");
			else if(v1 instanceof BooleanValue || v2 instanceof BooleanValue) throw new RuntimeError(this.first_line, "真偽型の足し算はできません");
			else if(v1 instanceof StringValue || v2 instanceof StringValue) // 一方でも文字列なら文字列結合
			{
				this.rtnv = new StringValue(v1.value + v2.value, this.loc);
			}
			else	// 数値どうし
			{
				let v = v1.value + v2.value; 
				if(v1 instanceof FloatValue || v2 instanceof FloatValue)	// 一方が実数型なら結果は実数型
				{
					if(!isFinite(v)) throw new RuntimeError(this.first_line, "オーバーフローしました");
					this.rtnv = new FloatValue(v, this.loc);
				}
				else	// 整数型
				{
					if(!isSafeInteger(v)) throw new RuntimeError(this.first_line, "整数で表される範囲を越えました");
					this.rtnv = new IntValue(v, this.loc);
				}
			}
			this.state = 0;
		}
	}
	getCode()
	{
		let v1 = this.value[0], v2 = this.value[1];
		let c1 = constructor_name(v1), c2 = constructor_name(v2);
		let brace1 = false, brace2 = false;
		if(c1 == "Minus") brace1 = true;
		if(c2 == "Minus") brace2 = true;
		return (brace1 ? '(' : '') + v1.getCode() + (brace1 ? ')' : '')
			+ '+'
			+ (brace2 ? '(' : '') + v2.getCode() + (brace2 ? ')' : '')
	}
	getValue()
	{
		return this.rtnv;
	}
}

class Sub extends Value
{
	constructor(x, y, loc)
	{
		super([x,y], loc);
		this.rtnv = null;
		this.state = 0;
	}
	clone()
	{
		var rtnv = new Sub(this.value[0], this.value[1], this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index:0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			let v1 = this.value[0].getValue(), v2 = this.value[1].getValue();
			if(v1 instanceof ArrayValue || v2 instanceof ArrayValue) throw new RuntimeError(this.first_line, "配列の引き算はできません");
			if(v1 instanceof BooleanValue || v2 instanceof BooleanValue) throw new RuntimeError(this.first_line, "真偽型の引き算はできません");
			if(v1 instanceof StringValue || v2 instanceof StringValue) throw new RuntimeError(this.first_line, "文字列の引き算はできません");
			let v = v1.value - v2.value;
			if(v1 instanceof FloatValue || v2 instanceof FloatValue)
			{
				if(!isFinite(v)) throw new RuntimeError(this.first_line, "オーバーフローしました");
				this.rtnv = new FloatValue(v, this.loc);
			}
			else
			{
				if(!isSafeInteger(v)) throw new RuntimeError(this.first_line, "整数で表される範囲を越えました");
				this.rtnv = new IntValue(v, this.loc);
			}
			this.state = 0;
		}
	}
	getCode()
	{
		let v1 = this.value[0], v2 = this.value[1];
		let c1 = constructor_name(v1), c2 = constructor_name(v2);
		let brace1 = false, brace2 = false;
		if(c1 == "Minus") brace1 = true;
		if(c2 == "Minus") brace2 = true;
		return (brace1 ? '(' : '') + v1.getCode() + (brace1 ? ')' : '')
			+ '-'
			+ (brace2 ? '(' : '') + v2.getCode() + (brace2 ? ')' : '')
	}
	getValue()
	{
		return this.rtnv;
	}
}

class Mul extends Value
{
	constructor(x, y, loc)
	{
		super([x,y], loc);
		this.rtnv = null;
		this.state = 0;
	}
	clone()
	{
		var rtnv = new Mul(this.value[0], this.value[1], this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index: 0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			let v1 = this.value[0].getValue(), v2 = this.value[1].getValue();
			if(v1 instanceof BooleanValue || v2 instanceof BooleanValue) throw new RuntimeError(this.first_line, "真偽型のかけ算はできません");
			else if(v1 instanceof StringValue || v2 instanceof StringValue)
			{
				let va = null, vn = null;
				if(v1 instanceof IntValue){va = v2; vn = v1;}
				else if(v2 instanceof IntValue){va = v1; vn = v2;}
				else throw new RuntimeError(this.first_line, "文字列には整数しか掛けられません");
				let v = '';
				for(let i = 0; i < vn.value; i++)
					v += va.value;
				this.rtnv = new StringValue(v, this.loc);
			}
			else if(v1 instanceof ArrayValue || v2 instanceof ArrayValue)
			{
				let va = null, vn = null;
				if(v1 instanceof IntValue){va = v2; vn = v1;}
				else if(v2 instanceof IntValue){va = v1; vn = v2;}
				else throw new RuntimeError(this.first_line, "配列には整数しか掛けられません");
				let v = []
				for(let i = 0; i < vn.value; i++)
					for(let j = 0; j < va.length; j++) v.push(va.value[j]);
				this.rtnv = new ArrayValue(v, this.loc);
			} 
			else
			{
				let v = v1.value * v2.value;
				if(v1 instanceof FloatValue || v2 instanceof FloatValue)
				{
					if(!isFinite(v)) throw new RuntimeError(this.first_line, "オーバーフローしました");
					this.rtnv = new FloatValue(v, this.loc);
				}
				else
				{
					if(!isSafeInteger(v)) throw new RuntimeError(this.first_line, "整数で表される範囲を越えました");
					this.rtnv = new IntValue(v, this.loc);
				}
			}
			this.state = 0;
		}
	}
	getCode()
	{
		let v1 = this.value[0], v2 = this.value[1];
		let c1 = constructor_name(v1), c2 = constructor_name(v2);
		let brace1 = false, brace2 = false;
		if(c1 == "Minus" || c1 == "Add" || c1 == "Sub") brace1 = true;
		if(c2 == "Minus" || c2 == "Add" || c2 == "Sub") brace2 = true;
		return (brace1 ? '(' : '') + v1.getCode() + (brace1 ? ')' : '')
			+ '*'
			+ (brace2 ? '(' : '') + v2.getCode() + (brace2 ? ')' : '')
	}
	getValue()
	{
		return this.rtnv;
	}
}

class Div extends Value	// /
{
	constructor(x, y, loc)
	{
		super([x,y], loc);
		this.rtnv = null;
		this.state = 0;
	}
	clone()
	{
		var rtnv = new Div(this.value[0], this.value[1], this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index: 0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			let v1 = this.value[0].getValue(), v2 = this.value[1].getValue();
			if(v1 instanceof ArrayValue || v2 instanceof ArrayValue) throw new RuntimeError(this.first_line, "配列のわり算はできません");
			if(v1 instanceof BooleanValue || v2 instanceof BooleanValue) throw new RuntimeError(this.first_line, "真偽型のわり算はできません");
			if(v1 instanceof StringValue || v2 instanceof StringValue) throw new RuntimeError(this.first_line, "文字列のわり算はできません");
			if(v2.value == 0) throw new RuntimeError(this.first_line, "0でわり算をしました");
			let v = v1.value / v2.value;
			if(!isFinite(v)) throw new RuntimeError(this.first_line, "オーバーフローしました");
			this.rtnv = new FloatValue(v, this.loc);
			this.state = 0;
		}
	}
	getCode()
	{
		let v1 = this.value[0], v2 = this.value[1];
		let c1 = constructor_name(v1), c2 = constructor_name(v2);
		let brace1 = false, brace2 = false;
		if(c1 == "Minus" || c1 == "Add" || c1 == "Sub") brace1 = true;
		if(c2 == "Minus" || c2 == "Add" || c2 == "Sub") brace2 = true;
			return (brace1 ? '(' : '') + v1.getCode() + (brace1 ? ')' : '')
			+ '/'
			+ (brace2 ? '(' : '') + v2.getCode() + (brace2 ? ')' : '')
	}
	getValue()
	{
		return this.rtnv;
	}
}

class Div2 extends Value // ÷
{
	constructor(x, y, loc)
	{
		super([x,y], loc);
		this.rtnv = null;
		this.state = 0;
	}
	clone()
	{
		var rtnv = new DivInt(this.value[0], this.value[1], this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index: 0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			let v1 = this.value[0].getValue(), v2 = this.value[1].getValue();
			if(v1 instanceof IntValue && v2 instanceof IntValue)
			{
				if(v2.value == 0) throw new RuntimeError(this.first_line, "0でわり算をしました");
				let v = Math.floor(v1.value / v2.value);
				if(!isSafeInteger(v)) throw new RuntimeError(this.first_line, "整数で表される範囲を越えました");
				this.rtnv = new IntValue(v, this.loc);
			}
			else throw new RuntimeError(this.first_line, '整数にしか「÷」は使えません');
			this.state = 0;
		}
	}
	getCode()
	{
		let v1 = this.value[0], v2 = this.value[1];
		let c1 = constructor_name(v1), c2 = constructor_name(v2);
		let brace1 = false, brace2 = false;
		if(c1 == "Minus" || c1 == "Add" || c1 == "Sub") brace1 = true;
		if(c2 == "Minus" || c2 == "Add" || c2 == "Sub") brace2 = true;
			return (brace1 ? '(' : '') + v1.getCode() + (brace1 ? ')' : '')
			+ '÷'
			+ (brace2 ? '(' : '') + v2.getCode() + (brace2 ? ')' : '')
	}
	getValue()
	{
		return this.rtnv;
	}
}

class Mod extends Value
{
	constructor(x, y, loc)
	{
		super([x,y], loc);
		this.rtnv = null;
		this.state = 0;
	}
	clone()
	{
		var rtnv = new Mod(this.value[0], this.value[1], this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index: 0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			let v1 = this.value[0].getValue(), v2 = this.value[1].getValue();
			if(v1 instanceof IntValue && v2 instanceof IntValue)
			{
				if(v2.value == 0) throw new RuntimeError(this.first_line, "0でわり算をしました");
				let v = v1.value - Math.floor(v1.value / v2.value) * v2.value;
				if(!isSafeInteger(v)) throw new RuntimeError(this.first_line, "整数で表される範囲を越えました");
				this.rtnv = new IntValue(v, this.loc);
			}
			else
				throw new RuntimeError(this.first_line, "余りを出す計算は整数でしかできません");
			this.state = 0;
		}
	}
	getCode()
	{
		let v1 = this.value[0], v2 = this.value[1];
		let c1 = constructor_name(v1), c2 = constructor_name(v2);
		let brace1 = false, brace2 = false;
		if(c1 == "Minus" || c1 == "Add" || c1 == "Sub") brace1 = true;
		if(c2 == "Minus" || c2 == "Add" || c2 == "Sub") brace2 = true;
		return (brace1 ? '(' : '') + v1.getCode() + (brace1 ? ')' : '')
			+ '%'
			+ (brace2 ? '(' : '') + v2.getCode() + (brace2 ? ')' : '')
	}
	getValue()
	{
		return this.rtnv;
	}
}

class Minus extends Value
{
	constructor(x, loc)
	{
		super([x], loc);
		this.rtnv = null;
		this.state = 0;
	}
	clone()
	{
		var rtnv = new Minus(this.value[0], this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index: 0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			let v1 = this.value[0].getValue();
			if(v1 instanceof IntValue || v1 instanceof FloatValue)
			{
				let v = -v1.value;
				if(v1 instanceof IntValue && !isSafeInteger(v)) throw new RuntimeError(this.first_line, "整数で表される範囲を越えました");
				if(v1 instanceof FloatValue && !isFinite(v)) throw new RuntimeError(this.first_line, "オーバーフローしました");
				this.rtnv = v1 instanceof IntValue ? new IntValue(v, this.loc) : new FloatValue(v, this.loc);
			}
			else
				throw new RuntimeError(this.first_line, "マイナスは数値にしかつけられません");
			this.state = 0;
		}
	}
	getCode()
	{
		let v1 = this.value[0];
		let c1 = constructor_name(v1);
		let brace1 = false;
		if(c1 == "Minus" || c1 == "Add" || c1 == "Sub") brace1 = true;
		return '-' + (brace1 ? '(' : '') + v1.getCode() + (brace1 ? ')' : '');
	}
	getValue()
	{
		return this.rtnv;
	}
}

class And extends Value
{
	constructor(x, y, loc)
	{
		super([x,y],loc);
		this.rtnv = null;
		this.state = 0;
	}
	clone()
	{
		var rtnv = new And(this.value[0], this.value[1], this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index:0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			let v1 = this.value[0].getValue();
			if(v1 instanceof BooleanValue)
			{
				if(!v1.value) this.rtnv = new BooleanValue(false, this.loc);
				else
				{
					let v2 = this.value[1].getValue();
					if(v2 instanceof BooleanValue) this.rtnv = new BooleanValue(v2.value, this.loc);
				}
			}
			else
				throw new RuntimeError(this.first_line, "「かつ」は真偽値にしか使えません");
			this.state = 0;
		}
	}
	getCode()
	{
		let v1 = this.value[0], v2 = this.value[1];
		let c1 = constructor_name(v1), c2 = constructor_name(v2);
		let brace1 = false, brace2 = false;
		if(c2 == "And" || c2 == "Or" || c2 == "Not") brace2 = true;
		return (brace1 ? '(' : '') + v1.getCode() + (brace1 ? ')' : '')
			+ 'かつ'
			+ (brace2 ? '(' : '') + v2.getCode() + (brace2 ? ')' : '')
	}
	getValue()
	{
		return this.rtnv;
	}
}

class Or extends Value
{
	constructor(x, y, loc)
	{
		super([x,y],loc);
		this.rtnv = null;
		this.state = 0;
	}
	clone()
	{
		var rtnv = new Or(this.value[0], this.value[1], this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index:0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			let v1 = this.value[0].getValue();
			if(v1 instanceof BooleanValue)
			{
				if(v1.value) this.rtnv = new BooleanValue(true, this.loc);
				else
				{
					let v2 = this.value[1].getValue();
					if(v2 instanceof BooleanValue) this.rtnv = new BooleanValue(v2.value, this.loc);
				}
			}
			else
				throw new RuntimeError(this.first_line, "「または」は真偽値にしか使えません");
			this.state = 0;
		}
	}
	getCode()
	{
		let v1 = this.value[0], v2 = this.value[1];
		let c1 = constructor_name(v1), c2 = constructor_name(v2);
		let brace1 = false, brace2 = false;
		if(c2 == "And" || c2 == "Or" || c2 == "Not") brace2 = true;
		return (brace1 ? '(' : '') + v1.getCode() + (brace1 ? ')' : '')
			+ 'または'
			+ (brace2 ? '(' : '') + v2.getCode() + (brace2 ? ')' : '')
	}
	getValue()
	{
		return this.rtnv;
	}
}

class Not extends Value
{
	constructor(x, loc)
	{
		super([x],loc);
		this.rtnv = null;
		this.state = 0;
	}
	clone()
	{
		var rtnv = new Not(this.value[0], this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index:0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			let v1 = this.value[0].getValue();
			if(v1 instanceof BooleanValue) this.rtnv = new BooleanValue(!v1.value, this.loc);
			else throw new RuntimeError(this.first_line, "「でない」は真偽値にしか使えません");
			this.state = 0;
		}
	}
	getCode()
	{
		let v1 = this.value[0];
		let c1 = constructor_name(v1);
		let brace1 = false;
	//	if(c2 == "And" || c2 == "Or" || c2 == "Not") brace2 = true;
		return (brace1 ? '(' : '') + v1.getCode() + (brace1 ? ')' : '')
			+ 'でない';
	}
	getValue()
	{
		return this.rtnv;
	}
}

class BitAnd extends Value
{
	constructor(x, y, loc)
	{
		super([x,y], loc);
		this.rtnv = null;
		this.state = 0;
	}
	clone()
	{
		var rtnv = new BitAnd(this.value[0], this.value[1], this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index:0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			let v1 = this.value[0].getValue(), v2 = this.value[1].getValue();
			if(v1 instanceof ArrayValue || v2 instanceof ArrayValue) throw new RuntimeError(this.first_line, "配列のビット積はできません");
			else if(v1 instanceof StringValue || v2 instanceof StringValue) throw new RuntimeError(this.first_line, "文字列のビット積はできません");
			else if(v1 instanceof BooleanValue && v2 instanceof BooleanValue) this.rtnv = new BooleanValue(v1.value & v2.value, this.loc);
			else if(v1 instanceof FloatValue || v2 instanceof FloatValue) throw new RuntimeError(this.first_line, "実数のビット積はできません");
			else
			{
				if(v1 instanceof BooleanValue) v1 = new IntValue(v1.value ? 1 : 0, this.loc);
				if(v2 instanceof BooleanValue) v2 = new IntValue(v2.value ? 1 : 0, this.loc);
				this.rtnv = new IntValue(v1.value & v2.value, this.loc);
			}
			this.state = 0;
		}
	}
	getCode()
	{
		let v1 = this.value[0], v2 = this.value[1];
		let c1 = constructor_name(v1), c2 = constructor_name(v2);
		let brace1 = false, brace2 = false;
		if(c1 == "Minus" || c1 == "BitRShift" || c1 == "BitLShift") brace1 = true;
		if(c2 == "Minus" || c2 == "BitRShift" || c2 == "BitLShift") brace2 = true;
		return (brace1 ? '(' : '') + v1.getCode() + (brace1 ? ')' : '')
			+ '&'
			+ (brace2 ? '(' : '') + v2.getCode() + (brace2 ? ')' : '')
	}
	getValue()
	{
		return this.rtnv;
	}
}

class BitOr extends Value
{
	constructor(x, y, loc)
	{
		super([x,y], loc);
		this.rtnv = null;
		this.state = 0;
	}
	clone()
	{
		var rtnv = new BitOr(this.value[0], this.value[1], this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index:0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			let v1 = this.value[0].getValue(), v2 = this.value[1].getValue();
			if(v1 instanceof ArrayValue || v2 instanceof ArrayValue) throw new RuntimeError(this.first_line, "配列のビット和はできません");
			else if(v1 instanceof StringValue || v2 instanceof StringValue) throw new RuntimeError(this.first_line, "文字列のビット和はできません");
			else if(v1 instanceof BooleanValue && v2 instanceof BooleanValue) this.rtnv = new BooleanValue(v1.value & v2.value, this.loc);
			else if(v1 instanceof FloatValue || v2 instanceof FloatValue) throw new RuntimeError(this.first_line, "実数のビット和はできません");
			else
			{
				if(v1 instanceof BooleanValue) v1 = new IntValue(v1.value ? 1 : 0, this.loc);
				if(v2 instanceof BooleanValue) v2 = new IntValue(v2.value ? 1 : 0, this.loc);
				this.rtnv = new IntValue(v1.value | v2.value, this.loc);
			}
			this.state = 0;
		}
	}
	getCode()
	{
		let v1 = this.value[0], v2 = this.value[1];
		let c1 = constructor_name(v1), c2 = constructor_name(v2);
		let brace1 = false, brace2 = false;
		if(c1 == "Minus" || c1 == "BitRShift" || c1 == "BitLShift" || c1 == "BitNot") brace1 = true;
		if(c2 == "Minus" || c2 == "BitRShift" || c2 == "BitLShift" || c2 == "BitNot") brace2 = true;
		return (brace1 ? '(' : '') + v1.getCode() + (brace1 ? ')' : '')
			+ '|'
			+ (brace2 ? '(' : '') + v2.getCode() + (brace2 ? ')' : '')
	}
	getValue()
	{
		return this.rtnv;
	}
}

class BitXor extends Value
{
	constructor(x, y, loc)
	{
		super([x,y], loc);
		this.rtnv = null;
		this.state = 0;
	}
	clone()
	{
		var rtnv = new BitXor(this.value[0], this.value[1], this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index: 0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			let v1 = this.value[0].getValue(), v2 = this.value[1].getValue();
			if(v1 instanceof ArrayValue || v2 instanceof ArrayValue) throw new RuntimeError(this.first_line, "配列の排他的ビット和はできません");
			else if(v1 instanceof StringValue || v2 instanceof StringValue) throw new RuntimeError(this.first_line, "文字列の排他的ビット和はできません");
			else if(v1 instanceof BooleanValue && v2 instanceof BooleanValue) this.rtnv = new BooleanValue(v1.value & v2.value, this.loc);
			else if(v1 instanceof FloatValue || v2 instanceof FloatValue) throw new RuntimeError(this.first_line, "実数の排他的ビット和はできません");
			else
			{
				if(v1 instanceof BooleanValue) v1 = new IntValue(v1.value ? 1 : 0, this.loc);
				if(v2 instanceof BooleanValue) v2 = new IntValue(v2.value ? 1 : 0, this.loc);
				this.rtnv = new IntValue(v1.value ^ v2.value, this.loc);
			}
			this.state = 0;
		}
	}
	getCode()
	{
		let v1 = this.value[0], v2 = this.value[1];
		let c1 = constructor_name(v1), c2 = constructor_name(v2);
		let brace1 = false, brace2 = false;
		if(c1 == "Minus" || c1 == "BitRShift" || c1 == "BitLShift" || c1 == "BitNot") brace1 = true;
		if(c2 == "Minus" || c2 == "BitRShift" || c2 == "BitLShift" || c2 == "BitNot") brace2 = true;
		return (brace1 ? '(' : '') + v1.getCode() + (brace1 ? ')' : '')
			+ '^'
			+ (brace2 ? '(' : '') + v2.getCode() + (brace2 ? ')' : '')
	}
	getValue()
	{
		return this.rtnv;
	}
}

class BitNot extends Value
{
	constructor(x, loc)
	{
		super([x], loc);
		this.rtnv = null;
		this.state = 0;
	}
	clone()
	{
		var rtnv = new BitNot(this.value[0], this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index: 0});
			this.state = 0;
		}
		else
		{
			code[0].stack[0].index++;
			let v1 = this.value[0].getValue()
			if(v1 instanceof ArrayValue) throw new RuntimeError(this.first_line, "配列のビット反転はできません");
			else if(v1 instanceof StringValue) throw new RuntimeError(this.first_line, "文字列のビット反転はできません");
			else if(v1 instanceof BooleanValue) this.rtnv = new BooleanValue(!v1.value, this.loc);
			else if(v1 instanceof FloatValue) throw new RuntimeError(this.first_line, "実数のビット反転はできません");
			else
			{
				this.rtnv = new IntValue(~v1.value, this.loc);
			}
			this.state = 0;
		}
	}
	getCode()
	{
		let v1 = this.value[0];
		let c1 = constructor_name(v1);
		let brace1 = false;
		if(c1 == "Minus" || c1 == "BitRShift" || c1 == "BitLShift") brace1 = true;
		return '~' + (brace1 ? '(' : '') + v1.getCode() + (brace1 ? ')' : '');
	}
	getValue()
	{
		return this.rtnv;
	}
}

class BitLShift extends Value
{
	constructor(x, y, loc)
	{
		super([x,y], loc);
		this.rtnv = null;
		this.state = 0;
	}
	clone()
	{
		var rtnv = new BitLShift(this.value[0], this.value[1], this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index: 0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			let v1 = this.value[0].getValue(), v2 = this.value[1].getValue();
			if(v1 instanceof ArrayValue || v2 instanceof ArrayValue) throw new RuntimeError(this.first_line, "配列のビットシフトはできません");
			else if(v1 instanceof StringValue || v2 instanceof StringValue) throw new RuntimeError(this.first_line, "文字列のビットシフトはできません");
			else if(v1 instanceof FloatValue || v2 instanceof FloatValue) throw new RuntimeError(this.first_line, "実数のビットシフトはできません");
			else
			{
				if(v1 instanceof BooleanValue) v1 = new IntValue(v1.value ? 1 : 0, this.loc);
				if(v2 instanceof BooleanValue) v2 = new IntValue(v2.value ? 1 : 0, this.loc);
				this.rtnv = new IntValue(v1.value << v2.value, this.loc);
			}
			this.state = 0;
		}
	}
	getCode()
	{
		let v1 = this.value[0], v2 = this.value[1];
		let c1 = constructor_name(v1), c2 = constructor_name(v2);
		let brace1 = false, brace2 = false;
		if(c1 == "Minus" || c1 == "BitNot") brace1 = true;
		if(c2 == "Minus" || c2 == "BitNot") brace2 = true;
		return (brace1 ? '(' : '') + v1.getCode() + (brace1 ? ')' : '')
			+ '<<'
			+ (brace2 ? '(' : '') + v2.getCode() + (brace2 ? ')' : '')
	}
	getValue()
	{
		return this.rtnv;
	}
}

class BitRShift extends Value
{
	constructor(x, y, loc)
	{
		super([x,y], loc);
		this.rtnv = null;
		this.state = 0;
	}
	clone()
	{
		var rtnv = new BitRShift(this.value[0], this.value[1], this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index: 0});
			this.state = 1;
		}
		else
		{
			code[0].state[0].index++;
			let v1 = this.value[0].getValue(), v2 = this.value[1].getValue();
			if(v1 instanceof ArrayValue || v2 instanceof ArrayValue) throw new RuntimeError(this.first_line, "配列のビットシフトはできません");
			else if(v1 instanceof StringValue || v2 instanceof StringValue) throw new RuntimeError(this.first_line, "文字列のビットシフトはできません");
			else if(v1 instanceof FloatValue || v2 instanceof FloatValue) throw new RuntimeError(this.first_line, "実数のビットシフトはできません");
			else
			{
				if(v1 instanceof BooleanValue) v1 = new IntValue(v1.value ? 1 : 0, this.loc);
				if(v2 instanceof BooleanValue) v2 = new IntValue(v2.value ? 1 : 0, this.loc);
				this.rtnv = new IntValue(v1.value >> v2.value, this.loc);
			}
			this.state = 0;
		}
	}
	getCode()
	{
		let v1 = this.value[0], v2 = this.value[1];
		let c1 = constructor_name(v1), c2 = constructor_name(v2);
		let brace1 = false, brace2 = false;
		if(c1 == "Minus" || c1 == "BitNot") brace1 = true;
		if(c2 == "Minus" || c2 == "BitNot") brace2 = true;
		return (brace1 ? '(' : '') + v1.getCode() + (brace1 ? ')' : '')
			+ '>>'
			+ (brace2 ? '(' : '') + v2.getCode() + (brace2 ? ')' : '')
	}
	getValue()
	{
		return this.rtnv;
	}
}



/**
 * @returns boolean
 * @param {ArrayValue} v1 
 * @param {ArrayValue} v2 
 */
 function ArrayCompare(v1, v2)
 {
	 var rtnv = true;
	 if(v1 instanceof ArrayValue && v2 instanceof ArrayValue)
	 {
		 if(v1.length != v2.length) return false;
		 for(let i = 0; i < v1.length; i++) rtnv = rtnv && ArrayCompare(v1.getValue().value[i], v2.getValue().value[i]);
	 }
	 else rtnv = rtnv && typeof v1 == typeof v2 && v1.value == v2.value;
	 return rtnv;
 }
 
class EQ extends Value
{
	constructor(x, y, loc){super([x,y], loc); this.state = 0;}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index: 0});
			this.state = 1;
		}
		else
		{
			let v1 = this.value[0].getValue(), v2 = this.value[1].getValue();
			if(v1 instanceof ArrayValue || v2 instanceof ArrayValue) this.rtnv = new BooleanValue(ArrayCompare(v1, v2), this.loc);
			else this.rtnv = new BooleanValue(v1.value == v2.value, this.loc);
			code[0].stack[0].index++;
			this.state = 0;
		}
	}
	clone()
	{
		var rtnv = new EQ(this.value[0], this.value[1], this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	getCode()
	{
		let v1 = this.value[0], v2 = this.value[1];
		let c1 = constructor_name(v1), c2 = constructor_name(v2);
		let brace1 = false, brace2 = false;
		return (brace1 ? '(' : '') + v1.getCode() + (brace1 ? ')' : '')
			+ ' = '
			+ (brace2 ? '(' : '') + v2.getCode() + (brace2 ? ')' : '')
	}
	getValue()
	{
		return this.rtnv;
	}
}

class NE extends Value
{
	constructor(x, y, loc){super([x,y], loc); this.state = 0;}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index: 0});
			this.state = 1;
		}
		else
		{
			let v1 = this.value[0].getValue(), v2 = this.value[1].getValue();
			if(v1 instanceof ArrayValue || v2 instanceof ArrayValue) this.rtnv = new BooleanValue(!ArrayCompare(v1, v2), this.loc);
			else this.rtnv = new BooleanValue(v1.value != v2.value, this.loc);
			code[0].stack[0].index++;
			this.state = 0;
		}
	}
	clone()
	{
		var rtnv = new NE(this.value[0], this.value[1], this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	getCode()
	{
		let v1 = this.value[0], v2 = this.value[1];
		let c1 = constructor_name(v1), c2 = constructor_name(v2);
		let brace1 = false, brace2 = false;
		return (brace1 ? '(' : '') + v1.getCode() + (brace1 ? ')' : '')
			+ ' != '
			+ (brace2 ? '(' : '') + v2.getCode() + (brace2 ? ')' : '')
	}
	getValue()
	{
		return this.rtnv;
	}
}

class GT extends Value
{
	constructor(x, y, loc){super([x,y], loc); this.state = 0;}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index: 0});
			this.state = 1;
		}
		else
		{
			let v1 = this.value[0].getValue(), v2 = this.value[1].getValue();
			if(v1 instanceof ArrayValue || v2 instanceof ArrayValue) throw new RuntimeError(this.first_line, "配列を比べることはできません")
			this.rtnv = new BooleanValue(v1.value > v2.value, this.loc);
			code[0].stack[0].index++;
			this.state = 0;
		}
	}
	clone()
	{
		var rtnv = new GT(this.value[0], this.value[1], this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	getCode()
	{
		let v1 = this.value[0], v2 = this.value[1];
		let c1 = constructor_name(v1), c2 = constructor_name(v2);
		let brace1 = false, brace2 = false;
		return (brace1 ? '(' : '') + v1.getCode() + (brace1 ? ')' : '')
			+ ' > '
			+ (brace2 ? '(' : '') + v2.getCode() + (brace2 ? ')' : '')
	}
	getValue()
	{
		return this.rtnv;
	}
}

class GE extends Value
{
	constructor(x, y, loc){super([x,y], loc); this.state = 0;}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index: 0});
			this.state = 1;
		}
		else
		{
			let v1 = this.value[0].getValue(), v2 = this.value[1].getValue();
			if(v1 instanceof ArrayValue || v2 instanceof ArrayValue) throw new RuntimeError(this.first_line, "配列を比べることはできません")
			this.rtnv = new BooleanValue(v1.value >= v2.value, this.loc);
			code[0].stack[0].index++;
			this.state = 0;
		}
	}
	clone()
	{
		var rtnv = new GE(this.value[0], this.value[1], this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	getCode()
	{
		let v1 = this.value[0], v2 = this.value[1];
		let c1 = constructor_name(v1), c2 = constructor_name(v2);
		let brace1 = false, brace2 = false;
		return (brace1 ? '(' : '') + v1.getCode() + (brace1 ? ')' : '')
			+ ' >= '
			+ (brace2 ? '(' : '') + v2.getCode() + (brace2 ? ')' : '')
	}
	getValue()
	{
		return this.rtnv;
	}
}

class LT extends Value
{
	constructor(x, y, loc){super([x,y], loc); this.state = 0;}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index: 0});
			this.state = 1;
		}
		else
		{
			let v1 = this.value[0].getValue(), v2 = this.value[1].getValue();
			if(v1 instanceof ArrayValue || v2 instanceof ArrayValue) throw new RuntimeError(this.first_line, "配列を比べることはできません")
			this.rtnv = new BooleanValue(v1.value < v2.value, this.loc);
			code[0].stack[0].index++;
			this.state  = 0;
		}
	}
	clone()
	{
		var rtnv = new LT(this.value[0], this.value[1], this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	getCode()
	{
		let v1 = this.value[0], v2 = this.value[1];
		let c1 = constructor_name(v1), c2 = constructor_name(v2);
		let brace1 = false, brace2 = false;
		return (brace1 ? '(' : '') + v1.getCode() + (brace1 ? ')' : '')
			+ ' < '
			+ (brace2 ? '(' : '') + v2.getCode() + (brace2 ? ')' : '')
	}
	getValue()
	{
		return this.rtnv;
	}
}

class LE extends Value
{
	constructor(x, y, loc){super([x,y], loc); this.state = 0;}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index: 0});
			this.state = 1;
		}
		else
		{
			let v1 = this.value[0].getValue(), v2 = this.value[1].getValue();
			if(v1 instanceof ArrayValue || v2 instanceof ArrayValue) throw new RuntimeError(this.first_line, "配列を比べることはできません")
			this.rtnv = new BooleanValue(v1.value <= v2.value, this.loc);
			code[0].stack[0].index++;
			this.state = 0;
		}
	}
	clone()
	{
		var rtnv = new LE(this.value[0], this.value[1], this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	getCode()
	{
		let v1 = this.value[0], v2 = this.value[1];
		let c1 = constructor_name(v1), c2 = constructor_name(v2);
		let brace1 = false, brace2 = false;
		return (brace1 ? '(' : '') + v1.getCode() + (brace1 ? ')' : '')
			+ ' <= '
			+ (brace2 ? '(' : '') + v2.getCode() + (brace2 ? ')' : '')
	}
	getValue()
	{
		return this.rtnv;
	}
}

class IN extends Value
{
	constructor(x, y, loc)
	{
		super([x,y], loc);
		this.rtnv = null;
		this.state = 0;
	}
	clone()
	{
		var rtnv = new IN(this.value[0], this.value[1], this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index: 0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			let v1 = this.value[0].getValue(), v2 = this.value[1].getValue();
			var flag = false;
			if(v1 instanceof ArrayValue)
				for(let i = 0; i < v1.value.length; i++) flag |= ArrayCompare(v1.value[i], v2);
			else throw new RuntimeError(this.first_line, "\"の中に\"の前には配列が必要です");
			this.rtnv = new BooleanValue(flag, this.loc);
			this.state = 0;
		}
	}
	getCode()
	{
		let v1 = this.value[0], v2 = this.value[1];
		let brace1 = false, brace2 = false;
		return (brace1 ? '(' : '') + v1.getCode() + (brace1 ? ')' : '')
			+ 'の中に'
			+ (brace2 ? '(' : '') + v2.getCode() + (brace2 ? ')' : '')
	}
	getValue()
	{
		return this.rtnv;
	}
}

class NumberOf extends Value
{
	constructor(x, y, loc)
	{
		super([x,y], loc);
		this.rtnv = null;
		this.state = 0;
	}
	clone()
	{
		var rtnv = new NumberOf(this.value[0].clone(), this.value[1].clone(), this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index: 0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			let vt = new varTable();
			let statementlist = [];
			let globalvarTable = varTables[varTables.length - 1];
			for(let v of Object.keys(globalvarTable.vars)) vt.vars[v] = globalvarTable.vars[v].getValue().clone();
			for(let v of Object.keys(varTables[0].vars)) vt.vars[v] = varTables[0].vars[v].getValue().clone();
			// 空リストを'!'という変数に代入する。カウンタは'!!'
			let var1 = new Variable('!', null, this.loc);
			let var2 = new Variable('!!', null, this.loc);
			statementlist.push(new Assign(var1, new ArrayValue([], this.loc), null, this.loc));
			statementlist.push(new ForInc(var2, new IntValue(1, this.loc),this.value[0].getValue(), new IntValue(1, this.loc),
				[this.value[1], new Append(var1, this.value[1], this.loc)], this.loc));
			// statementlist.push(new runBeforeGetValue([var1], this.loc));
			statementlist.push(var1);
			statementlist.push(new ReturnStatement(var1, this.loc));
	
			setCaller(statementlist, this);
			code.unshift(new parsedFunction(statementlist));
			varTables.unshift(vt);
			this.state = 0;
		}
	}
	getCode()
	{
		let v1 = this.value[0], v2 = this.value[1];
		let brace1 = false, brace2 = false;
		return (brace1 ? '(' : '') + v1.getCode() + (brace1 ? ')' : '')
			+ '個の'
			+ (brace2 ? '(' : '') + v2.getCode() + (brace2 ? ')' : '')
	}
	getValue()
	{
		return this.rtnv;
	}
	setValue(v)
	{
		this.rtnv = v.clone();
	}
}

class Variable extends Value
{
	/**
	 * 
	 * @param {string} x 
	 * @param {ArrayValue} y 
	 * @param {Location} loc 
	 */
	constructor(x, y, loc)
	{
		super([x,y],loc); 
		this.rtnv = null;
		this.state = 0;
	}
	clone()
	{
		var rtnv = new Variable(this.value[0], this.value[1] ? this.value[1].clone() : null, this.loc);
		rtnv.rtnv = this.rtnv;
		return rtnv;
	}
	get varname(){return this.value[0];}
	get args(){return this.value[1];}
	run()
	{
		if(this.state == 0)
		{
			if(this.args) code[0].stack.unshift({statementlist: this.args.value, index: 0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			let vn = this.varname;		// 変数名
			let vt = findVarTable(vn);	// 変数は定義されてるか
			if(vt)
			{
				let v = vt.vars[vn];
				this.rtnv = getValueByArgs(v, this.args ? this.args.value : null, this.loc);
			}
			else throw new RuntimeError(this.first_line, "変数" + this.varname + "が定義されていません");
			this.state = 0;
		}
	}
	getCode()
	{
		let vn = this.value[0];
		let pm = this.value[1];
		if(pm != null)
		{
			let ag = new Array(pm.length);
			for(let i = 0; i < pm.length; i++)
			{
				ag[i] = pm.value[i].getCode();
			}
			vn += '['+ag.join(',')+']';
		}
		return vn;
	}
	getValue()
	{
		return this.rtnv;
	}
	append(a)
	{
		if(this.args) this.args.append(a);
		else this.value[1] = new ArrayValue(a, this.loc);
	}
}

/**
 * 定義済み関数クラス
 */
 class DefinedFunction
 {
	 /**
	  * @constructor
	  * @param {number} argc 引数の個数
	  * @param {function} func 実際の関数
	  */
	 constructor(argc, func) { 
		 this.argc = argc; this.func = func;
		 this.caller = null;
		 this.loc = null;
	 }
	 /**
	  * 関数の値を返す
	  * @param {Array<Value>} parameters 
	  * @param {Location} loc 
	  * @returns {any}
	  */
	 run()
	 {
		 if((this.argc instanceof Array && this.argc[0] <= this.parameters.length && this.argc[1] >= this.parameters.length)
			 || this.parameters.length == this.argc)
			 {
				 code[0].stack[0].index++;
				 this.caller.setValue(this.func(this.parameters, this.loc));
				 code.shift();
			 }
		 else throw new RuntimeError(this.loc.first_line, "引数の個数が違います");
	 }
	 clone()
	 {
		 return new DefinedFunction(this.argc, this.func);
	 }
	 setCaller(caller)
	 {
		 this.caller = caller;
	 }
	 setParameter(params)
	 {
		 this.parameters = params;
	 }
	 setLocation(loc)
	 {
		 this.loc = loc;
	 }
 }
 
/**
 * 定義済み関数一覧
 */
 var definedFunction = {
	"keys": new DefinedFunction(1, function(param, loc){
		var par1 = param[0].getValue();
		if(par1 instanceof DictionaryValue)
		{
			var args = [];
			var keys = Object.keys(par1.value);
			keys.sort();
			for(let i = 0; i < keys.length; i++) args.push(new StringValue(keys[i], loc));
			return new ArrayValue(args, this.loc);
		}
		else throw new RuntimeError(loc.first_line, 'keysは辞書にしか使えません');
	}),
	"abs": new DefinedFunction(1, function (param, loc){
		var par1 = param[0].getValue();
		if(par1 instanceof IntValue) return new IntValue(Math.abs(par1.value), loc);
		else if(par1 instanceof FloatValue) return new FloatValue(Math.abs(par1.value), loc);
		else throw new RuntimeError(loc.first_line, "absは数値にしか使えません");
	}),
	"random": new DefinedFunction([0,1], function(param, loc){
		if(param.length == 0) return new FloatValue(Math.random(), this.loc);
		else{
			var par1 = param[0].getValue();
			if(par1 instanceof IntValue) return new IntValue(Math.floor(Math.random() * Math.floor(par1.value + 1)), this.loc);
			else throw new RuntimeError(loc.first_line, "randomは整数にしか使えません");
		} 
	}),
	"ceil": new DefinedFunction(1, function(param, loc){
		var par1 = param[0].getValue();
		if(par1 instanceof IntValue) return par1;
		else if(par1 instanceof FloatValue) return new IntValue(Math.ceil(par1.value), this.loc);
		else throw new RuntimeError(loc.first_line, "ceilは数値にしか使えません");
	}),
	"floor": new DefinedFunction(1, function(param, loc){
		var par1 = param[0].getValue();
		if(par1 instanceof IntValue) return par1;
		else if(par1 instanceof FloatValue) return new IntValue(Math.floor(par1.value), this.loc);
		else throw new RuntimeError(loc.first_line, "floorは数値にしか使えません");
	}),
	"round": new DefinedFunction(1, function(param, loc){
		var par1 = param[0].getValue();
		if(par1 instanceof IntValue) return par1;
		else if(par1 instanceof FloatValue) return new IntValue(Math.round(par1.value), this.loc);
		else throw new RuntimeError(loc.first_line, "roundは数値にしか使えません");
	}),
	"sin": new DefinedFunction(1, function(param, loc){
		var par1 = param[0].getValue();
		if(par1 instanceof IntValue || par1 instanceof FloatValue)
			return new FloatValue(Math.sin(par1.value), this.loc);
		else throw new RuntimeError(loc.first_line, "sinは数値にしか使えません");
	}),
	"cos": new DefinedFunction(1, function(param, loc){
		var par1 = param[0].getValue();
		if(par1 instanceof IntValue || par1 instanceof FloatValue)
			return new FloatValue(Math.cos(par1.value), this.loc);
		else throw new RuntimeError(loc.first_line, "cosは数値にしか使えません");
	}),
	"tan": new DefinedFunction(1, function(param, loc){
		var par1 = param[0].getValue();
		if(par1 instanceof IntValue || par1 instanceof FloatValue)
		{
			let v = Math.tan(par1.value);
			if(isFinite(v)) return new FloatValue(v, this.loc);
			else throw new RuntimeError(loc.first_line, "オーバーフローしました");
		}
		else throw new RuntimeError(loc.first_line, "tanは数値にしか使えません");
	}),
	"asin": new DefinedFunction(1, function(param, loc){
		var par1 = param[0].getValue();
		if(par1 instanceof IntValue || par1 instanceof FloatValue)
		{
			if(par1.value > 1.0 || par1.value < -1.0)
				throw new RuntimeError(loc.first_line, "asinの定義域外の値が使われました");
			else
				return new FloatValue(Math.asin(par1.value), this.loc);
		}
		else throw new RuntimeError(loc.first_line, "asinは数値にしか使えません");
	}),
	"acos": new DefinedFunction(1, function(param, loc){
		var par1 = param[0].getValue();
		if(par1 instanceof IntValue || par1 instanceof FloatValue)
		{
			if(par1.value > 1.0 || par1.value < -1.0)
				throw new RuntimeError(loc.first_line, "acosの定義域外の値が使われました");
			else
				return new FloatValue(Math.acos(par1.value), this.loc);
		}
		else throw new RuntimeError(loc.first_line, "acosは数値にしか使えません");
	}),
	"atan": new DefinedFunction(1, function(param, loc){
		var par1 = param[0].getValue();
		if(par1 instanceof IntValue || par1 instanceof FloatValue)
			return new FloatValue(Math.atan(par1.value), this.loc);
		else throw new RuntimeError(loc.first_line, "atanは数値にしか使えません");
	}),
	"atan2": new DefinedFunction(2, function(param, loc){
		var par1 = param[0].getValue();
		var par2 = param[1].getValue();
		if((par1 instanceof IntValue || par1 instanceof FloatValue) && 
		   (par2 instanceof IntValue || par2 instanceof FloatValue))
			return new FloatValue(Math.atan2(par1.value, par2.value), this.loc);
		else throw new RuntimeError(loc.first_line, "atan2は数値にしか使えません");
	}),
	"sqrt": new DefinedFunction(1, function(param, loc){
		var par1 = param[0].getValue();
		if(par1 instanceof IntValue || par1 instanceof FloatValue)
		{
			if(par1.value < 0) throw new RuntimeError(loc.first_line, "負の数のルートを求めようとしました");
			 return new FloatValue(Math.sqrt(par1.value), this.loc);
		}
		else throw new RuntimeError(this.first_line, "sqrtは数値にしか使えません");
	}),
	"log": new DefinedFunction(1, function(param, loc){
		var par1 = param[0].getValue();
		if(par1 instanceof IntValue || par1 instanceof FloatValue)
		{
			if(par1.value <= 0) throw new RuntimeError(loc.first_line, "正でない数の対数を求めようとしました");
			let v = Math.log(par1.value);
			if(isFinite(v)) return new FloatValue(v, this.loc);
			throw new RuntimeError(this.first_line, "オーバーフローしました");
		}
		else throw new RuntimeError(loc.first_line, "logは数値にしか使えません");
	}),
	"exp": new DefinedFunction(1, function(param, loc){
		var par1 = param[0].getValue();
		if(par1 instanceof IntValue || par1 instanceof FloatValue)
		{
			let v = Math.exp(par1.value);
			if(isFinite(v)) return new FloatValue(v, this.loc);
			throw new RuntimeError(loc.first_line, "オーバーフローしました");
		}
		else throw new RuntimeError(loc.first_line, "expは数値にしか使えません");
	}),
	"pow": new DefinedFunction(2, function(param, loc){
		var par1 = param[0].getValue();
		var par2 = param[1].getValue();
		if(par1 instanceof IntValue && par2 instanceof IntValue && par2.value >= 0)
		{
			if(par1.value == 0 && par2.value <= 0) throw new RuntimeError(loc.first_line, "0は正の数乗しかできません");
			let v = Math.pow(par1.value, par2.value);
			if(isSafeInteger(v)) return new IntValue(v, this.loc);
			else throw new RuntimeError(loc.first_line, "整数で表せる範囲を越えました");
		}
		else if((par1 instanceof IntValue || par1 instanceof FloatValue) &&
			(par2 instanceof IntValue || par2 instanceof FloatValue))
		{
			if(par1.value < 0 && !Number.isInteger(par2.value)) throw new RuntimeError(loc.first_line, "負の数の非整数乗はできません");
			if(par1.value == 0 && par2.value <= 0) throw new RuntimeError(loc.first_line, "0は正の数乗しかできません");
			let v = Math.pow(par1.value, par2.value);
			if(isFinite(v)) return new FloatValue(v, this.loc);
			else throw new RuntimeError(loc.first_line, "オーバーフローしました");
		}
		else throw new RuntimeError(loc.first_line, "powerは数値にしか使えません");
	}),
	"length": new DefinedFunction(1, function(param, loc){
		var par1 = param[0].getValue();
		if(par1 instanceof StringValue) return new IntValue(par1.value.length, this.loc);
		else if(par1 instanceof ArrayValue) return new IntValue(par1.length, this.loc);
		else throw new RuntimeError(loc.first_line, "lengthは文字列と配列にしか使えません");
	}),
	"substring": new DefinedFunction([2,3], function(param, loc){
		var par1 = param[0].getValue();
		var par2 = param[1].getValue();
		var par3 = param.length == 3 ? param[2].getValue() : null;
		if(par1 instanceof StringValue &&
			par2 instanceof IntValue &&
			(par3 == null || par3 instanceof IntValue))
		{
			var v;
			if(par3 == null) v = par1.value.substr(par2.value);
			else v = par1.value.substr(par2.value, par3.value);
			return new StringValue(v, this.loc);
		}
		else throw new RuntimeError(loc.first_line, "substringの引数の型が違います");
	}),
	"append": new DefinedFunction(2, function(param, loc){
		var par1 = param[0].getValue();
		var par2 = param[1].getValue();
		if(par2 instanceof StringValue && par2 instanceof StringValue)
		{
			return new StringValue(par1.value + par2.value, this.loc);
		}
		else throw new RuntimeError(loc.first_line, "appendの引数の型が違います");
	}),
	"split": new DefinedFunction([1,2], function(param, loc){
		var par1 = param[0].getValue();
		var par2 = param.length == 2 ? param[1].getValue() : null;
		if(par1 instanceof StringValue && (par2 instanceof StringValue || par2 == null))
		{
			var v1 = par1.value;
			var v = par2 ? v1.split(par2.value) : v1.split("");
			var vr = [];
			for(var i = 0; i < v.length; i++) vr.push(new StringValue(v[i], this.loc));
			return new ArrayValue(vr, this.loc);
		}
		else throw new RuntimeError(loc.first_line, "splitの引数の型が違います");
	}),
	"extract": new DefinedFunction(3, function(param, loc){
		var par1 = param[0].getValue();
		var par2 = param[1].getValue();
		var par3 = param[2].getValue();
		if(par1 instanceof StringValue && par2 instanceof StringValue && par3 instanceof IntValue)
		{
			var v1 = par1.value;
			var v2 = par2.value;
			var v3 = par3.value;
			var v = v1.split(v2);
			if(v3 >= 0 && v3 < v.length) return new StringValue(v[v3], this.loc);
			else throw new RuntimeError(loc.first_line, "番号の値が不正です");
		}
		else throw new RuntimeError(loc.first_line, "extractの引数の型が違います");
	}),
	"insert": new DefinedFunction(3, function(param, loc){
		var par1 = param[0].getValue();
		var par2 = param[1].getValue();
		var par3 = param[2].getValue();
		if(par1 instanceof StringValue && par2 instanceof IntValue && par3 instanceof StringValue)
		{
			var v1 = par1.value;
			var v2 = par2.value;
			var v3 = par3.value;
			if(v2 < 0 || v2 > v1.length) throw new RuntimeError(loc.first_line, "位置の値が不正です");
			var s1 = v1.substr(0, v2);
			var s2 = v1.substr(v2);
			return new StringValue(s1 + v3 + s2, this.loc);
		}
		else throw new RuntimeError(loc.first_line, "insertの引数の型が違います");
	}),
	"replace": new DefinedFunction(4, function(param, loc){
		var par1 = param[0].getValue();
		var par2 = param[1].getValue();
		var par3 = param[2].getValue();
		var par4 = param[3].getValue();
		if(par1 instanceof StringValue && par2 instanceof IntValue && par3 instanceof IntValue && par4 instanceof StringValue)
		{
			var v1 = par1.value;
			var v2 = par2.value;
			var v3 = par3.value;
			var v4 = par4.value;

			if(v2 < 0 || v2 > v1.length) throw new RuntimeError(loc.first_line, "位置の値が不正です");
			if(v3 < 0 || v2 + v3 > v1.length)throw new RuntimeError(loc.first_line, "長さの値が不正です");
			var s1 = v1.substr(0, v2);
			var s2 = v1.substr(v2 + v3);
			return new StringValue(s1 + v4 + s2, this.loc);
		}
		else throw new RuntimeError(loc.first_line, "replaceの引数の型が違います");
	}),
	"isfile": new DefinedFunction(1, function(param, loc){
		var par = param[0].getValue();
		if(par instanceof StringValue) return new BooleanValue(storage.getItem(par.value) != null, loc);
		else throw new RuntimeError(loc.first_line, "ファイル名は文字列でなくてはいけません");
	}),
	"openr": new DefinedFunction(1, function(param, loc){
		var par = param[0].getValue();
		if(par instanceof StringValue) return new IntValue(filesystem.openr(par.value), loc);
		else throw new RuntimeError(loc.first_line, "ファイル名は文字列でなくてはいけません");
	}),
	"openw": new DefinedFunction(1, function(param, loc){
		var par = param[0].getValue();
		if(par instanceof StringValue) return new IntValue(filesystem.openw(par.value), loc);
		else throw new RuntimeError(loc.first_line, "ファイル名は文字列でなくてはいけません");
	}),
	"opena": new DefinedFunction(1, function(param, loc){
		var par = param[0].getValue();
		if(par instanceof StringValue) return new IntValue(filesystem.opena(par.value), loc);
		else throw new RuntimeError(loc.first_line, "ファイル名は文字列でなくてはいけません");
	}),
	"getline": new DefinedFunction(1, function(param, loc){
		var par1 = param[0].getValue();
		if(par1 instanceof IntValue)
		{
			var rtnv = filesystem.read_line(par1.value);
			if(rtnv == null) throw new RuntimeError(loc.first_line, "ファイル番号が不正です");
			return new StringValue(rtnv, loc);
		}
		else throw new RuntimeError(loc.first_line, "ファイル番号が必要です");
	}),
	"getchar": new DefinedFunction(1, function(param, loc){
		var par1 = param[0].getValue();
		if(par1 instanceof IntValue)
		{
			var rtnv = filesystem.read_ch(par1.value);
			if(rtnv == null) throw new RuntimeError(loc.first_line, "ファイル番号が不正です");
			return new StringValue(rtnv, loc);
		}
		else throw new RuntimeError(loc.first_line, "ファイル番号が必要です");
	}),
};

function setCaller(statementlist, caller)
{
	for(let i = 0; i < statementlist.length; i++)
	{
		if(statementlist[i].statementlist) setCaller(statementlist[i].statementlist, caller);
		if(statementlist[i].state) setCaller(statementlist[i].state, caller);
		if(statementlist[i].state1) setCaller(statementlist[i].state1, caller);
		if(statementlist[i].state2) setCaller(statementlist[i].state2, caller);
		if(statementlist[i] instanceof ReturnStatement) statementlist[i].setCaller(caller, true);
	}
}


function cloneStatementlist(statementlist)
{
	var rtnv = [];
	for(let i = 0; i < statementlist.length; i++) rtnv.push(statementlist[i].clone());
	return rtnv;
}

/**
 * 関数呼び出し
 */
 class CallFunction extends Value
 {
	 /**
	  * @constructor
	  * @param {string} funcname 
	  * @param {Array<Value>} parameter 
	  * @param {Location} loc 
	  */
	 constructor(funcname, parameter, loc)
	 {
		 super([funcname, parameter], loc);
		 this.rtnv = null;
		 this.state = 0;
	 }
	 clone()
	 {
		 var rtnv = new CallFunction(this.value[0], this.value[1], this.loc);
		 rtnv.rtnv = this.rtnv;
		 return rtnv;
	 }
	 run()
	 {
		 if(this.state == 0)
		 {
			 code[0].stack.unshift({statementlist: this.value[1], index: 0});
			 this.state = 1;
		 }
		 else if(this.state == 1)
		 {
			 code[0].stack[0].index++;
			 const func = this.value[0], param = this.value[1];
			 if(definedFunction[func])
			 {
				 let fn = definedFunction[func].clone();
				 fn.setCaller(this);
				 fn.setParameter(param);
				 fn.setLocation(this.loc);
				 let statementlist = [fn];
				 code.unshift(new parsedFunction(statementlist));
			 }
			 else if(myFuncs[func])
			 {
				 let fn = myFuncs[func];
				 let vt = new varTable();
				 let globalVarTable = varTables[varTables.length - 1];
				 for(let i of Object.keys(globalVarTable.vars)) vt.vars[i] = globalVarTable.vars[i].getValue().clone();
				 for(let i = 0; i < fn.params.length; i++) vt.vars[fn.params[i].varname] = param[i].getValue().clone();
				 let statementlist = cloneStatementlist(fn.statementlist);
				 setCaller(statementlist, this);
				 statementlist.push(new notReturnedFunction(fn.loc));
				 let pf = new parsedFunction(statementlist);
				 code.unshift(pf);
				 varTables.unshift(vt);
			 }
			 else
				 throw new RuntimeError(this.first_line, '関数 '+func+' は定義されていません');
			 this.state = 0;
		 }
	 }
	 setValue(v)
	 {
		 this.rtnv = v.clone();
	 }
	 getValue()
	 {
		 return this.rtnv;
	 }
	 getCode()
	 {
		 let func = this.value[0], param = this.value[1];
		 let ag = [];
		 for(let i = 0; i < param.length; i++)
			 ag.push(param[i].getCode());
		 return func + '(' + ag.join(',') + ')';
	 }
 }
 
 class Connect extends Value
 {
	 constructor(x,y,loc)
	 {
		 super([x,y],loc);
		 this.state = 0;
	 }
	 clone()
	 {
		 var rtnv = new Connect(this.value[0].clone(), this.value[1].clone(), this.loc);
		 rtnv.rtnv = this.rtnv;
		 return rtnv;
	 }
	 run()
	 {
		 if(this.state == 0)
		 {
			 code[0].stack.unshift({statementlist: this.value, index: 0});
			 this.state = 1;
		 }
		 else
		 {
			 code[0].stack[0].index++;
			 let v1 = array2text(this.value[0].getValue());
			 let v2 = array2text(this.value[1].getValue());
			 let v = v1 + v2;
			 this.rtnv = new StringValue(v, this.loc);
			 this.state = 0;
		 }
	 }
	 getCode()
	 {
		 return this.value[0].getCode() + "と" + this.value[1].getCode();
	 }
	 getValue()
	 {
		 return this.rtnv;
	 }
 }
 
 class SliceValue extends Value
 {
	 constructor(x,y,loc)
	 {
		 super([x,y],loc);
		 this.state = 0;
	 }
	 clone()
	 {
		 var rtnv = new SliceValue(this.value[0].clone(), this.value[1].clone(), this.loc);
		 rtnv.rtnv = this.rtnv;
		 return rtnv;
	 }
	 run()
	 {
		 if(this.state == 0)
		 {
			 code[0].stack.unshift({statementlist: this.value, index: 0});
			 this.state = 1;
		 }
		 else
		 {
			 code[0].stack[0].index++;
			 this.state = 0;
		 }
	 }
	 getCode()
	 {
		 return this.value[0].getCode() + ":" + this.value[1].getCode();
	 }
	 getValue()
	 {
		 return this;
	 }
	 getValue1()
	 {
		 return this.value[0];
	 }
	 getValue2()
	 {
		 return this.value[1];
	 }
 }
 
/**
 * 命令クラス
 */
 class Statement
 {
	 /**
	  * @constructor
	  * @param {Location} loc 
	  */
	 constructor(loc)
	 {
		 this._loc = loc;
		 this.state = 0;
	 }
	 get first_line() {return this._loc.first_line;}
	 get last_line() {return this._loc.last_line;}
	 get loc(){return this._loc;}
	 run(){throw new RuntimeError(this.first_line, "これを呼んではいけない");}
	 /**
	  * 
	  * @param {number} indent 
	  */
	 clone()
	 {
		 throw new RuntimeError(this.first_line, constructor_name(this) + "はcloneが作られていません");
	 }
 }
 
/**
 * 手続き定義クラス
 */
 class DefineStep extends Statement {
	/**
	 * @constructor
	 * @param {string} funcName 
	 * @param {Array<Value>} params 
	 * @param {Array<Statement>} statementlist 
	 * @param {Location} loc 
	 */
	constructor(funcName, params, statementlist, loc) {
    	super(loc);
    	if (definedFunction[funcName]) throw new RuntimeError(this.first_line, '手続き '+funcName+' と同名の標準関数が存在します');
    	if (myFuncs[funcName]) throw new RuntimeError(this.first_line, '手続き '+funcName+' と同名の関数、または手続きが既に定義されています');
		this.params = params;
		this.statementlist = statementlist;
		this.funcName = funcName;
		myFuncs[funcName] = this;
	}
	run()
	{
		code[0].stack[0].index++;
	}
}

/**
 * 手続き呼び出しが終わった後の処理
 */
 class afterCallStep
 {
	 run()
	 {
		 varTables.shift();
		 code.shift();
	 }
 }
 
/**
 * 手続き呼び出し
 */
 class CallStep extends Statement {
	constructor(funcName, args, loc) {
	  super(loc);
	  this.funcName = funcName;
	  this.args = args;
  }
  clone()
  {
	  return new CallStep(this.funcName, this.args.clone(), this.loc);
  }  
   run() {
	  if(this.state == 0)
	  {
		  code[0].stack.unshift({statementlist: this.args, index: 0});
		  this.state = 1;
	  }
	  else
	  {
		  code[0].stack[0].index++;
		  const fn = this.funcName
		  const args = this.args;
		  if(myFuncs[fn])
		  {
			  let vt = new varTable();
			  let globalVarTable = varTables[varTables.length - 1];
			  for(let i of Object.keys(globalVarTable.vars))
				  vt.vars[i] = globalVarTable.vars[i].getValue().clone();
			  for(let i = 0; i < myFuncs[fn].params.length; i++)
				  vt.vars[myFuncs[fn].params[i].varname] = args[i].getValue().clone();
			  let statementlist = myFuncs[fn].statementlist.concat();
			  // TODO 呼ばれる保証がない
			  statementlist.push(new afterCallStep());
			  code.unshift(new parsedStep(statementlist));
			  varTables.unshift(vt);
		  }
		  else
			  throw new RuntimeError(this.first_line, '手続き '+fn+' は定義されていません');
		  this.state = 0;
	  }
  }
}

class ExitStatement extends Statement {
	constructor(loc) {
 		super(loc);
	}
	clone()
	{
		return new ExitStatement(this.loc);
	}
	run() {
		if(code[0] instanceof parsedStep)
		{
			code.shift();
			varTables.shift();
		}
		else throw new RuntimeError(this.first_line, "手続きの中ではありません");
	}
}

class DefineFunction extends Statement {
	constructor(funcName, params, statementlist, loc) {
		super(loc);
		if (definedFunction[funcName]) throw new RuntimeError(this.first_line, '関数 '+funcName+' と同名の標準関数が存在します');
		if (myFuncs[funcName]) throw new RuntimeError(this.first_line, '関数 '+funcName+' と同名の関数、または手続きが既に定義されています');
		this.params = params;
		this.funcName = funcName;
		myFuncs[funcName] = this;
		this.statementlist = statementlist;
	}
	clone()
	{
		throw new RuntimeError(this.first_line, 'これはクローンされるべきでない');
	}
	run() {
		code[0].stack[0].index++;
	}
}

/**
 * 関数から値を返す
 */
 class ReturnStatement extends Statement {
	constructor(value, loc) {
		super(loc);
		this.value = value.clone();
		this.caller = null;
		this.flag = false;
		this.state = 0;
	}
	clone()
	{
		let rtnv = new ReturnStatement(this.value.clone(), this.loc);
		rtnv.caller  = this.caller;
		rtnv.flag  = this.flag;
		return rtnv;
	}
	setCaller(caller, flag)
	{
		this.caller = caller;
		this.flag = flag;
	}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: [this.value], index: 0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			if(code[0] instanceof parsedFunction)
			{
				this.caller.setValue(this.value.getValue());
				code.shift();
				if(this.flag) varTables.shift();
			}
			else throw new RuntimeError(this.first_line, "関数の中ではありません");
			this.state = 0;
		}
	}
}

class notReturnedFunction extends Statement {
	constructor(loc){super(loc);}
	run()
	{
		throw new RuntimeError(this.last_line, "関数が値を返さずに終了しました");
	}
}

function dump()
{
	textareaAppend("*** 変数確認 ***\n");
	var vars = varTables[0].varnames([]);
	if(varTables.length > 1) vars = varTables[varTables.length - 1].varnames(vars);
	for(var i = 0; i < vars.length; i++)
	{
		if(vars[i][0] == '!') continue;
		let vartable = findVarTable(vars[i]);
		let v = vartable.vars[vars[i]];
		textareaAppend(vars[i] + ":" + array2code(v) + "\n");
	}
}


class DumpStatement extends Statement
{
	constructor(loc){super(loc);}
	clone()
	{
		return new DumpStatement(this.loc);
	}
	run()
	{
		dump();
		code[0].stack[0].index++;
	}
}

/**
 * ArrayValueを文字列表現にする
 * @param {ArrayValue} args 
 * @returns {string}
 */
function argsString(args)
{
	if(args instanceof ArrayValue)
	{
		let a = [];
		for(let i = 0; i < args.value.length; i++) a.push(args.value[i].getValue().value);
		return '[' + a.join(',') + ']';
	}
	return '';
}

class Assign extends Statement
{
	/**
	 * @constructor
	 * @param {Variable} variable 
	 * @param {Value} value 
	 * @param {String} operator
	 * @param {Location} loc
	 */
	constructor(variable,value, operator, loc)
	{
		super(loc);
		if(!(variable instanceof Variable || variable instanceof UNDEFINED)) throw new RuntimeError(loc.first_line, "変数でないものに代入はできません");
		this.variable = variable;
		this.value = value;
		this.operator = operator;
		this.state = 0;
	}
	clone()
	{
		return new Assign(this.variable.clone(), this.value.clone(),this.operator, this.loc);
	}
	run()
	{
		if(this.variable instanceof UNDEFINED) throw new RuntimeError(this.first_line, "未完成のプログラムです");
		if(this.state == 0)
		{
			let a=[];
			if(this.operator) a.push(this.variable);
			else if(this.variable.args) a = a.concat(this.variable.args.value);
			a.push(this.value);
			code[0].stack.unshift({statementlist: a, index: 0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			let vn = this.variable.varname;
			let ag = this.variable.args;
			let vl = this.value.getValue();
			let vt = findVarTable(vn);
			if(vt) // 変数が定義されている
			{
				let va = vt.vars[vn];
				if(this.operator)
				{
					va = getValueByArgs(va, ag ? ag.value : null, this.loc);
					let v1 = va.getValue(), v2 = vl, v3 = null;
					switch(this.operator)
					{
					case '+':
						if(v1 instanceof BooleanValue) v1 = new IntValue(v1.value ? 1 : 0, this.loc);
						if(v2 instanceof BooleanValue) v2 = new IntValue(v2.value ? 1 : 0, this.loc);
						if(v1 instanceof ArrayValue || v2 instanceof ArrayValue) throw new RuntimeError(this.first_line, "配列の足し算はまだサポートしていません");
						else if(v1 instanceof DictionaryValue || v2 instanceof DictionaryValue) throw new RuntimeError(this.first_line, "辞書の足し算はまだサポートしていません");
						else if(v1 instanceof StringValue || v2 instanceof StringValue) v3 = new StringValue(String(v1.value) + String(v2.value), this.loc);
						else if(v1 instanceof IntValue && v2 instanceof IntValue) v3 = new IntValue(v1.value + v2.value, this.loc);
						else if(v1 instanceof FloatValue || v2 instanceof FloatValue) v3 = new FloatValue(v1.value + v2.value, this.loc);
						break;
					case '-':
						if(v1 instanceof BooleanValue) v1 = new IntValue(v1.value ? 1 : 0, this.loc);
						if(v2 instanceof BooleanValue) v2 = new IntValue(v2.value ? 1 : 0, this.loc);
						if(v1 instanceof ArrayValue || v2 instanceof ArrayValue) throw new RuntimeError(this.first_line, "配列の引き算はできません");
						else if(v1 instanceof DictionaryValue || v2 instanceof DictionaryValue) throw new RuntimeError(this.first_line, "辞書の引き算はできません");
						else if(v1 instanceof StringValue || v2 instanceof StringValue) throw new RuntimeError(this.first_line, "文字列の引き算はできません");
						else if(v1 instanceof IntValue && v2 instanceof IntValue) v3 = new IntValue(v1.value - v2.value, this.loc);
						else if(v1 instanceof FloatValue || v2 instanceof FloatValue) v3 = new FloatValue(v1.value - v2.value, this.loc);
						break;
					case '*':
						if(v1 instanceof BooleanValue) v1 = new IntValue(v1.value ? 1 : 0, this.loc);
						if(v2 instanceof BooleanValue) v2 = new IntValue(v2.value ? 1 : 0, this.loc);
						if(v1 instanceof ArrayValue || v2 instanceof ArrayValue) throw new RuntimeError(this.first_line, "配列の掛け算は出来ません");
						else if(v1 instanceof DictionaryValue || v2 instanceof DictionaryValue) throw new RuntimeError(this.first_line, "辞書の掛け算はできません");
						else if(v1 instanceof StringValue)
						{
							if(v2 instanceof IntValue) v3 = new StringValue(v1.value.repeat(v2.value >= 0 ? v2.value : 0), this.loc);
							else throw new RuntimeError(this.first_line, "文字列に掛けられるのは整数だけです")
						}
						else if(v1 instanceof IntValue && v2 instanceof IntValue) v3 = new IntValue(v1.value * v2.value, this.loc);
						else if(v1 instanceof FloatValue || v2 instanceof FloatValue) v3 = new FloatValue(v1.value * v2.value, this.loc);
						break;
					case '/':
						if(v1 instanceof BooleanValue) v1 = new IntValue(v1.value ? 1 : 0, this.loc);
						if(v2 instanceof BooleanValue) v2 = new IntValue(v2.value ? 1 : 0, this.loc);
						if(v1 instanceof ArrayValue || v2 instanceof ArrayValue) throw new RuntimeError(this.first_line, "配列の割り算はできません");
						else if(v1 instanceof DictionaryValue || v2 instanceof DictionaryValue) throw new RuntimeError(this.first_line, "辞書の割り算はできません");
						else if(v1 instanceof StringValue || v2 instanceof StringValue) throw new RuntimeError(this.first_line, "文字列の割り算はできません");
						else
						{
							if(v2.value == 0) throw new RuntimeError(this.first_line, '0で割り算をしました');
							else v3 = new FloatValue(v1.value / v2.value, this.loc);
						}
						break;
					case '//':
						if(v1 instanceof BooleanValue) v1 = new IntValue(v1.value ? 1 : 0, this.loc);
						if(v2 instanceof BooleanValue) v2 = new IntValue(v2.value ? 1 : 0, this.loc);
						if(v1 instanceof ArrayValue || v2 instanceof ArrayValue) throw new RuntimeError(this.first_line, "配列の割り算はできません");
						else if(v1 instanceof DictionaryValue || v2 instanceof DictionaryValue) throw new RuntimeError(this.first_line, "辞書の割り算はできません");
						else if(v1 instanceof StringValue || v2 instanceof StringValue) throw new RuntimeError(this.first_line, "文字列の割り算はできません");
						else
						{
							if(v2.value == 0) throw new RuntimeError(this.first_line, '0で割り算をしました');
							let v4 = Math.floor(v1.value / v2.value);
							if(v1 instanceof IntValue && v2 instanceof IntValue ) v3 = new IntValue(v4, this.loc);
							else v3 = new FloatValue(v4, this.loc);
						}
						break;
					case '%':
						if(v1 instanceof BooleanValue) v1 = new IntValue(v1.value ? 1 : 0, this.loc);
						if(v2 instanceof BooleanValue) v2 = new IntValue(v2.value ? 1 : 0, this.loc);
						if(v1 instanceof ArrayValue || v2 instanceof ArrayValue) throw new RuntimeError(this.first_line, "配列の割り算はできません");
						else if(v1 instanceof DictionaryValue || v2 instanceof DictionaryValue) throw new RuntimeError(this.first_line, "辞書の割り算はできません");
						else if(v1 instanceof StringValue || v2 instanceof StringValue) throw new RuntimeError(this.first_line, "文字列の割り算はできません");
						else
						{
							if(v2.value == 0) throw new RuntimeError(this.first_line, '0で割り算をしました');
							let v4 = v1.value - Math.floor(v1.value / v2.value) * v2.value;
							if(v1 instanceof IntValue && v2 instanceof IntValue ) v3 = new IntValue(v4, this.loc);
							else v3 = new FloatValue(v4, this.loc);
						}
						break;
					case '&':
						if(v1 instanceof ArrayValue || v2 instanceof ArrayValue) throw new RuntimeError(this.first_line, "配列のビット積はできません");
						else if(v1 instanceof DictionaryValue || v2 instanceof DictionaryValue) throw new RuntimeError(this.first_line, "辞書のビット積はできません");
						else if(v1 instanceof StringValue || v2 instanceof StringValue) throw new RuntimeError(this.first_line, "文字列のビット積はできません");
						else if(v1 instanceof FloatValue || v2 instanceof FloatValue) throw new RuntimeError(this.first_line, "実数のビット積はできません");
						else if(v1 instanceof BooleanValue && v2 instanceof BooleanValue) v3 = new BooleanValue(v1.value && v2.value, this.loc);
						else
						{
							if(v1 instanceof BooleanValue) v1 = new IntValue(v1.value ? 1 : 0, this.loc);
							if(v2 instanceof BooleanValue) v2 = new IntValue(v2.value ? 1 : 0, this.loc);
							v3 = new IntValue(v1.value & v2.value, this.loc);
						} 
						break;
					case '|':
						if(v1 instanceof ArrayValue || v2 instanceof ArrayValue) throw new RuntimeError(this.first_line, "配列のビット和はできません");
						else if(v1 instanceof DictionaryValue || v2 instanceof DictionaryValue) throw new RuntimeError(this.first_line, "辞書のビット和はできません");
						else if(v1 instanceof StringValue || v2 instanceof StringValue) throw new RuntimeError(this.first_line, "文字列のビット和はできません");
						else if(v1 instanceof FloatValue || v2 instanceof FloatValue) throw new RuntimeError(this.first_line, "実数のビット和はできません");
						else if(v1 instanceof BooleanValue && v2 instanceof BooleanValue) v3 = new BooleanValue(v1.value && v2.value, this.loc);
						else
						{
							if(v1 instanceof BooleanValue) v1 = new IntValue(v1.value ? 1 : 0, this.loc);
							if(v2 instanceof BooleanValue) v2 = new IntValue(v2.value ? 1 : 0, this.loc);
							v3 = new IntValue(v1.value | v2.value, this.loc);
						} 
						break;
					case '^':
						if(v1 instanceof ArrayValue && v2 instanceof ArrayValue) throw new RuntimeError(this.first_line, "配列の排他的論理和はできません");
						else if(v1 instanceof DictionaryValue || v2 instanceof DictionaryValue) throw new RuntimeError(this.first_line, "辞書の排他的論理和はできません");
						else if(v1 instanceof StringValue || v2 instanceof StringValue) throw new RuntimeError(this.first_line, "文字列の排他的論理和はできません");
						else if(v1 instanceof FloatValue || v2 instanceof FloatValue) throw new RuntimeError(this.first_line, "実数の排他的論理和はできません");
						else if(v1 instanceof BooleanValue && v2 instanceof BooleanValue) v3 = new BooleanValue(v1.value && v2.value, this.loc);
						else
						{
							if(v1 instanceof BooleanValue) v1 = new IntValue(v1.value ? 1 : 0, this.loc);
							if(v2 instanceof BooleanValue) v2 = new IntValue(v2.value ? 1 : 0, this.loc);
							v3 = new IntValue(v1.value ^ v2.value, this.loc);
						} 
						break;
					case '<<':
						if(v1 instanceof ArrayValue && v2 instanceof ArrayValue) throw new RuntimeError(this.first_line, "配列のビットシフトはできません");
						else if(v1 instanceof DictionaryValue || v2 instanceof DictionaryValue) throw new RuntimeError(this.first_line, "辞書のビットシフトはできません");
						else if(v1 instanceof StringValue || v2 instanceof StringValue) throw new RuntimeError(this.first_line, "文字列のビットシフトはできません");
						else if(v1 instanceof FloatValue || v2 instanceof FloatValue) throw new RuntimeError(this.first_line, "実数のビットシフトはできません");
						else if(v1 instanceof BooleanValue && v2 instanceof BooleanValue) v3 = new BooleanValue(v1.value && v2.value, this.loc);
						else
						{
							if(v1 instanceof BooleanValue) v1 = new IntValue(v1.value ? 1 : 0, this.loc);
							if(v2 instanceof BooleanValue) v2 = new IntValue(v2.value ? 1 : 0, this.loc);
							v3 = new IntValue(v1.value << v2.value, this.loc);
						} 
						break;
					case '>>':
						if(v1 instanceof ArrayValue && v2 instanceof ArrayValue) throw new RuntimeError(this.first_line, "配列のビットシフトはできません");
						else if(v1 instanceof DictionaryValue || v2 instanceof DictionaryValue) throw new RuntimeError(this.first_line, "辞書のビットシフトはできません");
						else if(v1 instanceof StringValue || v2 instanceof StringValue) throw new RuntimeError(this.first_line, "文字列のビットシフトはできません");
						else if(v1 instanceof FloatValue || v2 instanceof FloatValue) throw new RuntimeError(this.first_line, "実数のビットシフトはできません");
						else if(v1 instanceof BooleanValue && v2 instanceof BooleanValue) v3 = new BooleanValue(v1.value && v2.value, this.loc);
						else
						{
							if(v1 instanceof BooleanValue) v1 = new IntValue(v1.value ? 1 : 0, this.loc);
							if(v2 instanceof BooleanValue) v2 = new IntValue(v2.value ? 1 : 0, this.loc);
							v3 = new IntValue(v1.value >> v2.value, this.loc);
						} 
						break;
					}
					if(!v3) throw new RuntimeError(this.first_line, '複合代入演算子の使い方が間違っています');
					setVariableByArgs(vt,vn, ag ? ag.value : null, v3, this.loc);
				}
				else setVariableByArgs(vt, vn, ag ? ag.value : null, vl.clone(), this.loc);
			}
			else // 変数が定義されていない
			{
				if(this.operator) throw new RuntimeError(this.first_line, '宣言されていない変数に複合代入演算子が使われました');
				vt = varTables[0];
				vt.vars[vn] = new NullValue(this.loc);
				setVariableByArgs(vt, vn, ag ? ag.value : null, vl.clone(), this.loc);
			}
			this.state = 0;
		}
	}
}

class Append extends Statement
{
	/**
	 * @constructor
	 * @param {Variable} variable 
	 * @param {Value} value 
	 * @param {Location} loc 
	 */
	constructor(variable,value,loc)
	{
		super(loc);
		if(!(variable instanceof Variable || variable instanceof UNDEFINED))throw new RuntimeError(loc.first_line, "追加されるものは変数でなくてはいけません");
		this.variable = variable;
		this.value = value;
		this.state = 0;
	}
	clone()
	{
		return new Append(this.variable.clone(), this.value.clone(), this.loc);
	}
	run()
	{
		if(this.variable instanceof UNDEFINED) throw new RuntimeError(this.first_line, "未完成のプログラムです");
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: [this.variable, this.value], index: 0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			let vn = this.variable.varname;
			let ag = this.variable.args;
			let vl = this.value.getValue();
			let vt = findVarTable(vn);
			if(vt) // 変数が定義されている
			{
				let va = vt.vars[vn];
				if(ag && ag.value.length > 0) // 配列の添字がある
				{
					for(let i = 0; i < ag.value.length; i++) 
					{
						if(ag.value[i].getValue() instanceof StringValue)
						{
							va = va.value[ag.value[i].getValue().value];
						}
						else if(ag.value[i].getValue() instanceof IntValue)
						{
							if(va.value[ag.value[i].getValue().value])
								va = va.value[ag.value[i].getValue().value];
							else throw new RuntimeError(this.first_line, '配列の範囲を超えたところに追加しようとしました')
						}
						else throw new RuntimeError(this.first_line, '添字に使えないデータ型です');
					}
				}
				if(va instanceof ArrayValue) va.value.push(vl.clone());
				else throw new RuntimeError(this.first_line, '配列でない変数に追加はできません');
			}
			else // 変数が定義されていない
				throw new RuntimeError(this.first_line, '存在しない配列に追加はできません');
			this.state = 0;
		}
	}
}

class Extend extends Statement
{
	/**
	 * @constructor
	 * @param {Variable} variable 
	 * @param {Value} value 
	 * @param {Location} loc 
	 */
	constructor(variable,value,loc)
	{
		super(loc);
		if(!(variable instanceof Variable || variable instanceof UNDEFINED))throw new RuntimeError(loc.first_line, "連結されるものは変数でなくてはいけません");
		this.variable = variable;
		this.value = value;
	}
	clone()
	{
		return new Extend(this.variable.clone(), this.value.clone(), this.loc);
	}
	run()
	{
		code[0].stack[0].index++;
		if(this.variable instanceof UNDEFINED) throw new RuntimeError(this.first_line, "未完成のプログラムです");

		let vn = this.variable.varname;
		let ag = this.variable.args;
		let vl = this.value.getValue();
		let vt = findVarTable(vn);
		if(vt) // 変数が定義されている
		{
			let va = vt.vars[vn];
			if(ag && ag.value.length > 0) // 配列の添字がある
			{
				for(let i = 0; i < ag.value.length; i++) 
				{
					ag.value[i].run();
					if(ag.value[i] instanceof StringValue)
					{
						va = va.value[ag.value[i].getValue().value];					}
					else if(ag.value[i] instanceof IntValue)
					{
						if(va.value[ag.value[i].getValue().value])
							va = va.value[ag.value[i].getValue().value];
						else throw new RuntimeError(this.first_line, '配列の範囲を超えたところに連結しようとしました')
					}
					else throw new RuntimeError(this.first_line, "添字に使えないデータ型です");
				}
			}
			if(va instanceof ArrayValue)
			{
				if(vl instanceof ArrayValue)
				{
					var l = vl.value.length;
					for(var i = 0; i < l; i++) va.value.push(vl.value[i].clone());
				}
				else throw new RuntimeError(this.first_line, '配列でない値を連結することはできません');
			} 
			else throw new RuntimeError(this.first_line, '配列でない変数に連結はできません');
		}
		else // 変数が定義されていない
			throw new RuntimeError(this.first_line, '存在しない配列に連結はできません');
	}
}

class Input extends Statement
{
	constructor(x, type,loc)
	{
		super(loc);
		if(!(x instanceof Variable || x instanceof UNDEFINED))throw new RuntimeError(loc.first_line, "入力されるものは変数でなくてはいけません");
		this.varname = x;
		this.type = type;
		this.state = 0;
	}
	clone()
	{
		return new Input(this.varname.clone(), this.type, this.loc);
	}
	run()
	{
		if(selected_quiz < 0)	// 通常時
		{
			code[0].stack[0].index++;
			if(this.varname instanceof UNDEFINED) throw new RuntimeError(this.first_line, "未完成のプログラムです");
			var list = [new InputBegin(this.loc), new InputEnd(this.varname, this.type, this.loc)];
			code[0].stack.unshift({statementlist: list, index: 0});
		}
		else	// 自動採点時
		{
			if(this.state == 0)
			{
				if(this.varname.args) code[0].stack.unshift({statementlist: this.varname.args, index: 0});
				this.state = 1;
			}
			else
			{
				code[0].stack[0].index++;
				if(selected_quiz_input < Quizzes[selected_quiz].inputs(selected_quiz_case).length)
				{
					if(this.varname instanceof UNDEFINED) throw new RuntimeError(this.first_line, "未完成のプログラムです");
					let va = new Variable(this.varname.varname, this.varname.args, this.loc);
					let vl = Quizzes[selected_quiz].inputs(selected_quiz_case)[selected_quiz_input++];
					// va.run();
					let assign = null;
					let re = /^(0+|false|偽|)$/i;
					if(this.type == typeOfValue.typeInt)assign = new Assign(this.varname, new IntValue(Number(toHalf(vl, this.loc)), this.loc),null, this.loc);
					else if(this.type == typeOfValue.typeFloat)assign = new Assign(this.varname, new FloatValue(Number(toHalf(vl, this.loc)), this.loc), null, this.loc);
					else if(this.type == typeOfValue.typeString) assign = new Assign(this.varname, new StringValue(vl + '', this.loc), null, this.loc);
					else if(this.type == typeOfValue.typeBoolean) assign = new Assign(this.varname, new BooleanValue(!re.exec(vl), this.loc), null, this.loc);
					code[0].stack.unshift({statementlist: [assign], index: 0});
				}
				else throw new RuntimeError(this.first_line, '必要以上の入力を求めています。');
				this.state = 0;
			}
		}
	}
}
function openInputWindow()
{
	setRunflag(false);
	setEditableflag(false);
	var input_area = document.getElementById("input_area");
	input_area.value = '';
	input_area.readOnly = false;
	input_area.focus();
	document.getElementById("input_status").style.visibility = 'visible';
	document.getElementById("sourceTextarea").readOnly = true;
	// editor.options.readOnly = true;
	// editor.getWrapperElement().classList.add("readonly");
}

function closeInputWindow()
{
	var val = document.getElementById("input_area").value;
	document.getElementById("input_area").readOnly = true;
	document.getElementById("input_status").style.visibility = 'hidden';
	return val;
}

function keydownInput(e)
{
	var evt = e || window.event
	if(evt.keyCode == 13)
	{
		setRunflag(true);
		step();
	}
	else if(evt.keyCode == 27)
	{
		closeInputWindow();
		code.shift();
	}
}

class InputBegin extends Statement
{
	/**
	 * @constructor
	 * @param {Location} loc 
	 */
	constructor(loc)
	{
		super(loc);
	}
	clone()
	{
		return new InputBegin(this.loc);
	}
	run()
	{
		openInputWindow();
		code[0].stack[0].index++;
	}
}

class InputEnd extends Statement
{
	/**
	 * @constructor
	 * @param {Variable} x
	 * @param {typeOfValue} type 
	 * @param {Location} loc 
	 */
	constructor(x, type, loc)
	{
		super(loc);
		this.varname = x;
		this.type = type;
		this.state = 0;
	}
	clone()
	{
		return new InputEnd(this.varname.clone(), this.type, this.loc);
	}
	run()
	{
		if(this.varname instanceof UNDEFINED) throw new RuntimeError(this.first_line, "未完成のプログラムです");
		if(this.state == 0)
		{
			if(this.varname.args) code[0].stack.unshift({statementlist:this.varname.args, index: 0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			try{
				let vl = closeInputWindow();
				// va.run();
				let assign = null;
				let re = /^(0+|false|偽|)$/i;
				if(this.type == typeOfValue.typeInt)assign = new Assign(this.varname, new IntValue(Number(toHalf(vl, this.loc)), this.loc), null, this.loc);
				else if(this.type == typeOfValue.typeFloat)assign = new Assign(this.varname, new FloatValue(Number(toHalf(vl, this.loc)), this.loc), null, this.loc);
				else if(this.type == typeOfValue.typeString) assign = new Assign(this.varname, new StringValue(vl + '', this.loc), null, this.loc);
				else if(this.type == typeOfValue.typeBoolean) assign = new Assign(this.varname, new BooleanValue(!re.exec(vl), this.loc), null, this.loc);
				code[0].stack.unshift({statementlist: [assign], index: 0});
			}
			catch(e)
			{
				closeInputWindow();
				throw e;
			}
			this.state = 0;
		}
	}
}

class Newline extends Statement
{
	constructor(loc){super(loc);}
	clone()
	{
		return new Newline(this.loc);
	}
	run()
	{
		code[0].stack[0].index++;
		if(selected_quiz < 0)
		{
			textareaAppend("\n");
		}
		else
		{
			output_str += "\n";
		}
	}
}

class Output extends Statement
{
	/**
	 * 
	 * @param {Array<Value>} x 
	 * @param {boolean} ln 
	 * @param {Location} loc 
	 */
	constructor(x, ln, loc)
	{
		super(loc);
		this.value = x;
		this.ln = ln;
		this.state = 0;
	}
	clone()
	{
		var val = [];
		for(var i = 0; i < this.value.length; i++) val.push(this.value[i].clone());
		return new Output(val, this.ln, this.loc);
	}
	run()
	{
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: this.value, index: 0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			let s = '';
			for(var i = 0; i < this.value.length; i++)
			{
				let v = this.value[i];
				s += (i > 0 ? ' ' : '') + array2text(v);
			}
			if(this.ln)	s += '\n';
			if(selected_quiz < 0) textareaAppend(s);
			else output_str += s;
			this.state = 0;
		}
	}
}

function array2text(v)
{
	if(!v) return '';
	if(v instanceof Value)
	{
		let v0 = v.getValue();
		if(v0 instanceof ArrayValue)
		{
			let v1 = [];
			for(let i = 0; i < v0.value.length; i++)
			{
				v1.push(array2text(v0.value[i]));
			}
			return '[' + v1.join(',') + ']';
		}
		else if(v0 instanceof DictionaryValue)
		{
			let v1 = [];
			let keys = Object.keys(v0.value);
			keys.sort();
			for(let i = 0; i < keys.length; i++) 
			{
				v1.push(keys[i] + ':' + array2text(v0.value[keys[i]]));
			}
			return '{' + v1.join(',') + '}';
		}
		else if(v0 instanceof BooleanValue) return v0.value ? 'True' : 'False';
		else if(v0 instanceof FloatValue && isInteger(v0.value) && !v0.value.toString().match(/[Ee]/)) return v0.value + '.0';
		else return new String(v0.value);
	}
	else return new String(v);
}

function array2code(v)
{
	if(!v) return '';
	let v0 = v.getValue();
	if(v0 instanceof ArrayValue)
	{
		let v1 = [];
		for(let i = 0; i < v0.value.length; i++) v1.push(array2text(v0.value[i]));
		return '[' + v1.join(',') + ']';
	}
	else if(v0 instanceof DictionaryValue)
	{
		let v1 = [];
		let keys = Object.keys(v0.value);
		keys.sort();
		for(let i = 0; i < keys.length; i++) v1.push(keys[i] + ':' + array2text(v0.value[keys[i]]));
		return '{' + v1.join(',') + '}';
	}
	else if(v0 instanceof StringValue) return '"' + v0.value + '"';
	else if(v0 instanceof FloatValue && isInteger(v0.value) && !v0.value.toString().match(/[Ee]/)) return v0.value + '.0';
	return v0.value;
}

class FileIOStatement extends Statement
{
	constructor(command, args, loc)
	{
		super(loc);
		this.command = command;
		this.args = args;
		this.state = 0;
	}
	clone()
	{
		var args = [];
		for(var i = 0; i < this.args.length; i++) args.push(this.args[i].clone());
		return new FileIOStatement(this.command, args, this.loc);
	}
	run()
	{
		if(this.state == 0)
		{
			if(this.args) code[0].stack.unshift({statementlist: this.args, index: 0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			if(this.command == 'putline')
			{
				var str = array2text(this.args[1].getValue());
				var rtnv = filesystem.write_str(this.args[0].getValue().value, str, true);
				if(!rtnv) throw new RuntimeError(this.first_line, "呼び出しが不正です");
			}
			else if(this.command == 'putstr')
			{
				var str = array2text(this.args[1].getValue());
				var rtnv = filesystem.write_str(this.args[0].getValue().value, str, false);
				if(!rtnv) throw new RuntimeError(this.first_line, "呼び出しが不正です");
			}
			else if(this.command == 'close')
			{
				var rtnv = filesystem.close(this.args[0].getValue().value, true);
				if(!rtnv) throw new RuntimeError(this.first_line, "呼び出しが不正です");
			}
		}
	}
}

class GraphicStatement extends Statement
{
	constructor(command, args, loc)
	{
		super(loc);
		this.command = command;
		this.args = args;
		this.state = 0;
	}
	clone()
	{
		var args = [];
		for(var i = 0; i < this.args.length; i++) args.push(this.args[i].clone());
		return new GraphicStatement(this.command, args, this.loc);
	}
	run()
	{
		if(this.state == 0)
		{
			if(this.args) code[0].stack.unshift({statementlist: this.args, index: 0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			if(this.command == 'gOpenWindow')
			{
				var canvas = document.getElementById('canvas');
				context = canvas.getContext('2d');
				canvas.setAttribute("width", this.args[0].getValue().value + "px");
				canvas.setAttribute("height", this.args[1].getValue().value + "px");
				canvas.style.display="block";
			}
			else if(this.command == 'gCloseWindow')
			{
				var canvas = document.getElementById('canvas');
				canvas.style.display = "none";
				context = null;
			}
			else if(this.command == 'gClearWindow')
			{
				var canvas = document.getElementById('canvas');
				context.clearRect(0,0,canvas.width, canvas.height)
			}
			else if(this.command == 'gSetLineColor')
			{
				if(context == null) throw new RuntimeError(this.first_line, "描画領域がありません");
				let r = this.args[0].getValue().value, g = this.args[1].getValue().value, b = this.args[2].getValue().value;
				context.strokeStyle = "rgb(" + r + "," + g + "," + b + ")";
			}
			else if(this.command == 'gSetFillColor')
			{
				if(context == null) throw new RuntimeError(this.first_line, "描画領域がありません");
				let r = this.args[0].getValue().value, g = this.args[1].getValue().value, b = this.args[2].getValue().value;
				context.fillStyle = "rgb(" + r + "," + g + "," + b + ")";
			}
			else if(this.command == 'gSetTextColor')
			{
				if(context == null) throw new RuntimeError(this.first_line, "描画領域がありません");
				let r = this.args[0].getValue().value, g = this.args[1].getValue().value, b = this.args[2].getValue().value;
				context.textStyle = "rgb(" + r + "," + g + "," + b + ")";
			}
			else if(this.command == 'gSetLineWidth')
			{
				if(context == null) throw new RuntimeError(this.first_line, "描画領域がありません");
				context.lineWidth = this.args[0].getValue().value;
			}
			else if(this.command == 'gSetFontSize')
			{
				if(context == null) throw new RuntimeError(this.first_line, "描画領域がありません");
				context.font = this.args[0].getValue().value + "px 'sans-serif'";
			}
			else if(this.command == 'gDrawText')
			{
				if(context == null) throw new RuntimeError(this.first_line, "描画領域がありません");
				var temp = context.fillStyle;
				context.fillStyle = context.textStyle;
				context.fillText(this.args[0].getValue().value, this.args[1].getValue().value, this.args[2].getValue().value);
				context.fillStyle = temp;
			}
			else if(this.command == 'gDrawLine')
			{
				if(context == null) throw new RuntimeError(this.first_line, "描画領域がありません");
				let x1 = this.args[0].getValue().value, y1 = this.args[1].getValue().value,
					x2 = this.args[2].getValue().value, y2 = this.args[3].getValue().value;
				context.beginPath();
				context.moveTo(x1, y1);
				context.lineTo(x2, y2);
				context.stroke();
			}
			else if(this.command == 'gDrawPoint')
			{
				if(context == null) throw new RuntimeError(this.first_line, "描画領域がありません");
				let x1 = this.args[0].getValue().value, y1 = this.args[1].getValue().value;
				context.beginPath();
				context.arc(x1, y1, 1, 0, Math.PI * 2, false);
				context.stroke();
			}
			else if(this.command == 'gDrawBox')
			{
				if(context == null) throw new RuntimeError(this.first_line, "描画領域がありません");
				let x1 = this.args[0].getValue().value, y1 = this.args[1].getValue().value,
					width = this.args[2].getValue().value, height = this.args[3].getValue().value;
				context.beginPath();
				context.strokeRect(x1, y1, width, height);
				context.stroke();
			}
			else if(this.command == 'gFillBox')
			{
				if(context == null) throw new RuntimeError(this.first_line, "描画領域がありません");
				let x1 = this.args[0].getValue().value, y1 = this.args[1].getValue().value,
					width = this.args[2].getValue().value, height = this.args[3].getValue().value;
				context.fillRect(x1, y1, width, height);
				context.beginPath();
				context.strokeRect(x1, y1, width, height);
				context.stroke();
			}
			else if(this.command == 'gDrawCircle')
			{
				if(context == null) throw new RuntimeError(this.first_line, "描画領域がありません");
				let x1 = this.args[0].getValue().value, y1 = this.args[1].getValue().value, r = this.args[2].getValue().value;
				context.beginPath();
				context.arc(x1, y1, r, 0, Math.PI * 2, false);
				context.stroke();
			}
			else if(this.command == 'gFillCircle')
			{
				if(context == null) throw new RuntimeError(this.first_line, "描画領域がありません");
				let x1 = this.args[0].getValue().value, y1 = this.args[1].getValue().value, r = this.args[2].getValue().value;
				for(var i = 0; i < 2; i++)
				{
					context.beginPath();
					context.arc(x1, y1, r, 0, Math.PI * 2, false);
					if(i == 0) context.fill();
					else context.stroke();
				}
			}
			else if(this.command == 'gDrawOval')
			{
				if(context == null) throw new RuntimeError(this.first_line, "描画領域がありません");
				let x1 = this.args[0].getValue().value, y1 = this.args[1].getValue().value, w = this.args[2].getValue().value, h = this.args[3].getValue().value;
				context.beginPath();
				context.ellipse(x1 + w / 2, y1 + h / 2, w / 2, h / 2, 0, 0, Math.PI * 2);
				context.stroke();
			}
			else if(this.command == 'gFillOval')
			{
				if(context == null) throw new RuntimeError(this.first_line, "描画領域がありません");
				let x1 = this.args[0].getValue().value, y1 = this.args[1].getValue().value, w = this.args[2].getValue().value, h = this.args[3].getValue().value;
				for(var i = 0; i < 2; i++)
				{
					context.beginPath();
					context.ellipse(x1 + w / 2, y1 + h / 2, w / 2, h / 2, 0, 0, Math.PI * 2);
					if(i == 0) context.fill();
					else context.stroke();
				}
			}
			else if(this.command == 'gDrawArc')
			{
				if(context == null) throw new RuntimeError(this.first_line, "描画領域がありません");
				let x1 = this.args[0].getValue().value, y1 = this.args[1].getValue().value, w = this.args[2].getValue().value, h = this.args[3].getValue().value,
					theta1 = this.args[4].getValue().value, theta2 = this.args[5].getValue().value, style = this.args[6].getValue().value;
				context.beginPath();
				context.ellipse(x1 + w / 2, y1 + h / 2, w / 2, h / 2, 0, -theta1 * Math.PI / 180, -theta2 * Math.PI / 180, true);
				switch(style)
				{
					case 2: // 半径
						context.lineTo(x1 + w / 2, y1 + h / 2);
						// fall through
					case 1: // 弦
						context.closePath();
				}
				context.stroke();
			}
			else if(this.command == 'gFillArc')
			{
				if(context == null) throw new RuntimeError(this.first_line, "描画領域がありません");
				let x1 = this.args[0].getValue().value, y1 = this.args[1].getValue().value, w = this.args[2].getValue().value, h = this.args[3].getValue().value,
					theta1 = this.args[4].getValue().value, theta2 = this.args[5].getValue().value, style = this.args[6].getValue().value;
				for(var i = 0; i < 2; i++)
				{
					context.beginPath();
					context.ellipse(x1 + w / 2, y1 + h / 2, w / 2, h / 2, 0, -theta1 * Math.PI / 180, -theta2 * Math.PI / 180, true);
					switch(style)
					{
						case 2: // 半径
							context.lineTo(x1 + w / 2, y1 + h / 2);
							// fall through
						case 1: // 弦
							context.closePath();
					}
					if(i == 0) context.fill();
					else context.stroke();
				}
			}
			else if(this.command == 'gBarplot')
			{
				if(context == null)
				{
					var canvas = document.getElementById('canvas');
					var w = this.args[0].getValue().value, h = this.args[1].getValue().value;
					context = canvas.getContext('2d');
					canvas.setAttribute("width", w + "px");
					canvas.setAttribute("height", h + "px");
					canvas.style.display="block";
				}
				// 値の取得
				var values = array2values(this.args[2], this.loc);
				var max = 0, min = 0, maxn = 0;
				for(var i = 0; i < values.length; i++)
				{
					var l = values[i].length;
					if(l > maxn) maxn = l;
					for(var j = 0; j < l; j++)
					{
						var v1 = values[i][j];
						if(v1 > max) max = v1;
						if(v1 < min) min = v1;
					}
				}
				if(max == 0) max = 1;
				// 軸の描画
				var x0 = w * 0.05, y0 = h * 0.95;
				y0 *= max / (max - min);
				w *= 0.9; h *= 0.9;
				context.beginPath();
				context.moveTo(x0, y0 - h * max / (max - min));
				context.lineTo(x0, y0 - h * min / (max - min));
				context.moveTo(x0, y0);
				context.lineTo(x0 + w, y0);
				context.stroke();
				if(values.length > 0)
				{
					var w0 = w / maxn / values.length;
					for(var i = 0; i < values.length; i++)
					{
						context.fillStyle = graphColor[i % 6];
						context.beginPath();
						for(var j = 0; j < values[i].length; j++)
						{
							var x = x0 + w0 * j + w0 / 2, y = y0 - (values[i][j] / (max - min)) * h;
							if(values[i][j] >= 0)
								context.fillRect(x0 + w0 * j * values.length + w0 * 0.8 * i + w0 * 0.1, y0 - h * (values[i][j] / (max - min)),w0 * 0.8, h * (values[i][j] / (max - min)));
							else
								context.fillRect(x0 + w0 * j * values.length + w0 * 0.8 * i + w0 * 0.1, y0, w0 * 0.8, h * (-values[i][j] / (max - min)));
						}
						context.stroke();
					}
				}
			}
			else if(this.command == 'gLineplot')
			{
				if(context == null)
				{
					var canvas = document.getElementById('canvas');
					var w = this.args[0].getValue().value, h = this.args[1].getValue().value;
					context = canvas.getContext('2d');
					canvas.setAttribute("width", w + "px");
					canvas.setAttribute("height", h + "px");
					canvas.style.display="block";	
				}
				// 値の取得
				var values = array2values(this.args[2], this.loc);
				var max = 0, min = 0, maxn = 0;
				for(var i = 0; i < values.length; i++)
				{
					var l = values[i].length;
					if(l > maxn) maxn = l;
					for(var j = 0; j < l; j++)
					{
						var v1 = values[i][j];
						if(v1 > max) max = v1;
						if(v1 < min) min = v1;
					}
				}
				if(max == 0) max = 1;
				// 軸の描画
				var x0 = w * 0.05, y0 = h * 0.95;
				y0 *= max / (max - min);
				w *= 0.9; h *= 0.9;
				context.beginPath();
				context.moveTo(x0, y0 - h * max / (max - min));
				context.lineTo(x0, y0 - h * min / (max - min));
				context.moveTo(x0, y0);
				context.lineTo(x0 + w, y0);
				context.stroke();
				if(values.length > 0)
				{
					var w0 = w / maxn;
					for(var i = 0; i < values.length; i++)
					{
						context.strokeStyle = graphColor[i % 6];
						context.beginPath();
						for(var j = 0; j < values[i].length; j++)
						{
							var x = x0 + w0 * j + w0 / 2, y = y0 - (values[i][j] / (max - min)) * h;
							if(j == 0) context.moveTo(x, y);
							else context.lineTo(x, y);
						}
						context.stroke();
					}
				}
			}
			else if(this.command == 'gDrawGraph')
			{
				drawGraph(this.args[0].getValue(), this.args[1].getValue(), this.loc);
			}
			else if(this.command == 'gClearGraph')
			{
				clearGraph();
			}
			else
			{
				throw new RuntimeError(this.first_line, "未実装のコマンド" + this.command + "が使われました");
			}
			this.state = 0;
		}
	}
}

function clearGraph()
{
	Plotly.purge(document.getElementById("graph"));
}

// グラフ描画を行う
// graph{
//  title: 文字列
//  x:{
// 	  title: 文字列
//    min: 実数
//    max: 実数
//  }
//  y:{
// 	  title:
//    min:
//    max:
//  }
// }
// dataは{
//   x: 値の配列（省略時は0〜len(y)-1）
//   y: 値の配列（省略不可）
//   type: 'bar' or 'line' or 'scatter'
//   color: 
//   size: 整数（省略時は1）
// }の配列
function drawGraph(layout, data, loc)
{
	var div = document.getElementById('graph');
	var graph_data = [], graph_layout = {};
	if(layout instanceof DictionaryValue)
	{
		for(var key in layout.value)
		{
			var val = layout.value[key].getValue();
			if(val instanceof ArrayValue)
			{
				graph_layout[key] = {};
				for(var key1 in val.value)
					graph_layout[key][key1] = val2obj(val.value[key1].getValue());
			}
			else graph_layout[key] = val2obj(val);
		}
	}
	else if(layout) throw new RuntimeError(loc.first_line, "レイアウト情報が辞書になっていません");
	if(data instanceof ArrayValue)
	{
		var dl = data.value.length;
		for(var i = 0; i < dl; i++)
		{
			var d = data.value[i].getValue();
			if(d instanceof DictionaryValue)
			{
				var va = {};
				for(var key in d.value)
				{
					var val = d.value[key].getValue();
					va[key] = val2obj(val);
				}
				graph_data.push(va);
	
			}
			else throw new RuntimeError(loc.first_line, "データの" + i + "番目の要素が辞書になっていません");
		}
	}else throw new RuntimeError(loc.first_line, 'データが配列になっていません');
	Plotly.newPlot(div, graph_data, graph_layout);
}

/**
 * 
 * @param {ArrayValue} a 
 * @param {Location} loc 
 */
 function array2values(a, loc)
 {
	 var rtnv = [];
	 var array = null;
	 if(a.rtnv instanceof ArrayValue)
	 {
		 if(a.rtnv.value[0] instanceof ArrayValue) array = a.rtnv;
		 else if(a.rtnv.value instanceof Array) array = new ArrayValue([a.rtnv.value], loc);
		 else throw new RuntimeError(loc.first_line, "グラフに誤った型が使われています");
	 }
	 else if(a.rtnv instanceof Array) array = new ArrayValue(a.rtnv, loc);
	 else throw new RuntimeError(loc.first_line, "棒グラフ・線グラフには配列が必要です");
 
	 for(var i = 0; i < array.length; i++)
	 {
		 var rtnv1 = [];
		 for(var j = 0; j < array.value[i].length; j++)
			 rtnv1.push(array.value[i] instanceof ArrayValue ? array.value[i].value[j].value : array.value[i][j].value);
		 rtnv.push(rtnv1);
	 }
	 return rtnv;
 }
 
 class If extends Statement
 {
	 /**
	  * 
	  * @param {Value} condition 
	  * @param {Array<Statement>} state1 
	  * @param {Array<Statement>} state2 
	  * @param {Location} loc 
	  */
	 constructor(condition, state1, state2, loc)
	 {
		 super(loc);
		 this.condition = condition;
		 this.state1 = state1;
		 this.state2 = state2;
	 }
	 clone()
	 {
		 var state1 = [], state2 = [];
		 if(this.state1)
			 for(var i = 0; i < this.state1.length; i++) state1.push(this.state1[i].clone());
		 if(this.state2)
			 for(var i = 0; i < this.state2.length; i++) state2.push(this.state2[i].clone());
		 return new If(this.condition.clone(), state1, state2, this.loc);
	 }
	 run()
	 {
		 if(this.state == 0)
		 {
			 code[0].stack.unshift({statementlist: [this.condition], index: 0});
			 this.state = 1;
		 }
		 else
		 {
			 code[0].stack[0].index++;
			 if(this.condition.getValue() instanceof BooleanValue)
			 {
				 if(this.condition.getValue().value) code[0].stack.unshift({statementlist: this.state1, index: 0});
				 else if(this.state2 && this.state2.length > 0) code[0].stack.unshift({statementlist: this.state2, index: 0});
			 }
			 else throw new RuntimeError(this.first_line, "もし〜の構文で条件式が使われていません");
			 this.state = 0;
		 }
	 }
}
 
class LoopBegin extends Statement
{
	/**
	 * @constructor
	 * @param {Value} condition nullなら判定しない
	 * @param {boolean} continuous condition==continuousなら継続
	 * @param {Location} loc 
	 */
	constructor(condition, continuous, loc)
	{
		super(loc);
		this.condition = condition;
		this.continuous = continuous;
		this.state = 0;
	}
	clone()
	{
		return new LoopBegin(this.condition.clone(), this.continuous, this.loc);
	}
	run()
	{
		if(this.state == 0)
		{
			if(this.condition) code[0].stack.unshift({statementlist: [this.condition], index: 0});
			this.state = 1;
		}
		else
		{
			if(!this.condition || this.condition.getValue().value == this.continuous) code[0].stack[0].index++;
			else code[0].stack[0].index = -1;
			this.state = 0;
		}
	}
}

class LoopEnd extends Statement
{
	/**
	 * @constructor
	 * @param {Value} condition nullなら判定しない
	 * @param {boolean} continuous condition==continuousなら継続
	 * @param {Location} loc 
	 */
	constructor(condition, continuous, loc)
	{
		super(loc);
		this.condition = condition;
		this.continuous = continuous;
		this.state = 0;
	}
	clone()
	{
		return new LoopEnd(this.condition.clone(), this.continuous, this.loc);
	}
	run()
	{
		if(this.state == 0)
		{
			if(this.condition) code[0].stack.unshift({statementlist: [this.condition], index: 0});
			this.state = 1;
		}
		else
		{
			if(!this.condition || this.condition.getValue().value == this.continuous) code[0].stack[0].index = 0;
			else code[0].stack[0].index = -1;
			this.state = 0;
		}
	}
}

class ForIn extends Statement
{
	constructor(array, variable, statementlist, loc)
	{
		super(loc);
		if(!(variable instanceof Variable || variable instanceof UNDEFINED)) throw new RuntimeError(loc.first_line, "繰り返しのカウンタは変数でなくてはいけません");
		this.array = array;
		this.variable = variable;
		this.statementlist = statementlist;
	}
	clone()
	{
		var state = [];
		for(var i = 0; i < this.statementlist.length; i++) state.push(this.statementlist[i].clone());
		return new ForIn(this.array.clone(), this.variable.clone(), state, this.loc);
	}
	run()
	{
		if(this.varname instanceof UNDEFINED) throw new RuntimeError(this.first_line, "未完成のプログラムです");
		if(this.state == 0)
		{
			code[0].stack.unshift({statementlist: [this.array], index: 0});
			this.state = 1;
		}
		else
		{
			code[0].stack[0].index++;
			let variable = new Variable(this.variable.varname, this.variable.args, this.loc);
			let loop = [new ForIn_step(this, variable, this.array, this.loc), new LoopBegin(new BooleanValue(true, this.loc), true, this.loc)];
			for(let i = 0; i < this.statementlist.length; i++)loop.push(this.statementlist[i]);
			loop.push(new LoopEnd(null, true, this.loc));
			code[0].stack.unshift({statementlist: loop, index: 0});
			this.state = 0;
		}
	}
}

class ForIn_step extends Statement
{
	constructor(forin, variable, array, loc)
	{
		super(loc);
		this.forin = forin;
		this.variable = variable;
		this.array = array;
		this.index = 0;
	}
	clone()
	{
		return new ForIn_step(this.forin.clone(), this.variable.clone(), this.array.clone(), this.loc);
	}
	run()
	{
		code[0].stack[0].index++;
		if(this.index < this.array.rtnv.length)
		{
			let assign = new Assign(this.variable, this.array.rtnv.rtnv.value[this.index++], null, this.loc);
			code[0].stack.unshift({statementlist: [assign], index: 0});
		}
		else
		{
			code[0].stack[0].statementlist[1] = new LoopBegin(new BooleanValue(false, true, this.loc),true, this.loc);
		}
	}
}


/**
 * forループ（加算）
 */
 class ForInc extends Statement
 {
	 /**
	  * @constructor
	  * @param {Variable} varname 
	  * @param {Value} begin 
	  * @param {Value} end 
	  * @param {Value} step 
	  * @param {Array<Statement>} statementlist 
	  * @param {Location} loc 
	  */
	 constructor(varname, begin, end, step, statementlist,loc)
	 {
		 super(loc);
		 if(!(varname instanceof Variable || varname instanceof UNDEFINED)) throw new RuntimeError(loc.first_line, "繰り返しのカウンタは変数でなくてはいけません");
		 this.varname = varname;
		 this.begin = begin;
		 this.end = end;
		 this.step = step;
		 this.statementlist = statementlist;
		 this.state = 0;
	 }
	 clone()
	 {
		 var state = [];
		 for(var i = 0; i < this.statementlist.length; i++) state.push(this.statementlist[i].clone());
		 return new ForInc(this.varname.clone(), this.begin.clone(), this.end.clone(), this.step.clone(), state, this.loc);
	 }
	 run()
	 {
		 if(this.varname instanceof UNDEFINED) throw new RuntimeError(this.first_line, "未完成のプログラムです");
		 if(this.state == 0)
		 {
			 code[0].stack.unshift({statementlist: [new Assign(this.varname, this.begin, null, this.loc)], index: 0});
			 this.state = 1;
		 }
		 else
		 {
			 code[0].stack[0].index++;
			 if(this.begin.getValue() instanceof IntValue || this.begin.getValue() instanceof FloatValue)
			 {
				 let variable = new Variable(this.varname.varname, this.varname.args,this.loc);
				 let condition = new LE(variable, this.end, this.loc);	// IncとDecの違うところ
				 let loop = [variable, condition, new LoopBegin(condition, true, this.loc)];
				 for(let i = 0; i < this.statementlist.length; i++)loop.push(this.statementlist[i]);
				 loop.push(this.step);
				 loop.push(new Assign(variable, this.step, '+', this.loc));	// IncとDecの違うところ
				 loop.push(new LoopEnd(null, true, this.loc));
				 code[0].stack.unshift({statementlist: loop, index: 0});
			 }
			 else throw new RuntimeError(this.first_line, '初期値は数値型である必要があります');
			 this.state = 0;
		 }
	 }
 }

 class ForDec extends Statement
 {
	 constructor(varname, begin, end, step, statementlist,loc)
	 {
		 super(loc);
		 if(!(varname instanceof Variable || varname instanceof Variable)) throw new RuntimeError(loc.first_line, "繰り返しのカウンタは変数でなくてはいけません");
		 this.varname = varname;
		 this.begin = begin;
		 this.end = end;
		 this.step = step;
		 this.statementlist = statementlist;
		 this.state = 0;
	 }
	 clone()
	 {
		 var state = [];
		 for(var i = 0; i < this.statementlist.length; i++) state.push(this.statementlist[i].clone());
		 return new ForInc(this.varname.clone(), this.begin.clone(), this.end.clone(), this.step.clone(), state, this.loc);
	 }
	 run()
	 {
		 if(this.varname instanceof UNDEFINED) throw new RuntimeError(this.first_line, "未完成のプログラムです");
		 if(this.state == 0)
		 {
			 code[0].stack.unshift({statementlist: [new Assign(this.varname, this.begin.getValue(), null, this.loc)], index: 0});
			 this.state = 1;
		 }
		 else
		 {
			 code[0].stack[0].index++;
			 if(this.begin.getValue() instanceof IntValue || this.begin.getValue() instanceof FloatValue)
			 {
				 let variable = new Variable(this.varname.varname, this.varname.args,this.loc);
				 let condition = new GE(variable, this.end, this.loc);	// IncとDecの違うところ
				 let loop = [variable, condition, new LoopBegin(condition, true, this.loc)];
				 for(let i = 0; i < this.statementlist.length; i++)loop.push(this.statementlist[i]);
				 loop.push(this.step);
				 loop.push(new Assign(variable, this.step, '-', this.loc));	// IncとDecの違うところ
				 loop.push(new LoopEnd(null, true, this.loc));
				 code[0].stack.unshift({statementlist: loop, index: 0});
			 }
			 else throw new RuntimeError(this.first_line, '初期値は数値型である必要があります');
			 this.state = 0;
		 }
	 }
 }
 
class Until extends Statement
{
	constructor(statementlist, condition, loc)
	{
		super(loc);
		this.condition = condition;
		this.statementlist = statementlist;
	}
	clone()
	{
		var state = []
		for(var i = 0; i < this.statementlist.length; i++) state.push(this.statementlist[i].clone());
		return new Until(this.condition.clone(), state, this.loc);
	}
	run()
	{
		code[0].stack[0].index++;
		let loop = [new LoopBegin(null, false, this.loc)];
		for(var i = 0; i < this.statementlist.length; i++) loop.push(this.statementlist[i].clone());
		loop.push(new LoopEnd(this.condition, false, this.loc));
		code[0].stack.unshift({statementlist: loop, index: 0});
	}
}

class While extends Statement
{
	constructor(condition, statementlist, loc)
	{
		super(loc);
		this.condition = condition;
		this.statementlist = statementlist;
		this.state = 0;
	}
	clone()
	{
		var state = [];
		for(var i = 0; i < this.statementlist.length; i++) state.push(this.statementlist[i].clone());
		return new While(this.condition.clone(), state, this.loc);
	}
	run()
	{
		code[0].stack[0].index++;
		let loop = [new LoopBegin(this.condition, true, this.loc)];
		for(var i = 0; i < this.statementlist.length; i++) loop.push(this.statementlist[i].clone());
		loop.push(new LoopEnd(null, false, this.loc));
		code[0].stack.unshift({statementlist: loop, index: 0});
	}
}

class SleepStatement extends Statement
{
	constructor(sec, loc)
	{
		super(loc)
		this.sec = new IntValue(sec.value, loc); // milli seconds
	}
	clone()
	{
		return new SleepStatement(this.sec.clone(), this.loc);
	}
	run()
	{
		wait_time = this.sec.value;
		code[0].stack[0].index++;
	}
}

class NopStatement extends Statement
{
	constructor(loc) {super(loc);}
	clone()	{return new NopStatement(this.loc);}
	run(){ code[0].stack[0].index++;}
}

class PauseStatement extends Statement
{
	constructor(loc) {super(loc);}
	clone(){return new PauseStatement(this.loc);}
	run(){code[0].stack[0].index++; }
}

class BreakStatement extends Statement
{
	constructor(loc){super(loc);}
	clone()
	{
		return new BreakStatement(this.loc);
	}
	run()
	{
		while(true)
		{
			var block = code[0].stack.shift();
			if(!block) throw new RuntimeError(this.first_line, '繰り返しの中ではありません。');
			for(var i = 0; i < block.statementlist.length; i++)
				if(block.statementlist[i] instanceof LoopBegin) return;
		}
	}
}

function highlightLine(l)
{
	var elem = document.getElementById('bcralnit_sourceTextarea0').firstElementChild;
	var child = elem.firstElementChild;
	var line = 1;
//	$("#sourceTextarea").focus();
	while(child)
	{
		if(child.tagName == 'SPAN')
		{
			if(line++ == l)
			{
				child.style.background = 'red';
				child.style.color = 'white';
			}
			else
			{
				child.style.background = 'transparent';
				child.style.color = 'black';
			}
		}
		child = child.nextElementSibling;
	}
}

/**
 *  実行状態の初期化
 *  @param {boolean} b 出力エリアを初期化する
 */
function reset(b = true)
{
	varTables = [new varTable()];
	myFuncs = {};
	current_line = -1;
	if(b){
		textareaClear();
		highlightLine(-1);
		var canvas = document.getElementById('canvas');
		canvas.style.display = 'none';
		document.getElementById('input_status').style.visibility = 'hidden';
		context = null;
		Plotly.purge(document.getElementById('graph'));
	}
	setRunflag(false);
	code = null;
	var input_area = document.getElementById('input_area');
	input_area.readOnly = true;
	input_area.value = '';
	wait_time = 0;
	timeouts = [];
	selected_quiz_input = selected_quiz_output = 0;
	output_str = '';
	storage_list_update();
}
 
function setRunflag(b)
{
	run_flag = b;
	document.getElementById("sourceTextarea").readOnly = b;
	document.getElementById("runButton").innerHTML = b & !step_flag ? "中断" : "実行";
	document.getElementById("dumpButton").disabled = !step_flag;
	setEditableflag(!b);
}
/**
 *  編集可能フラグの設定
 * @param {boolean} b 
 */
 function setEditableflag(b)
 {
	 editable_flag = b;
	 document.getElementById("drawButton").disabled = !b;
	//  document.getElementById("urlButton").disabled = !b;
 }
 
function run()
{
	if(code == null)
	{
		try
		{
			reset();
			var source = document.getElementById("sourceTextarea").value+"\n";
			code = [new parsedMainRoutine(dncl.parse(source))];
		}
		catch(e)
		{
			if(selected_quiz < 0)
			{
				if(e.line) textareaAppend(e.line + "行目");
				textareaAppend("構文エラーです\n" + e.message + "\n");
				reset(false);
				return;
			}
			else throw e;
		}
	}
	setRunflag(true);
	step();
}

// busy wait !!
function wait(ms)
{
	let t1 = Date.now();
	while(Date.now() - t1 < ms)
		;
}

function step()
{
	if(selected_quiz < 0)
	{
		// 次の行まで進める
		var l = current_line;
		do{
			next_line();
		}while(run_flag && l == current_line);
		if(!code) return;
		if(code[0] && code[0].stack.length > 0)
		{
			if(run_flag && !step_flag)
			{
				if(wait_time > 0) 
				{
					wait(wait_time);
					wait_time = 0;
				}
				setZeroTimeout(step, 0);
			}
		}
		else
		{
			textareaAppend("---\n");
			reset(false);
		}
	}
	else
	{
		do{
			next_line();
			if(Date.now() > test_limit_time) throw new RuntimeError(-1, '時間がかかりすぎです。');
		}while(run_flag && code[0] && code[0].stack.length > 0);
	}
}

function next_line()
{
	var index = code[0].stack[0].index;
	var statement = code[0].stack[0].statementlist[index];
	if(statement)
	{
		try{
			// code_dump();
			statement.run();
		}
		catch(e)
		{
			if(selected_quiz < 0)
			{
				if(e instanceof RuntimeError) textareaAppend("実行時エラーです\n" + e.line + "行目:" + e.message + "\n");
				else textareaAppend("実行時エラーです\n" + e + "\n");
				reset(false);
			}
			else throw e;
		}
	}
	else code[0].stack[0].index++;
	if(!code || !code[0]) return;
	// 不要になったコードをstackから捨てる
	index = code[0].stack[0] ? code[0].stack[0].index : -1;
	while(index < 0 || index >= code[0].stack[0].statementlist.length)
	{
		code[0].stack.shift();
		while(code[0] && code[0].stack.length < 1) code.shift();
		if(code.length < 1) break;
		index = (code[0] && code[0].stack[0]) ? code[0].stack[0].index : -1;
	}
	if(selected_quiz < 0)
	{
		// 次の行をハイライト表示する
		if(code[0] && code[0].stack[0])
		{
			index = code[0].stack[0].index;
			statement = code[0].stack[0].statementlist[index];
			if(statement && (statement instanceof Statement))
			{
				if(statement.loc) highlightLine(current_line = statement.first_line);
				if(statement instanceof PauseStatement) step_flag = true;
			}
		}
		else highlightLine(++current_line);
	}
}


function editButton(add_code)
{
	if(document.getElementById("sourceTextarea").readOnly) 
	{
		window.alert("プログラム実行・中断中はプログラムを編集できません");
		return;
	}

	var sourceTextArea = document.getElementById("sourceTextarea");
	var pos = sourceTextArea.selectionStart;
	var code = sourceTextArea.value;
	var code1 = code.slice(0, pos);
	var code2 = code.slice(pos, code.length);
	var re1 = /[│ 　]*$/;
	var re2 = /[│ 　\n]/;
	var add_codes = add_code.split("\n");
	var tab = "";
	var array = re1.exec(code1);
	if(array != null) tab = array[0];
	// console.log("[" + pos + ":" +code[pos]+"]");
	if((code[pos] && code[pos] != "\n") || (pos > 0 && !re2.exec(code[pos - 1])))
	{
		alert("この位置で入力支援ボタンを押してはいけません");
		sourceTextArea.focus();
		return;
	}
	for(var c in add_codes) if(c > 0) add_codes[c] = tab + add_codes[c];
	sourceTextArea.value = code1 + add_codes.join("\n") + code2;
	sourceTextArea.selectionStart = sourceTextArea.selectionEnd = sourceTextArea.value.length - code2.length;
	sourceTextArea.focus();
}

function keyUp(e)
{
	var evt = e || window.event;
	var sourceTextArea = document.getElementById("sourceTextarea");
	var pos = sourceTextArea.selectionStart;
	var code = sourceTextArea.value;
	var code1 = code.slice(0, pos);
	var code2 = code.slice(pos, code.length);
	var re1 = /《[^》《]*$/;
	var re2 = /^[^》《]*》/;
	var re3 = /\n?([│]*)([^│\n]*?)\n$/;
	var re4 = /(ならば|なければ|(の間|繰り返し|繰返し|(増|減)やし(ながら|つつ))[，,、])$/;
	var re4a= /^(関数|手続き).*\(.*\)$/;
	var re5 = /^\n/;
	var tab = "";
	switch(evt.keyCode)
	{
	case 37: case 38: case 39: case 40:
		if(pos > 0)
		{
			var match1 = re1.exec(code1);
			var match2 = re2.exec(code2);
			if(match1 != null && match2 != null)
			{
				sourceTextArea.setSelectionRange(pos - match1[0].length, pos + match2[0].length);
				return false;
			}
		}
	case 13:	// \n
		if(!re5.exec(code2)) return true;
		var match = re3.exec(code1);
		if(match)
		{
			 tab = match[1] ;
			 if(re4.exec(match[2]) || re4a.exec(match[2])) tab = "│" + tab;
		}
		sourceTextArea.value = code1 + tab + code2;
		pos = code1.length + tab.length;
		sourceTextArea.setSelectionRange(pos, pos);
		return false;
	default:
//		console.log(window.event.keyCode);
		break;
	}
	return true;
}

function mouseClick()
{
	var sourceTextArea = document.getElementById("sourceTextarea");
	var pos = sourceTextArea.selectionStart;
	var code = sourceTextArea.value;
	var code1 = code.slice(0, pos);
	var code2 = code.slice(pos, code.length);
	var re1 = /《[^》《]*$/;
	var re2 = /^[^》《]*》/;
	var match1 = re1.exec(code1);
	var match2 = re2.exec(code2);
	if(match1 != null && match2 != null)
	{
		var start = pos - match1[0].length;
		var end = pos + match2[0].length;
		sourceTextArea.setSelectionRange(start, end);
	}
}

function sampleButton(num)
{
	var sourceTextArea = document.getElementById("sourceTextarea");
	if(dirty && !window.confirm("プログラムをサンプルプログラムに変更していいですか？")) return;
	sourceTextArea.value = sample[num];
	reset();
	if(flowchart) codeChange();
	$('#sourceTextarea').focus();
	makeDirty(false);
}


function insertCode(add_code)
{
	if(document.getElementById("sourceTextarea").readOnly) 
	{
		window.alert("プログラム実行・中断中はプログラムを編集できません");
		return;
	}
	var sourceTextArea = document.getElementById("sourceTextarea");
	var pos1 = sourceTextArea.selectionStart;
	var pos2 = sourceTextArea.selectionEnd;
	var code = sourceTextArea.value;
	var code1 = code.slice(0, pos1);
	var code2 = code.slice(pos2, code.length);
	sourceTextArea.value = code1 + add_code + code2;
}

function registerEvent(elem, ev, func)
{
	if(elem.addEventListener) elem.addEventListener(ev, func);
	else if(elem.attachEvent) elem.attachEvent('on' + ev, func);
}


/**************************************** flowchart **********************************/

var dragging = false;
var mouseX, mouseY;

class point
{
	constructor(){this._x = this._y = 0;}
	get x(){return this._x;} set x(v){this._x = v;}
	get y(){return this._y;} set y(v){this._y = v;}
	clone(){var p = new point(); p.x = this.x; p.y = this.y; return p;}
}

function mouseDown(e)
{
	var rect = document.getElementById("flowchart").getBoundingClientRect();
	var x = e.clientX - rect.left;
	var y = e.clientY - rect.top;
	var parts = flowchart.findParts(x, y);
	if(parts == null) return;
	dragging = true;
	mouseX = x; mouseY = y;
}

function mouseUp(e)
{
	dragging = false;
}

function mouseMove(e)
{
	if(dragging)
	{
		var rect = document.getElementById("flowchart").getBoundingClientRect();
		var x = e.clientX - rect.left;
		var y = e.clientY - rect.top;
		flowchart.moveOrigin(x - mouseX, y - mouseY);
		mouseX = x; mouseY = y;
		flowchart.paint();
	}
}

function doubleclick_Flowchart(evt)
{
	dragging = false;
	var rect = evt.target.getBoundingClientRect();
	var x = evt.clientX - rect.left;
	var y = evt.clientY - rect.top;
	var parts = flowchart.findParts(x, y);
	if(parts == null || parts instanceof Parts_Terminal
		|| parts instanceof Parts_Bar || parts instanceof Parts_Null) return;
	parts.editMe();
}

function variableChange(e)
{
	flowchart.flowchart2code();
	makeDirty(true);
}

function contextMenu_Flowchart(trigger, event)
{
	dragging = false;
	var x = event.offsetX, y = event.offsetY;
	var parts = flowchart.findParts(x, y);
	if(parts == null || parts instanceof Parts_Terminal || parts instanceof Parts_Null) return false;
	if(parts instanceof Parts_Bar)
		return {
			selectableSubMenu: true,
			events:{
				show: function(){parts.highlight();},
				hide: function(){flowchart.paint(); flowchart.flowchart2code();}
			},
			callback: function(k, e){callbackPartsBar(parts, k);},
			items: {
				input: {name: "入力", icon: "input"},
				output: {name: "出力", icon: "output"},
				substitute: {name: "代入", icon: "assign"},
				if:{name:"分岐", icon: "if"},
				loop:{name:"ループ", icon: "loop",
					items:{
						loop1: {name:"前条件"},
						loop2: {name:"後条件"},
						loopinc:{name:"増やしながら"},
						loopdec:{name:"減らしながら"}
					}
				},
				misc:{name:"各種命令"}
//				separator2:"-----",
//				paste:{name:"ペースト"}
			}
		};
	return {
		callback: function(k,e){callbackParts(parts, k);},
		events:{
			show: function(){parts.highlight();},
			hide: function(){flowchart.paint(); flowchart.flowchart2code();}
		},
		items: {
			edit:{ name:"編集"},
			delete: { name:"削除"}
//			cut:{name:"カット"}
		}
	};
}

function callbackPartsBar(bar, key)
{
	if(document.getElementById("sourceTextarea").readOnly) 
	{
		window.alert("プログラム実行・中断中はプログラムを編集できません");
		return;
	}
	bar.highlight();
	if(key == "input") Parts_Input.appendMe(bar);
	else if(key == "output") Parts_Output.appendMe(bar);
	else if(key == "substitute") Parts_Substitute.appendMe(bar);
	else if(key == "if") Parts_If.appendMe(bar);
	else if(key == "loop1") Parts_LoopBegin1.appendMe(bar);
	else if(key == "loop2") Parts_LoopBegin2.appendMe(bar);
	else if(key == "loopinc") Parts_LoopBeginInc.appendMe(bar);
	else if(key == "loopdec") Parts_LoopBeginDec.appendMe(bar);
	else if(key == "misc") Parts_Misc.appendMe(bar);
	else return;
	makeDirty(true);
}

function callbackParts(parts, key)
{
	if(document.getElementById("sourceTextarea").readOnly)
	{
		window.alert("プログラム実行・中断中はプログラムを編集できません");
		return;
	}
	if(parts instanceof Parts_Terminal) return false;
	if(key == "edit"){parts.editMe();}
	else if(key == "delete"){parts.deleteMe(); makeDirty(true);}
	else if(key == "cut"){parts.cutMe(); makeDirty(true);}
}

var FlowchartSetting = {
    size: 6,
    fontsize: 12,
};

function changeSize(v)
{
	if(v > 0) FlowchartSetting.size++;
	else if(v < 0)
	{
		if(FlowchartSetting.size > 3) FlowchartSetting.size--;
	}
	else FlowchartSetting.size = 6;
	flowchart.paint();
}


function variable2code(ty, id)
{
	var code = document.getElementById(id).value.trim();
	if(code != "") return ty + ' ' + code + "\n";
	return '';
}


class Flowchart
{
    constructor()
    {
		this._canvas = document.getElementById("flowchart");
		this._context = this._canvas.getContext('2d');
        this.makeEmpty();
    }
	get x0(){return this._x0;}
	get y0(){return this._y0;}
	get canvas(){return this._canvas;}
	get context(){return this._context;}
	setOrigin(x, y) {this._x0 = x; this._y0 = y;}
	moveOrigin(x, y){this._x0 += x; this._y0 += y;}
    makeEmpty()
    {
		this.setOrigin(this.canvas.width / 2, FlowchartSetting.size);
        this.top = new Parts_Terminal();
        var bar = new Parts_Bar();
        var end = new Parts_Terminal();
        this.top.next = bar;
        bar.next = end;
        this.top.setValue("はじめ");
        end.setValue("おわり");
    }
    code2flowchart(parse)
    {
		flowchart.makeEmpty();
		Flowchart.appendParts(this.top.next, parse);
		flowchart.paint();
	}
	static appendParts(parts, statementlist)
	{
		for(var i = 0; i < statementlist.length; i++)
		{
			var p = statementlist[i];
			if(!p) continue;
			var statement = constructor_name(p);
			if(statement == "Assign")
			{
				var p1 = new Parts_Substitute();
				var b1 = new Parts_Bar();
				p1.setValue(p.variable.getCode(), p.value.getCode());
				parts.next = p1;
				parts = p1.next = b1;
			}
			else if(statement == "Input")
			{
				var p1 = new Parts_Input();
				var b1 = new Parts_Bar();
				p1.setValue(p.varname.getCode());
				parts.next = p1;
				parts = p1.next = b1;
			}
			else if(statement == "Output")
			{
				var p1 = new Parts_Output();
				var b1 = new Parts_Bar();
				var v0 = [];
				for(var j = 0; j < p.value.length; j++) v0.push(p.value[j].getCode());
				p1.setValue(v0.join(','), p.ln);
				parts.next = p1;
				parts = p1.next = b1;
			}
			else if(statement == "Newline")
			{
				var p1 = new Parts_Output();
				var b1 = new Parts_Bar();
				p1.setValue('改行', true);
				parts.next = p1;
				parts = p1.next = b1;
			}
			else if(statement == "If")
			{
				var p1 = new Parts_If();
				var b1 = new Parts_Bar(), b2 = new Parts_Bar(), b3 = new Parts_Bar();
				var n1 = new Parts_Null(), n2 = new Parts_Null(), n3 = new Parts_Null();
				p1.setValue(p.condition.getCode());
				parts.next = p1; 
				p1.next = n1; n1.next = b1;
				p1.left = b2; b2._prev = p1; b2.next = n2;
				p1.right = b3; b3._prev = p1; b3.next = n3;
				if(p.state1) Flowchart.appendParts(b2, p.state1);
				if(p.state2) Flowchart.appendParts(b3, p.state2);
				parts = b1;
			}
			else if(statement == "ForInc")
			{
				var p1 = new Parts_LoopBeginInc(), p2 = new Parts_LoopEnd();
				var b1 = new Parts_Bar(), b2 = new Parts_Bar();
				p1.setValue(p.varname.getCode(), p.begin.getCode(), p.end.getCode(), p.step.getCode());
				parts.next = p1; 
				p1.next = b1; b1.next = p2; p2.next = b2;
				p1._end = p2; p2._begin = p1;
				Flowchart.appendParts(b1, p.statementlist);
				parts = b2;
			}
			else if(statement == "ForDec")
			{
				var p1 = new Parts_LoopBeginDec(), p2 = new Parts_LoopEnd();
				var b1 = new Parts_Bar(), b2 = new Parts_Bar();
				p1.setValue(p.varname.getCode(), p.begin.getCode(), p.end.getCode(), p.step.getCode());
				parts.next = p1; 
				p1.next = b1; b1.next = p2; p2.next = b2;
				p1._end = p2; p2._begin = p1;
				Flowchart.appendParts(b1, p.statementlist);
				parts = b2;
			}
			else if(statement == "Until")
			{
				var p1 = new Parts_LoopBegin2(), p2 = new Parts_LoopEnd2();
				var b1 = new Parts_Bar(), b2 = new Parts_Bar();
				p1.setValue(p.condition.getCode());
				parts.next = p1; 
				p1.next = b1; b1.next = p2; p2.next = b2;
				p1._end = p2; p2._begin = p1;
				Flowchart.appendParts(b1, p.statementlist);
				parts = b2;
			}
			else if(statement == "While")
			{
				var p1 = new Parts_LoopBegin1(), p2 = new Parts_LoopEnd();
				var b1 = new Parts_Bar(), b2 = new Parts_Bar();
				p1.setValue(p.condition.getCode());
				parts.next = p1; 
				p1.next = b1; b1.next = p2; p2.next = b2;
				p1._end = p2; p2._begin = p1;
				Flowchart.appendParts(b1, p.statementlist);
				parts = b2;
			}
			else if(statement == "FileIOStatement")
			{
				var p1 = new Parts_Misc();
				var b1 = new Parts_Bar();
				p1.setValue(p.command, p.args);
				parts.next = p1;
				parts = p1.next = b1;
			}
			else if(statement == "GraphicStatement")
			{
				var p1 = new Parts_Misc();
				var b1 = new Parts_Bar();
				p1.setValue(p.command, p.args);
				parts.next = p1;
				parts = p1.next = b1;
			}
			else if(statement == "SleepStatement")
			{
				var p1 = new Parts_Misc();
				var b1 = new Parts_Bar();
				p1.setValue("sleep", [p.sec]);
				parts.next = p1;
				parts = p1.next = b1;
			}
			else if(statement == "DumpStatement")
			{
				var p1 = new Parts_Misc();
				var b1 = new Parts_Bar();
				p1.setValue("dump",[]);
				parts.next = p1;
				parts = p1.next = b1;
			}
		}
	}

	flowchart2code()
    {
		if(!flowchart_display) return;
        var code = '';
		document.getElementById("sourceTextarea").value = code;
    }
    paint()
    {
        if(!flowchart_display) return;

		var canvas_width = this.canvas.width;
		var canvas_height = this.canvas.height;
		var p0 = new point(), p1 = new point(), p2 = new point();
		this.context.clearRect(0, 0, canvas_width, canvas_height);
        FlowchartSetting.fontsize = FlowchartSetting.size * 2;
        this.context.font = FlowchartSetting.fontsize + "px 'sans-serif'";
        this.context.strokeStyle = "rgb(0,0,0)";
        this.context.fillStyle = "rgb(0,0,0)";
        this.context.lineWidth = "1px";
        this.top.calcSize(p0, p1, p2);	// p1が左上，p2が右下
        this.top.paint({x:this.x0, y:this.y0});
    }

	findParts(x, y)
	{
		return this.top.findParts(x, y);
	}

}

class Parts
{
    constructor()
    {
        this._text = "";
        this._next = this._prev = null;
        this._textwidth = this._textheight = this._width = this._height = 0;
		this._hspace = this._hspace2 = 0;
    }
    get x1(){return this._x1;} set x1(x){this._x1 = x;} // paintで計算する
    get y1(){return this._y1;} set y1(y){this._y1 = y;}
    get x2(){return this._x2;} set x2(x){this._x2 = x;}
    get y2(){return this._y2;} set y2(y){this._y2 = y;}
    get text(){return this._text;}
    get next(){return this._next;}
	set next(p){
		p._next = this.next;
		p._prev = this;
		if(this.next != null) this.next._prev = p;
		this._next = p;
	}
    get prev(){return this._prev;}
	get end(){return this;}						// ブロックの終わりのパーツ
    get width(){return this._width;}          // calcSizeで計算する
    get height(){return this._height;}         // calcSizeで計算する
    get textWidth(){return this._textwidth;}      // calcSizeで計算する
    get textHeight(){return this._textheight;}     // calcSizeで計算する
	get hspace(){return this._hspace;}
	get hspace2(){return this._hspace2;}

	get isBlockEnd(){return false;}

	inside(x, y)
	{
		return this.x1 <= x && x <= this.x2 && this.y1 <= y && y <= this.y2;
	}
	findParts(x, y)
	{
		var p = this;
		while(p != null && ! (p instanceof Parts_Null))
		{
			if(p.inside(x, y)) return p;
			if(p instanceof Parts_If)
			{
				var p1 = p.left.findParts(x, y);
				if(p1) return p1;
				p1 = p.right.findParts(x, y);
				if(p1) return p1;
				p = p.end.next;
			}
			else p = p.next;
		}
		if(p != null && p.next != null) return p.next.findParts(x,y);
		return null;
	}

    paint(position)
    {
        if(this.next != null) return this.next.paint(position);
		return this;
    }
    calcTextsize()
    {
        if(this.text != null && this.text != "")
        {
			var size = FlowchartSetting.size;
            var metrics = flowchart.context.measureText(this.text);
			this._hspace = 0;
            this._textwidth = metrics.width;
			if(this._textwidth < size * 4)
			{
				this._hspace = (size * 4 - this._textwidth) / 2;
				this._textwidth = size * 4;
			}
            this._textheight = FlowchartSetting.fontsize;
        }
    }
    calcSize(p0, p1, p2)
    {
		if(this.next == null || this.isBlockEnd) return this;
		return this.next.calcSize(p0,p1,p2);
    }
	static appendMe(bar)
	{

	}
    appendCode(code, indent)
	{
		if(this.next != null) return this.next.appendCode(code, indent);
		return code;
	}
    static makeIndent(indent_level)
    {
        var s = "";
        for(var i = 0; i < indent_level; i++) s += "│";
        return s;
    }
	editMe()
	{

	}
	deleteMe()
	{
		this.prev._next = this.end.next.next;
		this.end.next.next._prev = this.prev;
		this.end._next = null;
		this._next = null;
	}
	cutMe()
	{

	}

	paint_highlight()
	{
		flowchart.context.strokeStyle = "rgb(255,0,0)";
		flowchart.context.fillStyle = "rgb(255,0,0)";
		flowchart.context.clearRect(this.x1, this.y1, this.x2 - this.x1, this.y2 - this.y1);
		this.paint(null);
		flowchart.context.strokeStyle = "rgb(0,0,0)";
		flowchart.context.fillStyle = "rgb(0,0,0)";
	}
	paint_unhighlight()
	{
		flowchart.context.clearRect(this.x1, this.y1, this.x2 - this.x1, this.y2 - this.y1);
		this.paint(null);
	}

	highlight()
	{
		this.paint_highlight();
	}
	unhighlight()
	{
		this.paint_unhighlight();
	}
}

class Parts_Null extends Parts
{
	get isBlockEnd(){ return true;}
}

class Parts_Bar extends Parts
{
    calcSize(p0,p1,p2)
    {
        this._width = 0;
        this._height = FlowchartSetting.size * 3;
		p0.y += this._height;
		if(p0.y > p2.y) p2.y = p0.y;
		return this.next.calcSize(p0,p1,p2);
    }
	inside(x, y)
	{
		var near = 4;
		return this.x1 - near <= x && x <= this.x2 + near && this.y1 <= y && y <= this.y2;
	}
    paint(position)
    {
		if(position != null)
		{
			this.x1 = this.x2 = position.x;
	        this.y1 = position.y;
	        this.y2 = this.y1 + this.height;
		}
        flowchart.context.beginPath();
        flowchart.context.moveTo(this.x1, this.y1);
        flowchart.context.lineTo(this.x2, this.y2);
        flowchart.context.stroke();
		if(position != null)
		{
			position.x = this.x2; position.y = this.y2;
	        return this.next.paint(position);
		}
		return this;
    }
}

class Parts_Terminal extends Parts
{
    calcSize(p0,p1,p2)
    {
        this.calcTextsize();    // textWidth, textHeightの計算
        this._height = this._textheight + FlowchartSetting.size * 2;
        this._width = this._textwidth + this._height;
		var x1 = p0.x - this.width / 2;
		var x2 = p0.x + this.width / 2;
		var y2 = p0.y + this.height;
		if(x1 < p1.x) p1.x = x1;
		if(x2 > p2.x) p2.x = x2;
		if(y2 > p2.y) p2.y = y2;
		p0.y = y2;
		if(this.next == null) return this;
		return this.next.calcSize(p0,p1,p2);
    }
	setValue(v)
	{
		this._text = v;
	}
    paint(position)
    {
		var size = FlowchartSetting.size;
		if(position != null)
		{
			this.x1 = position.x - this.textWidth / 2 - this.height / 2;
	        this.x2 = position.x + this.textWidth / 2 + this.height / 2;
	        this.y1 = position.y;
	        this.y2 = position.y + this.height;
		}
        flowchart.context.beginPath();    // 上
        flowchart.context.moveTo(this.x1 + this.height / 2, this.y1);
        flowchart.context.lineTo(this.x2 - this.height / 2, this.y1);
        flowchart.context.stroke();
        flowchart.context.beginPath();    // 右
        flowchart.context.arc(this.x2 - this.height / 2, this.y1 + this.height / 2,
            this.height / 2, Math.PI / 2, - Math.PI / 2, true);
        flowchart.context.stroke();
        flowchart.context.beginPath();    // 下
        flowchart.context.moveTo(this.x1 + this.height / 2, this.y2);
        flowchart.context.lineTo(this.x2 - this.height / 2, this.y2);
        flowchart.context.stroke();
        flowchart.context.beginPath();    // 左
        flowchart.context.arc(this.x1 + this.height / 2, this.y1 + this.height / 2,
            this.height / 2, 3 * Math.PI / 2, Math.PI / 2, true);
        flowchart.context.stroke();
        flowchart.context.fillText(this.text, this.x1 + this.height / 2, this.y2 - FlowchartSetting.size);
		if(position != null)
		{
			position.y += this.height;
			if(this.end.next != null) return this.end.next.paint(position);
			return this.end;
		}
		return this;
    }
}

class Parts_Output extends Parts
{
	constructor()
	{
		super();
		this.setValue("《値》", true);
	}
	get newline(){return this._newline;}
	setValue(v, nl)
	{
		this._text = v;
		this._newline = nl;
	}
	calcSize(p0,p1,p2)
    {
        this.calcTextsize();    // textWidth, textHeightの計算
		var size = FlowchartSetting.size;
        this._height = this._textheight + size * 2;
        this._width = this._textwidth + size * 2 + this._height / 2;
		var x1 = p0.x - this.width / 2;
		var x2 = p0.x + this.width / 2;
		var y2 = p0.y + this.height;
		if(x1 < p1.x) p1.x = x1;
		if(x2 > p2.x) p2.x = x2;
		if(y2 > p2.y) p2.y = y2;
		p0.y = y2;
		return this.next.calcSize(p0,p1,p2);
    }
    paint(position)
	{
		var size = FlowchartSetting.size;
		if(position != null)
		{
			this.x1 = position.x - this.width / 2;
			this.x2 = position.x + this.width / 2;
			this.y1 = position.y;
			this.y2 = this.y1 + this.height;
		}
		flowchart.context.beginPath();
		flowchart.context.moveTo(this.x2 - this.height / 2, this.y1);
		flowchart.context.lineTo(this.x1 + size * 2, this.y1);
		flowchart.context.lineTo(this.x1, (this.y1 + this.y2) / 2);
		flowchart.context.lineTo(this.x1 + size * 2, this.y2);
		flowchart.context.lineTo(this.x2 - this.height / 2, this.y2);
		flowchart.context.stroke();
		flowchart.context.beginPath();
		flowchart.context.arc(this.x2 - this.height / 2, (this.y1 + this.y2) / 2, this.height / 2,
			Math.PI / 2, -Math.PI /2, true);
		flowchart.context.stroke();

		flowchart.context.fillText(this.text, this.x1 + size * 2 + this.hspace, this.y2 - size);

		if(!this.newline && this.text != '改行')	// 改行なしマーク
		{
			var x = this.x2 - this.height / 2;
			var y = this.y1 + size;
			flowchart.context.beginPath();
			flowchart.context.moveTo(x + size, y);
			flowchart.context.lineTo(x + size, y + this.textHeight);
			flowchart.context.lineTo(x , y + this.textHeight);
			flowchart.context.stroke();
			flowchart.context.beginPath();
			flowchart.context.moveTo(x + size / 2, y + this.textHeight - size / 4);
			flowchart.context.lineTo(x , y + this.textHeight);
			flowchart.context.lineTo(x + size / 2, y + this.textHeight + size / 4);
			flowchart.context.stroke();
			x += this.height / 4; y += this.textHeight / 2;
			flowchart.context.beginPath(); flowchart.context.moveTo(x - size / 2, y - size / 2); flowchart.context.lineTo(x + size / 2, y + size / 2); flowchart.context.stroke();
			flowchart.context.beginPath(); flowchart.context.moveTo(x + size / 2, y - size / 2); flowchart.context.lineTo(x - size / 2, y + size / 2); flowchart.context.stroke();
		}
		if(position != null)
		{
			position.y = this.y2;
			if(this.end.next != null) return this.end.next.paint(position);
			return this.end;
		}
		return this;
	}
	static appendMe(bar)
	{
		var parts = new Parts_Output();
		bar.next = parts;
		parts.next = new Parts_Bar();
		return parts.next;
	}
	appendCode(code, indent)
	{
		code += Parts.makeIndent(indent);
		if(this.text == '改行') code += '改行する\n';
		else code += this.text + "を" + (this.newline ? "" : "改行なしで") + "表示する\n";
		if(this.next != null) return this.next.appendCode(code, indent);
		return code;
	}
	editMe()
	{
		var subtitle = ["値", "改行"];
		var values = [ this.text , this.newline];
		openModalWindowforOutput("出力の編集", subtitle, values, this);
	}
	edited(values)
	{
		if(values != null)
		{
			this.setValue(values[0], values[1]);
		}
		flowchart.paint();
		flowchart.flowchart2code();
	}
}

class Parts_Input extends Parts
{
	constructor()
	{
		super();
		this.setValue("《変数》");
	}
	setValue(v)
	{
		this._var = v;
		this._text = v + "を入力";
	}
	get var(){return this._var;}
	calcSize(p0,p1,p2)
    {
        this.calcTextsize();    // textWidth, textHeightの計算
		var size = FlowchartSetting.size;
        this._height = this._textheight + size * 2;
        this._width = this._textwidth + size * 4;
		var x1 = p0.x - this.width / 2;
		var x2 = p0.x + this.width / 2;
		var y2 = p0.y + this.height;
		if(x1 < p1.x) p1.x = x1;
		if(x2 > p2.x) p2.x = x2;
		if(y2 > p2.y) p2.y = y2;
		p0.y = y2;
		if(this.next == null || this.isBlockEnd) return this;
		return this.next.calcSize(p0,p1,p2);
    }
    paint(position)
	{
		var size = FlowchartSetting.size;
		if(position != null)
		{
			this.x1 = position.x - this.width / 2;
			this.x2 = position.x + this.width / 2;
			this.y1 = position.y;
			this.y2 = this.y1 + this.height;
		}
		flowchart.context.beginPath();
		flowchart.context.moveTo(this.x1, this.y1 + size);
		flowchart.context.lineTo(this.x2, this.y1 - size);
		flowchart.context.lineTo(this.x2, this.y2);
		flowchart.context.lineTo(this.x1, this.y2);
		flowchart.context.lineTo(this.x1, this.y1 + size);
		flowchart.context.stroke();
		flowchart.context.fillText(this.text, this.x1 + size * 2, this.y2 - size);
		if(position != null)
		{
			position.y = this.y2;
			if(this.end.next != null) return this.end.next.paint(position);
			return this.end;
		}
		return this;
	}
	static appendMe(bar)
	{
		var parts = new Parts_Input();
		bar.next = parts;
		parts.next = new Parts_Bar();
		return parts.next;
	}
	appendCode(code, indent)
	{
		code += Parts.makeIndent(indent);
		code += this.var + " を入力する\n";
		if(this.next != null) return this.next.appendCode(code, indent);
		return code;
	}
	editMe()
	{
		var subtitle = ["変数"];
		var values = [ this.var ];
		openModalWindow("入力の編集", subtitle, values, this);
	}
	edited(values)
	{
		if(values != null)
		{
			this.setValue(values[0]);
		}
		flowchart.paint();
		flowchart.flowchart2code();
	}
}

class Parts_Substitute extends Parts
{
	constructor()
	{
		super();
		this.setValue("《変数》","《値》");
	}
	setValue(variable,value)
	{
		this._var = variable;
		this._val = value;

		this._text = this._var + "←" + this._val;
	}
	get var(){return this._var;}
	get val(){return this._val;}
	calcSize(p0,p1,p2)
    {
        this.calcTextsize();    // textWidth, textHeightの計算
		var size = FlowchartSetting.size;
        this._height = this._textheight + size * 2;
        this._width = this._textwidth + size * 4;
		var x1 = p0.x - this.width / 2;
		var x2 = p0.x + this.width / 2;
		var y2 = p0.y + this.height;
		if(x1 < p1.x) p1.x = x1;
		if(x2 > p2.x) p2.x = x2;
		if(y2 > p2.y) p2.y = y2;
		p0.y = y2;
		if(this.next == null || this.isBlockEnd) return this;
		return this.next.calcSize(p0,p1,p2);
    }
    paint(position)
	{
		var size = FlowchartSetting.size;
		if(position != null)
		{
			this.x1 = position.x - this.width / 2;
			this.x2 = position.x + this.width / 2;
			this.y1 = position.y;
			this.y2 = this.y1 + this.height;
		}
		flowchart.context.beginPath();
		flowchart.context.moveTo(this.x1, this.y1);
		flowchart.context.lineTo(this.x2, this.y1);
		flowchart.context.lineTo(this.x2, this.y2);
		flowchart.context.lineTo(this.x1, this.y2);
		flowchart.context.lineTo(this.x1, this.y1);
		flowchart.context.stroke();
		flowchart.context.fillText(this.text, this.x1 + size * 2, this.y2 - size);

		if(position != null)
		{
			position.y = this.y2;
			if(this.end.next != null) return this.end.next.paint(position);
			return this.end;
		}
		return this;
	}
	static appendMe(bar)
	{
		var parts = new Parts_Substitute();
		bar.next = parts;
		parts.next = new Parts_Bar();
		return parts.next;
	}
	appendCode(code, indent)
	{
		code += Parts.makeIndent(indent);
		code += this.var + " ← " + this.val + "\n";
		if(this.next != null) return this.next.appendCode(code, indent);
		return code;
	}
	editMe()
	{
		var subtitle = ["変数", "値"];
		var values = [ this.var , this.val];
		openModalWindow("代入の編集", subtitle, values, this);
	}
	edited(values)
	{
		if(values != null)
		{
			this.setValue(values[0], values[1]);
		}
		flowchart.paint();
		flowchart.flowchart2code();
	}
}

class Parts_If extends Parts
{
	constructor()
	{
		super();
		this.setValue("《条件》");
		this.left = this.right = null;
		this.left_bar_expand = this.right_bar_expand = 0;
	}
	setValue(cond)
	{
		this._cond = cond;
		this._text = this._cond;
	}
	get condition(){return this._cond;}
	get end(){return this.next;}

	calcTextsize()
    {
        if(this.text != null && this.text != "")
        {
			var size = FlowchartSetting.size;
            var metrics = flowchart.context.measureText(this.text);
			this._hspace = 0;
            this._textwidth = metrics.width;
			if(this._textwidth < size * 6)
			{
				this._hspace = (size * 6 - this._textwidth) / 2;
				this._textwidth = size * 6;
			}
            this._textheight = FlowchartSetting.fontsize;
        }
    }

	calcSize(p0,p1,p2)
    {
        this.calcTextsize();    // textWidth, textHeightの計算
		var size = FlowchartSetting.size;
//		if(this._textwidth < size * 6) this._textwidth = size * 6;
//		if(this._textheight < size * 2) this._textheight = size * 2;
		this.v_margin = size * 2;
		this.h_margin = this.textWidth * this.textHeight / this.v_margin / 4;
        this._height = this.textHeight + this.v_margin * 2;
        this._width = this.textWidth + this.h_margin * 2;
		var x1 = p0.x - this.width / 2;
		var x2 = p0.x + this.width / 2;
		// 左枝
		var pl = new point(); pl.x = x1; pl.y = p0.y;
		var pl1 = pl.clone(), pl2 = pl.clone();
		this.left.calcSize(pl, pl1, pl2);
		this.left_bar_expand = (pl2.x - pl.x); // - this.width / 2;
		if(this.left_bar_expand < size) this.left_bar_expand = size;
		pl1.x -= this.left_bar_expand;
		pl2.x -= this.left_bar_expand;
		if(pl1.x < p1.x) p1.x = pl1.x;
		if(pl1.y > p1.y) p1.y = pl1.y;
		if(pl2.y > p1.y) p1.y = pl2.y;

		// 右枝
		var pr = new point(); pr.x = x2; pr.y = p0.y;
		var pr1 = pr.clone(), pr2 = pr.clone();
		this.right.calcSize(pr, pr1, pr2);
		this.right_bar_expand = (pr.x - pr1.x); // - this.width / 2;
		if(this.right_bar_expand < size) this.right_bar_expand = size;
		pr1.x += this.right_bar_expand;
		pr2.x += this.right_bar_expand;
		if(pr2.x > p2.x) p2.x = pr2.x;
		if(pr1.y > p2.y) p2.y = pr1.y;
		if(pr2.y > p2.y) p2.y = pr2.y;
		// 左枝と右枝がぶつかっていたら，右枝をちょっと伸ばす
		if(pr1.x < pl2.x + size)
		{
			this.right_bar_expand += pl2.x - pr1.x + size;
			p2.x += pl2.x - pr1.x + size;
		}
		return this.end.next.calcSize(p0,p1,p2);
    }
    paint(position)
	{
		var size = FlowchartSetting.size;
		if(position != null)
		{
			this.x1 = position.x - this.width / 2;
			this.x2 = position.x + this.width / 2;
			this.y1 = position.y;
			this.y2 = this.y1 + this.height;
		}
		var x0 = (this.x1 + this.x2) / 2, y0 = (this.y1 + this.y2) / 2;
		flowchart.context.beginPath();
		flowchart.context.moveTo(x0, this.y1);
		flowchart.context.lineTo(this.x1, y0);
		flowchart.context.lineTo(x0, this.y2);
		flowchart.context.lineTo(this.x2, y0);
		flowchart.context.lineTo(x0, this.y1);
		flowchart.context.stroke();
		flowchart.context.fillText(this.text, x0 - this.textWidth / 2 + this.hspace,
			y0 + this.textHeight / 2);

		if(position != null)
		{
			// 左側
			flowchart.context.beginPath();
			flowchart.context.moveTo(this.x1, y0);
			flowchart.context.lineTo(this.x1 - this.left_bar_expand, y0);
			flowchart.context.stroke();
			flowchart.context.fillText('Y', this.x1 - size * 1, y0 - size);
			var left_parts = this.left.paint({x:this.x1 - this.left_bar_expand, y:y0}).prev;
			// 右側
			flowchart.context.beginPath();
			flowchart.context.moveTo(this.x2, y0);
			flowchart.context.lineTo(this.x2 + this.right_bar_expand, y0);
			flowchart.context.stroke();
			flowchart.context.fillText('N', this.x2 + size * 0, y0 - size);
			var right_parts = this.right.paint({x:this.x2 + this.right_bar_expand, y:y0}).prev;
			// 線の下の部分
			var y; 
			if(left_parts.y2 > right_parts.y2)
			{
				y = left_parts.y2;
				flowchart.context.beginPath();
				flowchart.context.moveTo(this.x2 + this.right_bar_expand, right_parts.y2);
				flowchart.context.lineTo(this.x2 + this.right_bar_expand, y);
				flowchart.context.stroke();
				right_parts.y2 = y;
			}
			else
			{
				y = right_parts.y2;
				flowchart.context.beginPath();
				flowchart.context.moveTo(this.x1 - this.left_bar_expand, left_parts.y2);
				flowchart.context.lineTo(this.x1 - this.left_bar_expand, y);
				flowchart.context.stroke();
				left_parts.y2 = y;
			}
			flowchart.context.beginPath();
			flowchart.context.moveTo(this.x1 - this.left_bar_expand, y);
			flowchart.context.lineTo(this.x2 + this.right_bar_expand, y);
			flowchart.context.stroke();
			position.y = y;
			if(this.end.next != null) return this.end.next.paint(position);
//			return this.end;
		}
		return this.end.next;
	}
	static appendMe(bar)
	{
		var parts = new Parts_If();
		bar.next = parts;
		parts.next = new Parts_Null();
		parts.next.next = new Parts_Bar();
		parts.left = new Parts_Bar();
		parts.left._prev = parts;
		parts.left.next = new Parts_Null();
		parts.right = new Parts_Bar();
		parts.right._prev = parts;
		parts.right.next = new Parts_Null();

		return parts.end.next.next;
	}
	appendCode(code, indent)
	{
		code += Parts.makeIndent(indent);
		code += "もし " + this.condition + " ならば\n";
		if(this.left.next instanceof Parts_Null) code += Parts.makeIndent(indent + 1) + "\n";
		else code += this.left.appendCode('', indent + 1);
		if(!(this.right.next instanceof Parts_Null))
		{
			code += Parts.makeIndent(indent) + "を実行し，そうでなければ\n"
			code += this.right.appendCode('', indent + 1);
		}
		code += Parts.makeIndent(indent);
		code += "を実行する\n";

		if(this.end.next != null) return this.end.next.appendCode(code, indent);
		return code;
	}
	editMe()
	{
		var subtitle = ["条件"];
		var values = [ this.condition ];
		openModalWindow("分岐の編集", subtitle, values, this);
	}
	edited(values)
	{
		if(values != null)
		{
			this.setValue(values[0]);
		}
		flowchart.paint();
		flowchart.flowchart2code();
	}
}

class Parts_LoopBegin extends Parts
{
	get hasText(){return false;}
	get end(){return this._end;}
	calcTextsize()
    {
        if(this.hasText)
        {
			var size = FlowchartSetting.size;
			this._textwidth = size * 6;
			this._hspace = this._hspace2 = 0;
			var tw = flowchart.context.measureText(this.text).width;
			if(tw > this._textwidth) this._textwidth = tw;
			var tw2 = flowchart.context.measureText(this.text2).width;
			if(tw2 > this._textwidth) this._textwidth = tw2;
			if(tw < this._textwidth) this._hspace = (this._textwidth - tw) / 2;
			if(tw2 < this._textwidth) this._hspace2 = (this._textwidth - tw2) / 2;
            this._textheight = FlowchartSetting.fontsize;
        }
		else
		{
			this.end.calcTextsize();
			this._textwidth = this.end.textWidth;
			this._textheight = this.end.textHeight;
		}
    }
	calcSize(p0,p1,p2)
    {
        this.calcTextsize();    // textWidth, textHeightの計算
		var size = FlowchartSetting.size;

        this._height = this.textHeight * (this.hasText ? 2 : 1) + size * 2;
        this._width = this.textWidth + size * 2;
		var x1 = p0.x - this.width / 2;
		var x2 = p0.x + this.width / 2;
		var y2 = p0.y + this.height;
		if(x1 < p1.x) p1.x = x1;
		if(x2 > p2.x) p2.x = x2;
		if(y2 > p2.y) p2.y = y2;
		p0.y = y2;
		var n = this.next;
		while(n != this.end) n = n.calcSize(p0, p1, p2);
//		this.next.calcSize(p0,p1,p2);
		return this.end.next.calcSize(p0,p1,p2);
    }
	paint(position)
	{
		var size = FlowchartSetting.size;
		if(position != null)
		{
			this.x1 = position.x - this.width / 2;
			this.x2 = position.x + this.width / 2;
			this.y1 = position.y;
			this.y2 = this.y1 + this.height;
		}
		flowchart.context.beginPath();
		flowchart.context.moveTo(this.x1 + size, this.y1);
		flowchart.context.lineTo(this.x2 - size, this.y1);
		flowchart.context.lineTo(this.x2, this.y1 + size);
		flowchart.context.lineTo(this.x2, this.y2);
		flowchart.context.lineTo(this.x1, this.y2);
		flowchart.context.lineTo(this.x1, this.y1 + size);
		flowchart.context.lineTo(this.x1 + size, this.y1);
		flowchart.context.stroke();
		if(this.hasText)
		{
			flowchart.context.fillText(this.text, this.x1 + size + this.hspace, this.y1 + size + this.textHeight);
			flowchart.context.fillText(this.text2, this.x1 + size + this.hspace2, this.y1 + size + this.textHeight * 2);
		}

		if(position != null)
		{
			position.y = this.y2;
			this.next.paint(position);
			return this.end.next.paint(position);;
		}
		return this;
	}
	deleteMe()
	{
		this.prev._next = this.end.next.next;
		this.end.next.next._prev = this.prev;
		this.end._next = null;
		this._next = null;
	}
	highlight()
	{
		this.paint_highlight();
		this.end.paint_highlight();
	}
	unhighlight()
	{
		this.paint_unhighlight();
		this.end.paint_unhighlight();
	}


}

class Parts_LoopBegin1 extends Parts_LoopBegin
{
	get hasText(){return true;}
	constructor()
	{
		super();
		this.setValue("《条件》");
	}
	setValue(cond)
	{
		this._cond = cond;
		this._text = this._cond;
	}
	get condition(){return this._cond;}
	get text2(){return "の間";}

	static appendMe(bar)
	{
		var parts = new Parts_LoopBegin1();
		bar.next = parts;
		parts.next = new Parts_Bar();
		parts.next.next = new Parts_LoopEnd();
		parts.next.next.next = new Parts_Bar();
		parts._end = parts.next.next;
		parts.next.next._begin = parts;

		return parts.end;
	}

	appendCode(code, indent)
	{
		code += Parts.makeIndent(indent);
		code += this.condition + " の間，\n";
		var code_inner = this.next.appendCode('', indent + 1);
		if(code_inner == '') code += Parts.makeIndent(indent + 1) + "\n";
		else code += code_inner;
		code += Parts.makeIndent(indent) + "を繰り返す\n";

		if(this.end.next != null) return this.end.next.appendCode(code, indent);
		return code;
	}

	editMe()
	{
		var subtitle = ["条件（〜の間）"];
		var values = [ this.condition ];
		openModalWindow("繰り返しの編集", subtitle, values, this);
	}
	edited(values)
	{
		if(values != null)
		{
			this.setValue(values[0]);
		}
		flowchart.paint();
		flowchart.flowchart2code();
	}
}

class Parts_LoopBegin2 extends Parts_LoopBegin
{
	constructor()
	{
		super();
		this.setValue("《条件》");
	}
	setValue(cond)
	{
		this._cond = cond;
		this._text = this._cond;
	}
	get condition(){return this._cond;}

	static appendMe(bar)
	{
		var parts = new Parts_LoopBegin2();
		bar.next = parts;
		parts.next = new Parts_Bar();
		parts.next.next = new Parts_LoopEnd2();
		parts.next.next.next = new Parts_Bar();
		parts._end = parts.next.next;
		parts.next.next._begin = parts;

		return parts.end;
	}

	appendCode(code, indent)
	{
		code += Parts.makeIndent(indent) + "繰り返し，\n";
		var code_inner = this.next.appendCode('', indent + 1);
		if(code_inner == '') code += Parts.makeIndent(indent + 1) + "\n";
		else code += code_inner;
		code += Parts.makeIndent(indent) + "を， "+ this.condition +" になるまで実行する\n";

		if(this.end.next != null) return this.end.next.appendCode(code, indent);
		return code;
	}

	editMe()
	{
		var subtitle = ["条件（〜になるまで）"];
		var values = [ this.condition ];
		openModalWindow("繰り返しの編集", subtitle, values, this);
	}
	edited(values)
	{
		if(values != null)
		{
			this.setValue(values[0]);
		}
		flowchart.paint();
		flowchart.flowchart2code();
	}
}

class Parts_LoopBeginInc extends Parts_LoopBegin
{
	get hasText(){return true;}
	constructor()
	{
		super();
		this.setValue("《変数》","《値》","《値》","《値》");
	}
	setValue(variable, start, goal, step)
	{
		this._var = variable;
		this._start = start;
		this._goal = goal;
		this._step = step;
		this._text = this.var + ':' + this.start + "→" + this.goal;
	}
	get var(){return this._var;}
	get start(){return this._start;}
	get goal(){return this._goal;}
	get step(){return this._step;}
	get text2(){return this.step + "ずつ増";}

	static appendMe(bar)
	{
		var parts = new Parts_LoopBeginInc();
		bar.next = parts;
		parts.next = new Parts_Bar();
		parts.next.next = new Parts_LoopEnd();
		parts.next.next.next = new Parts_Bar();
		parts._end = parts.next.next;
		parts.next.next._begin = parts;

		return parts.end;
	}

	appendCode(code, indent)
	{
		code += Parts.makeIndent(indent);
		code += this.var +" を " + this.start + " から " + this.goal + " まで " + this.step + " ずつ増やしながら，\n";
		var code_inner = this.next.appendCode('', indent + 1);
		if(code_inner == '') code += Parts.makeIndent(indent + 1) + "\n";
		else code += code_inner;
		code += Parts.makeIndent(indent) + "を繰り返す\n";

		if(this.end.next != null) return this.end.next.appendCode(code, indent);
		return code;
	}

	editMe()
	{
		var subtitle = ["変数","〜から","〜まで","増加分"];
		var values = [ this.var, this.start, this.goal, this.step ];
		openModalWindow("繰り返しの編集", subtitle, values, this);
	}
	edited(values)
	{
		if(values != null)
		{
			this.setValue(values[0], values[1], values[2], values[3]);
		}
		flowchart.paint();
		flowchart.flowchart2code();
	}
}

class Parts_LoopBeginDec extends Parts_LoopBegin
{
	get hasText(){return true;}
	constructor()
	{
		super();
		this.setValue("《変数》","《値》","《値》","《値》");
	}
	setValue(variable, start, goal, step)
	{
		this._var = variable;
		this._start = start;
		this._goal = goal;
		this._step = step;
		this._text = this.var + ':' + this.start + "→" + this.goal;
	}
	get var(){return this._var;}
	get start(){return this._start;}
	get goal(){return this._goal;}
	get step(){return this._step;}
	get text2(){return this.step + "ずつ減";}

	static appendMe(bar)
	{
		var parts = new Parts_LoopBeginDec();
		bar.next = parts;
		parts.next = new Parts_Bar();
		parts.next.next = new Parts_LoopEnd();
		parts.next.next.next = new Parts_Bar();
		parts._end = parts.next.next;
		parts.next.next._begin = parts;

		return parts.end;
	}

	appendCode(code, indent)
	{
		code += Parts.makeIndent(indent);
		code += this.var +" を " + this.start + " から " + this.goal + " まで " + this.step + " ずつ減らしながら，\n";
		var code_inner = this.next.appendCode('', indent + 1);
		if(code_inner == '') code += Parts.makeIndent(indent + 1) + "\n";
		else code += code_inner;
		code += Parts.makeIndent(indent) + "を繰り返す\n";

		if(this.end.next != null) return this.end.next.appendCode(code, indent);
		return code;
	}

	editMe()
	{
		var subtitle = ["変数","〜から","〜まで","減少分"];
		var values = [ this.var, this.start, this.goal, this.step ];
		openModalWindow("繰り返しの編集", subtitle, values, this);
	}
	edited(values)
	{
		if(values != null)
		{
			this.setValue(values[0], values[1], values[2], values[3]);
		}
		flowchart.paint();
		flowchart.flowchart2code();
	}
}


class Parts_LoopEnd extends Parts
{
	get hasText(){return false;}
	get begin(){return this._begin;}
	get isBlockEnd(){return true;}
	editMe()
	{
		this.begin.editMe();
	}
	calcTextsize()
    {
        if(this.hasText)
        {
			var size = FlowchartSetting.size;
			this._textwidth = size * 6;
			this._hspace = this._hspace2 = 0;
			var tw = flowchart.context.measureText(this.text).width;
			if(tw > this._textwidth) this._textwidth = tw;
			var tw2 = flowchart.context.measureText(this.text2).width;
			if(tw2 > this._textwidth) this._textwidth = tw2;
			if(tw < this._textwidth) this._hspace = (this._textwidth - tw) / 2;
			if(tw2 < this._textwidth) this._hspace2 = (this._textwidth - tw2) / 2;
            this._textheight = FlowchartSetting.fontsize;
            this._textheight = FlowchartSetting.fontsize;
        }
		else
		{
			this._textwidth = this.begin.textWidth;
			this._textheight = this.begin.textHeight;
		}
    }
	calcSize(p0,p1,p2)
    {
        this.calcTextsize();    // textWidth, textHeightの計算
		var size = FlowchartSetting.size;

		this._height = this.textHeight * (this.hasText ? 2 : 1) + size * 2;
        this._width = this.textWidth + size * 2;
		var x1 = p0.x - this.width / 2;
		var x2 = p0.x + this.width / 2;
		var y2 = p0.y + this.height;
		if(x1 < p1.x) p1.x = x1;
		if(x2 > p2.x) p2.x = x2;
		if(y2 > p2.y) p2.y = y2;
		p0.y = y2;
		return this; // isBlockEnd is true.
    }
	paint(position)
	{
		var size = FlowchartSetting.size;
		if(position != null)
		{
			this.x1 = position.x - this.width / 2;
			this.x2 = position.x + this.width / 2;
			this.y1 = position.y;
			this.y2 = this.y1 + this.height;
		}
		flowchart.context.beginPath();
		flowchart.context.moveTo(this.x1, this.y1);
		flowchart.context.lineTo(this.x2, this.y1);
		flowchart.context.lineTo(this.x2, this.y2 - size);
		flowchart.context.lineTo(this.x2 - size, this.y2);
		flowchart.context.lineTo(this.x1 + size, this.y2);
		flowchart.context.lineTo(this.x1, this.y2 - size);
		flowchart.context.lineTo(this.x1, this.y1);
		flowchart.context.stroke();
		if(this.hasText)
		{
			flowchart.context.fillText(this.text, this.x1 + size + this.hspace, this.y1 + size + this.textHeight);
			flowchart.context.fillText(this.text2, this.x1 + size + this.hspace2, this.y1 + size + this.textHeight * 2);
		}

		if(position != null)
		{
			position.y = this.y2;
		}
		return this;
	}
	appendCode(code, indent)
	{
		return code;
	}
	editMe()
	{
//		this.highlight();
		this.begin.editMe();
	}
	deleteMe()
	{
		this.begin.deleteMe();
	}
	cutMe()
	{
		this.begin.cutMe();
	}
	highlight()
	{
		this.paint_highlight();
		this.begin.paint_highlight();
	}
	unhighlight()
	{
		this.paint_unhighlight();
		this.begin.paint_unhighlight();
	}
}

class Parts_LoopEnd2 extends Parts_LoopEnd
{
	get hasText(){return true;}
	get text(){return this.begin.text;}
	get text2(){return "になるまで";}
}

var misc_menu_ja =[
	//表示            識別子            プログラム上の表現            [引数の意味]
	["《各種処理》"  , "none"           , "《各種処理》"              ,[]],
	["描画領域開く"  , "gOpenWindow"    , "描画領域開く(	,	)"       ,["幅","高さ"]],
	["描画領域閉じる", "gCloseWindow"   , "描画領域閉じる()"           ,[]],
	["描画領域全消去", "gClearWindow"   , "描画領域全消去()"           ,[]],
	["線色設定"     , "gSetLineColor"  , "線色設定(	,	,	)"         ,["赤","青","緑"]],
	["塗色設定"     , "gSetFillColor"  , "塗色設定(	,	,	)"         ,["赤","青","緑"]],
	["文字色設定"	, "gSetTextColor"	, "文字色設定(	,	,	)"			,["赤","青","緑"]],
	["線太さ設定"   , "gSetLineWidth"   , "線太さ設定(	)"            ,["太さ"]],
	["文字サイズ設定", "gSetFontSize"   , "文字サイズ設定(	)"         ,["サイズ"]],
	["文字描画"     , "gDrawText"      , "文字描画(	,	,	)"        ,["文字列","x","y"]],
	["点描画"		, "gDrawPoint"		,"点描画(	,	)"				,["x","y"]]
	["線描画"       , "gDrawLine"      , "線描画(	,	,	,	)"        ,["x1","y1","x2","y2"]],
	["矩形描画"     , "gDrawBox"       , "矩形描画(	,	,	,	)"      ,["x","y","幅","高さ"]],
	["矩形塗描画"   , "gFillBox"       , "矩形塗描画(	,	,	,	)"    ,["x","y","幅","高さ"]],
	["円描画"      , "gDrawCircle"     , "円描画(	,	,	)"          ,["x","y","半径"]],
	["円塗描画"     , "gFillCircle"    , "円塗描画(	,	,	)"        ,["x","y","半径"]],
	["楕円描画"      , "gDrawOval"     , "楕円描画(	,	,	,	)"          ,["x","y","幅","高さ"]],
	["楕円塗描画"     , "gFillOval"    , "楕円塗描画(	,	,	,	)"        ,["x","y","幅","高さ"]],
	["弧描画"      , "gDrawArc"     	, "弧描画(	,	,	,	,	,	,	)"          ,["x","y","幅","高さ","開始角","終了角","閉じ方"]],
	["弧塗描画"     , "gFillArc"    	, "弧塗描画(	,	,	,	,	,	,	)"        ,["x","y","幅","高さ","開始角","終了角","閉じ方"]],
	["putline"		, "putline"			,"putline(	,	)"				,["ファイル番号","文字列"]],
	["putstr"		, "putstr"			,"putstr(	,	)"				,["ファイル番号","文字列"]],
	["close"		, "close"			,"close(	)"					,["ファイル番号"]],
	["待つ"       , "sleep"           , "	 ミリ秒待つ"                 ,["ミリ秒数"]],
	["一時停止する", "PauseStatement", "一時停止する",[]],
	["変数を確認する", "dump"			,"変数を確認する",[]]
];

var misc_menu_en =[
	//表示            識別子            プログラム上の表現            [引数の意味]
	["《各種処理》"  , "none"           , "《各種処理》"              ,[]],
	["gOpenWindow"  , "gOpenWindow"    , "gOpenWindow(	,	)"       ,["幅","高さ"]],
	["gCloseWindow", "gCloseWindow"   , "gCloseWindow()"           ,[]],
	["gClearWindow", "gClearWindow"   , "gClearWindow()"           ,[]],
	["gSetLineColor"     , "gSetLineColor"  , "gSetLineColor(	,	,	)"         ,["赤","青","緑"]],
	["gSetFillColor"     , "gSetFillColor"  , "gSetFillColor(	,	,	)"         ,["赤","青","緑"]],
	["gSetTextColor"	, "gSetTextColor"	, "gSetTextColor(	,	,	)"			,["赤","青","緑"]],
	["gSetLineWidth"   , "gSetLineWidth"   , "gSetLineWidth(	)"            ,["太さ"]],
	["gSetFontSie", "gSetFontSize"   , "gSetFontSize(	)"         ,["サイズ"]],
	["gDrawText"     , "gDrawText"      , "gDrawText(	,	,	)"        ,["文字列","x","y"]],
	["gDrawLine"       , "gDrawLine"      , "gDrawLine(	,	,	,	)"        ,["x1","y1","x2","y2"]],
	["gDrawPoint"     , "gDrawPoint"       , "gDrawPoint(	,	,	,	)"      ,["x","y"]],
	["gDrawBox"     , "gDrawBox"       , "gDrawBox(	,	,	,	)"      ,["x","y","幅","高さ"]],
	["gFillBox"   , "gFillBox"       , "gFillBox(	,	,	,	)"    ,["x","y","幅","高さ"]],
	["gDrawCircle"      , "gDrawCircle"     , "gDrawCircle(	,	,	)"          ,["x","y","半径"]],
	["gFillCircle"     , "gFillCircle"    , "gFillCircle(	,	,	)"        ,["x","y","半径"]],
	["gDrawOval"      , "gDrawOval"     , "gDrawOval(	,	,	,	)"          ,["x","y","幅","高さ"]],
	["gFillOval"     , "gFillOval"    , "gFillOval(	,	,	,	)"        ,["x","y","幅","高さ"]],
	["gDrawArc"      , "gDrawArc"     	, "gDrawArc(	,	,	,	,	,	,	)"          ,["x","y","幅","高さ","開始","終了","閉じ方"]],
	["gFillArc"     , "gFillArc"    	, "gFillArc(	,	,	,	,	,	,	)"        ,["x","y","幅","高さ","開始","終了","閉じ方"]],
	["putline"		, "putline"			,"putline(	,	)"				,["ファイル番号","文字列"]],
	["putstr"		, "putstr"			,"putstr(	,	)"				,["ファイル番号","文字列"]],
	["close"		, "close"			,"close(	)"					,["ファイル番号"]],
	["待つ"       , "sleep"           , "	 ミリ秒待つ"                 ,["ミリ秒数"]],
	["一時停止する", "PauseStatement", "一時停止する",[]],
	["変数を確認する", "dump"			,"変数を確認する",[]]
];

var misc_menu = setting.graphic_command == 0 ? misc_menu_ja : misc_menu_en;

class Parts_Misc extends Parts
{
	constructor()
	{
		super();
		this.setValue("none", []);
	}
	setValue(identifier, values)
	{
		this._identifier = identifier;
		this._values = [];
		for(var i = 0; i < values.length; i++) this._values.push(values[i].getCode());
		for(var i = 0; i < misc_menu.length; i++)
		{
			if(this._identifier != misc_menu[i][1]) continue;
			this._command = misc_menu[i][0];
			var code = misc_menu[i][2];
			for(var j = 0; j < this.values.length; j++)
				code = code.replace("\t",this.values[j]);
			this._text = code;
			break;
		}
	}

	setValuebyText(identifier, values)
	{
		this._identifier = identifier;
		this._values = [];
		for(var i = 0; i < values.length; i++) this._values.push(values[i]);
		for(var i = 0; i < misc_menu.length; i++)
		{
			if(this._identifier != misc_menu[i][1]) continue;
			this._command = misc_menu[i][0];
			var code = misc_menu[i][2];
			for(var j = 0; j < this.values.length; j++)
				code = code.replace("\t",this.values[j]);
			this._text = code;
			break;
		}
	}

	get identifier(){return this._identifier;}
	get values(){return this._values;}

	calcSize(p0,p1,p2)
    {
        this.calcTextsize();    // textWidth, textHeightの計算
		var size = FlowchartSetting.size;
        this._height = this._textheight + size * 2;
        this._width = this._textwidth + size * 4;
		var x1 = p0.x - this.width / 2;
		var x2 = p0.x + this.width / 2;
		var y2 = p0.y + this.height;
		if(x1 < p1.x) p1.x = x1;
		if(x2 > p2.x) p2.x = x2;
		if(y2 > p2.y) p2.y = y2;
		p0.y = y2;
		if(this.next == null || this.isBlockEnd) return this;
		return this.next.calcSize(p0,p1,p2);
    }
    paint(position)
	{
		var size = FlowchartSetting.size;
		if(position != null)
		{
			this.x1 = position.x - this.width / 2;
			this.x2 = position.x + this.width / 2;
			this.y1 = position.y;
			this.y2 = this.y1 + this.height;
		}
		flowchart.context.beginPath();
		flowchart.context.moveTo(this.x1, this.y1);
		flowchart.context.lineTo(this.x2, this.y1);
		flowchart.context.lineTo(this.x2, this.y2);
		flowchart.context.lineTo(this.x1, this.y2);
		flowchart.context.lineTo(this.x1, this.y1);
		flowchart.context.stroke();
		flowchart.context.fillText(this.text, this.x1 + size * 2, this.y2 - size);

		if(position != null)
		{
			position.y = this.y2;
			if(this.end.next != null) return this.end.next.paint(position);
			return this.end;
		}
		return this;
	}
	static appendMe(bar)
	{
		var parts = new Parts_Misc();
		bar.next = parts;
		parts.next = new Parts_Bar();
		return parts.next;
	}
	appendCode(code, indent)
	{
		code += Parts.makeIndent(indent);
		code += this.text + "\n";
		if(this.next != null) return this.next.appendCode(code, indent);
		return code;
	}
	editMe()
	{
		openModalWindowforMisc(this);
	}
	edited(identifier, values)
	{
		if(values != null)
		{
			this.setValuebyText(identifier, values);
		}
		flowchart.paint();
		flowchart.flowchart2code();
	}

}

/* 編集ダイアログ */

var modal_title,modal_subtitle,modal_values,modal_parts;

function openModalWindow(title, subtitle, values, parts)
{
	var html = "<p>" + title + "</p>";
	modal_subtitle = subtitle;
	modal_values = values;
	modal_parts = parts;
	html += "<table>";
	for(var i = 0; i < modal_subtitle.length; i++)
		html += "<tr><td>" + subtitle[i] + "</td><td><input type=\"text\" " +
			"id=\"inputarea" + i + "\" value=\"" + values[i] + "\" " +
			"onfocus=\"select();\" "+
			"onkeydown=\"keydownModal(event);\" spellcheck=\"false\"></td></tr>";
	html += "</table>";
	html += "<button type=\"button\" onclick=\"closeModalWindow(true);\">OK</button>";
	html += "<button type=\"button\" onclick=\"closeModalWindow(false);\">キャンセル</button>";
	modal_parts.highlight();
	$("#input").html(html);
	$("#input").height(100 + subtitle.length * 30);
	$("#input-overlay").fadeIn();
	$("#input").fadeIn();
	$("#inputarea0").focus();
}

function openModalWindowforOutput(title, subtitle, values, parts)
{
	var html = "<p>" + title + "</p>";
	modal_subtitle = subtitle;
	modal_values = values;
	modal_parts = parts;
	html += "<table>";
	html += "<tr><td>" + subtitle[0] + "</td><td><input type=\"text\" " +
		"id=\"inputarea0\" value=\"" + values[0] + "\" " +
		"onfocus=\"select();\" "+
		"onkeydown=\"keydownModal(event);\" spellcheck=\"false\"></td></tr>";
	html += "<tr><td></td><td><input type=\"checkbox\" " +
		"id=\"inputarea1\"" + (values[1] ? " checked=\"checked\"" : "") + ">改行する</td></tr>";
	html += "</table>";
	html += "<button type=\"button\" onclick=\"closeModalWindow(true);\">OK</button>";
	html += "<button type=\"button\" onclick=\"closeModalWindow(false);\">キャンセル</button>";
	modal_parts.highlight();
	$("#input").html(html);
	$("#input").height(100 + subtitle.length * 40);
	$("#input-overlay").fadeIn();
	$("#input").fadeIn();
	$("#inputarea0").focus();
}


function closeModalWindow(ok)
{
	if(ok)
	{
		for(var i = 0; i < modal_subtitle.length; i++)
		{
			var $j = $("#inputarea" + i);
			if($j.prop("type") == "checkbox") modal_values[i] = $j.prop("checked");
			else modal_values[i] = $j.val();
		}
	}
	$("#input").hide();
	$("#input-overlay").hide();
	modal_parts.unhighlight();
	if(ok) makeDirty(true);
	modal_parts.edited(ok ? modal_values : null); // parts must have function 'edited'
}

function keydownModal(e)
{
	var evt = e || window.event;
	if(evt.keyCode == 27) // ESC
		closeModalWindow(false);
	else if(evt.keyCode == 13) // Enter
		closeModalWindow(true);
}

var misc_identifier;

function openModalWindowforMisc(parts)
{
	var html = "<p>各種処理の編集</p>";
	modal_parts = parts;
	modal_values = [];
	for(var i = 0; i < parts.values.length; i++) modal_values.push(parts.values[i]);
	html += "<select id=\"misccommands\" onchange=\"onmiscchanged();\">";
	for(var i = 0; i < misc_menu.length; i++)
		html += "<option value=\"" + misc_menu[i][1] + "\""
			+(misc_menu[i][1] == parts.identifier ? " selected" : "" )+">" 
			+ misc_menu[i][0] + "</option>";
	html += "</select>";
	html += "<table id=\"miscvalues\">";
	html += "</table>";
	html += "<button type=\"button\" onclick=\"closeModalWindowforMisc(true);\">OK</button>";
	html += "<button type=\"button\" onclick=\"closeModalWindowforMisc(false);\">キャンセル</button>";
	modal_parts.highlight();
	$("#input").html(html);
	$("#input-overlay").fadeIn();
	$("#input").fadeIn();
	setIdentifierforMisc(parts.identifier);
//	$("#inputarea0").focus();
}

function onmiscchanged()
{
	var index = document.getElementById("misccommands").selectedIndex;
	setIdentifierforMisc(misc_menu[index][1]);

}

function setIdentifierforMisc(identifier)
{
	misc_identifier = identifier;
	// 今のinputareaの値をmodal_valuesに退避する
	for(var i = 0; i < modal_values.length; i++)
	{
		var elem = document.getElementById("inputarea" + i);
		if(elem) modal_values[i] = elem.value;
		if(/《.*》/.test(modal_values[i])) modal_values[i] = null;
	}
	
	var table = document.getElementById("miscvalues");
	// tableの子をすべて消す
	while(table.firstChild) table.removeChild(table.firstChild);
	for(var i = 0; i < misc_menu.length; i++)
	{
		if(identifier != misc_menu[i][1])continue;
		var tmp_values = [];
		for(var j = 0; j < misc_menu[i][3].length; j++)
		{
			var v = "《" + misc_menu[i][3][j] + "》";
			if(modal_values.length > j && modal_values[j] != null) v = modal_values[j]
			else if(modal_parts.values.length > j && modal_parts.values[j] != null) v = modal_parts.values[j];
			tmp_values.push(v);
			var tr = document.createElement("tr");
			var td = document.createElement("td");
			td.innerHTML = misc_menu[i][3][j];
			tr.appendChild(td);
			td = document.createElement("td");
			var input = document.createElement("input");
			input.setAttribute("id", "inputarea" + j);
			input.setAttribute("value", v);
			input.setAttribute("onfocus", "select();");
			input.setAttribute("onkeydown", "keydownModalforMisc(event);")
			input.setAttribute("spellcheck", "false");
			td.appendChild(input);
			tr.appendChild(td);
			table.appendChild(tr);
		}
		modal_values = tmp_values;
	}
	$("#input").height(120 + modal_values.length * 35);
}

function closeModalWindowforMisc(ok)
{
	if(ok)
	{
		for(var i = 0; i < modal_values.length; i++)
		{
			modal_values[i] = document.getElementById("inputarea" + i).value;
		}
	}
	$("#input").hide();
	$("#input-overlay").hide();
	modal_parts.unhighlight();
	modal_parts.edited(misc_identifier, ok ? modal_values : null); // parts must have function 'edited'
}

function keydownModalforMisc(e)
{
	var evt = e || window.event;
	if(evt.keyCode == 27) // ESC
		closeModalWindowforMisc(false);
	else if(evt.keyCode == 13) // Enter
		closeModalWindowforMisc(true);
}

onload = function(){
	var sourceTextArea = document.getElementById("sourceTextarea");
	var resultTextArea = document.getElementById("resultTextarea");
	var newButton     = document.getElementById("newButton");
	var runButton     = document.getElementById("runButton");
	var flowchartButton = document.getElementById("flowchartButton");
	var resetButton   = document.getElementById("resetButton");
	var stepButton    = document.getElementById("stepButton");
	var loadButton    = document.getElementById("loadButton");
	var file_prefix   = document.getElementById("file_prefix");
	var flowchart_canvas = document.getElementById("flowchart");
	var resultArea = document.getElementById("resultArea");
	$("#sourceTextarea").bcralnit();
	sourceTextArea.onchange = function(){
		makeDirty(true);
	}
	makeDirty(false);
	textarea = resultTextArea;
	runButton.onclick = function(){
		if(run_flag && !step_flag)
		{
			setRunflag(false);
			document.getElementById("sourceTextarea").readOnly = true;
		}
		else
		{
			step_flag = false;
			run();
		}
	};
	stepButton.onclick = function()
	{
		step_flag = true;
		run();
	}
	newButton.onclick = function(){
		if(dirty && !window.confirm("プログラムを削除していいですか？")) return;
		sourceTextArea.value = "";
		code = null;
		reset();
		if(flowchart)
		{
			flowchart.makeEmpty();
			flowchart.paint();
		}
		makeDirty(false);
	}
	resetButton.onclick = function(){
		reset();
	};
	document.getElementById("loadButton1").onclick = function(ev){
		loadButton.click();
		return false;
	};
	loadButton.addEventListener("change", function(ev)
	{
		var file = ev.target.files;
		var reader = new FileReader();
		reader.readAsText(file[0], "UTF-8");
		reader.onload = function(ev)
		{
			sourceTextArea.value = reader.result;
			reset();
			if(flowchart) codeChange();
		}
	}
	,false);
	downloadLink.onclick = function()
	{
		var filename = file_prefix.value.trim();
		if(filename.length < 1)
		{
			alert("ファイル名を入力しないと保存できません");
			return false;
		}
		filename +=	'.PEN';
		var blob = new Blob([sourceTextArea.value], {type:"text/plain"});
		if(window.navigator.msSaveBlob)
		{
			window.navigator.msSaveBlob(blob, filename);
		}
		else
		{
			window.URL = window.URL || window.webkitURL;
			downloadLink.setAttribute("href", window.URL.createObjectURL(blob));
			downloadLink.setAttribute("download", filename);
		}
		makeDirty(false);
	};
	document.getElementById("storage_download").onclick = function(ev)
	{
		var element = document.getElementById("storage_download");
		var list = document.getElementById("storage_list");
		var n = list.options.selectedIndex;
		if(n >= 0 && n < storage.length)
		{
			var filename = list.options[n].value;
			var str = storage.getItem(filename);
			var blob = new Blob([str], {type:"text/plain"});
			if(window.navigator.msSaveBlob)
			{
				window.navigator.msSaveBlob(blob, filename);
			}
			else
			{
				window.URL = window.URL || window.webkitURL;
				element.setAttribute("href", window.URL.createObjectURL(blob));
				element.setAttribute("download", filename);
			}
		}
		else
		{
			element.removeAttribute("href");
		}
	};
	
	
	document.getElementById("storage_upload1").onclick = function(ev){
		document.getElementById("storage_upload").click();
		return false;
	}
	
	document.getElementById("storage_upload").addEventListener("change", function(ev){
		var file = ev.target.files;
		var reader = new FileReader();
		reader.readAsText(file[0], "UTF-8");
		reader.onload = function(ev)
		{
			var data = reader.result;
			try{
				storage.setItem(file[0].name,data);
				storage_list_update();
			}
			catch(e)
			{
				window.alert("ストレージに保存できませんでした");
			}
		}
	});
	
	document.getElementById("storage_remove").onclick = function(ev)
	{
		var list = document.getElementById("storage_list");
		var n = list.options.selectedIndex;
		if(n >= 0)
		{
			var key = list.options[n].value;
			storage.removeItem(key);
			storage_list_update();	
		}
	};
	
	document.getElementById("storage_clear").onclick = function(ev)
	{
		if(window.confirm("ストレージを空にしていいですか？"))
		{
			storage.clear();
			storage_list_update();	
		}
	};
	
	flowchartButton.onchange = function(){
		flowchart_display = this.checked;
		var flowchart_area = document.getElementById("Flowchart_area");
		var drawButton = document.getElementById("drawButton");
		if(flowchart_display)
		{
			flowchart_area.style.display = "block";
			drawButton.style.display = "inline";
			flowchart = new Flowchart();
			codeChange();
//			flowchart.paint();
		}
		else
		{
			flowchart_area.style.display = "none";
			drawButton.style.display = "none";
			flowchart = null;
		}
	}
	flowchartButton.click();
	sourceTextArea.ondrop = function(e)
	{
		var filelist = e.dataTransfer.files;
		if(!filelist) return;
		for(var i = 0; i < filelist.length; i++)
		{
			if(!/\.pen$/i.exec(filelist[i].name)) continue;
			if(window.FileReader)
			{
				try{
					var reader = new FileReader();
					var text = reader.readAsText(filelist[i]);
					reader.onload = function(event)
					{
						sourceTextArea.value = event.target.result;
						codeChange();
					}
					break;
				}
				catch(e){}
			}
		}
		return false;
	}
	registerEvent(sourceTextArea, "keyup", keyUp);
	registerEvent(flowchart_canvas, "mousedown", mouseDown);
	registerEvent(flowchart_canvas, "mouseup", mouseUp);
	registerEvent(flowchart_canvas, "mousemove", mouseMove);
	registerEvent(flowchart_canvas, "dblclick", doubleclick_Flowchart);

	$.contextMenu(
		{
			selector: "#sourceTextarea",
			items:{
//				copyAll: {name: "プログラムをコピー", callback(k,e){document.getElementById("sourceTextarea").select(); document.execCommand('copy');}},
				zenkaku: {name: "入力補助",
					items:{
						かつ:	{name:"かつ",	callback: function(k,e){insertCode("《値》 かつ 《値》");}},
						または:	{name:"または",	callback: function(k,e){insertCode("《値》 または 《値》");}},
						でない:	{name:"でない",	callback: function(k,e){insertCode("《値》 でない");}},
						と:		{name:"と",		callback: function(k,e){insertCode("《値》と《値》");}},
						割り算: {name:"÷",	   callback: function(k,e){insertCode("《値》÷《値》");}},
						カッコ:	{name:"「」",	callback: function(k,e){insertCode("「《値》」");}},
					}
				},
				math:{ name:"数学関数",
				 	items:{
						abs:	{name:"abs 絶対値", callback: function(k,e){insertCode("abs(《値》)");}},
						random1:{name: "random 乱数（整数）", callback: function(k,e){insertCode("random(《整数》)");}},
						random2:{name: "random 乱数（0〜1）", callback: function(k,e){insertCode("random()");}},
						ceil:	{name: "ceil 切り上げ", callback: function(k,e){insertCode("ceil(《実数》)");}},
						floor:	{name: "floor 切り捨て", callback: function(k,e){insertCode("floor(《実数》)");}},
						round:	{name: "round 四捨五入", callback: function(k,e){insertCode("round(《実数》)");}},
						sin:	{name: "sin サイン", callback: function(k,e){insertCode("sin(《実数》)");}},
						cos:	{name: "cos コサイン", callback: function(k,e){insertCode("cos(《実数》)");}},
						tan:	{name: "tan タンジェント", callback: function(k,e){insertCode("tan(《実数》)");}},
						sqrt:	{name: "sqrt ルート", callback: function(k,e){insertCode("sqrt(《実数》)");}},
						log:	{name: "log 自然対数", callback: function(k,e){insertCode("log(《実数》)");}},
						exp:	{name: "exp 指数関数", callback: function(k,e){insertCode("exp(《実数》)");}},
						pow:	{name: "pow 累乗", callback: function(k,e){insertCode("pow(《実数》,《実数》)");}}
					}
				},
				str:{name:"文字列関数",
					items:{
						length:	{name: "length 長さ", callback: function(k,e){insertCode("length(《文字列》)");}},
						append:	{name: "append 文字列結合", callback: function(k,e){insertCode("append(《文字列》,《文字列》)");}},
						split1:	{name: "split 文字列分割（1文字ずつ）", callback: function(k,e){insertCode("split(《文字列》)");}},
						split2:	{name: "split 文字列分割（区切り文字指定）", callback: function(k,e){insertCode("split(《文字列》,《文字列》)");}},
						substring1:	{name: "substring 部分文字列（最後まで）", callback: function(k,e){insertCode("substring(《文字列》,《開始位置》)");}},
						substring2:	{name: "substring 部分文字列（長さ指定）", callback: function(k,e){insertCode("substring(《文字列》,《開始位置》,《長さ》)");}},
						extract:	{name: "extract 部分文字列（長さ指定）", callback: function(k,e){insertCode("extract(《文字列》,《区切文字列》,《番号》)");}},
						insert:	{name: "insert 挿入", callback: function(k,e){insertCode("insert(《文字列》,《位置》,《文字列》)");}},
						replace:	{name: "replace 置換", callback: function(k,e){insertCode("replace(《文字列》,《位置》,《長さ》,《文字列》)");}},
					}
				},
				fileio:{name: "File I/O",
					items:{
						openr: {name:"openr 読込用オープン", callback: function(k,e){insertCode("openr(《ファイル名》)");}},
						openw: {name:"openr 書込用オープン", callback: function(k,e){insertCode("openw(《ファイル名》)");}},
						opena: {name:"openr 追記用オープン", callback: function(k,e){insertCode("opena(《ファイル名》)");}},
						getline: {name:"getline", callback: function(k,e){insertCode("getline(《ファイル番号》)");}},
						getchar: {name:"getchar", callback: function(k,e){insertCode("getchar(《ファイル番号》)");}},
						putline: {name:"putline", callback: function(k,e){insertCode("putline(《ファイル番号》,《文字列》)");}},
						putstr:  {name:"putstr",  callback: function(k,e){insertCode("putstr(《ファイル番号》,《文字列》)");}},
					}
				},
				graphic1:{ name:"グラフィック命令（日本語）",
					items:{
						gOpenWindow:{name:"描画領域開く", callback: function(k,e){insertCode("描画領域開く(《幅》,《高さ》)");}},
						gCloseWindow:{name:"描画領域閉じる", callback: function(k,e){insertCode("描画領域閉じる()");}},
						gClearWindow:{name:"描画領域全消去", callback: function(k,e){insertCode("描画領域全消去()");}},
						gSetLineColor:{name:"線色設定", callback: function(k,e){insertCode("線色設定(《赤》,《緑》,《青》)");}},
						gSetFillColor:{name:"塗色設定", callback: function(k,e){insertCode("塗色設定(《赤》,《緑》,《青》)");}},
						gSetTextColor:{name:"文字色設定", callback: function(k,e){insertCode("文字色設定(《赤》,《緑》,《青》)");}},
						gSetLineWidth:{name:"線太さ設定", callback: function(k,e){insertCode("線太さ設定(《太さ》)");}},
						gSetFontSize:{name:"文字サイズ設定", callback: function(k,e){insertCode("文字サイズ設定(《サイズ》)");}},
						gDrawText:{name:"文字描画", callback: function(k,e){insertCode("文字描画(《文字列》,《x》,《y》)");}},
						gDrawPoint:{name:"点描画", callback: function(k,e){insertCode("点描画(《x》,《y》)");}},
						gDrawLine:{name:"線描画", callback: function(k,e){insertCode("線描画(《x1》,《y1》,《x2》,《y2》)");}},
						gDrawBox:{name:"矩形描画", callback: function(k,e){insertCode("矩形描画(《x》,《y》,《幅》,《高さ》)");}},
						gFillBox:{name:"矩形塗描画", callback: function(k,e){insertCode("矩形塗描画(《x》,《y》,《幅》,《高さ》)");}},
						gDrawCircle:{name:"円描画", callback: function(k,e){insertCode("円描画(《x》,《y》,《半径》)");}},
						gFillCircle:{name:"円塗描画", callback: function(k,e){insertCode("円塗描画(《x》,《y》,《半径》)");}},
						gDrawOval:{name:"楕円描画", callback: function(k,e){insertCode("楕円描画(《x》,《y》,《幅》,《高さ》)");}},
						gFillOval:{name:"楕円塗描画", callback: function(k,e){insertCode("楕円塗描画(《x》,《y》,《幅》,《高さ》)");}},
						gDrawArc:{name:"弧描画", callback: function(k,e){insertCode("弧描画(《x》,《y》,《幅》,《高さ》,《開始角》,《終了角》,《閉じ方》)");}},
						gFillArc:{name:"弧塗描画", callback: function(k,e){insertCode("弧塗描画(《x》,《y》,《幅》,《高さ》,《開始角》,《終了角》,《閉じ方》)");}},
						},
					},
				graphic2:{ name:"グラフィック命令（英語）",
					items:{
						gOpenWindow:{name:"gOpenWindow", callback: function(k,e){insertCode("gOpenWindow(《幅》,《高さ》)");}},
						gCloseWindow:{name:"gCloseWindow", callback: function(k,e){insertCode("gCloseWindow()");}},
						gClearWindow:{name:"gClearWindow", callback: function(k,e){insertCode("gClearWindow()");}},
						gSetLineColor:{name:"gSetLineColor", callback: function(k,e){insertCode("gSetLineColor(《赤》,《緑》,《青》)");}},
						gSetFillColor:{name:"gSetFillColor", callback: function(k,e){insertCode("gSetFillColor(《赤》,《緑》,《青》)");}},
						gSetTextColor:{name:"gSetTextColor", callback: function(k,e){insertCode("gSetTextColor(《赤》,《緑》,《青》)");}},
						gSetLineWidth:{name:"gSetLineWidth", callback: function(k,e){insertCode("gSetLineWidth(《太さ》)");}},
						gSetFontSize:{name:"gSetFontSize", callback: function(k,e){insertCode("gSetFontSize(《サイズ》)");}},
						gDrawText:{name:"gDrawText", callback: function(k,e){insertCode("gDrawText(《文字列》,《x》,《y》)");}},
						gDrawPoint:{name:"gDrawPoint", callback: function(k,e){insertCode("gDrawPoint(《x》,《y》)");}},
						gDrawLine:{name:"gDrawLine", callback: function(k,e){insertCode("gDrawLine(《x1》,《y1》,《x2》,《y2》)");}},
						gDrawBox:{name:"gDrawBox", callback: function(k,e){insertCode("gDrawBox(《x》,《y》,《幅》,《高さ》)");}},
						gFillBox:{name:"gFillBox", callback: function(k,e){insertCode("gFillBox(《x》,《y》,《幅》,《高さ》)");}},
						gDrawCircle:{name:"gDrawCircle", callback: function(k,e){insertCode("gDrawCircle(《x》,《y》,《半径》)");}},
						gFillCircle:{name:"gFillCircle", callback: function(k,e){insertCode("gFillCircle(《x》,《y》,《半径》)");}},
						gDrawOval:{name:"gDrawOval", callback: function(k,e){insertCode("gDrawOval(《x》,《y》,《幅》,《高さ》)");}},
						gFillOval:{name:"gFillOval", callback: function(k,e){insertCode("gFillOval(《x》,《y》,《幅》,《高さ》)");}},
						gDrawArc:{name:"gDrawArc", callback: function(k,e){insertCode("gDrawArc(《x》,《y》,《幅》,《高さ》,《開始角》,《終了角》,《閉じ方》)");}},
						gFillArc:{name:"gFillArc", callback: function(k,e){insertCode("gFillArc(《x》,《y》,《幅》,《高さ》,《開始角》,《終了角》,《閉じ方》)");}},
					},
				},
				misc:{name: "各種命令",
					items:{
						sleep:{name:"待つ", callback: function(k,e){insertCode("《ミリ秒数》 ミリ秒待つ");}},
						pause:{name:"一時停止する", callback: function(k,e){insertCode("一時停止する");}},
						dump:{name:"変数を確認する", callback: function(k,e){insertCode("変数を確認する");}}
					}
				}
			}
		}
	);
	$.contextMenu(
		{
			selector: "#flowchart",
			build: contextMenu_Flowchart
		}
	);
	// this code is from David Baron's Weblog
	// https://dbaron.org/log/20100309-faster-timeouts
//	var timeouts = [];
	var messageName = "zero-timeout-message";

	// Like setTimeout, but only takes a function argument.  There's
	// no time argument (always zero) and no arguments (you have to
	// use a closure).
	function setZeroTimeout(fn) {
		timeouts.push(fn);
		window.postMessage(messageName, "*");
	}

	function handleMessage(event) {
		if (event.source == window && event.data == messageName) {
			event.stopPropagation();
			if (timeouts.length > 0) {
				var fn = timeouts.shift();
				fn();
			}
		}
	}

	if(window.addEventListener) window.addEventListener("message", handleMessage, true);
	else if(window.attachEvent) window.attachEvent("onmessage", handleMessage);

	// Add the one thing we want added to the window object.
	window.setZeroTimeout = setZeroTimeout;

	$(window).bind("beforeunload", function(){if(dirty) return "プログラムが消去されます";});

	reset();

	let sample_area = document.getElementById('SampleButtons')
	for(let i = 0; i < sample.length; i++)
	{
		let button = document.createElement('button');
		button.innerText = 'サンプル' + (i + 1);
		button.setAttribute('type', 'button');
		button.setAttribute('class', 'sampleButton');
		button.onclick = function(){sampleButton(i);};
		if(i > 0 && i % 8 == 0) sample_area.appendChild(document.createElement('br'));
		sample_area.appendChild(button);
	}
	if(setting.quiz_mode == 1 && Quizzes.length > 0)
	{
		let quiz_select = document.getElementById('quiz_select');
		quiz_select.onchange = function ()
		{
			let i = quiz_select.selectedIndex;
			if(i > 0) document.getElementById('quiz_question').innerHTML = Quizzes[i - 1].question();
			else document.getElementById('quiz_question').innerHTML = '';
		};
		let option = document.createElement('option');
		option.val = 0;
		option.appendChild(document.createTextNode('問題選択'));
		quiz_select.appendChild(option);
	
		for(let i = 0; i < Quizzes.length; i++)
		{
			option = document.createElement('option');
			option.val = i + 1;
			option.appendChild(document.createTextNode('Q' + (i + 1) + ':' + Quizzes[i].title()));
			quiz_select.appendChild(option);
		}
		document.getElementById('quiz_marking').onclick = function ()
		{
			let i = quiz_select.selectedIndex;
			if(i > 0) auto_marking(i - 1);
			else textarea.value='問題が選択されていないので採点できません。'; 
		}
	}
	else
	{
		document.getElementById('Quiz_area').style.display = 'none';
	}
	document.getElementById('urlButton').onclick = function()
	{
		var code = sourceTextArea.value.trim();
		if(code == '') return;
		code = B64encode(code);
		if(code){
			var url = window.location;
			textareaClear();
			highlightLine(-1);
			textareaAppend(url.protocol + '//' + url.hostname + url.pathname + '?code=' + code);
		} 
	}
	var param_code = getParam('code');
	if(param_code) sourceTextArea.value = param_code;
	storage_list_update();
}
function storage_list_update()
{
	var list = document.getElementById("storage_list");
	while(list.options.length) list.options.remove(0);
	var n = storage.length;
	if(n > 0)
	{
		for(var i = 0; i < n; i++)
		{
			var option = document.createElement("option");
			option.text = option.value = storage.key(i);
			list.appendChild(option);
		}
	}
	else
	{
		var option = document.createElement("option");
		option.text = "--空--";
		// option.attributes.add("disabled");
		list.appendChild(option);
	}
}

function auto_marking(i)
{
	setRunflag(true);
	document.getElementById('runButton').disabled = true;
	document.getElementById('stepButton').disabled = true;
	document.getElementById('resetButton').disabled = true;
	textareaClear();
	textareaAppend('*** 採点開始 ***\n');
	selected_quiz = i;
	let all_clear = true;
	for(let j = 0; j < Quizzes[i].cases(); j++)
	{
		let clear = true;
		textareaAppend('ケース' + (j + 1) + '...');
		try{
			selected_quiz_case = j;
			test_limit_time = Date.now() + Quizzes[selected_quiz].timeout();
			run();
			if(selected_quiz_input != Quizzes[selected_quiz].inputs(selected_quiz_case).length) throw new RuntimeError(-1, '入力の回数がおかしいです。');
			else if(output_str.trim() != Quizzes[selected_quiz].output(selected_quiz_case).toString().trim()) throw new RuntimeError(-1, '結果が違います。');
			textareaAppend('成功\n');
		}
		catch(e)
		{
			textareaAppend('失敗\n');
			textareaAppend(e.message+"\n");
			clear = false;
		}
		all_clear &= clear;
		code = null;
	}
	if(all_clear)textareaAppend('*** 合格 ***\n');
	else textareaAppend('--- 不合格 ---\n');
	selected_quiz = -1;
	document.getElementById('runButton').disabled = false;
	document.getElementById('stepButton').disabled = false;
	document.getElementById('resetButton').disabled = false;
	setRunflag(false);
}

function font_size(updown)
{
	if(fontsize + updown < 12 || fontsize + updown > 30) return;
	fontsize += updown;
	var elem = document.getElementById('sourceTextarea');
	elem.style.fontSize = fontsize + 'px';
	elem.style.lineHeight = (fontsize + 2) + 'px';
	elem = document.getElementById('resultTextarea');
	elem.style.fontSize = fontsize + 'px';
	elem.style.lineHeight = (fontsize + 2) + 'px';
	elem = document.getElementsByClassName('bcr_number')[0];
	elem.style.fontSize = fontsize + 'px';
	elem.style.lineHeight = (fontsize + 2) + 'px';
}

function getParam(name)
{
	var getparam = window.location.search;
	if(getparam)
	{
		var params = getparam.slice(1).split('&');
		for(var param of params)
		{
			var p = param.split('=');
			if(p[0] == name){
				return B64decode(p[1]);
			} 
		}
	}
	return null;
}
