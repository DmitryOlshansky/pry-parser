module parser;

import pry.stream;

alias Stream = SimpleStream!string;

bool expr(ref Stream s, ref int val, ref Stream.Error err){
	int v;
	if(!term(s, v, err)){
		return false;
	}
	if(s.empty){
		val = v;
		return true;
	}
	switch(s.front){
	case '+':
		s.popFront();
		int v2;
		if(!expr(s, v2, err)){
			return false;
		}
		val = v + v2;
		return true;
	case '-':
		s.popFront();
		int v2;
		if(!expr(s, v2, err)){
			return false;
		}
		val = v - v2;
		return true;
	default:
		val = v;
		return true;
	}
}

bool term(ref Stream s, ref int val, ref Stream.Error err){
	int v;
	if(!primary(s, v, err)){
		return false;
	}
	if(s.empty){
		val = v;
		return true;
	}
	switch(s.front){
	case '*':
		s.popFront();
		int v2;
		if(!term(s, v2, err)){
			return false;
		}
		val = v * v2;
		return true;
	case '/':
		s.popFront();
		int v2;
		if(!term(s, v2, err)){
			return false;
		}
		val = v / v2;
		return true;
	default:
		val = v;
		return true;
	}
}

bool primary(ref Stream s, ref int val, ref Stream.Error err){
	import std.conv;
	if(s.empty){
		err = Stream.Error(s.location, "Unexpected end of stream");
		return false;
	}
	switch(s.front){
	case '0' : .. case '9':
		auto m = s.mark();
		do{
			s.popFront();
		}while(!s.empty && s.front >= '0' && s.front <= '9');
		val = to!int(s.slice(m));
		return true;
	case '(':
		s.popFront();
		if(!expr(s, val, err)){
			return false;
		}
		if(s.front != ')'){
			err = Stream.Error(s.location, "Expected ')'");
		}
		s.popFront();
		return true;
	default:
		err = Stream.Error(s.location, "Unrecognized token");
		return false;
	}
}
