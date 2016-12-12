module pry.combinators;

import pry.traits;

private auto repImpl(bool collect, size_t minTimes, P)(P parser)
if(isParser!P){
	alias Stream = ParserStream!P;
	static struct Parser {
		P p;
		static if(collect)
			ParserValue!P[] value;
		else
			Stream.Range value;
		typeof(Stream.init.context) errContext;

		bool parse(ref Stream stream) {
			auto start = stream.mark;
			for(size_t i = 0; i<minTimes; i++) {
				if(!p.parse(stream)){
					errContext = stream.context;
					stream.restore(start);
					return false;
				}
				static if(collect) value ~= p.value;
			}
			while(p.parse(stream)){
				static if(collect) value ~= p.value;
			}
			static if(!collect)
				value = stream.slice(start);
			return true;
		}

		Stream.Error error(ref Stream stream) {
			return Stream.Error(errContext, p.error(stream).reason);
		}
	}
	return Parser();
}


/// Apply parser for minTimes times or more and return consumed range.
auto rep(size_t minTimes=1, P)(P parser)
if(isParser!P){
	return repImpl!(false, minTimes, P)(parser);
}

/// Apply parser for minTimes times or more and return array of results.
auto array(size_t minTimes=1, P)(P parser)
if(isParser!P){
	return repImpl!(true, minTimes, P)(parser);
}

unittest
{
	import pry.atoms, pry.stream;
	alias S = SimpleStream!string;
	with(parsers!S)
	{
		auto p = rep!2(tk!'a');
		auto s = S("aaac");
		assert(p.parse(s));
		assert(p.value == "aaa");
		
		s = S("a");
		assert(!p.parse(s));
		
		auto p2 = array(tk!'a');
		s = S("aaa");
		assert(p2.parse(s));
		assert(p2.value == "aaa"d);
		assert(s.empty);
	}
}
