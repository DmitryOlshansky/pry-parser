module pry.atoms;

import pry.stream, pry.traits;
import std.conv, std.range.primitives;

template parsers(Stream)
if(is(Stream == string))
{
	import pry.stream;
	alias parsers = parsers!(SimpleStream!string);
}

template parsers(Stream)
if(!is(Stream == string))
{
	struct Tk(alias c)
	if(is(typeof(c) : ElementType!Stream)) {
		static immutable msg = "expected '" ~ to!string(c)~"'";

		bool parse(ref Stream stream, ref ElementType!Stream value, ref Stream.Error err) const {
			if(stream.empty) {
				err.location = stream.location;
				err.reason = "unexpected end of stream";
				return false;
			}
			if(stream.front == c){
				value = c;
				stream.popFront();
				return true;
			}
			else {
				err.location = stream.location;
				err.reason = msg;
				return false;
			}
		}
	}

	/// Single element token.
	auto tk(alias c)(){ return Tk!c(); }

	/// Single element token, skipping any whitespace at front.
	auto stk(alias c)()
	if(is(typeof(c) : ElementType!Stream) && is(typeof(c) : dchar)){
		import pry.combinators;
		return tk!c.skipWs;
	}

	struct Range(alias low, alias high)
	if(is(typeof(low): ElementType!Stream) && is(typeof(high) : ElementType!Stream)){
		static immutable msg = "expected in a range of " ~ to!string(low) ~ ".." ~ to!string(high);

		bool parse(ref Stream stream, ref ElementType!Stream value, ref Stream.Error err) const {
			if(stream.empty) {
				err.location = stream.location;
				err.reason = "unexpected end of stream";
				return false;
			}
			auto v = stream.front;
			if(v >= low && v <= high) {
				value = v;
				stream.popFront();
				return true;
			}
			else {
				err.location = stream.location;
				err.reason = msg;
				return false;
			}
		}
	}

	// In a range of elements.
	auto range(alias low, alias high)(){ return Range!(low, high)(); }

	interface DynamicParser(V) {
		bool parse(ref Stream stream, ref V value, ref Stream.Error err) const;
	}

	// Use LINE & FILE to provide unique types of dynamic.
	auto dynamic(V, size_t line=__LINE__, string file=__FILE__)(){
		static class Dynamic : DynamicParser!V {
			DynamicParser!V wrapped;
		final:
			void opAssign(P)(P parser)
			if(isParser!P && !is(P : Dynamic)){
				wrapped = wrap(parser);
			}

			bool parse(ref Stream stream, ref V value, ref Stream.Error err) const {
				assert(wrapped, "Use of empty dynamic parser");
				return wrapped.parse(stream, value, err);
			}
		}
		return new Dynamic();
	}

	auto wrap(Parser)(Parser parser)
	if(isParser!Parser){
		alias V = ParserValue!Parser;
		static class Wrapped: DynamicParser!V {
			Parser p;

			this(Parser p){
				this.p = p;
			}

			bool parse(ref Stream stream, ref V value, ref Stream.Error err) const {
				return p.parse(stream, value, err);
			}
		}
		return new Wrapped(parser);
	}

	struct Set(alias set) {
		import std.uni, std.conv;
		enum val = set.byInterval.length;
		static if(val <= 6) {
			mixin("static " ~ set.toSourceCode("test"));
		}
		else {
			alias Trie = CodepointSetTrie!(13, 8);
			alias makeTrie = codepointSetTrie!(13, 8);

			static struct BitTable {
				uint[4] table;

				this(CodepointSet set){
					foreach (iv; set.byInterval)
					{
						foreach (v; iv.a .. iv.b)
							add(v);
					}
				}

				void add()(dchar ch){
					immutable i = ch & 0x7F;
					table[i >> 5]  |=  1<<(i & 31);
				}

				bool opIndex()(dchar ch) const{
					immutable i = ch & 0x7F;
					return (table[i >> 5]>>(i & 31)) & 1;
				}
			}

			static struct CharMatcher {
				BitTable ascii; // fast path for ASCII
				Trie trie;	  // slow path for Unicode

				this(CodepointSet set){
					auto asciiSet = set & unicode.ASCII;
					ascii = BitTable(asciiSet);
					trie = makeTrie(set);
				}

				bool opIndex()(dchar ch) const{
					if (ch < 0x80)
						return ascii[ch];
					else
						return trie[ch];
				}
			}

			static immutable matcher = CharMatcher(set);

			static bool test(dchar ch){
				return matcher[ch];
			}
		}

		static immutable string msg = (){
			import std.format;
			string message = "expected one of ";
			set.toString((const(char)[] s){ message ~= s; }, FormatSpec!char("%x"));
			return message;
		}();

		bool parse(ref Stream stream, ref dchar value, ref Stream.Error err) const {
			if(stream.empty){
				err.location = stream.location;
				err.reason = "unexpected end of stream";
				return false;
			}
			immutable c = stream.front;
			if(test(c)){
				value = stream.front;
				stream.popFront();
				return true;
			}
			err.location = stream.location;
			err.reason = msg;
			return false;
		}
	}

	auto set(alias s)(){
		import std.uni;
		static assert(isCodepointSet!(typeof(s)), "set only works with std.uni.CodepointSet");
		return Set!s();
	}

	struct _Literal(alias literal) {
		static immutable msg = "expected '"~literal~"' literal";

		bool parse(ref Stream stream, ref Stream.Range value, ref Stream.Error err) const {
			auto m = stream.mark();
			auto t = literal.save();
			for(;;) {
				if (t.empty) {
					value = stream.slice(m);
					return true;
				}
				if (stream.empty) {
					stream.restore(m);
					err.location = stream.location;
					err.reason = "unexpected end of stream";
					return false;
				}
				if (t.front != stream.front) {
					stream.restore(m);
					err.location = stream.location;
					err.reason = msg;
					return false;
				}
				t.popFront();
				stream.popFront();
			}
		}
	}

	auto literal(alias lit)()
	if(isForwardRange!(typeof(lit))
	&& is(ElementType!(typeof(lit)) : ElementType!(Stream.Range))) {
		return _Literal!lit();
	}

	struct Eof {
		bool parse(ref Stream stream, ref Nothing _, ref Stream.Error err) const {
			bool r =  stream.empty;
			if(!r) {
				err.location = stream.location;
				err.reason = "input not fully consumed";
			}
			return r;
		}
	}

	/// Zero-width assertion that parses successfully on input end.
	/// Use to make sure the whole stream was consumed.
	auto eof(){
		return Eof();
	}
}

unittest {
	alias S = SimpleStream!string;
	with(parsers!S) {
		auto parser = dynamic!dchar;
		parser = tk!'a';
		S s = S("a");
		dchar c;
		S.Error err;
		assert(parser.parse(s, c, err));
		assert(c == 'a');
		assert(s.empty);
	}
}

unittest {
	alias S = SimpleStream!string;
	with(parsers!S) {
		auto s = " a".stream;
		auto p = stk!'a';
		dchar c;
		S.Error err;
		assert(p.parse(s, c, err));
		assert(c == 'a');
		assert(s.empty);

		auto s2 = "a".stream;
		assert(p.parse(s2, c, err));
		assert(c == 'a');
		assert(s2.empty);
	}
}

unittest {
	import std.uni;
	alias S = SimpleStream!string;
	with(parsers!S) {
		auto p = set!(CodepointSet('A', 'Z'+1, 'a', 'z'+1));
		auto s = "aZ0".stream;
		dchar c;
		S.Error err;
		assert(p.parse(s, c, err));
		assert(c == 'a');
		assert(p.parse(s, c, err));
		assert(c == 'Z');
		assert(!p.parse(s, c, err));
		assert(s.front == '0');
		auto p2 = set!(unicode.L);
		s = "Яz".stream;
		assert(p2.parse(s, c, err));
		assert(c == 'Я');
		assert(p2.parse(s, c, err));
		assert(c == 'z');
	}
}

unittest {
	alias S = SimpleStream!string;
	with(parsers!S) {
		auto p = literal!"abc";
		auto s = "abcd".stream;
		S.Error err;
		string slice;
		assert(p.parse(s, slice, err));
		assert(s.front == 'd');
		assert(slice == "abc");
		s = "ab".stream;
		assert(!p.parse(s, slice, err));
		assert(s.front == 'a');
		s = "abd".stream;
		assert(!p.parse(s, slice, err));
		assert(s.front == 'a');
	}
}

unittest {
	alias S = SimpleStream!string;
	with(parsers!S) {
		auto p = eof;
		Nothing n;
		S.Error err;
		auto s = "".stream;
		assert(p.parse(s, n, err));
	}
}
