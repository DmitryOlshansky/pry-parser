module dpick.buffer.tests;

import dpick.buffer;

unittest
{
    import std.stdio, std.path, std.range, std.conv, std.algorithm;
    import std.file : tempDir;
    auto tmpName = buildPath(tempDir, "trash0000");
    {
        auto f = File(tmpName, "w");
        f.rawWrite(iota(1, 250).map!"cast(ubyte)a".array);
    }
    auto buf = buffer(fileInput(tmpName), 16, 4);
    assert(!buf.empty);
    assert(buf.front == 1);
    assert(buf.lookahead(10).length == 10);
    foreach(v; 1..40){
        //writeln(buf.cur, "=>", buf.buffer);
        assert(buf.front == v, text(buf.front, " vs ", v));
        auto luk = buf.lookahead(15);
        assert(luk[6] == v+6);
        assert(luk[9] == v+9);
        assert(luk[14] == v+14);
        buf.popFront();
    }
    
    {
        auto m = buf.mark();
        auto m2 = buf.mark();
        buf.seek(60); //load and skip over 60 bytes
        buf.seek(-60); // can do - it was pinned
        foreach(v; 40..100) {
            assert(buf.front == v);
            buf.popFront();
        }
        assert(equal(buf.slice(m2), buf.slice(m)));
        assert(equal(buf.slice(m), iota(40, 100)));
        buf.seek(m, 1);
        buf.seek(59); //60 in total
        auto m3 = buf.mark();
        foreach(v; 100..120) {
            assert(buf.front == v);
            buf.popFront();
        }
        buf.seek(m3, -60);
        assert(buf.lookahead(80).equal(iota(40, 120)));
        buf.seek(m3, 20);
        assert(equal(buf.slice(m2), buf.slice(m)));
        assert(equal(buf.slice(m), iota(40, 120)));
        assert(equal(buf.slice(m3), iota(100, 120)));
        buf.seek(m2);
        assert(equal(buf.slice(m3), iota(40, 100)));
        buf.seek(m3);
    }
    auto m = buf.mark();
    assert(equal(&buf, iota(100, 250)));
    buf.seek(m);
    assert(equal(&buf, iota(100, 250)));
    assert(equal(buf.slice(m), iota(100, 250)));
}
