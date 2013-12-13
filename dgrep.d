import std.stdio, std.datetime;
import dpick.buffer;
import regex;


void main(string[] args)
{
    if(args.length < 3) {
        writefln("Usage: %s <pattern> [file]", args[0]);
        return;
    }
    auto buf = buffer(fileInput(args[2]), 128);
    int count;
    StopWatch sw;
    sw.start();
    foreach(m; buf.matchAll(args[1]))
    {
        count++;
        //writeln(cast(string)m[0]);
    }
    sw.stop();
    writefln("%d matches in %s ms", count, sw.peek().msecs);
}