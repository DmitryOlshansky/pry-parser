import std.stdio, std.datetime, std.file;
import dpick.buffer;
import regex;


void main(string[] args)
{
    if(args.length < 3) {
        writefln("Usage: %s <pattern> [file]", args[0]);
        return;
    }
    StopWatch sw;
    sw.start();
    version(whole)
        auto buf = buffer(cast(ubyte[])std.file.read(args[2]));
    else
        auto buf = buffer(fileInput(args[2]), 32, 8192);
    int count;
    foreach(m; buf.matchAll(args[1]))
    {
        count++;
        //writeln(cast(string)m[0]);
    }
    sw.stop();
    writefln("%d matches in %s ms", count, sw.peek().msecs);
}