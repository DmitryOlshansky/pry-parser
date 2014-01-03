import std.stdio, std.datetime, std.file;
import dpick.buffer;
import regex;


int main(string[] args)
{
    if(args.length < 2) {
        writefln("Usage: %s <pattern> [file]", args[0]);
        return 1;
    }
    StopWatch sw;
    sw.start();
    version(whole){
        if(args.length < 3){
            writefln("Usage: %s <pattern> <file>", args[0]);
            return 1;
        }
        auto buf = buffer(cast(ubyte[])std.file.read(args[2]));
    }
    else{
        auto buf = buffer(args.length < 3 ? stdInput() : fileInput(args[2]),
            32, 16*1024);
    }
    int count;
    foreach(m; buf.matchAll(args[1]))
    {
        count++;
        //writeln(cast(string)m[0]);
    }
    sw.stop();
    writefln("%d matches in %s ms", count, sw.peek().msecs);
    return 0;
}
