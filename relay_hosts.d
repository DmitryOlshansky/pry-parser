import std.regex;
import core.stdc.stdio;
import core.stdc.string;
import std.datetime;
import std.string;
import std.stdio : writefln, writef;
/*
    Parses mail logs like this line:

Nov  1 19:37:59 themailhost sendmail[10620]: [ID 801593 mail.info] pA20bx2u010620: 
from=<some@one.com>, size=6988, class=0, nrcpts=2, 
msgid=<5693296.131.1320194279221.JavaMail.gfish@somemachine>, proto=ESMTP, 
daemon=MTA, relay=somehost.domain.com [1.2.3.4]

    Looks for the relay lines and stores the number of times a 
    particular relay appears. 

    Only relays matching foo.com and bar.corp are stored.

    At the end the list of relays is displayed along with
    the number of times that relay appeared in the log. 
 */

void main (string[] args)
{
    int[string] relayHosts;
    //auto regex = ctRegex!(`relay=([\w-.]+[\w]+)[.,]*\s`);
    auto regex = regex(r"relay=([0-9a-zA-Z\-\.]+[0-9a-zA-Z]+)[\.\,]*\s");
    size_t cnt;
    foreach (arg; args[1 .. args.length])
    {
        char[4096] buf = void;
        //auto file = new BufferedFile(arg, FileMode.In, 268_435_456); // 250 MB
        auto file = fopen(toStringz(arg), "r".ptr);
        if(!file)
            continue;
        scope(exit) fclose(file);
     
        StopWatch sw;
        sw.start();   
        while(fgets(buf.ptr, buf.sizeof, file) != null)
        {
            auto line = buf[0..strlen(buf.ptr)];
            // Find the relay portion of the string (if any)
            auto cap = matchFirst(line, regex);
            if(!cap.empty) {
                cnt++;
            }
        }
        sw.stop();
        writefln("%s in %d ms", cnt, sw.peek().msecs);
    }
}

