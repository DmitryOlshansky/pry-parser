module dpick.buffer.stream;

import dpick.buffer.traits;

import std.range, std.algorithm, std.exception;

version(Windows)
struct Win32FileInput {
    @disable this(this);

    import std.conv;
    import core.sys.windows.windows;

    this(in char[] path) {
        this(to!wstring(path));
    }

    this(in wchar[] path) {
        const(wchar)[] result;        
        bool zStr = path.length && path[$-1] == 0;
        if(zStr)
            result = path;
        else {
            //TODO: any decent temporary allocator
            wchar[] buf = new wchar[path.length + (zStr ? 0 : 1)];
            ushort[] tail = repr(buf);
            copy(repr(path), tail);
            if(!zStr)
                tail[$-1] = 0;
            result = buf;
        }
        file = CreateFileW(
            result.ptr, GENERIC_READ, cast(uint)FILE_SHARE_READ, null,
            cast(uint)OPEN_EXISTING, cast(uint)FILE_ATTRIBUTE_NORMAL, null
        );
        enforce(file != INVALID_HANDLE_VALUE);
    }

    size_t read(ubyte[] dest){
        if (exhasted)
            return 0;
        size_t got;
        enforce(ReadFile(file, dest.ptr, dest.length, &got, null));
        if (got != dest.length)
            exhasted = true;
        return got;
    }

    void close(){
        if(file != INVALID_HANDLE_VALUE){
            enforce(CloseHandle(file));
            file = INVALID_HANDLE_VALUE;
        }
    }

    @property bool eof(){ return exhasted; }

    ~this(){
        close();
    }

private:
    static inout(ushort)[] repr(inout(wchar)[] arg) { 
        return cast(inout(ushort)[])arg;
    }
    HANDLE file = INVALID_HANDLE_VALUE;
    bool exhasted;
}

version(Posix)
struct PosixFileInput {
    import core.sys.posix.unistd : _read = read, _close = close;
    import core.sys.posix.fcntl;
    
    @disable this(this);
    
    this(string path) {
        import std.string : toStringz;
        file = open(toStringz(path), O_RDONLY);
        enforce(file >= 0);
    }

    size_t read(ubyte[] dest){
        if (exhasted)
            return 0;
        ptrdiff_t got;
        got = _read(file, dest.ptr, dest.length);
        enforce(got >= 0);
        if (got != dest.length)
            exhasted = true;
        return got;
    }

    void close(){
        if(file >= 0){
            enforce(_close(file) == 0);
            file = -1;
        }
    }

    @property bool eof(){ return exhasted; }

    ~this(){
        close();
    }
private:
    int file = -1;
    bool exhasted;
}

version(Windows)
    alias FileInput = Win32FileInput;
else version(Posix)
    alias FileInput = PosixFileInput;
else
    static assert("Unsupported platform");

/**
    Create an input stream from file, that directly uses system I/O calls.
    
    See also dpick.buffer.traits.isInputStream.
*/
auto fileInput(C)(in C[] path)
    if(is(C : dchar))
{
    import std.conv;
    version(Windows){
        static if(is(Unqual!C == wchar))
            return FileInput(path);
        else
            return FileInput(to!wstring(path));
    }
    else version(Posix){
        static if(is(Unqual!C == char))
            return FileInput(path);
        else
            return FileInput(to!string(path));
    }
    else
        static assert("Unsupported platform");
}

static assert(isInputStream!FileInput);
