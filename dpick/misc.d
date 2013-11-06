module dpick.misc;

struct Stack(T)
{
@safe pure:
    T[] data;
    @property bool empty(){ return data.empty; }

    @property size_t length(){ return data.length; }

    void push(T val){ data ~= val;  }
    
    T pop()
    {
        assert(!empty);
        auto val = data[$ - 1];
        data = data[0 .. $ - 1];
        return val;
    }

    @property ref T top()
    {
        assert(!empty);
        return data[$ - 1]; 
    }
}