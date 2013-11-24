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

struct BitMask(size_t bitSize)
{
@safe pure:
    uint[(1<<bitSize)/32] mask;

    void mark(int start, int end)
    {
        //TODO: optimize, e.g. can mark a word at at time
        for(int idx = start; idx<end; idx++)
        {
            mask[idx/32] |= 1<<(idx%32);
        }
    }

    uint opIndex(uint idx)
    {
        return mask[idx/32] & 1<<idx;
    }

    void invert()
    {
        foreach(ref w; mask)
            w = ~w;
    }
}