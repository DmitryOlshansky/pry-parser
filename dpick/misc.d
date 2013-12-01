module dpick.misc;

struct Stack(T)
{
@safe pure:
    enum growFixed = 8, growLinQ = 14, growLinD = 10;
    T[] data;
    size_t tip;
    @property bool empty(){ return tip == 0; }

    @property size_t length(){ return tip; }

    void push(T val) {
        if(tip == data.length)
            data.length = growFixed + data.length * growLinQ/growLinD;
        data[tip++] = val;
    }
    
    T pop()
    {
        assert(!empty);
        auto val = data[--tip];
        return val;
    }

    @property ref T top()
    {
        assert(!empty);
        return data[tip - 1]; 
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