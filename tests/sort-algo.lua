--
-- GSL shell interactive interface to GSL library
-- Based on the Lua programming language
--
-- Copyright (C) 2009-2013 Francesco Abbate.
-- See Copyright Notice in gsl-shell-jit.c
--
-- This file provided an implementation of the quicksort algorithm.
-- Based on the libstdc++ std::sort implementation included with GCC.
--

local bit = require 'bit'

local band, rshift = bit.band, bit.rshift

local insertion_thresold = 16

local function less_than(a, b)
    return a < b
end

local function lg2(a)
    local c = 0
    while a > 0 do
        a = rshift(a, 1)
        c = c + 1
    end
    return c - 1
end

local function div2(a)
    return rshift(a, 1)
end

local function heapsort(array, i0, i1, f)
    f = f or less_than

    local function push_heap(first, hole, top, value)
        local parent = div2(hole - 1)
        while hole > top and f(array[first + parent], value) do
            array[first + hole] = array[first + parent]
            hole = parent
            parent = div2(hole - 1)
        end
        array[first + hole] = value
    end

    local function adjust_heap(first, hole, len, value)
        local top = hole
        local second = hole
        while second < div2(len - 1) do
            second = 2 * (second + 1)
            if f(array[first + second], array[first + (second - 1)]) then
                second = second - 1
            end
            array[first + hole] = array[first + second]
            hole = second
        end
        if band(len, 1) == 0 and second == div2(len - 2) then
            second = 2 * (second + 1)
            array[first + hole] = array[first + (second - 1)]
            hole = second - 1
        end
        push_heap(first, hole, top, value)
    end

    local function pop_heap(first, last, result)
        local value = array[result]
        array[result] = array[first]
        adjust_heap(first, 0, last - first, value)
    end

    local function make_heap(first, last)
        if last - first < 2 then return end
        local len = last - first
        local parent = div2(len - 2)
        while true do
            local value = array[first + parent]
            adjust_heap(first, parent, len, value)
            if parent == 0 then
                return
            end
            parent = parent - 1
        end
    end

    local function heap_select(first, middle, last)
        make_heap(first, middle)
        for i = middle, last - 1 do
            if f(array[i], array[first]) then
                pop_heap(first, middle, i)
            end
        end
    end

    local function sort_heap(first, last)
        while last - first > 1 do
            last = last - 1
            pop_heap(first, last, last)
        end
    end

    heap_select(i0, i1 + 1, i1 + 1)
    sort_heap(i0, i1 + 1)
end

local function insertion_sort(array, compare, istart, iend)
    for i = istart + 1, iend do
        local current_value = array[i]
        local hole_index = i
        while hole_index > istart and compare(current_value, array[hole_index - 1]) do
            array[hole_index] = array[hole_index - 1]
            hole_index = hole_index - 1
        end
        array[hole_index] = current_value
    end
end

local function quicksort(array, i0, i1, f)
    f = f or less_than

    local function move_median_first(a, b, c)
        if f(array[a], array[b]) then
            if f(array[b], array[c]) then
                array[a], array[b] = array[b], array[a]
            else
                array[a], array[c] = array[c], array[a]
            end
        elseif f(array[a], array[c]) then
            return
        elseif f(array[b], array[c]) then
            array[a], array[c] = array[c], array[a]
        else
            array[a], array[b] = array[b], array[a]
        end
    end

    local function partition(first, last, pivot_value)
        while true do
            while f(array[first], pivot_value) do
                first = first + 1
            end
            while f(pivot_value, array[last]) do
                last = last - 1
            end
            if first >= last then
                return first
            end
            array[first], array[last] = array[last], array[first]
            first = first + 1
            last = last - 1
        end
    end

    local function partition_pivot(first, last)
        local mid = div2(first + last)
        move_median_first(first, mid, last)
        return partition(first + 1, last, array[first])
    end

    local function quicksort_loop(first, last, depth)
        while last - first > insertion_thresold do
            if depth == 0 then
                heapsort(array, first, last, f)
                return
            end
            depth = depth - 1
            local cut = partition_pivot(first, last)
            quicksort_loop(cut, last, depth)
            -- array[first], array[first + 1] = array[first + 1], array[first]
            last = cut - 1
        end
    end

    local complete = quicksort_loop(i0, i1, 2 * lg2(i1 - i0 + 1))
    insertion_sort(array, f, i0, i1)
end

local function array_search(array, i0, i1, val)
    for k = i0, i1 do
        if array[k] == val then return k end
    end
end

-- sort arrays "array" and "slave" in place for indices from i0 to i1
-- based on values of "array" using the comparison function "f"
local function quicksort_mirror(array, slave, i0, i1, f)

    local function swap(index, a, b)
        array[a], array[b] = array[b], array[a]
        slave[a], slave[b] = slave[b], slave[a]
        index[a], index[b] = index[b], index[a]
    end

    local n = i1 - i0 + 1
    local id, iv = {}, {}
    for k = 1, n do id[k], iv[k] = k, k end
    quicksort(id, i0, i1, function(a, b) return f(array[a], array[b]) end)
    for k = 1, n do
        local val = id[k]
        if val > k then
            swap(iv, k, val)
        elseif val < k then
            val = array_search(iv, k, n, val)
            swap(iv, k, val)
        end
    end
end

-- Use a very weak pseudo-number generator just for testing purpose.
local function my_random(s)
    s.x = (16807 * s.x) % 2147483647
    return s.x
end

local function test_sort(name, sort_fn, gen)
    local s = { x = 934 } -- random runmber generator
    print(string.format("******** %s **********", name))
    local x={}; for i = 1, 1000 do x[i] = gen(s, i) end
    quicksort(x, 1, #x)
    for k = 1, 1000 do print(x[k]) end
end

test_sort("QUICKSORT 1", quicksort, function(s) return my_random(s) % 65536 end)
test_sort("QUICKSORT 2", quicksort, function(s, i) return i end)
test_sort("QUICKSORT 3", quicksort, function(s, i) return 1000 - i end)
test_sort("HEAPSORT 1", heapsort, function(s) return my_random(s) % 65536 end)
test_sort("HEAPSORT 2", heapsort, function(s, i) return i end)
test_sort("HEAPSORT 3", heapsort, function(s, i) return 1000 - i end)
